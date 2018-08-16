use std::thread;
use std::sync::{Arc, Mutex};

use websocket::OwnedMessage;
use websocket::sync::Server;

use std::iter::Iterator;

use language::*;

fn permuted<T: Clone>(values: &[T], ordering: &[usize]) -> Vec<T> {
    ordering.iter().map(|&ix| values[ix].clone()).collect()
}

impl Values {
    fn permuted(&self, ordering: &[usize]) -> Self {
        match self {
            &Values::Boolean(ref booleans) => Values::Boolean(permuted(booleans, ordering)),
            &Values::Integer(ref integers) => Values::Integer(permuted(integers, ordering)),
            &Values::String(ref strings) => Values::String(permuted(strings, ordering)),
            &Values::Any(ref values) => Values::Any(permuted(values, ordering)),
        }
    }
}

impl Relation {
    fn sorted(&self, ordering: &[usize]) -> Relation {
        let len = if self.columns.len() > 0 {
            self.columns[0].len()
        } else {
            0
        };
        let mut ixes = (0..len).collect::<Vec<_>>();
        for &c in ordering.iter().rev() {
            // stable sort
            ixes.sort_by(|&r1, &r2| {
                self.columns[c].get(r1).cmp(&self.columns[c].get(r2))
            });
        }
        let sorted_columns = self.columns
            .iter()
            .map(|column| column.permuted(&*ixes))
            .collect();
        Relation { columns: sorted_columns }
    }
}

pub fn gallop_le_inner<T1: ::std::borrow::Borrow<T2>, T2: Ord + ?Sized>(
    values: &[T1],
    mut lo: usize,
    hi: usize,
    value: &T2,
) -> usize {
    if lo < hi && values[lo].borrow() < value {
        let mut step = 1;
        while lo + step < hi && values[lo + step].borrow() < value {
            lo = lo + step;
            step = step << 1;
        }

        step = step >> 1;
        while step > 0 {
            if lo + step < hi && values[lo + step].borrow() < value {
                lo = lo + step;
            }
            step = step >> 1;
        }

        lo += 1
    }
    lo
}

pub fn gallop_leq_inner<T1: ::std::borrow::Borrow<T2>, T2: Ord + ?Sized>(
    values: &[T1],
    mut lo: usize,
    hi: usize,
    value: &T2,
) -> usize {
    if lo < hi && values[lo].borrow() <= value {
        let mut step = 1;
        while lo + step < hi && values[lo + step].borrow() <= value {
            lo = lo + step;
            step = step << 1;
        }

        step = step >> 1;
        while step > 0 {
            if lo + step < hi && values[lo + step].borrow() <= value {
                lo = lo + step;
            }
            step = step >> 1;
        }

        lo += 1
    }
    lo
}



fn gallop_le(values: &Values, lo: usize, hi: usize, value: &Value) -> usize {
    match (values, value) {
        (&Values::Boolean(ref bools), &Value::Boolean(ref bool)) => {
            gallop_le_inner(bools, lo, hi, bool)
        }
        (&Values::Integer(ref integers), &Value::Integer(ref integer)) => {
            gallop_le_inner(integers, lo, hi, integer)
        }
        (&Values::String(ref strings), &Value::String(ref string)) => {
            gallop_le_inner(strings, lo, hi, string.as_ref())
        }
        _ => panic!("Type error: gallop {} in {:?}", value, values),
    }
}

fn gallop_leq(values: &Values, lo: usize, hi: usize, value: &Value) -> usize {
    match (values, value) {
        (&Values::Boolean(ref bools), &Value::Boolean(ref bool)) => {
            gallop_leq_inner(bools, lo, hi, bool)
        }
        (&Values::Integer(ref integers), &Value::Integer(ref integer)) => {
            gallop_leq_inner(integers, lo, hi, integer)
        }
        (&Values::String(ref strings), &Value::String(ref string)) => {
            gallop_leq_inner(strings, lo, hi, string.as_ref())
        }
        _ => panic!("Type error: gallop {} in {:?}", value, values),
    }
}

struct Stack<'a> {
    ranges: Vec<LoHi>,
    variables: Vec<Value<'a>>,
    result_vars: Vec<(String, usize)>,
    results: Vec<Value<'static>>,
}

fn stage<'a>(
    constraints: &[Constraint],
    indexes: &'a [Relation],
) -> Box<FnMut(&mut Stack<'a>) -> Result<(), String> + 'a> {
    if constraints.len() > 0 {
        let mut tail = stage(&constraints[1..], indexes);
        let mut buffer = vec![(0, 0); indexes.len()];
        let mut local = vec![(0, 0); indexes.len()];
        match constraints[0].clone() {
            Constraint::Join(var_ix, result_already_fixed, rowcols) => {
                if result_already_fixed {
                    box move |stack| {

                        // copy previous state
                        for (i, &(row_ix, _)) in rowcols.iter().enumerate() {
                            buffer[i] = stack.ranges[row_ix];
                        }

                        // search in each of rowcols
                        let mut i = 0;
                        {
                            let value = &stack.variables[var_ix];
                            while i < rowcols.len() {
                                let (row_ix, col_ix) = rowcols[i];
                                let column = &indexes[row_ix].columns[col_ix];
                                let (old_lo, old_hi) = stack.ranges[row_ix];
                                let lo = gallop_le(column, old_lo, old_hi, value);
                                let hi = gallop_leq(column, lo, old_hi, value);
                                if lo < hi {
                                    stack.ranges[row_ix] = (lo, hi);
                                    i += 1;
                                } else {
                                    break;
                                }
                            }
                        }

                        // if all succeeded, continue with rest of constraints
                        if i == rowcols.len() {
                            tail(stack)?;
                        }

                        // restore previous state
                        for (i, &(row_ix, _)) in rowcols.iter().enumerate() {
                            stack.ranges[row_ix] = buffer[i];
                        }

                        Ok(())
                    }
                } else {
                    box move |stack| {

                        // copy previous state
                        for (i, &(row_ix, _)) in rowcols.iter().enumerate() {
                            buffer[i] = stack.ranges[row_ix];
                            local[i] = stack.ranges[row_ix];
                        }

                        // find smallest range
                        let (min_ix, &(row_ix, col_ix)) = rowcols
                            .iter()
                            .enumerate()
                            .min_by_key(|&(_, &(row_ix, _))| {
                                let (lo, hi) = stack.ranges[row_ix];
                                hi - lo
                            })
                            .unwrap();
                        let column = &indexes[row_ix].columns[col_ix];
                        let (old_lo, old_hi) = local[min_ix];
                        let mut lo = old_lo;

                        // loop over rowcols[min_ix]
                        while lo < old_hi {
                            let value = &column.get(lo);
                            let hi = gallop_leq(column, lo + 1, old_hi, value);
                            stack.ranges[row_ix] = (lo, hi);
                            {
                                // search in each of rowcols[-min_ix]
                                let mut i = 0;
                                while i < rowcols.len() {
                                    if i != min_ix {
                                        let (row_ix, col_ix) = rowcols[i];
                                        let column = &indexes[row_ix].columns[col_ix];
                                        let (old_lo, old_hi) = local[i];
                                        let lo = gallop_le(column, old_lo, old_hi, value);
                                        let hi = gallop_leq(column, lo, old_hi, value);
                                        if lo < hi {
                                            stack.ranges[row_ix] = (lo, hi);
                                            local[i] = (hi, old_hi);
                                        } else {
                                            break;
                                        }
                                    }
                                    i += 1;
                                }
                                // if all succeeded, continue with rest of constraints
                                if i == rowcols.len() {
                                    stack.variables[var_ix] = column.get(lo);
                                    tail(stack)?;
                                }
                            }
                            lo = hi;
                        }

                        // restore previous state
                        for (i, &(row_ix, _)) in rowcols.iter().enumerate() {
                            stack.ranges[row_ix] = buffer[i];
                        }

                        Ok(())
                    }
                }
            }
            Constraint::Apply(result_ix, result_already_fixed, function) => {
                if result_already_fixed {
                    box move |stack| {
                        let result = function.apply(&*stack.variables)?;
                        if stack.variables[result_ix] == result {
                            tail(stack)
                        } else {
                            Ok(())
                        }
                    }
                } else {
                    box move |stack| {
                        let result = function.apply(&*stack.variables)?;
                        stack.variables[result_ix] = result;
                        tail(stack)
                    }
                }
            }
        }
    } else {
        box move |stack| {
            for &(_, var_ix) in stack.result_vars.iter() {
                stack.results.push(
                    stack.variables[var_ix].really_to_owned(),
                );
            }
            Ok(())
        }
    }
}

type LoHi = (usize, usize);

fn constrain<'a>(
    constraints: &[Constraint],
    indexes: &'a [Relation],
    ranges: &mut [LoHi],
    locals: &mut [&mut [LoHi]],
    buffers: &mut [&mut [LoHi]],
    variables: &mut [Value<'a>],
    result_vars: &[(String, usize)],
    results: &mut Vec<Value>,
) -> Result<(), String> {
    if constraints.len() > 0 {
        let (buffer, other_buffers) = buffers.split_first_mut().unwrap();
        let (local, other_locals) = locals.split_first_mut().unwrap();
        match &constraints[0] {
            &Constraint::Join(var_ix, result_already_fixed, ref rowcols) => {
                if result_already_fixed {
                    // copy previous state
                    for (i, &(row_ix, _)) in rowcols.iter().enumerate() {
                        buffer[i] = ranges[row_ix];
                    }
                    // search in each of rowcols
                    let mut i = 0;
                    {
                        let value = &variables[var_ix];
                        while i < rowcols.len() {
                            let (row_ix, col_ix) = rowcols[i];
                            let column = &indexes[row_ix].columns[col_ix];
                            let (old_lo, old_hi) = ranges[row_ix];
                            let lo = gallop_le(column, old_lo, old_hi, value);
                            let hi = gallop_leq(column, lo, old_hi, value);
                            if lo < hi {
                                ranges[row_ix] = (lo, hi);
                                i += 1;
                            } else {
                                break;
                            }
                        }
                    }
                    // if all succeeded, continue with rest of constraints
                    if i == rowcols.len() {
                        constrain(
                            &constraints[1..],
                            indexes,
                            ranges,
                            other_locals,
                            other_buffers,
                            variables,
                            result_vars,
                            results,
                        )?;
                    }
                } else {
                    // copy previous state
                    for (i, &(row_ix, _)) in rowcols.iter().enumerate() {
                        buffer[i] = ranges[row_ix];
                        local[i] = ranges[row_ix];
                    }

                    // find smallest range
                    let (min_ix, &(row_ix, col_ix)) = rowcols
                        .iter()
                        .enumerate()
                        .min_by_key(|&(_, &(row_ix, _))| {
                            let (lo, hi) = ranges[row_ix];
                            hi - lo
                        })
                        .unwrap();
                    let column = &indexes[row_ix].columns[col_ix];
                    let (old_lo, old_hi) = local[min_ix];
                    let mut lo = old_lo;

                    // loop over rowcols[min_ix]
                    while lo < old_hi {
                        let value = &column.get(lo);
                        let hi = gallop_leq(column, lo + 1, old_hi, value);
                        ranges[row_ix] = (lo, hi);
                        {
                            // search in each of rowcols[-min_ix]
                            let mut i = 0;
                            while i < rowcols.len() {
                                if i != min_ix {
                                    let (row_ix, col_ix) = rowcols[i];
                                    let column = &indexes[row_ix].columns[col_ix];
                                    let (old_lo, old_hi) = local[i];
                                    let lo = gallop_le(column, old_lo, old_hi, value);
                                    let hi = gallop_leq(column, lo, old_hi, value);
                                    if lo < hi {
                                        ranges[row_ix] = (lo, hi);
                                        local[i] = (hi, old_hi);
                                    } else {
                                        break;
                                    }
                                }
                                i += 1;
                            }
                            // if all succeeded, continue with rest of constraints
                            if i == rowcols.len() {
                                variables[var_ix] = column.get(lo);
                                constrain(
                                    &constraints[1..],
                                    indexes,
                                    ranges,
                                    other_locals,
                                    other_buffers,
                                    variables,
                                    result_vars,
                                    results,
                                )?;
                            }
                        }
                        lo = hi;
                    }
                }
                // restore previous state
                for (i, &(row_ix, _)) in rowcols.iter().enumerate() {
                    ranges[row_ix] = buffer[i];
                }
            }
            &Constraint::Apply(result_ix, result_already_fixed, ref function) => {
                let result = function.apply(variables)?;
                if result_already_fixed {
                    if variables[result_ix] == result {
                        constrain(
                            &constraints[1..],
                            indexes,
                            ranges,
                            other_locals,
                            other_buffers,
                            variables,
                            result_vars,
                            results,
                        )?;
                    } else {
                        // failed, backtrack
                    }
                } else {
                    variables[result_ix] = result;
                    constrain(
                        &constraints[1..],
                        indexes,
                        ranges,
                        other_locals,
                        other_buffers,
                        variables,
                        result_vars,
                        results,
                    )?;
                }
            }
        }
    } else {
        for &(_, var_ix) in result_vars.iter() {
            results.push(variables[var_ix].really_to_owned());
        }
    }
    Ok(())
}

pub struct Prepared {
    pub indexes: Vec<Relation>,
    pub ranges: Vec<LoHi>,
    pub locals: Vec<LoHi>,
    pub buffers: Vec<LoHi>,
}

pub fn prepare_block(block: &Block, db: &DB) -> Result<Prepared, String> {
    let mut indexes: Vec<Relation> = vec![];
    time!(
        "indexing",
        for (name, ordering) in block.row_names.iter().zip(block.row_orderings.iter()) {
            indexes.push(
                db.relations
                    .get(name)
                    .ok_or_else(|| format!("Couldn't find relation: {}", name))?
                    .sorted(ordering),
            )
        }
    );
    let ranges: Vec<LoHi> = indexes
        .iter()
        .map(|index| (0, index.columns[0].len()))
        .collect();
    let buffers: Vec<LoHi> = vec![(0, 0); indexes.len() * block.constraints.len()];
    let locals = buffers.clone();
    Ok(Prepared {
        indexes,
        ranges,
        locals,
        buffers,
    })
}

pub fn run_block(block: &Block, prepared: &mut Prepared) -> Result<Vec<Value<'static>>, String> {
    let &mut Prepared {
        ref indexes,
        ref mut ranges,
        ref mut locals,
        ref mut buffers,
    } = prepared;
    let mut buffers: Vec<&mut [LoHi]> = buffers.chunks_mut(indexes.len()).collect();
    let mut locals: Vec<&mut [LoHi]> = locals.chunks_mut(indexes.len()).collect();
    let mut results = vec![];
    time!(
        "query",
        constrain(
            &*block.constraints,
            &*indexes,
            &mut *ranges,
            &mut *locals,
            &mut *buffers,
            &mut block.variables.clone(),
            &*block.result_vars,
            &mut results,
        )?
    );
    Ok(results)
}

pub fn run_staged_block(block: &Block, prepared: &Prepared) -> Result<Vec<Value<'static>>, String> {
    let mut stack = Stack {
        ranges: prepared.ranges.clone(),
        variables: block.variables.clone(),
        result_vars: block.result_vars.clone(),
        results: vec![],
    };
    let mut staged = time!("stage", stage(&*block.constraints, &*prepared.indexes));
    time!("query", staged(&mut stack)?);
    Ok(stack.results)
}

pub fn run_code(db: &DB, code: &str, cursor: i64) {
    let code_ast = code_ast(code, cursor);

    let mut status: Vec<Result<(Block, Vec<Value>), String>> = vec![];

    // TODO bring back when output works
    // for block in code_ast.blocks.iter() {
    for block in code_ast.focused.iter().map(|ix| &code_ast.blocks[*ix]) {
        match block {
            &Err(ref error) => status.push(Err(format!("Parse error: {}", error))),
            &Ok(ref block) => {
                // print!("{:?}\n\n", block);
                match plan(block) {
                    Err(error) => status.push(Err(format!("Compile error: {}", error))),
                    Ok(block) => {
                        // print!("{:?}\n\n", block);
                        let prepared = prepare_block(&block, db);
                        match prepared {
                            Err(error) => status.push(Err(format!("Prepare error: {}", error))),
                            Ok(mut prepared) => {
                                match run_staged_block(&block, &mut prepared) {
                                    // match run_block(&block, &mut prepared) {
                                    Err(error) => status.push(Err(format!("Run error: {}", error))),
                                    Ok(results) => {
                                        status.push(Ok((block, results)));
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    if let Some(ix) = code_ast.focused {
        // TODO bring back when output works
        // match &status[ix] {
        match &status[0] {
            &Err(ref error) => print!("{}\n\n", error),
            &Ok((ref block, ref results)) => {
                let result_vars = &block.result_vars;

                print!(
                    "Ok: {} results\n\n",
                    if result_vars.len() > 0 {
                        results.len() / result_vars.len()
                    } else {
                        0
                    }
                );

                if result_vars.len() > 0 {
                    for (i, row) in results.chunks(result_vars.len()).take(10).enumerate() {
                        for (&(ref name, _), value) in result_vars.iter().zip(row.iter()) {
                            print!("{}={}\t", name, value);
                        }
                        if i == 9 {
                            print!("...\n");
                        } else {
                            print!("\n");
                        }
                    }
                    print!("\n");
                }

                // print!("{:?}\n\n{:?}\n\n", code_ast.blocks[ix], block);
            }
        }

    } else {
        print!("Nothing focused\n\n");
    }
}

#[derive(Debug, Serialize, Deserialize)]
enum EditorEvent {
    State(String, i64),
}

// #[derive(Debug, Serialize, Deserialize)]
// enum Command {
//     Render(String),
// }

// fn send_command(sender: &mut websocket::sender::Writer<std::net::TcpStream>, c: Command) {
//     sender
//         .send_message(&OwnedMessage::Text(json!(c).to_string()))
//         .unwrap()
// }

pub fn serve_editor(db: DB) {
    println!("Tables: {:?}", db.relations.keys().collect::<Vec<_>>());

    let state = Arc::new(Mutex::new(("".to_owned(), 0)));

    let server = Server::bind("127.0.0.1:8081").unwrap();

    thread::spawn({
        let state = state.clone();
        move || {
            let mut last_state = state.lock().unwrap().clone();
            loop {
                let state: (String, i64) = state.lock().unwrap().clone();
                if state != last_state {
                    print!("\x1b[2J\x1b[1;1H");
                    let (ref code, cursor) = state;
                    time!("full run", {
                        run_code(&db, &*code, cursor)
                    });
                    last_state = state.clone();
                }
            }
        }
    });

    for request in server.filter_map(Result::ok) {
        let state = state.clone();
        thread::spawn(move || {
            let client = request.accept().unwrap();
            let ip = client.peer_addr().unwrap();
            println!("Connection from {}", ip);

            let (mut receiver, mut sender) = client.split().unwrap();

            for message in receiver.incoming_messages() {
                let message = message.unwrap();

                match message {
                    OwnedMessage::Close(_) => {
                        let message = OwnedMessage::Close(None);
                        sender.send_message(&message).unwrap();
                        println!("Client {} disconnected", ip);
                        return;
                    }
                    OwnedMessage::Ping(ping) => {
                        let message = OwnedMessage::Pong(ping);
                        sender.send_message(&message).unwrap();
                    }
                    OwnedMessage::Text(ref text) => {
                        // println!("Received: {}", text);
                        let event: EditorEvent = ::serde_json::from_str(text).unwrap();
                        match event {
                            EditorEvent::State(code, cursor) => {
                                *state.lock().unwrap() = (code, cursor);
                            }
                        }
                    }
                    _ => {
                        panic!("A weird message! {:?}", message);
                    }
                }
            }
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        let col = Values::Integer((0..1000).collect());
        for i in 0..1000 {
            assert_eq!(
                col.get(gallop_le(&col, 0, 1000, &Value::Integer(i))),
                Value::Integer(i)
            );
        }
        assert_eq!(gallop_le(&col, 0, col.len(), &Value::Integer(-1)), 0);
        assert_eq!(gallop_le(&col, 0, col.len(), &Value::Integer(1000)), 1000);

        for i in 0..999 {
            assert_eq!(
                col.get(gallop_leq(&col, 0, col.len(), &Value::Integer(i))),
                Value::Integer(i + 1)
            );
        }
        assert_eq!(gallop_leq(&col, 0, col.len(), &Value::Integer(-1)), 0);
        assert_eq!(gallop_leq(&col, 0, col.len(), &Value::Integer(999)), 1000);
    }
}
