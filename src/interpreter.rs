use std::thread;
use std::sync::{Arc, Mutex};

use websocket::OwnedMessage;
use websocket::sync::Server;

use std::iter::Iterator;
use std::cell::{Cell, RefCell};
use std::cmp::Ordering;

use language::*;

pub fn gallop<T, F>(values: &[T], mut lo: usize, hi: usize, f: F) -> usize
where
    F: Fn(&T) -> bool,
{
    if lo < hi && f(&values[lo]) {
        let mut step = 1;
        while lo + step < hi && f(&values[lo + step]) {
            lo = lo + step;
            step = step << 1;
        }

        step = step >> 1;
        while step > 0 {
            if lo + step < hi && f(&values[lo + step]) {
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
            gallop(bools, lo, hi, |t| t < bool)
        }
        (&Values::Integer(ref integers), &Value::Integer(ref integer)) => {
            gallop(integers, lo, hi, |t| t < integer)
        }
        (&Values::String(ref strings), &Value::String(ref string)) => {
            let string = string.as_ref();
            gallop(strings, lo, hi, |t| &**t < string)
        }
        _ => panic!("Type error: gallop {} in {:?}", value, values),
    }
}

fn gallop_leq(values: &Values, lo: usize, hi: usize, value: &Value) -> usize {
    match (values, value) {
        (&Values::Boolean(ref bools), &Value::Boolean(ref bool)) => {
            gallop(bools, lo, hi, |t| t <= bool)
        }
        (&Values::Integer(ref integers), &Value::Integer(ref integer)) => {
            gallop(integers, lo, hi, |t| t <= integer)
        }
        (&Values::String(ref strings), &Value::String(ref string)) => {
            let string = string.as_ref();
            gallop(strings, lo, hi, |t| &**t <= string)
        }
        _ => panic!("Type error: gallop {} in {:?}", value, values),
    }
}

fn smaller((a_lo, a_hi): (usize, usize), (b_lo, b_hi): (usize, usize)) -> bool {
    (a_hi - a_lo) < (b_hi - b_lo)
}

pub fn narrow<T, Cmp, F>(values: &[T], (lo, hi): (usize, usize), cmp: Cmp, mut f: F)
where
    Cmp: Fn(&T) -> Ordering,
    F: FnMut((usize, usize)),
{
    let lo = gallop(values, lo, hi, |v| cmp(v) == Ordering::Less);
    let hi = gallop(values, lo, hi, |v| cmp(v) != Ordering::Greater);
    if lo < hi {
        f((lo, hi))
    }
}

pub fn join1<T, F>(a: &[T], (mut a_lo, a_hi): (usize, usize), mut f: F)
where
    T: Ord,
    F: FnMut((usize, usize)),
{
    while a_lo < a_hi {
        let value = &a[a_lo];
        let a_next_lo = gallop(a, a_lo, a_hi, |v| v < value);
        f((a_lo, a_next_lo));
        a_lo = a_next_lo;
    }
}

pub fn join_inner2<T, F>(
    a: &[T],
    b: &[T],
    a_range: (usize, usize),
    b_range: (usize, usize),
    mut f: F,
) where
    T: Ord,
    F: FnMut((usize, usize), (usize, usize)),
{
    let mut b_lo = b_range.0;
    join1(a, a_range, |a_range| {
        let value = &a[a_range.0];
        narrow(b, (b_lo, b_range.1), |v| v.cmp(value), |b_range| {
            f(a_range, b_range);
            b_lo = b_range.0;
        });
    });
}

pub fn join_inner3<T, F>(
    a: &[T],
    b: &[T],
    c: &[T],
    a_range: (usize, usize),
    b_range: (usize, usize),
    c_range: (usize, usize),
    mut f: F,
) where
    T: Ord,
    F: FnMut((usize, usize), (usize, usize), (usize, usize)),
{
    let mut c_lo = c_range.0;
    join_inner2(a, b, a_range, b_range, |a_range, b_range| {
        let value = &a[a_range.0];
        narrow(c, (c_lo, c_range.1), |v| v.cmp(value), |c_range| {
            f(a_range, b_range, c_range);
            c_lo = c_range.0;
        });
    });
}

pub fn join_inner4<T, F>(
    a: &[T],
    b: &[T],
    c: &[T],
    d: &[T],
    a_range: (usize, usize),
    b_range: (usize, usize),
    c_range: (usize, usize),
    d_range: (usize, usize),
    mut f: F,
) where
    T: Ord,
    F: FnMut((usize, usize),
          (usize, usize),
          (usize, usize),
          (usize, usize)),
{
    let mut d_lo = d_range.0;
    join_inner3(
        a,
        b,
        c,
        a_range,
        b_range,
        c_range,
        |a_range, b_range, c_range| {
            let value = &a[a_range.0];
            narrow(d, (d_lo, d_range.1), |v| v.cmp(value), |d_range| {
                f(a_range, b_range, c_range, d_range);
                d_lo = d_range.0;
            });
        },
    );
}

pub fn join2<T, F>(a: &[T], b: &[T], a_range: (usize, usize), b_range: (usize, usize), mut f: F)
where
    T: Ord,
    F: FnMut((usize, usize), (usize, usize)),
{
    if smaller(a_range, b_range) {
        join_inner2(
            a,
            b,
            a_range,
            b_range,
            |a_range, b_range| { f(a_range, b_range); },
        );
    } else {
        join_inner2(
            b,
            a,
            b_range,
            a_range,
            |b_range, a_range| { f(a_range, b_range); },
        );
    }
}

pub fn join3<T, F>(
    a: &[T],
    b: &[T],
    c: &[T],
    a_range: (usize, usize),
    b_range: (usize, usize),
    c_range: (usize, usize),
    mut f: F,
) where
    T: Ord,
    F: FnMut((usize, usize), (usize, usize), (usize, usize)),
{
    if smaller(a_range, b_range) && smaller(a_range, c_range) {
        join_inner3(
            a,
            b,
            c,
            a_range,
            b_range,
            c_range,
            |a_range, b_range, c_range| { f(a_range, b_range, c_range); },
        );
    } else if smaller(b_range, a_range) && smaller(b_range, c_range) {
        join_inner3(
            b,
            a,
            c,
            b_range,
            a_range,
            c_range,
            |b_range, a_range, c_range| { f(a_range, b_range, c_range); },
        );
    } else {
        join_inner3(
            c,
            b,
            a,
            c_range,
            b_range,
            a_range,
            |c_range, b_range, a_range| { f(c_range, b_range, a_range); },
        );
    }
}

pub fn join4<T, F>(
    a: &[T],
    b: &[T],
    c: &[T],
    d: &[T],
    a_range: (usize, usize),
    b_range: (usize, usize),
    c_range: (usize, usize),
    d_range: (usize, usize),
    mut f: F,
) where
    T: Ord,
    F: FnMut((usize, usize),
          (usize, usize),
          (usize, usize),
          (usize, usize)),
{
    if smaller(a_range, b_range) && smaller(a_range, c_range) && smaller(a_range, d_range) {
        join_inner4(a, b, c, d, a_range, b_range, c_range, d_range, |a_range,
         b_range,
         c_range,
         d_range| {
            f(a_range, b_range, c_range, d_range);
        });
    } else if smaller(b_range, a_range) && smaller(b_range, c_range) && smaller(b_range, d_range) {
        join_inner4(b, a, c, d, b_range, a_range, c_range, d_range, |b_range,
         a_range,
         c_range,
         d_range| {
            f(b_range, a_range, c_range, d_range);
        });
    } else if smaller(c_range, a_range) && smaller(c_range, b_range) && smaller(c_range, d_range) {
        join_inner4(c, b, a, d, c_range, b_range, a_range, d_range, |c_range,
         b_range,
         a_range,
         d_range| {
            f(a_range, b_range, c_range, d_range);
        });
    } else {
        join_inner4(d, b, c, a, d_range, b_range, c_range, a_range, |d_range,
         b_range,
         c_range,
         a_range| {
            f(a_range, b_range, c_range, d_range);
        });
    }
}

#[derive(Debug, Clone)]
pub enum Variable<'a> {
    Boolean(Cell<bool>),
    Integer(Cell<i64>),
    String(Cell<&'a str>),
}

#[derive(Debug, Clone)]
struct State<'a> {
    indexes: &'a [Relation],
    ranges: Vec<Cell<LoHi>>,
    variables: Vec<Variable<'a>>,
}

type Staged<'a> = Box<FnMut() -> () + 'a>;

fn stage_narrow<'a, T, Cmp>(
    values: &'a [T],
    range: &'a Cell<LoHi>,
    cmp: Cmp,
    mut tail: Staged<'a>,
) -> Staged<'a>
where
    Cmp: Fn(&T) -> Ordering + 'a,
{
    box move || {
        let old_range = range.get();
        narrow(values, old_range, cmp, |new_range| {
            range.set(new_range);
            tail();
            range.set(old_range);
        });
    }
}

fn stage_join1<'a, T, SetVariable>(
    c0: &'a [T],
    range0: &'a Cell<LoHi>,
    set_variable: SetVariable,
    tail: Staged<'a>,
) -> Staged<'a>
where
    T: Ord,
    SetVariable: Fn(&T) + 'a,
{
    box move || {
        let old_range0 = range0.get();
        join1(c0, old_range0, |new_range0| {
            range0.set(new_range0);
            set_variable(&c0[new_range0.0]);
            tail();
            range0.set(old_range0);
        });
    }
}

fn stage_join2<'a, T, SetVariable>(
    c0: &'a [T],
    c1: &'a [T],
    range0: &'a Cell<LoHi>,
    range1: &'a Cell<LoHi>,
    set_variable: SetVariable,
    tail: Staged<'a>,
) -> Staged<'a>
where
    T: Ord,
    SetVariable: Fn(T) + 'a,
{
    box move || {
        let old_range0 = range0.get();
        let old_range1 = range1.get();
        join2(c0, c1, old_range0, old_range1, |new_range0, new_range1| {
            range0.set(new_range0);
            range1.set(new_range1);
            set_variable(c0[new_range0.0]);
            tail();
            range0.set(old_range0);
            range1.set(old_range1);
        });
    }
}

fn stage_join3<'a, T, SetVariable>(
    c0: &'a [T],
    c1: &'a [T],
    c2: &'a [T],
    range0: &'a Cell<LoHi>,
    range1: &'a Cell<LoHi>,
    range2: &'a Cell<LoHi>,
    set_variable: SetVariable,
    tail: Staged<'a>,
) -> Staged<'a>
where
    T: Ord,
    SetVariable: Fn(T) + 'a,
{
    box move || {
        let old_range0 = range0.get();
        let old_range1 = range1.get();
        let old_range2 = range2.get();
        join3(c0, c1, c2, old_range0, old_range1, old_range2, |new_range0,
         new_range1,
         new_range2| {
            range0.set(new_range0);
            range1.set(new_range1);
            range2.set(new_range2);
            set_variable(c0[new_range0.0]);
            tail();
            range0.set(old_range0);
            range1.set(old_range1);
            range2.set(old_range2);
        });
    }
}

fn stage_join4<'a, T, SetVariable>(
    c0: &'a [T],
    c1: &'a [T],
    c2: &'a [T],
    c3: &'a [T],
    range0: &'a Cell<LoHi>,
    range1: &'a Cell<LoHi>,
    range2: &'a Cell<LoHi>,
    range3: &'a Cell<LoHi>,
    set_variable: SetVariable,
    tail: Staged<'a>,
) -> Staged<'a>
where
    T: Ord,
    SetVariable: Fn(T) + 'a,
{
    box move || {
        let old_range0 = range0.get();
        let old_range1 = range1.get();
        let old_range2 = range2.get();
        let old_range3 = range3.get();
        join4(
            c0,
            c1,
            c2,
            c3,
            old_range0,
            old_range1,
            old_range2,
            old_range3,
            |new_range0, new_range1, new_range2, new_range3| {
                range0.set(new_range0);
                range1.set(new_range1);
                range2.set(new_range2);
                range3.set(new_range3);
                set_variable(c0[new_range0.0]);
                tail();
                range0.set(old_range0);
                range1.set(old_range1);
                range2.set(old_range2);
                range3.set(old_range3);
            },
        );
    }
}

impl Function {
    pub fn stage<'a>(&self, inputs: &'a [Variable<'a>], output: &'a Variable<'a>) -> Staged<'a> {
        match self {
            &Function::Add(a, b) => {
                match (&inputs[a], &inputs[b], output) {
                    (&Variable::Integer(ref a),
                     &Variable::Integer(ref b),
                     &Variable::Integer(ref output)) => box move || output.set(a.get() + b.get()),
                    (a, b, _) => panic!("Type error: {:?} + {:?}", a, b),
                }
            }
            &Function::Mul(a, b) => {
                match (&inputs[a], &inputs[b], output) {
                    (&Variable::Integer(ref a),
                     &Variable::Integer(ref b),
                     &Variable::Integer(ref output)) => box move || output.set(a.get() * b.get()),
                    (a, b, _) => panic!("Type error: {:?} * {:?}", a, b),
                }
            }
            &Function::Magic(a, b) => {
                match (&inputs[a], &inputs[b], output) {
                    (&Variable::Integer(ref a),
                     &Variable::Integer(ref b),
                     &Variable::Integer(ref output)) => {
                        box move || {
                            output.set(
                                (a.get() * a.get()) + (b.get() * b.get()) +
                                    (3 * a.get() * b.get()),
                            )
                        }
                    }
                    (a, b, _) => panic!("Type error: magic({:?},{:?})", a, b),
                }
            }
            &Function::Contains(a, b) => {
                match (&inputs[a], &inputs[b], output) {
                    (&Variable::String(ref a),
                     &Variable::String(ref b),
                     &Variable::Boolean(ref output)) => {
                        box move || output.set(a.get().contains(b.get()))
                    }
                    (a, b, _) => panic!("Type error: contains({:?}, {:?})", a, b),
                }
            }
            &Function::And(a, b) => {
                match (&inputs[a], &inputs[b], output) {
                    (&Variable::Boolean(ref a),
                     &Variable::Boolean(ref b),
                     &Variable::Boolean(ref output)) => box move || output.set(a.get() && b.get()),
                    (a, b, _) => panic!("Type error: {:?} && {:?}", a, b),
                }
            }
            &Function::Or(a, b) => {
                match (&inputs[a], &inputs[b], output) {
                    (&Variable::Boolean(ref a),
                     &Variable::Boolean(ref b),
                     &Variable::Boolean(ref output)) => box move || output.set(a.get() && b.get()),
                    (a, b, _) => panic!("Type error: {:?} || {:?}", a, b),
                }
            }
            &Function::Not(a) => {
                match (&inputs[a], output) {
                    (&Variable::Boolean(ref a), &Variable::Boolean(ref output)) => {
                        box move || output.set(!a.get())
                    }
                    (a, _) => panic!("Type error: !{:?}", a),
                }
            }
            &Function::Leq(a, b) => {
                match (&inputs[a], &inputs[b], output) {
                    (&Variable::Boolean(ref a),
                     &Variable::Boolean(ref b),
                     &Variable::Boolean(ref output)) => box move || output.set(a.get() <= b.get()),
                    (&Variable::Integer(ref a),
                     &Variable::Integer(ref b),
                     &Variable::Boolean(ref output)) => box move || output.set(a.get() <= b.get()),
                    (&Variable::String(ref a),
                     &Variable::String(ref b),
                     &Variable::Boolean(ref output)) => box move || output.set(a.get() <= b.get()),
                    (a, b, _) => panic!("Type error: {:?} <= {:?}", a, b),
                }
            }
            &Function::Le(a, b) => {
                match (&inputs[a], &inputs[b], output) {
                    (&Variable::Boolean(ref a),
                     &Variable::Boolean(ref b),
                     &Variable::Boolean(ref output)) => box move || output.set(a.get() < b.get()),
                    (&Variable::Integer(ref a),
                     &Variable::Integer(ref b),
                     &Variable::Boolean(ref output)) => box move || output.set(a.get() < b.get()),
                    (&Variable::String(ref a),
                     &Variable::String(ref b),
                     &Variable::Boolean(ref output)) => box move || output.set(a.get() < b.get()),
                    (a, b, _) => panic!("Type error: {:?} < {:?}", a, b),
                }
            }
            &Function::Geq(a, b) => {
                match (&inputs[a], &inputs[b], output) {
                    (&Variable::Boolean(ref a),
                     &Variable::Boolean(ref b),
                     &Variable::Boolean(ref output)) => box move || output.set(a.get() >= b.get()),
                    (&Variable::Integer(ref a),
                     &Variable::Integer(ref b),
                     &Variable::Boolean(ref output)) => box move || output.set(a.get() >= b.get()),
                    (&Variable::String(ref a),
                     &Variable::String(ref b),
                     &Variable::Boolean(ref output)) => box move || output.set(a.get() >= b.get()),
                    (a, b, _) => panic!("Type error: {:?} >= {:?}", a, b),
                }
            }
            &Function::Ge(a, b) => {
                match (&inputs[a], &inputs[b], output) {
                    (&Variable::Boolean(ref a),
                     &Variable::Boolean(ref b),
                     &Variable::Boolean(ref output)) => box move || output.set(a.get() > b.get()),
                    (&Variable::Integer(ref a),
                     &Variable::Integer(ref b),
                     &Variable::Boolean(ref output)) => box move || output.set(a.get() > b.get()),
                    (&Variable::String(ref a),
                     &Variable::String(ref b),
                     &Variable::Boolean(ref output)) => box move || output.set(a.get() > b.get()),
                    (a, b, _) => panic!("Type error: {:?} >= {:?}", a, b),
                }
            }
            &Function::Eq(a, b) => {
                match (&inputs[a], &inputs[b], output) {
                    (&Variable::Boolean(ref a),
                     &Variable::Boolean(ref b),
                     &Variable::Boolean(ref output)) => box move || output.set(a.get() == b.get()),
                    (&Variable::Integer(ref a),
                     &Variable::Integer(ref b),
                     &Variable::Boolean(ref output)) => box move || output.set(a.get() == b.get()),
                    (&Variable::String(ref a),
                     &Variable::String(ref b),
                     &Variable::Boolean(ref output)) => box move || output.set(a.get() == b.get()),
                    (a, b, _) => panic!("Type error: {:?} == {:?}", a, b),
                }
            }
        }
    }
}

fn stage_apply<'a, T>(
    result_already_fixed: bool,
    output: &'a Cell<T>,
    variable: &'a Cell<T>,
    staged_function: Staged<'a>,
    tail: Staged<'a>,
) -> Staged<'a>
where
    T: Copy + Eq,
{
    if result_already_fixed {
        box || {
            staged_function();
            if output.get() == variable.get() {
                tail();
            }
        }
    } else {
        box || {
            staged_function();
            variable.set(output.get());
        }
    }
}

impl Constraint {
    fn stage<'a>(&self, state: &'a State<'a>, mut tail: Staged<'a>) -> Staged<'a> {
        match self.clone() {
            Constraint::Join(var_ix, result_already_fixed, rowcols) => {
                if result_already_fixed {
                    for &(row, col) in rowcols.iter() {
                        tail = match (&state.indexes[row].columns[col], &state.variables[var_ix]) {
                            (&Values::Boolean(ref values), &Variable::Boolean(ref variable)) => {
                                stage_narrow(
                                    values,
                                    &state.ranges[row],
                                    |v| v.cmp(&variable.get()),
                                    tail,
                                )
                            }
                            (&Values::Integer(ref values), &Variable::Integer(ref variable)) => {
                                stage_narrow(
                                    values,
                                    &state.ranges[row],
                                    |v| v.cmp(&variable.get()),
                                    tail,
                                )
                            }
                            (&Values::String(ref values), &Variable::String(ref variable)) => {
                                stage_narrow(
                                    values,
                                    &state.ranges[row],
                                    |v| (**v).cmp(variable.get()),
                                    tail,
                                )
                            }
                            _ => panic!(),
                        }
                    }
                    tail
                } else {
                    let columns: Vec<&Values> = rowcols
                        .iter()
                        .map(|&(row, col)| &state.indexes[row].columns[col])
                        .collect();
                    match (&*columns, &state.variables[var_ix]) {
                        (&[&Values::Boolean(ref c0)], &Variable::Boolean(ref variable)) => {
                            stage_join1(c0, &state.ranges[rowcols[0].0], |t| variable.set(t), tail)
                        }
                        (&[&Values::Integer(ref c0)], &Variable::Integer(ref variable)) => {
                            stage_join1(c0, &state.ranges[rowcols[0].0], |t| variable.set(t), tail)
                        }
                        (&[&Values::String(ref c0)], &Variable::String(ref variable)) => {
                            stage_join1(
                                c0,
                                &state.ranges[rowcols[0].0],
                                |t| variable.set(&*t),
                                tail,
                            )
                        }
                        (&[&Values::Boolean(ref c0), &Values::Boolean(ref c1)],
                         &Variable::Boolean(ref variable)) => {
                            stage_join2(
                                c0,
                                c1,
                                &state.ranges[rowcols[0].0],
                                &state.ranges[rowcols[1].0],
                                |t| variable.set(t),
                                tail,
                            )
                        }
                        (&[&Values::Integer(ref c0), &Values::Integer(ref c1)],
                         &Variable::Integer(ref variable)) => {
                            stage_join2(
                                c0,
                                c1,
                                &state.ranges[rowcols[0].0],
                                &state.ranges[rowcols[1].0],
                                |t| variable.set(t),
                                tail,
                            )
                        }
                        (&[&Values::String(ref c0), &Values::String(ref c1)],
                         &Variable::String(ref variable)) => {
                            stage_join2(
                                c0,
                                c1,
                                &state.ranges[rowcols[0].0],
                                &state.ranges[rowcols[1].0],
                                |t| variable.set(&*t),
                                tail,
                            )
                        }
                        (&[&Values::Boolean(ref c0), &Values::Boolean(ref c1), &Values::Boolean(ref c2)],
                         &Variable::Boolean(ref variable)) => {
                            stage_join3(
                                c0,
                                c1,
                                c2,
                                &state.ranges[rowcols[0].0],
                                &state.ranges[rowcols[1].0],
                                &state.ranges[rowcols[2].0],
                                |t| variable.set(t),
                                tail,
                            )
                        }
                        (&[&Values::Integer(ref c0), &Values::Integer(ref c1), &Values::Integer(ref c2)],
                         &Variable::Integer(ref variable)) => {
                            stage_join3(
                                c0,
                                c1,
                                c2,
                                &state.ranges[rowcols[0].0],
                                &state.ranges[rowcols[1].0],
                                &state.ranges[rowcols[2].0],
                                |t| variable.set(t),
                                tail,
                            )
                        }
                        (&[&Values::String(ref c0), &Values::String(ref c1), &Values::String(ref c2)],
                         &Variable::String(ref variable)) => {
                            stage_join3(
                                c0,
                                c1,
                                c2,
                                &state.ranges[rowcols[0].0],
                                &state.ranges[rowcols[1].0],
                                &state.ranges[rowcols[2].0],
                                |t| variable.set(&*t),
                                tail,
                            )
                        }
                        (&[&Values::Boolean(ref c0), &Values::Boolean(ref c1), &Values::Boolean(ref c2), &Values::Boolean(ref c3)],
                         &Variable::Boolean(ref variable)) => {
                            stage_join4(
                                c0,
                                c1,
                                c2,
                                c3,
                                &state.ranges[rowcols[0].0],
                                &state.ranges[rowcols[1].0],
                                &state.ranges[rowcols[2].0],
                                &state.ranges[rowcols[3].0],
                                |t| variable.set(t),
                                tail,
                            )
                        }
                        (&[&Values::Integer(ref c0), &Values::Integer(ref c1), &Values::Integer(ref c2), &Values::Integer(ref c3)],
                         &Variable::Integer(ref variable)) => {
                            stage_join4(
                                c0,
                                c1,
                                c2,
                                c3,
                                &state.ranges[rowcols[0].0],
                                &state.ranges[rowcols[1].0],
                                &state.ranges[rowcols[2].0],
                                &state.ranges[rowcols[3].0],
                                |t| variable.set(t),
                                tail,
                            )
                        }
                        (&[&Values::String(ref c0), &Values::String(ref c1), &Values::String(ref c2), &Values::String(ref c3)],
                         &Variable::String(ref variable)) => {
                            stage_join4(
                                c0,
                                c1,
                                c2,
                                c3,
                                &state.ranges[rowcols[0].0],
                                &state.ranges[rowcols[1].0],
                                &state.ranges[rowcols[2].0],
                                &state.ranges[rowcols[3].0],
                                |t| variable.set(&*t),
                                tail,
                            )
                        }
                        _ => panic!(),
                    }
                }
            }
            Constraint::Apply(result_ix, result_already_fixed, function) => {
                let variable = &state.variables[result_ix];
                match (function.output_kind(), variable) {
                    (Kind::Boolean, &Variable::Boolean(ref variable)) => {
                        let output = Cell::new(false);
                        stage_apply(
                            result_already_fixed,
                            &output,
                            variable,
                            function.stage(&*state.variables, &Variable::Boolean(output)),
                            tail,
                        )
                    }
                    (Kind::Integer, &Variable::Integer(ref variable)) => {
                        let output = Cell::new(0);
                        stage_apply(
                            result_already_fixed,
                            &output,
                            variable,
                            function.stage(&*state.variables, &Variable::Integer(output)),
                            tail,
                        )
                    }
                    (Kind::String, &Variable::String(ref variable)) => {
                        let output = Cell::new("");
                        stage_apply(
                            result_already_fixed,
                            &output,
                            variable,
                            function.stage(&*state.variables, &Variable::String(output)),
                            tail,
                        )
                    }
                    _ => panic!(),
                }
            }
        }
    }

    fn init_variable<'a>(&self, variables: &mut [Variable<'a>], indexes: &[Relation]) {
        match self {
            &Constraint::Join(var_ix, _, rowcols) => {
                let (row, col) = rowcols[0];
                variables[var_ix] = match &indexes[row].columns[col] {
                    &Values::Boolean(_) => Variable::Boolean(Cell::new(false)),
                    &Values::Integer(_) => Variable::Integer(Cell::new(0)),
                    &Values::String(_) => Variable::String(Cell::new("")),
                    _ => panic!(),
                }
            }
            &Constraint::Apply(var_ix, _, function) => {
                variables[var_ix] = match function.output_kind() {
                    Kind::Boolean => Variable::Boolean(Cell::new(false)),
                    Kind::Integer => Variable::Integer(Cell::new(0)),
                    Kind::String => Variable::String(Cell::new("")),
                }
            }
        }
    }
}

impl Block {
    fn stage<'a>(
        &'a self,
        state: &'a mut State<'a>,
        results: &'a RefCell<Vec<Value<'static>>>,
    ) -> Staged<'a> {
        let variables: Vec<&Variable> = self.result_vars
            .iter()
            .map(|&(_, var_ix)| &state.variables[var_ix])
            .collect();
        let mut tail: Staged<'a> = box move || {
            let results = results.borrow_mut();
            for _ in variables.iter() {
                // TODO actually push results!
                results.push(Value::Boolean(false));
            }
        };
        for constraint in self.constraints.iter().rev() {
            tail = constraint.stage(state, tail);
            constraint.init_variable(&mut state.variables, &state.indexes);
        }
        tail
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
    let results = RefCell::new(vec![]);
    let state = State {
        indexes: &prepared.indexes,
        ranges: prepared
            .ranges
            .iter()
            .map(|&(lo, hi)| Cell::new((lo, hi)))
            .collect(),
        variables: block
            .variables
            .iter()
            .map(|_| Variable::Boolean(Cell::new(false)))
            .collect(),
    };
    let mut staged = time!("stage", block.stage(&mut state, &results));
    time!("query", staged());
    Ok(results.into_inner())
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
                print!("{:?}\n\n", block);
                match plan(block) {
                    Err(error) => status.push(Err(format!("Compile error: {}", error))),
                    Ok(block) => {
                        print!("{:?}\n\n", block);
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

                print!("{:?}\n\n{:?}\n\n", code_ast.blocks[ix], block);
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
