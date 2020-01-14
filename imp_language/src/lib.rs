#![feature(box_syntax)]
#![feature(box_patterns)]
#![feature(backtrace)]

#[macro_use]
mod macros;
mod analysis;
mod bir;
mod denotation;
mod dir;
mod expression;
mod pretty;
mod shared;
mod solver;
mod stdlib;
mod syntax;

use crate::shared::*;
pub use crate::shared::{Expression, Native, Scalar, ScalarType, Value, ValueType};

pub fn run(code: &str, debug_info: &mut Vec<String>) -> Result<(ValueType, Value), String> {
    let expr = if code.is_empty() {
        // mild hack
        Expression::None
    } else {
        parse(&code).map_err(|e| format!("Parse error: {:?}", e))?
    };

    let gensym = Gensym::new();

    let mut expr = expr.with_natives(&Native::stdlib());
    debug_info.push(format!("with_natives: {}", d!(&expr)));

    let mut type_cache = Cache::new();
    let _typ = expr
        .typecheck(&Environment::new(), &mut type_cache)
        .map_err(|e| format!("Type error: {}", e))?;
    match expr.as_bir(&mut BirContext {
        renames: vec![],
        type_cache: &type_cache,
        gensym: &gensym,
    }) {
        Ok(bir) => {
            debug_info.push(format!("bir: {}", d!(&bir)));

            let dnf = bir.dnf();
            debug_info.push(format!("dnf: {}", d!(&dnf)));
        }
        Err(err) => debug_info.push(format!("bir err: {}", err)),
    }

    let mut type_cache = Cache::new();
    let typ = expr
        .typecheck(&Environment::new(), &mut type_cache)
        .map_err(|e| format!("Type error: {}", e))?;
    expr.funify(&mut type_cache, &gensym);
    debug_info.push(format!("funify: {}", d!(&expr)));

    // expr = expr.simplify(&HashSet::new());

    let value = expr
        .clone()
        .eval(&Environment::new())
        .map_err(|e| format!("Eval error: {}", e))?;

    Ok((typ, value))
}
