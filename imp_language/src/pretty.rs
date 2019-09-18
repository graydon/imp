use crate::shared::*;

fn write_delimited<T, TS: IntoIterator<Item = T>, F: Fn(&mut fmt::Formatter, T) -> fmt::Result>(
    f: &mut fmt::Formatter,
    delimiter: &str,
    things: TS,
    write: F,
) -> fmt::Result {
    let mut iter = things.into_iter().enumerate().peekable();
    while let Some((_i, thing)) = iter.next() {
        write(f, thing)?;
        if let Some(_) = iter.peek() {
            write!(f, "{}", delimiter)?;
        }
    }
    Ok(())
}

fn write_environment<T>(f: &mut fmt::Formatter, env: &Environment<T>) -> fmt::Result
where
    T: std::fmt::Display,
{
    for (var, value) in &env.bindings {
        write!(f, "let {} = {} in ", var, value)?;
    }
    Ok(())
}

impl fmt::Display for Scalar {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Scalar::String(string) => write!(f, "{:?}", string)?,
            Scalar::Number(number) => write!(f, "{:?}", number)?,
            Scalar::Sealed(value) => {
                write!(f, "{{")?;
                write!(f, "{}", value)?;
                write!(f, "}}")?;
            }
        }
        Ok(())
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            v if v.is_nothing() => write!(f, "nothing")?,
            v if v.is_something() => write!(f, "something")?,
            Value::Set(set) => {
                let is_something = self.is_something();
                if is_something {
                    write!(f, "(")?;
                }
                write_delimited(f, " | ", set, |f, value| {
                    write_delimited(f, " x ", value, |f, scalar| write!(f, "{}", scalar))
                })?;
                if is_something {
                    write!(f, ")")?;
                }
            }
            Value::Closure(name, body, env) => {
                write!(f, "({} -> ", name)?;
                write_environment(f, env)?;
                write!(f, "{})", body)?;
            }
        }
        Ok(())
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Expression::*;
        match self {
            Nothing => write!(f, "nothing")?,
            Something => write!(f, "something")?,
            Scalar(scalar) => write!(f, "{}", scalar)?,
            Union(e1, e2) => write!(f, "({} | {})", e1, e2)?,
            Intersect(e1, e2) => write!(f, "({} & {})", e1, e2)?,
            Product(e1, e2) => write!(f, "({} x {})", e1, e2)?,
            Equal(e1, e2) => write!(f, "({} = {})", e1, e2)?,
            Negate(e) => write!(f, "!{}", e)?,
            Name(name) => write!(f, "{}", name)?,
            Let(name, value, body) => write!(f, "let {} = {} in {}", name, value, body)?,
            If(cond, if_true, if_false) => {
                write!(f, "if {} then {} else {}", cond, if_true, if_false)?
            }
            Abstract(arg, body) => {
                write!(f, "({} -> {})", arg, body)?;
            }
            Apply(fun, arg) => write!(f, "({} {})", fun, arg)?,
            ApplyNative(fun, args) => {
                write!(f, "(<{}> ", fun.name)?;
                write_delimited(f, " ", args, |f, arg| write!(f, "{}", arg))?;
                write!(f, ")")?;
            }
            Seal(e) => write!(f, "{{{}}}", e)?,
            Unseal(e) => write!(f, "${}", e)?,
            Exists(args, body) => {
                write!(f, "exists(")?;
                write_delimited(f, " ", args, |f, arg| write!(f, "{}", arg))?;
                write!(f, " -> {})", body)?;
            }
            Solve(e) => {
                write!(f, "?({})", e)?;
            }
        }
        Ok(())
    }
}