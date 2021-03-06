use crate::shared::*;

pub type Name = String; // non-empty

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
// TODO Eq/Ord for fn are dubious - should implement on name instead
pub struct Native {
    pub name: Name,
    pub input_arity: usize,
    pub output_arity: usize,
    pub fun: fn(Vec<Scalar>) -> Result<Set, String>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Expression {
    None,
    Some,
    Scalar(Scalar),
    Union(Box<Expression>, Box<Expression>),
    Intersect(Box<Expression>, Box<Expression>),
    Product(Box<Expression>, Box<Expression>),
    Equal(Box<Expression>, Box<Expression>),
    Negate(Box<Expression>),
    Name(Name),
    Let(Name, Box<Expression>, Box<Expression>),
    If(Box<Expression>, Box<Expression>, Box<Expression>),
    Abstract(Name, Box<Expression>),
    Apply(Box<Expression>, Box<Expression>),
    Native(Native),
    Reduce(Box<Expression>, Box<Expression>, Box<Expression>),
    Seal(Box<Expression>),
    Unseal(Box<Expression>),
    Solve(Box<Expression>),
}

#[derive(Debug)]
pub struct Gensym {
    next_tmp: Cell<usize>,
    next_names: RefCell<HashMap<String, usize>>,
}

impl Gensym {
    pub fn new() -> Self {
        Gensym {
            next_tmp: Cell::new(0),
            next_names: RefCell::new(HashMap::new()),
        }
    }

    pub fn name(&self) -> String {
        let name = format!("tmp{}", self.next_tmp.get());
        self.next_tmp.set(self.next_tmp.get() + 1);
        name
    }

    pub fn names(&self, n: usize) -> Vec<String> {
        (0..n).map(|_| self.name()).collect()
    }

    pub fn name_exprs(&self, n: usize) -> Vec<Expression> {
        (0..n)
            .map(|_| self.name())
            .into_iter()
            .map(|name| Expression::Name(name))
            .collect::<Vec<_>>()
    }

    pub fn unique_name(&self, name: &str) -> String {
        let mut next_names = self.next_names.borrow_mut();
        let i = next_names.entry(name.to_owned()).or_insert(0);
        let unique_name = format!("{}{}", name, i);
        *i += 1;
        unique_name
    }
}

impl Expression {
    pub fn _abstract(mut args: Vec<Name>, body: Expression) -> Expression {
        args.reverse();
        args.into_iter()
            .fold(body, |body, arg| Expression::Abstract(arg, box body))
    }

    pub fn _apply(fun: Expression, args: Vec<Name>) -> Expression {
        args.into_iter().fold(fun, |fun, arg| {
            Expression::Apply(box fun, box Expression::Name(arg))
        })
    }

    pub fn _product(a: Expression, args: Vec<Name>) -> Expression {
        args.into_iter().fold(a, |a, arg| {
            Expression::Product(box a, box Expression::Name(arg))
        })
    }

    pub fn apply(fun: Expression, args: Vec<Expression>) -> Expression {
        args.into_iter()
            .fold(fun, |fun, arg| Expression::Apply(box fun, box arg))
    }

    pub fn _let(lets: Vec<(Name, Expression)>, body: Expression) -> Expression {
        let mut expression = body;
        for (name, value) in lets.into_iter().rev() {
            expression = Expression::Let(name, box value, box expression);
        }
        expression
    }

    pub fn visit1<'a, F>(&'a self, f: &mut F) -> Result<(), String>
    where
        F: FnMut(&'a Expression) -> Result<(), String>,
    {
        use Expression::*;
        match self {
            None => (),
            Some => (),
            Scalar(_) => (),
            Union(e1, e2) => {
                f(&*e1)?;
                f(&*e2)?;
            }
            Intersect(e1, e2) => {
                f(&*e1)?;
                f(&*e2)?;
            }
            Product(e1, e2) => {
                f(&*e1)?;
                f(&*e2)?;
            }
            Equal(e1, e2) => {
                f(&*e1)?;
                f(&*e2)?;
            }
            Negate(e) => f(&*e)?,
            Name(_) => (),
            Let(_, value, body) => {
                f(&*value)?;
                f(&*body)?;
            }
            If(cond, if_true, if_false) => {
                f(&*cond)?;
                f(&*if_true)?;
                f(&*if_false)?;
            }
            Abstract(_, body) => f(&*body)?,
            Apply(fun, arg) => {
                f(&*fun)?;
                f(&*arg)?;
            }
            Native(_) => (),
            Reduce(init, vals, fun) => {
                f(&*init)?;
                f(&*vals)?;
                f(&*fun)?;
            }
            Seal(e) => f(&*e)?,
            Unseal(e) => f(&*e)?,
            Solve(e) => f(&*e)?,
        }
        Ok(())
    }

    pub fn visit<'a, F>(&'a self, f: &mut F) -> Result<(), String>
    where
        F: FnMut(&'a Expression) -> Result<(), String>,
    {
        self.visit1(&mut |e| e.visit(f))?;
        f(self)
    }

    pub fn visit1_mut<F>(&mut self, f: &mut F) -> Result<(), String>
    where
        F: FnMut(&mut Expression) -> Result<(), String>,
    {
        use Expression::*;
        match self {
            None => (),
            Some => (),
            Scalar(_) => (),
            Union(box e1, box e2) => {
                f(e1)?;
                f(e2)?;
            }
            Intersect(box e1, box e2) => {
                f(e1)?;
                f(e2)?;
            }
            Product(box e1, box e2) => {
                f(e1)?;
                f(e2)?;
            }
            Equal(box e1, box e2) => {
                f(e1)?;
                f(e2)?;
            }
            Negate(box e) => f(e)?,
            Name(_) => (),
            Let(_, box value, box body) => {
                f(value)?;
                f(body)?;
            }
            If(box cond, box if_true, box if_false) => {
                f(cond)?;
                f(if_true)?;
                f(if_false)?;
            }
            Abstract(_, box body) => f(body)?,
            Apply(box fun, box arg) => {
                f(fun)?;
                f(arg)?;
            }
            Native(_) => (),
            Reduce(init, vals, fun) => {
                f(init)?;
                f(vals)?;
                f(fun)?;
            }
            Seal(box e) => f(e)?,
            Unseal(box e) => f(e)?,
            Solve(box e) => f(e)?,
        }
        Ok(())
    }

    pub fn visit_mut<F>(&mut self, f: &mut F) -> Result<(), String>
    where
        F: FnMut(&mut Expression) -> Result<(), String>,
    {
        self.visit1_mut(&mut |e| e.visit_mut(f))?;
        f(self)
    }

    pub fn map1<F>(mut self, mut f: F) -> Result<Self, String>
    where
        F: FnMut(Expression) -> Result<Expression, String>,
    {
        self.visit1_mut(&mut |e| {
            *e = f(std::mem::replace(e, Expression::None))?;
            Ok(())
        })?;
        Ok(self)
    }

    pub fn map<F>(mut self, mut f: F) -> Result<Self, String>
    where
        F: FnMut(Expression) -> Result<Expression, String>,
    {
        self.visit_mut(&mut |e| {
            *e = f(e.take())?;
            Ok(())
        })?;
        Ok(self)
    }

    pub fn with_natives(self, natives: &[Native]) -> Expression {
        fn map(
            expr: Expression,
            natives: &BTreeMap<Name, &Native>,
            bound: &BTreeSet<Name>,
        ) -> Expression {
            use Expression::*;
            match expr {
                Name(name) => {
                    if let (false, Option::Some(native)) =
                        (bound.contains(&name), natives.get(&name))
                    {
                        Native((*native).clone())
                    } else {
                        Name(name)
                    }
                }
                Abstract(arg, box body) => {
                    let mut bound = bound.clone();
                    bound.insert(arg.clone());
                    Abstract(arg, box map(body, natives, &bound))
                }
                expr => expr.map1(|e| Ok(map(e, natives, bound))).unwrap(),
            }
        }
        let natives = BTreeMap::from_iter(natives.into_iter().map(|n| (n.name.clone(), n)));
        map(self, &natives, &BTreeSet::new())
    }

    pub fn rename(self, old: &Name, new: &Expression) -> Self {
        use Expression::*;
        match self {
            Name(name) => {
                if name == *old {
                    new.clone()
                } else {
                    Name(name)
                }
            }
            Let(name, value, body) => {
                let body = if name == *old {
                    body
                } else {
                    box body.rename(old, new)
                };
                Let(name, box value.rename(old, new), body)
            }
            Abstract(arg, body) => {
                let body = if arg == *old {
                    body
                } else {
                    box body.rename(old, new)
                };
                Abstract(arg, body)
            }
            _ => self.map1(|expr| Ok(expr.rename(old, new))).unwrap(),
        }
    }

    pub fn with_unique_names(
        &self,
        gensym: &Gensym,
        type_cache: &mut Cache<ValueType>,
    ) -> Box<Expression> {
        fn map(
            expression: &Expression,
            bound: &HashMap<Name, Name>,
            gensym: &Gensym,
            type_cache: &mut Cache<ValueType>,
        ) -> Box<Expression> {
            use Expression::*;
            let typ = type_cache.get(&expression).clone();
            let expression = match expression {
                Name(name) => match bound.get(name) {
                    Option::Some(unique_name) => Name(unique_name.clone()),
                    Option::None => Name(name.clone()),
                },
                Let(name, value, body) => {
                    let value = map(value, bound, gensym, type_cache);
                    let unique_name = gensym.unique_name(name);
                    let mut bound = bound.clone();
                    bound.insert(name.clone(), unique_name.clone());
                    let body = map(body, &bound, gensym, type_cache);
                    Let(unique_name, value, body)
                }
                Abstract(name, body) => {
                    // TODO renaming abstracts makes eq on seals weird
                    let unique_name = gensym.unique_name(name);
                    let mut bound = bound.clone();
                    bound.insert(name.clone(), unique_name.clone());
                    let body = map(body, &bound, gensym, type_cache);
                    Abstract(unique_name, body)
                }
                // rest are just an awkard map
                None | Some | Scalar(_) | Native(_) => expression.clone(),
                Union(a, b) => Union(
                    map(a, bound, gensym, type_cache),
                    map(b, bound, gensym, type_cache),
                ),
                Intersect(a, b) => Intersect(
                    map(a, bound, gensym, type_cache),
                    map(b, bound, gensym, type_cache),
                ),
                Product(a, b) => Product(
                    map(a, bound, gensym, type_cache),
                    map(b, bound, gensym, type_cache),
                ),
                Equal(a, b) => Equal(
                    map(a, bound, gensym, type_cache),
                    map(b, bound, gensym, type_cache),
                ),
                Negate(a) => Negate(map(a, bound, gensym, type_cache)),
                Apply(a, b) => Apply(
                    map(a, bound, gensym, type_cache),
                    map(b, bound, gensym, type_cache),
                ),
                If(c, t, f) => If(
                    map(c, bound, gensym, type_cache),
                    map(t, bound, gensym, type_cache),
                    map(f, bound, gensym, type_cache),
                ),
                Solve(a) => Solve(map(a, bound, gensym, type_cache)),
                Reduce(..) | Seal(..) | Unseal(..) => panic!("Unsupported {:?}", expression),
            };
            let expression = box expression;
            type_cache.insert(&expression, typ);
            expression
        }
        map(self, &HashMap::new(), gensym, type_cache)
    }

    pub fn lift_lets(self) -> Self {
        fn map(
            expression: Expression,
            lets: &mut Vec<(Name, Expression)>,
            contexts: &mut HashMap<Name, Vec<Name>>,
            context: &[Name],
        ) -> Expression {
            use Expression::*;
            match expression {
                Name(name) => match contexts.get(&name) {
                    Option::Some(context) => Expression::apply(
                        Name(name),
                        context
                            .iter()
                            .map(|name| Expression::Name(name.clone()))
                            .collect(),
                    ),
                    Option::None => Name(name),
                },
                Let(name, value, body) => {
                    let value = map(*value, lets, contexts, context);
                    let free_names = value.free_names();
                    let ordered_free_names = context
                        .iter()
                        .filter(|name| free_names.contains(&**name))
                        .map(|name| name.clone())
                        .collect();
                    let value = Expression::_abstract(ordered_free_names, value);
                    lets.push((name.clone(), value));
                    contexts.insert(name, context.to_vec());
                    map(*body, lets, contexts, context)
                }
                Abstract(name, body) => {
                    let mut context = context.to_vec();
                    context.push(name.clone());
                    let body = map(*body, lets, contexts, &context);
                    Abstract(name, box body)
                }
                _ => expression
                    .map1(|e| Ok(map(e, lets, contexts, context)))
                    .unwrap(),
            }
        }
        let mut lets = vec![];
        let expression = map(self, &mut lets, &mut HashMap::new(), &[]);
        Expression::_let(lets, expression)
    }

    pub fn take(&mut self) -> Expression {
        std::mem::replace(self, Expression::None)
    }
}
