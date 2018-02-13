module Interpreter

# TODO handle infinite relations?

# --- expressions ---

# TODO if we deref a scalar in a set context, should it be wrapped or should it be an error?
"The environment maps symbols to scalars or sets of tuples of scalars"
const Env = Dict{Symbol, Any}

"An expression with semantics `Env -> Bool`"
abstract type BoolExpr end

"True iff `body` tuple is contained in `head` set"
struct Application <: BoolExpr
  head::Symbol
  body::Vector{Symbol}
end

"Boolean and"
struct And <: BoolExpr
  left::BoolExpr
  right::BoolExpr
end

"Boolean or"
struct Or <: BoolExpr
  left::BoolExpr
  right::BoolExpr
end

"Boolean not. Can only occur as the rhs of an `And` expr i.e. `And(..., Not(...))`"
struct Not <: BoolExpr
  value::BoolExpr
end

"An expression with semantics `Env -> Set{Tuple}`"
abstract type SetExpr end

"A zero-column zero-row set"
struct False <: SetExpr
end

"A zero-column one-row set"
struct True <: SetExpr
end

"A singleton set"
struct Constant <: SetExpr
  value::Any
end

"Recall an existing binding"
struct Var <: SetExpr
  name::Symbol
end

"Create new bindings"
struct Let <: SetExpr
  bindings::Vector{Tuple{Symbol, SetExpr}}
  body::SetExpr
end

"Create new bindings under a fixpoint"
struct LetRec <: SetExpr
  bindings::Vector{Tuple{Symbol, SetExpr}}
  body::SetExpr
end

"Set comprehension"
struct Abstraction <: SetExpr
  variable::Symbol
  domain::BoolExpr
  value::SetExpr
end

"Reduce the last column of `body` using the binary operation `op`"
struct Reduce <: SetExpr
  op::SetExpr
  body::SetExpr
end

"Run some precompiled function"
struct Compiled <: SetExpr
  args::Vector{Symbol}
  f::Function
end

# --- BoolExpr interpreter ---

function interpret(env::Env, expr::Application, variable::Symbol) ::Set{Any}
  head = env[expr.head]
  ix = findfirst(expr.body, variable)
  is_match(row) = all(zip(row, expr.body)) do pair
    value, binding = pair
    (binding == :(_)) || ((binding == variable) && (row[ix] == value)) || (env[binding] == value)
  end
  Set((row[ix] for row in head if is_match(row)))
end

function interpret(env::Env, expr::And, variable::Symbol) ::Set{Any}
  @assert typeof(expr.left) != Not "`Not` may only occur as the right-hand side of an `And` expression"
  if typeof(expr.right) == Not  
    setdiff(interpret(env, expr.left, variable), interpret(env, expr.right.value, variable))
  else
    intersect(interpret(env, expr.left, variable), interpret(env, expr.right, variable))
  end
end

function interpret(env::Env, expr::Or, variable::Symbol) ::Set{Any}
  union(interpret(env, expr.left, variable), interpret(env, expr.right, variable))
end
  
function interpret(env::Env, expr::Not, variable::Symbol) ::Set{Any}
  error("`Not` may only occur as the right-hand side of an `And` expression")
end

# --- SetExpr interpreter ---

function interpret(env::Env, expr::False) ::Set
  Set{Tuple{}}()
end

function interpret(env::Env, expr::True) ::Set
  Set{Tuple{}}(((),))
end

function interpret(env::Env, expr::Constant) ::Set
  Set((expr.value,))
end

function interpret(env::Env, expr::Var) ::Set
  env[expr.name]
end

function interpret(env::Env, expr::Let) ::Set
  env = copy(env)
  for (name, value_expr) in expr.bindings
    env[name] = interpret(env, value_expr)
  end
  interpret(env, expr.body)
end

function interpret(env::Env, expr::LetRec) ::Set
  env = copy(env)
  for (name, _) in expr.bindings
    env[name] = Set()
  end
  while true
    changed = false
    for (name, value_expr) in expr.bindings
      new_set = interpret(env, value_expr)
      if env[name] != new_set
        changed = true
        env[name] = new_set
      end
    end
    if changed == false
      break
    end
  end
  interpret(env, expr.body)
end

function interpret(env::Env, expr::Abstraction) ::Set
  env = copy(env)
  result = Set()
  for value in interpret(env, expr.domain, expr.variable)
    env[expr.variable] = value
    # TODO do we care about totality here?
    for row in interpret(env, expr.value) 
      push!(result, (value, row...))
    end
  end
  result
end

function interpret(env::Env, expr::Reduce) ::Set
  op = Dict((row[1], row[2]) => row[3] for row in interpret(env, expr.op))
  body = sort(collect(interpret(env, expr.body)))
  result = reduce((a,b) -> op[(a,b)], (row[end] for row in body))
  Set((result,))
end

# --- examples ---

# do everything modulo 3 for now to keep it finite
env = Env(
  :+ => Set{Tuple{Int64, Int64, Int64}}(((a, b, (a + b) % 3) for a in 0:2 for b in 0:2)),
  :* => Set{Tuple{Int64, Int64, Int64}}(((a, b, (a * b) % 3) for a in 0:2 for b in 0:2)),
  :two => Set(((2,),)),
  :xx => Set(((i, i) for i in 0:2)),
  :yy => Set(((i, 2 - i) for i in 0:2)),
  :(=) => Set(((i,i) for i in 0:2)),
  )
  
interpret(env, Let([(:foo, Var(:xx))], Var(:foo)))
  
interpret(env, True())  
  
interpret(env, Abstraction(:i, Application(:xx, [:i, :(_)]), True())) 
  
# sum(i : xx(i, x), yy(i, y) . x * y)  
dot = begin
  Reduce(Var(:+),
    Abstraction(:i, And(Application(:xx, [:i, :(_)]), Application(:yy, [:i, :(_)])),
      Abstraction(:x, Application(:xx, [:i, :x]),
        Abstraction(:y, Application(:yy, [:i, :y]),
          Abstraction(:v, Application(:*, [:x, :y, :v]),
            True())))))
          end
  
interpret(env, dot)

# i : xx(i, x), yy(i, y) . x*x + y*y + 2*x*y
poly = begin
  Let([(:intermediate,
        Abstraction(:i, And(Application(:xx, [:i, :(_)]), Application(:yy, [:i, :(_)])),
          Abstraction(:x, Application(:xx, [:i, :x]),
            Abstraction(:y, Application(:yy, [:i, :y]),
              Abstraction(:t1, Application(:*, [:x, :x, :t1]),
                Abstraction(:t2, Application(:*, [:y, :y, :t2]),
                  Abstraction(:t3, Application(:two, [:t3]),
                    Abstraction(:t4, Application(:*, [:t3, :x, :t4]),
                      Abstraction(:t5, Application(:*, [:t4, :y, :t5]),
                        Abstraction(:t6, Application(:+, [:t1, :t2, :t6]),
                          Abstraction(:z, Application(:+, [:t5, :t6, :z]),
                            True())))))))))))],
      Abstraction(:i, Application(:intermediate, [:i, :(_), :(_), :(_), :(_), :(_), :(_), :(_), :(_), :(_)]),
        Abstraction(:z, Application(:intermediate, [:i, :(_), :(_), :(_), :(_), :(_), :(_), :(_), :(_), :z]),
          True())))
        end
  
interpret(env, poly)

# i, y : yy(i, y), i != y
diff = begin
  Abstraction(:i, Application(:yy, [:i, :(_)]),
    Abstraction(:y, And(Application(:yy, [:i, :y]), Not(Application(:(=), [:i, :y]))),
      True()))
    end
    
interpret(env, diff)

# closed = union(2, closed, last_column(y : closed(y) . y + y))
closed = begin
  LetRec([(:closed,
    Let([(:intermediate,
      Abstraction(:y, Application(:closed, [:y]),
        Abstraction(:v, Application(:+, [:y, :y, :v]),
          True())))],
      Abstraction(:y, Or(Application(:two, [:y]), 
                        Or(Application(:closed, [:y]), 
                           Application(:intermediate, [:(_), :y]))),
        True())))],
    Var(:closed))
  end
  
interpret(env, closed)

end
