module Interpreter

using Imp.Util
using Imp.Data
using Imp.Compiler

# --- expressions ---

abstract Expr

struct Var <: Expr
  name::Symbol
end

struct Let <: Expr
  bindings::Vector{Tuple{Symbol, Expr}}
  body::Expr
end

struct Multijoin <: Expr
  vars::Vector{Symbol}
  domain::Vector{Tuple{Expr, Vector{Symbol}}}
end

struct OrderedUnion <: Expr
  key::Vector{Int64} # column ix
  val::Vector{Int64}
  sets::Vector{Expr}
end

struct Group <: Expr
  key::Vector{Int64}
  val::Vector{Int64}
  set::Expr
end

struct Compiled <: Expr
  args::Vector{Symbol}
  f::Function
end

# --- functions for walking expression tree ---

map_keys(f::Function, pairs) = map((pair) -> f(pair[1]) => pair[2], pairs)
map_vals(f::Function, pairs) = map((pair) -> pair[1] => f(pair[2]), pairs)
map_exprs(f::Function, expr::Var) = expr
map_exprs(f::Function, expr::Let) = Let(map_vals(f, expr.bindings), f(expr.body))
map_exprs(f::Function, expr::Multijoin) = Multijoin(expr.vars, map_keys(f, expr.domain))
map_exprs(f::Function, expr::Var) = OrderedUnion(expr,key, expr.val, map(f, expr.sets))
map_exprs(f::Function, expr::Var) = Group(expr.key, expr.val, f(expr.set))
map_exprs(f::Function, expr::Compiled) = expr

# --- interpreter ---

function interpret(env::Dict{Symbol, Set}, expr::Var) ::Set
  env[expr.name]
end

function interpret(env::Dict{Symbol, Set}, expr::Let) ::Set
  env = copy(env)
  for (name, value_expr) in expr.bindings
    env[name] = interpret(env, value_expr)
  end
  interpret(env, expr.body)
end
    
function extend_binding(binding::Dict{Symbol, Any}, row::Tuple, vars::Vector{Symbol}) ::Dict{Symbol, Any} ::Union{Dict{Symbol, Any}, Void}
  extended_binding = copy(binding)
  for (col, var) in enumerate(vars)
    if var != :(_)
      if haskey(extended_binding, var) && (extended_binding[var] != row[col])
        return nothing 
      end
      extended_binding[var] = row[col]
    end
  end
  extended_binding
end

function interpret(env::Dict{Symbol, Set}, expr::Multijoin) ::Set
  bindings = [Dict{Symbol, Any}()]
  for (domain_expr, domain_vars) in expr.domain
    set = interpret(env, domain_expr)
    bindings = (extend_binding(binding, row, domain_vars) for binding in bindings for row in set)
    bindings = filter((e) -> e != nothing, bindings)
  end
  Set(ntuple((i) -> binding[expr.vars[i]], length(expr.vars)) for binding in bindings)
end

function interpret(env::Dict{Symbol, Set}, expr::OrderedUnion) ::Set
  union = Dict()
  for set in expr.sets
    for row in interpret(env, set)
      key = row[expr.key]
      if !haskey(union, key)
        union[key] = row[expr.val]
      end
    end
  end
  Set((tuple(key..., val...) for (key, val) in union))
end

function interpret(env::Dict{Symbol, Set}, expr::Group) ::Set
  grouped = Dict()
  for row in interpret(env, expr.set)
    group = get!(() -> Set(), grouped, row[expr.key])
    push!(group, row[expr.val])
  end
  Set((tuple(key..., val) for (key, val) in grouped))
end

function interpret(env::Dict{Symbol, Set}, expr::Compiled) ::Set
  expr.f(Dict((arg => env[arg] for arg in expr.args)))
end

# --- compiler ---

compile(expr::Expr) = map_exprs(compile, expr)

function relation_to_set(relation::Relation) ::Set
  columns = relation.columns[1:end-1] # drop the useless `true` column
  typ = Tuple{(eltype(column) for column in columns)...}
  Set{typ}((ntuple((col) -> columns[col][row], length(columns)) for row in 1:length(columns[1])))
end

function set_to_relation(set::Set{T}) ::Relation where {T <: Tuple} 
  columns = ntuple((col) -> Vector{T.parameters[col]}(), length(T.parameters))
  for row in set
    for col in 1:length(T.parameters)
      push!(columns[col], row[col])
    end
  end
  Relation(columns, length(T.parameters))
end

function compile(expr::Multijoin) ::Expr
  names = [gensym("arg") for _ in expr.domain]
  # TODO need to figure out type inference or staging to avoid compiling every time
  function f(env) 
    compiled_env = map_vals(set_to_relation, env)
    lambda = Lambda(
      gensym("lambda"),
      expr.vars,
      SumProduct(
        Ring{Bool}(|, &, true, false, nothing),
        [FunCall(name, typeof(compiled_env[name]), domain_vars) for (name, (_, domain_vars)) in zip(names, expr.domain)],
        [Constant(true)],
      ),
    )
    compiled = compile_relation(lambda)
    result = Base.invokelatest(compiled, compiled_env)
    relation_to_set(result)
  end
  Let(
    collect(zip(names, (domain_expr for (domain_expr, _) in expr.domain))),
    Compiled(names, f),
  )
end

# --- examples ---

env = Dict{Symbol, Set}(
  :+ => Set{Tuple{Int64, Int64, Int64}}(((a, b, (a + b) % 3) for a in 0:2 for b in 0:2)),
  :* => Set{Tuple{Int64, Int64, Int64}}(((a, b, (a * b) % 3) for a in 0:2 for b in 0:2)),
  :two => Set(((2,),)),
  :xx => Set(((i, i) for i in 0:2)),
  :yy => Set(((i, 2 - i) for i in 0:2)),
  )
  
interpret(env, Let([(:foo, Var(:xx))], Var(:foo)))
  
dot = Multijoin(
  [:i, :x, :y],
  [
    (Var(:xx), [:i, :x]),
    (Var(:yy), [:i, :y]),
  ],
  )
  
interpret(env, dot)

poly = Multijoin(
  [:i, :x, :y, :t1, :t2, :t3, :t4, :t5, :z],
  [
    (Var(:xx), [:i, :x]),
    (Var(:yy), [:i, :y]),
    (Var(:*), [:x, :x, :t1]),
    (Var(:*), [:y, :y, :t2]),
    (Var(:two), [:two]),
    (Var(:*), [:two, :x, :t3]),
    (Var(:*), [:t3, :y, :t4]),
    (Var(:+), [:t1, :t2, :t5]),
    (Var(:+), [:t4, :t5, :z]),
  ],
  )
  
interpret(env, poly)

# throws up on set <=> relation conversion
interpret(env, compile(poly))

using Base.Test
@test interpret(env, poly) == interpret(env, compile(poly))

end
