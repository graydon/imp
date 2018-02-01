module Interpreter

using Imp.Util
using Imp.Data
using Imp.Compiler

# TODO need to handle free variables? or compile as function?
# OrderedUnion as multiple values on Multijoin?
# Forumala=expr but solve for free variable?
# How does Boute handle forall/bool? Everything is a function?
# Does TC make sense, operating over whole relations rather than pointwise?

# --- expressions ---

abstract type BoolExpr end

struct Application <: BoolExpr
  head::Symbol
  body::Vector{Symbol}
end

struct And <: BoolExpr
  left::BoolExpr
  right::BoolExpr
end

struct Or <: BoolExpr
  left::BoolExpr
  right::BoolExpr
end

struct Not <: BoolExpr
  value::BoolExpr
end

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

const Env = Dict{Symbol, Any}

# --- functions for walking expression tree ---

map_keys(f::Function, pairs) = map((pair) -> f(pair[1]) => pair[2], pairs)
map_vals(f::Function, pairs) = map((pair) -> pair[1] => f(pair[2]), pairs)
map_exprs(f::Function, expr::Constant) = expr
map_exprs(f::Function, expr::Var) = expr
map_exprs(f::Function, expr::Let) = Let(map_vals(f, expr.bindings), f(expr.body))
map_exprs(f::Function, expr::LetRec) = LetRec(map_vals(f, expr.bindings), f(expr.body))
map_exprs(f::Function, expr::Multijoin) = Multijoin(expr.vars, map_keys(f, expr.domain))
map_exprs(f::Function, expr::Var) = OrderedUnion(expr,key, expr.val, map(f, expr.sets))
map_exprs(f::Function, expr::Var) = Group(expr.key, expr.val, f(expr.set))
map_exprs(f::Function, expr::Compiled) = expr

# --- bool_expr interpreter ---

function interpret(env::Env, expr::Application, variable::Symbol) ::Vector{Any}
end
  

# --- set_expr interpreter ---

function interpret(env::Env, expr::False) ::Set
  Set{Tuple{}}()
end

function interpret(env::Env, expr::True) ::Set
  Set{Tuple{}}(())
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

function interpret(env::Dict{Symbol, Set}, expr::LetRec) ::Set
  env = copy(env)
  while true
    changed = false
    for (name, value_expr) in expr.bindings
      new_set = interpret(env, value_expr)
      if !haskey(env, name) || (env[name] != new_set)
        changed = true
        env[name] = new_set
      end
    end
    if changed == true
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
    for row in interpret(env, expr.value) 
      push!(result, (value, row...))
    end
  end
end

function interpret(env::Env, expr::Reduce) ::Set
  op = Dict((row[1], row[2]) => row[3] for row in interpret(env, expr.op))
  body = sort(collect(interpret(env, expr.body)))
  reduce((a,b) -> op[(a,b)], (row[end] for row in body))
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

# # throws up on set <=> relation conversion
# interpret(env, compile(poly))
# 
# using Base.Test
# @test interpret(env, poly) == interpret(env, compile(poly))

end
