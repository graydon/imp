module Interpreter

abstract Expr

struct Var <: Expr
  name::Symbol
end

struct Let <: Expr
  name::Symbol
  value::Expr
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

function materialize(env::Dict{Symbol, Set}, expr::Var) ::Set
  env[expr.name]
end

function materialize(env::Dict{Symbol, Set}, expr::Let) ::Set
  env = copy(env)
  env[expr.name] = materialize(expr.value)
  materialize(env, expr.body)
end

function product(sets::Vector{Set})
  if isempty(sets)
    ((),)
  else
    (tuple(row, rest...) for row in sets[1] for rest in product(sets[2:end]))
  end
end
    
function extend_binding(binding::Dict{Symbol, Any}, row::Tuple, vars::Vector{Symbol}) ::Union{Dict{Symbol, Any}, Void}
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

function materialize(env::Dict{Symbol, Set}, expr::Multijoin) ::Set
  bindings = [Dict{Symbol, Any}()]
  for (domain_expr, domain_vars) in expr.domain
    set = materialize(env, domain_expr)
    bindings = (extend_binding(binding, row, domain_vars) for binding in bindings for row in set)
    bindings = filter((e) -> e != nothing, bindings)
  end
  Set(ntuple((i) -> binding[expr.vars[i]], length(expr.vars)) for binding in bindings)
end

function materialize(env::Dict{Symbol, Set}, expr::OrderedUnion) ::Set
  union = Dict()
  for set in expr.sets
    for row in materialize(env, set)
      key = row[expr.key]
      if !haskey(union, key)
        union[key] = row[expr.val]
      end
    end
  end
  Set((tuple(key..., val...) for (key, val) in union))
end

function materialize(env::Dict{Symbol, Set}, expr::Group) ::Set
  grouped = Dict()
  for row in materialize(env, expr.set)
    group = get!(() -> Set(), grouped, row[expr.key])
    push!(group, row[expr.val])
  end
  Set((tuple(key..., val) for (key, val) in grouped))
end

env = Dict{Symbol, Set}(
  :+ => Set(((a, b, (a + b) % 3) for a in 0:2 for b in 0:2)),
  :* => Set(((a, b, (a * b) % 3) for a in 0:2 for b in 0:2)),
  :two => Set(((2,),)),
  :xx => Set(((i, i) for i in 0:2)),
  :yy => Set(((i, 2 - i) for i in 0:2)),
  )
  
dot = Multijoin(
  [:i, :x, :y],
  [
    (Var(:xx), [:i, :x]),
    (Var(:yy), [:i, :y]),
  ],
  )
  
materialize(env, dot)

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
  
materialize(env, poly)

end
