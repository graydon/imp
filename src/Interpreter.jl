module Interpreter

const Relation = Set

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
  relations::Vector{Expr}
end

struct Group <: Expr
  key::Vector{Int64}
  val::Vector{Int64}
  relation::Expr
end

function materialize(env::Dict{Symbol, Relation}, expr::Var) ::Relation
  env[expr.name]
end

function materialize(env::Dict{Symbol, Relation}, expr::Let) ::Relation
  env = copy(env)
  env[expr.name] = materialize(expr.value)
  materialize(env, expr.body)
end

function product(relations::Vector{Set})
  if isempty(relations)
    ((),)
  else
    (tuple(row, rest...) for row in relations[1] for rest in product(relations[2:end]))
  end
end

function extend_bindings(bindings::Vector{Dict{Symbol, Any}}, relation::Relation, vars::Vector{Symbol})
  extended_bindings = Dict{Symbol, Any}[]
  for binding in bindings
    for row in relation
      satisfiable = all(enumerate(vars)) do col_var
        col, var = col_var
        (var == :(_)) || !haskey(binding, var) || (binding[var] == row[col])
      end
      if satisfiable
        extended_binding = copy(binding)
        for (col, var) in enumerate(vars)
          if var != :(_)
            extended_binding[var] = row[col]
          end
        end
        push!(extended_bindings, extended_binding)
      end
    end
  end
  extended_bindings
end

function materialize(env::Dict{Symbol, Relation}, expr::Multijoin) ::Relation
  bindings = [Dict{Symbol, Any}()]
  for (domain_expr, domain_vars) in expr.domain
    bindings = extend_bindings(bindings, materialize(env, domain_expr), domain_vars)
  end
  joined = Set()
  for binding in bindings
    push!(joined, ntuple((i) -> binding[expr.vars[i]], length(expr.vars)))
  end
  joined
end

function materialize(env::Dict{Symbol, Relation}, expr::OrderedUnion) ::Relation
  union = Dict()
  for relation in expr.relations
    for row in materialize(env, relation)
      key = row[expr.key]
      if !haskey(union, key)
        union[key] = row[expr.val]
      end
    end
  end
  Set((tuple(key..., val...) for (key, val) in union))
end

function materialize(env::Dict{Symbol, Relation}, expr::Group) ::Relation
  grouped = Dict()
  for row in materialize(env, expr.relation)
    group = get!(() -> Set(), grouped, row[expr.key])
    push!(group, row[expr.val])
  end
  Set((tuple(key..., val) for (key, val) in grouped))
end

env = Dict{Symbol, Relation}(
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
