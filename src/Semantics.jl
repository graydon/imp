module Semantics

import MacroTools
import MacroTools: @capture
using Rematch

abstract type Expr end

struct Constant{T} <: Expr
    value::T # scalar
end

struct Var <: Expr
    name::Symbol
end

struct Apply <: Expr
    f::Expr
    args::Vector{Expr}
end

struct Abstract <: Expr
    variable::Symbol
    value::Expr
end

# things which operate on relations rather than on values
struct Primitive <: Expr
    f::Symbol
    args::Vector{Expr}
end

const Env = Dict{Symbol, Set}

const truthy = Set([()])
const falsey = Set()
bool_to_set(bool::Bool)::Set = bool ? truthy : falsey
set_to_bool(set::Set)::Bool = length(set) > 0

function interpret(env::Env, expr::Constant) ::Set
    Set([(expr.value,)])
end

function interpret(env::Env, expr::Constant{Bool}) ::Set
    bool_to_set(expr.value)
end

function interpret(env::Env, expr::Var) ::Set
    env[expr.name]
end

function interpret(env::Env, expr::Apply) ::Set
    f = interpret(env, expr.f)
    for arg in map((arg) -> interpret(env, arg), expr.args)
        result = Set()
        if !isempty(arg)
            n = length(first((arg)))
            for row in f
                if row[1:n] in arg
                    push!(result, row[n+1:end])
                end
            end
            f = result
        end
    end
    f
end

function interpret(env::Env, expr::Abstract) ::Set
    env = copy(env)
    result = Set()
    for domain_row in env[:everything]
        env[expr.variable] = Set([domain_row])
        value = interpret(env, expr.value)
        for value_row in value
            push!(result, (domain_row..., value_row...))
        end
    end
    result
end

function interpret(env::Env, expr::Primitive) ::Set
    args = [interpret(env, arg) for arg in expr.args]
    @match (expr.f, args) begin
        (:|, _) => union(args...)
        (:&, _) => intersect(args...)
        (:!, [arg]) => bool_to_set(!set_to_bool(arg))
        (:(=>), [a, b]) => bool_to_set((!set_to_bool(a) || set_to_bool(b)))
        (:(==), _) => bool_to_set((==)(args...))
        (:iff, [cond, true_branch]) => set_to_bool(cond) ? true_branch : bool_to_set(false)
        (:iff, [cond, true_branch, false_branch]) => set_to_bool(cond) ? true_branch : false_branch
        (:reduce, [raw_op, raw_init, values]) => begin
            op = Dict(((a,b) => c for (a,b,c) in raw_op))
            @assert length(raw_init) == 1
            @assert length(first(raw_init)) == 1
            init = first(raw_init)[1]
            value = reduce((a,b) -> op[a,b[end]], init, values)
            Set([(value,)])
        end
        (:exists, [arg]) => bool_to_set(arg != env[:nothing])
        (:forall, [arg]) => bool_to_set(arg == env[:everything])
        _ => error("Unknown primitive: $expr")
    end
end

function parse(ast)
    if @capture(ast, constant_Int64_String_Bool)
        Constant(constant)
    elseif @capture(ast, name_Symbol)
        Var(name)
    elseif @capture(ast, f_(args__)) && length(args) > 0
        if f in [:|, :&, :!, :(=>), :(==), :reduce, :exists, :forall]
            Primitive(f, map(parse, args))
        else
            Apply(parse(f), map(parse, args))
        end
    elseif @capture(ast, if cond_ true_branch_ end)
        Primitive(:iff, [parse(cond), parse(true_branch)])
    elseif @capture(ast, if cond_ true_branch_ else false_branch_ end)
        Primitive(:iff, [parse(cond), parse(true_branch), parse(false_branch)])
    elseif @capture(ast, (variable_Symbol) -> value_)
        Abstract(variable, parse(value))
    else
        error("Unknown syntax: $ast")
    end
end

function unparse(expr)
    @match expr begin
        Constant(value) => value
        Var(name) => name
        Apply(f, args) => :($(unparse(f))($(map(unparse, args)...)))
        Abstract(variable, value) => :(($variable) -> $(unparse(value)))
        Primitive(:iff, [cond, true_branch]) => :(if $(unparse(cond)) $(unparse(true_branch)) end)
        Primitive(:iff, [cond, true_branch, false_branch]) => :(if $(unparse(cond)) $(unparse(true_branch)) else $(unparse(false_branch)) end)
        Primitive(f, args) => :($(unparse(f))($(map(unparse, args)...)))
    end
end

stdenv = Dict{Symbol, Set}(
  :everything => Set{Any}([(scalar,) for scalar in [0, 1, 2, "alice", "bob", "eve", "cthulu", "yes", "no"]]),
  :nothing => Set([]),
  )

# tests / examples

env = merge(stdenv, Dict{Symbol, Set}(
:person => Set([("alice",), ("bob",), ("eve",)]),
:string => Set([("alice",), ("bob",), ("eve",), ("cthulu",)]),
:evil => Set([("eve",), ("cthulu",)]),
:rsvp => Set([("alice", "yes"), ("bob", "no"), ("cthulu", "no")]),
:+ => Set([(a, b, (a+b) % 3) for a in 0:2 for b in 0:2]),
:points => Set([("alice", 0), ("bob", 1), ("cthulu", 1)]),
))

macro imp(ast)
    parse(ast)
end

macro imp!(ast)
    :(interpret($env, parse($(QuoteNode(ast)))))
end

macro test_imp(ast)
    @assert @capture(ast, left_ == right_)
    # TODO not sure why these don't need escaping...
    :(@test @imp!($left) == @imp!($right))
end

# TODO remove workaround for https://github.com/JuliaLang/julia/issues/26885
function Base.show(io::IO, set::Set)
    print(io, "Set(", collect(set), ")")
end

using Test

@test set_to_bool(bool_to_set(true)) == true
@test set_to_bool(bool_to_set(false)) == false

# basic booleans
@test @imp!(true) == Set([()])
@test @imp!(false) == Set()
@test_imp (true | true) == true
@test_imp (true | false) == true
@test_imp (false | true) == true
@test_imp (false | false) == false
@test_imp (true & true) == true
@test_imp (true & false) == false
@test_imp (false & true) == false
@test_imp (false & false) == false
@test_imp !true == false
@test_imp !false == true

# 'implicit' boolean
@test_imp person("alice") == true
@test_imp person(0) == false
@test_imp person == "alice" | "bob" | "eve"

# three-valued logic
@test_imp rsvp("alice") == "yes"
@test_imp rsvp("bob") == "no"
@test_imp rsvp("eve") == nothing

# convert value to boolean
@test_imp ("yes" == "yes") == true
@test_imp ("yes" == "no") == false
@test_imp (rsvp("alice") == "yes") == true
@test_imp (rsvp("bob") == "yes") == false
@test_imp (rsvp("eve") == "yes") == false
@test_imp rsvp("alice", "yes") == true
@test_imp rsvp("bob", "yes") == false
@test_imp rsvp("eve", "yes") == false

# convert boolean to value
# (`if` is taken so `iff`)
@test_imp if true "yes" else "no" end == "yes"
@test_imp if false "yes" else "no" end == "no"
@test_imp (true ? "yes" : "no") == "yes"
@test_imp (false ? "yes" : "no") == "no"

# abstraction
@test_imp (x -> true) == everything
@test_imp (x -> false) == nothing
@test_imp (p -> person(p)) == person
@test_imp (x -> "alice")(2) == "alice"

# domain expressed via `iff`
@test_imp (p -> if person(p) rsvp(p) end)("alice") == "yes"
@test_imp (p -> if person(p) rsvp(p) end)("cthulu") == false
@test_imp (p -> if person(p) rsvp(p, "yes") end) == "alice"
@test_imp (p -> if person(p) rsvp(p, "no") end) == "bob"
@test_imp (p -> rsvp(p, "no")) == ("bob" | "cthulu")

# defaults via `iff`
@test_imp (p -> if person(p) rsvp(p) else "n/a" end)("alice") == "yes"
@test_imp (p -> if person(p) rsvp(p) else "n/a" end)("cthulu") == "n/a"
@test_imp (p -> if person(p) rsvp(p) else "n/a" end)(2) == "n/a"
@test_imp (p -> if person(p) rsvp(p) else (if string(p) "n/a" end) end)("cthulu") == "n/a"
@test_imp (p -> if person(p) rsvp(p) else (if string(p) "n/a" end) end)(2) == false

# can do quantification via eq

# exists == X :: not(eq(X, nothing))
@test_imp exists(person) == true
@test_imp exists(nothing) == false

# forall == X :: eq(X, everything)
@test_imp forall(p -> person(p) => string(p)) == true
@test_imp forall(p -> person(p) => evil(p)) == false
@test_imp forall(p -> person(p) => rsvp(p, "yes")) == false
@test_imp forall(p -> person(p) => (rsvp(p, "yes") | rsvp(p, "no"))) == false
@test_imp forall(p -> person(p) => (rsvp(p, "yes") | rsvp(p, "no") | "eve"(p))) == true

# aggregation
@test_imp reduce(+, 0, points) == 2

# tricky expressions
@test_imp (x -> x(x)) == everything

end
