module Parser

using Match

using Imp.Data
using Imp.Compiler

function parse_when(expr, mod::Module) ::FunCall
  # collect variables
  args = Union{Symbol, Constant}[]
  walk(expr) = @match expr begin
    _::Symbol => push!(args, expr)
    Expr(:call, [head, exprs...], _) => walk(exprs)
    Expr(:(&&), exprs, _) => walk(exprs)
    Expr(:(||), exprs, _) => walk(exprs)
    Expr(_, _, _) => error("Unknown @when syntax: $expr $(typeof(expr))")
    _::AbstractArray => foreach(walk, expr)
    _ => () # some constant
  end
  walk(expr)
  args = convert(typeof(args), unique(args))
  
  # make a closure that evalutes expr
  code = :(($(args...),) -> $expr)
  fun = eval(mod, code)
  
  FunCall(fun, typeof(fun), vcat(args, [Constant(true)]))
end

function parse_return(exprs, mod::Module) ::Vector{Symbol}
  vars = Symbol[]
  args = @match exprs begin
    [Expr(:tuple, args, _)] => args
    _ => error("Unknown return syntax: $args")
  end
  # strip optional type annotations
  for arg in args
    @match arg begin
      _::Symbol => push!(vars, arg)
      Expr(:(::), [symbol::Symbol, _], _) => push!(vars, symbol)
      _ => error("Unknown return arg syntax: $arg")
    end
  end
  vars
end

function parse_call(exprs, mod::Module) ::FunCall
  name = exprs[1]
  args = map(exprs[2:end]) do expr
    @match expr begin
      _::Symbol => expr
      _::Union{Number, String} => Constant(expr)
      _ => error("Unknown call arg syntax: $expr")
    end
  end
  fun = try
    eval(mod, name)
  catch _
    error("Fun is not defined: $name")
  end
  # TODO need to avoid differing representations here
  if isa(fun, Function)
    FunCall(fun, typeof(fun), args)
  else
    FunCall(name, typeof(fun), args)
  end
end

function parse_query(body, mod::Module) ::Lambda
  # just use bool ring
  ring = Ring(|, &, true, false)
  value = [Constant(true)]
  
  # either a multi-line or single-line query
  lines = @match body begin
    Expr(:block, lines, _) => lines
    line => [line]
  end
  
  # dispatch to parse_*
  args = Symbol[]
  domain = FunCall[]
  for line in lines
    @match line begin
      Expr(:macrocall, [head, expr], _) => @match head begin
        Symbol("@when") => push!(domain, parse_when(expr, mod))
        _ => error("Unknown macro: $head")
      end
      Expr(:return, exprs, _) => append!(args, parse_return(exprs, mod))
      Expr(:call, exprs, _) => push!(domain, parse_call(exprs, mod))
      Expr(:line, _, _) => ()
      _ => error("Unknown query line syntax: $line")
    end
  end
  
  Lambda(gensym("query"), args, SumProduct(ring, domain, value))
end

macro query(body)
  parse_query(body, current_module())
end

using JobData

q = parse_query(quote 
  info_type.info(it, "top 250 rank")
  movie_info_idx.info_type(mi, it)
  movie_info_idx.movie(mi, t)
  title.title(t, title)
  title.production_year(t, production_year)
  movie_companies.movie(mc, t)
  movie_companies.company_type(mc, ct)
  company_type.kind(ct, "production companies")
  movie_companies.note(mc, note)
  @when !contains(note, "(as Metro-Goldwyn-Mayer Pictures)") &&
    (contains(note, "(co-production)") || contains(note, "(presents)"))
  return (note::String, title::String, production_year::Int64)
end, JobData)

fun_type(fun) = typeof(eval(fun))
p = compile_relation(q, fun_type)

names = filter((name) -> !isa(name, Function), map((call) -> call.name, q.body.domain))
env = Dict((name => eval(name) for name in names))
@code_warntype p(env)

t = Data.index(env[:(info_type.info)], [2,1])
i1 = Compiler.RelationIndex((t[2], t[1]))
t = Data.index(env[:(movie_info_idx.info_type)], [2,1])
i2 = Compiler.RelationIndex((t[2], t[1]))


end
