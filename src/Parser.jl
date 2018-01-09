module Parser

using Match

using Imp.Util
using Imp.Data
using Imp.Compiler

function parse_when(expr, mod::Module) ::FunCall
  # collect variables
  args = Union{Symbol, Constant}[]
  walk(expr) = @match expr begin
    _::Symbol => push!(args, expr)
    Expr(:call, [:in, var, set], _) => walk(var) 
    Expr(:call, [head, exprs...], _) => walk(exprs)
    Expr(:macrocall, [head, exprs...], _) => walk(exprs)
    Expr(:(&&), exprs, _) => walk(exprs)
    Expr(:(||), exprs, _) => walk(exprs)
    Expr(:comparison, [a, _, b, _, c], _) => walk([a,b,c])
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
  if name == :in
    var = exprs[2]
    name = exprs[3]
    @assert isa(var, Symbol)
    FunCall(:(Relation((collect($name),), 1)), Relation{Tuple{Vector{Any}}}, [var])
  else 
    args = map(exprs[2:end]) do expr
      @match expr begin
        _::Symbol => expr
        _::Union{Number, String} => Constant(expr)
        Expr(:call, _, _) => Constant(expr)
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
end

function parse_query(body, mod::Module) ::Lambda
  # just use bool ring
  ring = Ring(|, &, true, false, true)
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
        Symbol("@when") => @match expr begin
          Expr(:call, [:in, var, set], _) => push!(domain, parse_call([:in, var, set], mod))
          _ => push!(domain, parse_when(expr, mod))
        end
        _ => error("Unknown macro: $head")
      end
      Expr(:return, exprs, _) => append!(args, parse_return(exprs, mod))
      Expr(:call, exprs, _) => push!(domain, parse_call(exprs, mod))
      Expr(:(=), [var, value], _) => push!(domain, parse_call([identity, value, var], mod))
      Expr(:line, _, _) => ()
      _ => error("Unknown query line syntax: $line")
    end
  end
  
  Lambda(gensym("query"), args, SumProduct(ring, domain, value))
end

macro query(body)
  mod = current_module()
  parsed = parse_query(body, mod)
  compiled = compile_relation(parsed, (fun) -> 
    try 
      typeof(eval(mod, fun)) 
    catch _ 
      println("Warning - guessing type of $fun")
      Relation{Tuple{Vector{String}}}
    end)
  names = filter((name) -> !isa(name, Function), map((call) -> call.name, parsed.body.domain))
  :($compiled(Dict($(@splice name in names :($(Expr(:quote, name)) => $(esc(name)))))))
end

export @query

end
