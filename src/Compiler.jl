module Compiler

using Base.Cartesian
using Match

using Imp.Data

# --- ast ---

struct Ring{T}
  add::Function
  mult::Function
  one::T
  zero::T
end

const count_ring = Ring(+, *, 1, 0)

struct Constant
  value::Any
end

struct FunCall
  name::Union{Symbol, Function, Expr} # TODO Expr is a temporary hack to get parsing working
  typ::Type
  args::Vector{Union{Symbol, FunCall, Constant}}
end

struct IndexCall
  name::Symbol
  typ::Type
  args::Vector{Symbol}
end

struct SumProduct
  ring::Ring
  domain::Vector{Union{FunCall, IndexCall}}
  value::Vector{Union{Symbol, FunCall, Constant}}
end

struct Insert
  result_name::Symbol
  ring::Ring
  args::Vector{Symbol}
  value::Union{Constant, FunCall}
end

mutable struct Lambda
  name::Symbol
  args::Vector{Symbol}
  body::Union{SumProduct, Insert}
end

struct Return
  name::Symbol
  result_name::Symbol
  value::FunCall
end

struct Index
  name::Symbol
  typ::Type
  fun::Union{Symbol, Function, Expr} # TODO Expr is a temporary hack to get parsing working
  permutation::Vector{Int64}
end

struct Result
  name::Symbol
  typs::Vector{Type}
end

const State = Union{Index, Result}

mutable struct Program
  states::Vector{State}
  funs::Vector{Union{Lambda, Return}} # must in definition order - can't call funs that haven't been defined yet
end

# --- better IR rendering in Juno

if isdefined(Main, :Juno)
  head(ir) = repr(ir)
  head(ir::Symbol) = string(ir)
  head(ir::Constant) = repr(ir.value)
  head(ir::Union{FunCall, IndexCall}) = "$(head(ir.name))($(join(ir.args, ", ")))"
  head(ir::SumProduct) = "+= $(join(map(head, ir.value), " * ")) <- $(join(map(head, ir.domain), ", "))"
  head(ir::Insert) = "= $(head(ir.value)); $(ir.result_name) += ($(join(map(head, ir.args), ", ")))"
  head(ir::Lambda) = "$(ir.name)($(join(map(head, ir.args), ", "))) $(head(ir.body))"
  
  function Main.Juno.render(i::Main.Juno.Inline, ir::Union{FunCall,IndexCall, Lambda})
    t = Main.Juno.render(i, Main.Juno.defaultrepr(ir))
    t[:head] = Main.Juno.render(i, Text("$(typeof(ir)): $(head(ir))"))
    return t
  end
  
  macro render(expr)
    quote
      value = $(esc(expr))
      print($(string(expr)))
      println(" = ")
      Main.Juno.render(value)
      value
    end
  end
end

# --- util ---

function simplify_expr(expr::Expr)
  @match expr begin
    Expr(:block, lines, _) => begin
      flattened = []
      for line in map(simplify_expr, lines)
        @match line begin
          Expr(:block, more_lines, _) => append!(flattened, more_lines)
          Expr(:line, _, _) => nothing
          _ => push!(flattened, line)
        end
      end
      if length(flattened) == 1
        flattened[1]
      else
        Expr(:block, flattened...)
      end
    end
    Expr(head, args, _) => Expr(simplify_expr(head), map(simplify_expr, args)...)
  end
end

function simplify_expr(other)
  other
end

Base.eltype(ring::Ring{T}) where {T} = T

macro splice(iterator, body)
  @assert iterator.head == :call
  @assert iterator.args[1] == :in
  Expr(:..., :(($(esc(body)) for $(esc(iterator.args[2])) in $(esc(iterator.args[3])))))
end

function inline(function_expr::Expr, value)
  @match function_expr begin
    Expr(:->, [var::Symbol, body], _)  => quote
      let $var = $value
        $body
      end
    end
    _ => error("Can't inline $function_expr")
  end
end

# --- relation interface ---

# probably safe to extend?
import Base.&
(&)() = true

struct RelationIndex{T <: Tuple}
  columns::T
  los::Vector{Int64} # inclusive
  his::Vector{Int64} # exclusive
end

function RelationIndex(columns::T) where {T}
  los = fill(1, length(columns)+1)
  his = fill(length(columns[1])+1, length(columns)+1)
  RelationIndex(columns, los, his)
end

function gallop{T}(column::AbstractArray{T}, lo::Int64, hi::Int64, value::T, threshold::Int64) ::Int64
  if (lo < hi) && cmp(column[lo], value) < threshold
    step = 1
    while (lo + step < hi) && cmp(column[lo + step], value) < threshold
      lo = lo + step
      step = step << 1
    end

    step = step >> 1
    while step > 0
      if (lo + step < hi) && cmp(column[lo + step], value) < threshold
        lo = lo + step
      end
      step = step >> 1
    end

    lo += 1
  end
  lo
end

function next(index::RelationIndex, ::Type{Val{C}}) where {C}
  column = index.columns[C]
  prev_hi = index.his[C]
  lo = index.his[C+1]
  if lo < prev_hi
    value = column[lo]
    hi = gallop(column, lo+1, prev_hi, value, 1)
    index.los[C+1] = lo
    index.his[C+1] = hi
    lo < hi
  else
    false
  end
end

function seek(index::RelationIndex, ::Type{Val{C}}, value) where {C}
  column = index.columns[C]
  prev_hi = index.his[C]
  lo = index.his[C+1]
  if lo < prev_hi
    lo = gallop(column, lo, prev_hi, value, 0)
    hi = gallop(column, lo+1, prev_hi, value, 1)
    index.los[C+1] = lo
    index.his[C+1] = hi
    lo < hi
  else
    false
  end
end

function can_index(::Type{Relation{T}}) where {T}
  true
end

function get_index(::Type{Relation{T}}, fun::Symbol, permutation::Vector{Int64}) where {T}
  quote
    columns = Data.index($(esc(fun)), $permutation)
    permuted = ($(@splice ix in permutation :(columns[$ix])),)
    RelationIndex(permuted)
  end
end

# technically wrong, but good enough for now
function count(::Type{Relation{T}}, index::Symbol, args::Vector{Symbol}, var::Symbol) where {T}
  @assert var == args[end]
  first_column = findfirst(args, args[end]) # first repetition of last var
  quote
    $(esc(index)).his[$first_column] - $(esc(index)).los[$first_column]
  end
end

function iter(::Type{Relation{T}}, index::Symbol, args::Vector{Symbol}, var::Symbol, f) where {T}
  @assert var == args[end]
  first_column = findfirst(args, args[end]) # first repetition of last var
  last_column = length(args)
  quote
    while next($(esc(index)), $(Val{first_column}))
      $(esc(var)) = $(esc(index)).columns[$first_column][$(esc(index)).los[$first_column+1]]
      if (&)($(@splice column in (first_column+1):last_column quote
        $(esc(index)).his[$column+1] = $(esc(index)).los[$column]
        seek($(esc(index)), $(Val{column}), $(esc(var)))
      end),)
        $(esc(inline(f, var)))
      end
    end
  end
end

function prepare(::Type{Relation{T}}, index::Symbol, args::Vector{Symbol}) where {T}
  first_column = findfirst(args, args[end]) # first repetition of last var
  quote
    $(esc(index)).his[$first_column+1] = $(esc(index)).los[$first_column]
  end
end

function contains(::Type{Relation{T}}, index::Symbol, args::Vector{Symbol}) where {T}
  first_column = findfirst(args, args[end]) # first repetition of last var
  last_column = length(args)
  var = args[end]
  quote
    (&)(
      seek($(esc(index)), $(Val{first_column}), $(esc(var))),
      $(@splice column in (first_column+1):last_column quote
        $(esc(index)).his[$column+1] = $(esc(index)).los[$column]
        seek($(esc(index)), $(Val{column}), $(esc(var)))
      end) 
    )
  end
end

function narrow_types(::Type{Relation{T}}, fun_name, arg_types::Vector{Type}) where T
  # just return the types of the columns
  return Type[eltype(T.parameters[i]) for i in 1:length(arg_types)]
end

# --- function interface ---

function can_index(::Type{T}) where {T <: Function}
  false
end

function count(::Type{T}, fun, args::Vector{Symbol}, var::Symbol) where {T <: Function}
  if var == args[end]
    1
  else 
    typemax(Int64)
  end
end

function iter(::Type{T}, fun, args::Vector{Symbol}, var::Symbol, f) where {T <: Function}
  value = gensym("value")
  if var == args[end]
    quote
      $(esc(args[end])) = $(esc(fun))($(map(esc, args[1:end-1])...))
      $(esc(inline(f, var)))
    end
  else
    quote
      error($("Cannot join over $fun($(join(args, ", ")))"))
    end
  end
end

function prepare(::Type{T}, fun, args::Vector{Symbol}) where {T <: Function}
  nothing
end

function contains(::Type{T}, fun, args::Vector{Symbol}) where {T <: Function}
  quote
    $(esc(fun))($(map(esc, args[1:end-1])...)) == $(esc(args[end]))
  end
end

function narrow_types(::Type{T}, fun_name, arg_types::Vector{Type}) where {T <: Function}
  # use Julia's type inference to narrow the type of the last arg
  @assert isa(fun_name, Function) "Julia functions need to be early-bound (ie function pointers, not symbols) so we can infer types"
  return_types = Base.return_types(fun_name, tuple(arg_types[1:end-1]...))
  @assert !isempty(return_types) "This function cannot be called with these types: $fun_name $arg_types"
  new_arg_types = copy(arg_types)
  new_arg_types[end] = reduce(typejoin, return_types)
  return new_arg_types
end

# --- codegen ---

macro count(call, var); count(call.typ, call.name, convert(Vector{Symbol}, call.args), var); end
macro iter(call, var, f); iter(call.typ, call.name, convert(Vector{Symbol}, call.args), var, f); end
macro prepare(call); prepare(call.typ, call.name, convert(Vector{Symbol}, call.args)); end
macro contains(call); contains(call.typ, call.name, convert(Vector{Symbol}, call.args)); end

macro state(env, state::Index, fun_type::Function)
  fun = gensym("fun")
  quote 
    $(esc(fun))::$(fun_type(state.fun)) = $(esc(env))[$(Expr(:quote, state.fun))]
    const $(esc(state.name)) = $(get_index(state.typ, fun, state.permutation))
  end
end

macro state(env, state::Result, fun_type::Function)
  quote
    const $(esc(state.name)) = Relation(($(@splice typ in state.typs quote
      $typ[]
    end),))
  end
end

macro product(ring::Ring, domain::Vector{Union{FunCall, IndexCall}}, value::Vector{Union{Symbol, FunCall, Constant}})
  code = :result
  for call in reverse(value)
    called = @match call begin
      _::FunCall => :(($(call.name))($(call.args...)))
      _::Symbol => call
    end
    code = quote
      result = $(ring.mult)(result, $(esc(called)))
      if result == $(ring.zero)
        $(ring.zero)
      else
        $code
      end
    end
  end
  for call in reverse(domain)
    code = quote
      if !@contains($call)
        $(ring.zero)
      else
        $code
      end
    end
  end
  quote
    result = $(ring.one)
    $code
  end
end

macro sum(ring::Ring, call::Union{FunCall, IndexCall}, var, f)
  value = gensym("value")
  quote
    result = $(ring.zero)
    @iter($call, $(esc(var)), ($(esc(value))) -> begin
      result = $(ring.add)(result, $(esc(inline(f, value))))
    end)
    result
  end
end

macro body(args::Vector{Symbol}, body::SumProduct)
  @assert !isempty(body.domain) "Cant join on an empty domain"
  free_vars = setdiff(union(map((call) -> call.args, body.domain)...), args)
  @assert length(free_vars) == 1 "Need to have factorized all lambdas: $free_vars $body"
  var = free_vars[1]
  quote
    $(@splice call in body.domain quote
      @prepare($call)
    end)
    mins = tuple($(@splice call in body.domain quote
      @count($call, $(esc(var)))
    end))
    min = Base.min(mins...)
    $(@splice i in 1:length(body.domain) quote
      if mins[$i] == min
        return @sum($(body.ring), $(body.domain[i]), $(esc(var)), ($(esc(var))) -> begin
          @product($(body.ring), $(body.domain[1:length(body.domain) .!= i]), $(body.value))
        end)
      end
    end)
    error("Impossibles!")
  end
end

macro body(args::Vector{Symbol}, body::Insert)
  value = @match body.value begin
    call::FunCall => :(($(esc(call.name)))($(map(esc, call.args)...),))
    constant::Constant => constant.value
  end
  quote
    value = $value
    if value != $(body.ring.zero)
      $(@splice (arg_num,arg) in enumerate(body.args) quote
        push!($(esc(body.result_name)).columns[$arg_num], $(esc(arg)))
      end)
    end
    push!($(esc(body.result_name)).columns[end], value)
    value
  end
end

macro fun(fun::Lambda)
  quote
    const $(esc(fun.name)) = ($(map(esc, fun.args)...),) -> begin
      @body($(fun.args), $(fun.body))
    end 
  end
end

macro fun(fun::Return)
  quote
    ($(esc(fun.value.name)))($(map(esc, fun.value.args)...),)
    const $(esc(fun.name)) = $(esc(fun.result_name))
    # TODO reduce result
  end
end

macro program(program::Program, fun_type::Function)
  quote
    function setup(env) 
      $(@splice state in program.states quote
        @state(env, $state, $fun_type)
      end)
      $(@splice fun in program.funs quote
        @fun($fun)
      end)
    end
  end
end

# --- compiler ----

function Compiler.factorize(program::Program, vars::Vector{Symbol}) ::Program
  @assert length(program.funs) == 1
  lambda = program.funs[1]

  # for each call, figure out at which var we have all the args available
  latest_var_nums = map(lambda.body.domain) do call
    maximum(call.args) do arg
      findfirst(vars, arg)
    end
  end

  # make individual funs
  funs = map(1:length(vars)) do var_num
    var = vars[var_num]
    domain = lambda.body.domain[latest_var_nums .== var_num]
    value = (var in lambda.body.value) ? [var] : []
    body = SumProduct(lambda.body.ring, domain, value)
    args = union(lambda.args, vars[1:var_num-1])
    Lambda(gensym("lambda"), args, body)
  end

  # stitch them all together
  for i in 1:(length(funs)-1)
    push!(funs[i].body.value, FunCall(funs[i+1].name, Function, funs[i+1].args))
  end

  # return funs in definition order - cant call funs that haven't been defined yet
  Program(program.states, reverse(funs)) 
end

function insert_indexes(program::Program, vars::Vector{Symbol}, fun_type::Function) ::Program
  @assert length(program.funs) == 1
  lambda = program.funs[1]
  
  domain = Union{FunCall, IndexCall}[]
  indexes = Index[]
  for call in lambda.body.domain
    typ = fun_type(call.name)
    if can_index(typ)
      # sort args according to variable order
      n = length(call.args)
      permutation = Vector(1:n)
      sort!(permutation, by=(ix) -> findfirst(vars, call.args[ix]))
      name = gensym("index")
      push!(indexes, Index(name, typ, call.name, permutation))
      # insert all prefixes of args
      args = call.args[permutation]
      for i in 1:n
        if (i == length(args)) || (args[i] != args[i+1]) # don't emit repeated variables
          push!(domain, IndexCall(name, typ, args[1:i]))
        end
      end
    else
      push!(domain, FunCall(call.name, typ, call.args))
    end
  end
  
  lambda = Lambda(lambda.name, lambda.args, SumProduct(lambda.body.ring, domain, lambda.body.value))
  Program(vcat(program.states, indexes), [lambda])
end

function functionalize(lambda::Lambda) ::Lambda
  # rename args so they don't collide with vars
  args = map(gensym, lambda.args)
  
  domain = copy(lambda.body.domain)
  for (arg, var) in zip(args, lambda.args)
    push!(domain, FunCall(identity, typeof(identity), [arg, var])) # var = identity(arg)
  end
  
  Lambda(lambda.name, args, SumProduct(lambda.body.ring, domain, lambda.body.value))
end

function relationalize(program::Program, args::Vector{Symbol}, vars::Vector{Symbol}, var_type::Function) ::Program
  states = copy(program.states)
  funs = copy(program.funs) # TODO we mutate the funs - need to deepcopy :(
  
  # initialize a relation
  result_name = gensym("result")
  arg_types = map(var_type, args)
  push!(arg_types, eltype(funs[1].body.ring))
  result = Result(result_name, arg_types)
  push!(states, result)
  
  # TODO need to collect up all values, rather than just wrapping the end
  
  # find the earliest point at which all args are bound
  insert_point = findlast(funs) do fun
    all(args) do arg
      arg in fun.args
    end
  end

  # put an Insert somewhere 
  name = gensym("lambda")
  if insert_point == 0
    # go right at the end, call from the last fun
    old_fun = funs[1]
    new_fun_args = unique(vcat(old_fun.args, args))
    push!(old_fun.body.value, FunCall(name, Any, new_fun_args))
    new_fun = 
      Lambda(name, new_fun_args, 
        Insert(result_name, old_fun.body.ring, copy(args),
          Constant(old_fun.body.ring.one)))  
  else
    # go in the middle somewhere, wrap some existing fun by stealing its name 
    old_fun = funs[insert_point]
    new_fun_args = copy(old_fun.args)
    new_fun = 
      Lambda(old_fun.name, new_fun_args, 
        Insert(result_name, old_fun.body.ring, copy(args),
          FunCall(name, Function, new_fun_args)))
    old_fun.name = name
  end 
  insert!(funs, insert_point+1, new_fun) 
        
  # finish with a Return
  old_fun = funs[end]
  new_fun = 
    Return(gensym("return"), result_name,
      FunCall(old_fun.name, Function, copy(old_fun.args)))
  push!(funs, new_fun)
  
  Program(states, funs)
end

function order_vars(lambda::Lambda) ::Vector{Symbol}
  # just use order vars appear in the ast for now
  union(map((call) -> call.args, lambda.body.domain)...)
end

function lower_constants(lambda::Lambda) ::Lambda
  constants = FunCall[]

  lower_constant = (arg) -> begin
    if isa(arg, Constant)
      var = gensym("constant")
      fun = @eval () -> $(arg.value)
      push!(constants, FunCall(fun, typeof(fun), [var]))
      var
    else
      arg
    end
  end

  domain = map(lambda.body.domain) do call
    typeof(call)(call.name, call.typ, map(lower_constant, call.args))
  end

  value = map(lower_constant, lambda.body.value)

  Lambda(lambda.name, lambda.args, SumProduct(lambda.body.ring, vcat(constants, domain), value))
end

const inference_fixpoint_limit = 100

function infer_var_types(lambda::Lambda, fun_type::Function, vars::Vector{Symbol}) ::Dict{Symbol, Type}
  var_type = Dict{Symbol, Type}((var => Any for var in vars))
  
  # loop until fixpoint
  for i in 1:inference_fixpoint_limit
    new_var_type = copy(var_type)
    # infer type for each call in domain
    for call in lambda.body.domain
      narrowed_types = narrow_types(fun_type(call.name), call.name, Type[var_type[arg] for arg in call.args])
      for (arg, typ) in zip(call.args, narrowed_types)
        new_var_type[arg] = typeintersect(new_var_type[arg], typ)
      end
    end
    # if at fixpoint, return
    if new_var_type == var_type
      return var_type
    else
      var_type = new_var_type
    end
  end
  
  return error("Type inference failed to reach fixpoint after $inference_fixpoint_limit iterations")
end

function compile_function(lambda::Lambda, fun_type::Function)
  lambda = lower_constants(lambda)
  vars = order_vars(lambda)
  raw_var_type = infer_var_types(lambda, fun_type, vars)
  var_type = (var) -> raw_var_type[var]
  lambda = functionalize(lambda)
  program = Program([], [lambda])
  program = insert_indexes(program, vars, fun_type)
  program = factorize(program, vars)
  code = macroexpand(Compiler, quote
    @Compiler.program($program, $fun_type)
  end)
  # @render simplify_expr(code)
  eval(code)
end

function compile_relation(lambda::Lambda, fun_type::Function)
  lambda = lower_constants(lambda)
  vars = order_vars(lambda)
  raw_var_type = infer_var_types(lambda, fun_type, vars)
  var_type = (var) -> raw_var_type[var]
  args = lambda.args
  lambda = Lambda(lambda.name, Symbol[], lambda.body)
  program = Program([], [lambda])
  program = insert_indexes(program, vars, fun_type)
  program = factorize(program, vars)
  program = relationalize(program, args, vars, var_type)
  code = macroexpand(Compiler, quote
    @Compiler.program($program, $fun_type)
  end)
  # @render simplify_expr(code)
  eval(code)
end

export FunCall, Constant, Ring, SumProduct, Lambda, compile_relation, compile_function

end
