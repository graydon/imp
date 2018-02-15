module Util

macro showtime(expr)
  quote
    result = @time $(esc(expr))
    println($(string("^ ", expr)))
    println()
    result
  end
end

macro splice(iterator, body)
  @assert iterator.head == :call
  @assert iterator.args[1] == :in
  Expr(:..., :(($(esc(body)) for $(esc(iterator.args[2])) in $(esc(iterator.args[3])))))
end

export @showtime, @splice

function get_method_instance(f, typs)
  world = ccall(:jl_get_world_counter, UInt, ())
  tt = typs isa Type ? Tuple{typeof(f), typs.parameters...} : Tuple{typeof(f), typs...}
  results = Base._methods_by_ftype(tt, -1, world)
  @assert length(results) == 1 "get_method_instance returned multiple methods: $results"
  (_, _, meth) = results[1]
  # TODO not totally sure what jl_match_method is needed for - I think it's just extracting type parameters like `where {T}`
  (ti, env) = ccall(:jl_match_method, Any, (Any, Any), tt, meth.sig)::SimpleVector
  meth = Base.func_for_method_checked(meth, tt)
  linfo = ccall(:jl_specializations_get_linfo, Ref{Core.MethodInstance}, (Any, Any, Any, UInt), meth, tt, env, world)
end

function get_code_info(method_instance::Core.MethodInstance)
  world = ccall(:jl_get_world_counter, UInt, ())
  params = Core.Inference.InferenceParams(world)
  optimize = true # TODO optimize=false prevents inlining, which is useful, but also prevents static calls from being detected
  (_, code_info, return_typ) = Core.Inference.typeinf_code(method_instance, optimize, true, params)
  (code_info, return_typ)
end

# TODO in 0.7, should this be `isdispatchtuple`?
function is_safe_type(typ::Type)
  isleaftype(typ) && (typ != Core.Box)
end

"Does this expression never have a real type?"
function is_untypeable(expr::Expr)
  expr.head in [:(=), :line, :boundscheck, :gotoifnot, :return, :meta, :inbounds, :throw] || (expr.head == :call && expr.args[1] == :throw)
end

struct Body 
  typ::Type
end

@enum WarningKind NotConcretelyTyped Boxed DynamicCall

const Location = Union{Expr, TypedSlot, Body}

struct Warning
  kind::WarningKind
  location::Location
end

struct Warnings
  code_info::CodeInfo # needed for nice printing of Slot
  warnings::Vector{Warning}
end

function warn_type!(location::Location, typ::Type, warnings::Vector{Warning})
  if !isleaftype(typ)
    push!(warnings, Warning(NotConcretelyTyped, location))
  end
  
  if typ == Core.Box
    push!(warnings, Warning(Boxed, location))
  end
end

function warn!(body::Body, warnings::Vector{Warning})
  warn_type!(body, body.typ, warnings)
end

function warn!(expr::Expr, warnings::Vector{Warning})
  # many Exprs always have type Any
  if !(expr.head in [:(=), :line, :boundscheck, :gotoifnot, :return, :meta, :inbounds]) && !(expr.head == :call && expr.args[1] == :throw) 
    warn_type!(expr, expr.typ, warnings)
    if expr.head == :call
      # TODO logic for checking dynamic calls is tricky
      # should check that type of arg[1] is known eg GlobalRef
      # if any((arg) -> !is_safe_type(arg.typ), expr.args[2:end])
      #   push!(warnings, Warning(DynamicCall, expr))
      # end
    end
  end
end

function warn!(slot::TypedSlot, warnings::Vector{Warning})
  warn_type!(slot, slot.typ, warnings)
end

function warnings(method_instance::Core.MethodInstance)
  code_info, return_typ = get_code_info(method_instance)
  warnings = Warning[]
  
  warn!(Body(return_typ), warnings)
  
  slot_is_used = [false for _ in code_info.slotnames]
  function walk_expr(expr)
      if isa(expr, Slot)
        slot_is_used[expr.id] = true
      elseif isa(expr, Expr)
        warn!(expr, warnings)
        foreach(walk_expr, expr.args)
      end
  end
  foreach(walk_expr, code_info.code)
  
  for (slot, is_used) in enumerate(slot_is_used)
    if is_used
      typ = code_info.slottypes[slot]
      warn!(TypedSlot(slot, typ), warnings)
    end
  end
  
  Warnings(code_info, warnings)
end

function Base.show(io::IO, warning::Warning) 
  print(io, "Warning(")
  print(io, warning.kind)
  print(io, ", ")
  Base.show_unquoted(IOContext(io, :TYPEEMPHASIZE => true), warning.location)
  print(io, ")")
end

function Base.show(io::IO, warnings::Warnings) 
  slotnames = Base.sourceinfo_slotnames(warnings.code_info)
  print(io, "Warnings(\n")
  inner_io = IOContext(io, :SOURCEINFO => warnings.code_info, :SOURCE_SLOTNAMES => slotnames)
  # for some reason this does not indent
  # Base.show_list(inner_io, warnings.warnings, ",\n", 4)
  for warning in warnings.warnings
    print(io, "    ")
    show(inner_io, warning)
    println(io)
  end
  print(io, ")")
end

function get_child_calls(method_instance::Core.MethodInstance)
  code_info, return_typ = get_code_info(method_instance)
  calls = Set{Core.MethodInstance}()
  
  function walk_expr(expr)
      if isa(expr, Core.MethodInstance)
        push!(calls, expr)
      elseif isa(expr, Expr)
        foreach(walk_expr, expr.args)
      end
  end
  foreach(walk_expr, code_info.code)
  
  calls
end

function call_graph(method_instance::Core.MethodInstance, max_calls=1000::Int64) ::Vector{Pair{Core.MethodInstance, Set{Core.MethodInstance}}}
  all = Dict{Core.MethodInstance, Set{Core.MethodInstance}}()
  ordered = Vector{Core.MethodInstance}()
  unexplored = Set{Core.MethodInstance}((method_instance,))
  for _ in 1:max_calls
    if isempty(unexplored)
      return [call => all[call] for call in ordered]
    end
    method_instance = pop!(unexplored)
    child_calls= get_child_calls(method_instance)
    all[method_instance] = child_calls
    push!(ordered, method_instance)
    for child_call in child_calls
      if !haskey(all, child_call)
        push!(unexplored, child_call)
      end
    end
  end
  error("call_graph reached $max_calls calls and gave up")
end

function pretty(method_instance::Core.MethodInstance)
  original = string(method_instance)
  shortened = replace(original, "MethodInstance for ", "")
  method = method_instance.def
  "$shortened in $(method.module) at $(method.file):$(method.line)"
end

function analyze(f, typs)
  for (call, child_calls) in call_graph(get_method_instance(f, typs))
    print(pretty(call)); println();
    for child_call in child_calls
      print("  Calls: "); print(pretty(child_call)); println();
    end
    for warning in warnings(call).warnings
      print("  "); print(warning.kind); print(": "); show(warning.location); println();
    end
  end
end

@eval begin
  macro warnings(ex0)
    Base.gen_call_with_extracted_types($(Expr(:quote, :warnings)), ex0)
  end
  macro analyze(ex0)
    Base.gen_call_with_extracted_types($(Expr(:quote, :analyze)), ex0)
  end
end

end
