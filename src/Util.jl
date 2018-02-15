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
  params = Core.Inference.InferenceParams(world, inlining=false)
  optimize = true # TODO optimize=false prevents inlining, which is useful, but also prevents static calls from being detected
  cache = false # not sure if cached copies use the same params
  (_, code_info, return_typ) = Core.Inference.typeinf_code(method_instance, optimize, cache, params)
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

function is_throw(expr)
  expr == :throw ||
  expr == :throw_boundserror || 
  (expr isa GlobalRef && is_throw(expr.name))
end

"Is it pointless to analyze this expression?"
function should_ignore(expr::Expr)
  throws = [:throw, :throw_boundserror, GlobalRef]
  is_throw(expr.head) || 
  (expr.head == :call && is_throw(expr.args[1])) ||
  (expr.head == :call && expr.args[1] isa GlobalRef && is_throw(expr.args[1].name)) ||
  (expr.head == :invoke && expr.args[1] isa Core.MethodInstance && is_throw(expr.args[1].def.name))
end

struct MethodResult 
  typ::Type
end

@enum WarningKind NotConcretelyTyped Boxed DynamicCall

const Location = Union{Expr, TypedSlot, MethodResult}

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

function warn!(result::MethodResult, warnings::Vector{Warning})
  warn_type!(result, result.typ, warnings)
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

function get_warnings(method_instance::Core.MethodInstance)
  code_info, return_typ = get_code_info(method_instance)
  warnings = Warning[]
  
  warn!(MethodResult(return_typ), warnings)
  
  slot_is_used = [false for _ in code_info.slotnames]
  function walk_expr(expr)
      if isa(expr, Slot)
        slot_is_used[expr.id] = true
      elseif isa(expr, Expr)
        if !should_ignore(expr)
          warn!(expr, warnings)
          foreach(walk_expr, expr.args)
        end
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

function get_child_calls(method_instance::Core.MethodInstance)
  code_info, return_typ = get_code_info(method_instance)
  calls = Set{Core.MethodInstance}()
  
  function walk_expr(expr)
      if isa(expr, Core.MethodInstance)
        push!(calls, expr)
      elseif isa(expr, Expr)
        if !should_ignore(expr)
          foreach(walk_expr, expr.args)
        end
      end
  end
  foreach(walk_expr, code_info.code)
  
  calls
end

struct CallNode
  call::Core.MethodInstance
  parent_calls::Set{Core.MethodInstance}
  child_calls::Set{Core.MethodInstance}
end

function call_graph(method_instance::Core.MethodInstance, max_calls=1000::Int64) ::Vector{CallNode}
  all = Dict{Core.MethodInstance, CallNode}()
  ordered = Vector{Core.MethodInstance}()
  unexplored = Set{Tuple{Union{Void, Core.MethodInstance}, Core.MethodInstance}}(((nothing, method_instance),))
  for _ in 1:max_calls
    if isempty(unexplored)
      return [all[call] for call in ordered]
    end
    (parent, call) = pop!(unexplored)
    child_calls= get_child_calls(call)
    parent_calls = parent == nothing ? Set() : Set((parent,))
    all[call] = CallNode(call, parent_calls, child_calls)
    push!(ordered, call)
    for child_call in child_calls
      if !haskey(all, child_call)
        push!(unexplored, (call, child_call))
      else
        push!(all[child_call].parent_calls, call)
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

function pretty(code_info::CodeInfo, warning::Warning)
  slotnames = Base.sourceinfo_slotnames(code_info)
  buffer = IOBuffer()
  io = IOContext(buffer, :TYPEEMPHASIZE => true, :SOURCEINFO => code_info, :SOURCE_SLOTNAMES => slotnames)
  Base.emphasize(io, string(warning.kind)); print(io, ": "); Base.show_unquoted(io, warning.location)
  String(buffer)
end

function pretty(code_info::CodeInfo, return_typ::Type)
  slotnames = Base.sourceinfo_slotnames(code_info)
  buffer = IOBuffer()
  io = IOContext(buffer, :TYPEEMPHASIZE => true, :SOURCEINFO => code_info, :SOURCE_SLOTNAMES => slotnames)
  body = Expr(:body)
  body.args = code_info.code
  body.typ = return_typ
  Base.show_unquoted(io, body)
  String(buffer)
end

function analyze(f, typs, filter::Function)
  for call_node in call_graph(get_method_instance(f, typs))
    if filter(call_node.call)
      println();
      print(pretty(call_node.call)); println();
      for parent_call in call_node.parent_calls
        print("  Called from: "); print(pretty(parent_call)); println();
      end
      for child_call in call_node.child_calls
        print("  Calls: "); print(pretty(child_call)); println();
      end
      warnings = get_warnings(call_node.call)
      for warning in warnings.warnings
        print("  "); print(pretty(warnings.code_info, warning)); println();
      end
      if !isempty(warnings.warnings)
        code_info, return_typ = get_code_info(call_node.call)
        println(pretty(code_info, return_typ))
      end
    end
  end
end

@eval begin
  macro warnings(ex0)
    Base.gen_call_with_extracted_types($(Expr(:quote, :warnings)), ex0)
  end
  macro analyze(filter, ex0)
    expr = Base.gen_call_with_extracted_types($(Expr(:quote, :analyze)), ex0)
    push!(expr.args, esc(filter))
    expr
  end
  macro analyze(ex0)
    :(@analyze((_) -> true, $(esc(ex0))))
  end
end

# TODO distinguish better between static and dynamic calls
# invoke with non-leaf types may be dynamic?
# call with leaf types may be static? 

# STATUS 
# doesn't warn on dynamic calls
# unsure whether foo(Type{T} where T) is dynamic or underspecialized
# doesn't show calls to builtins

end
