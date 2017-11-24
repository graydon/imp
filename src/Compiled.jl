module Compiled

using Base.Cartesian

# interface for all functions

function is_function(fun_type)
  false
end 
# function call(fun, args...) end 
# function permute(fun, columns) end 

# interface for finite functions

function is_finite(fun_type) 
  false
end 
# function finger(fun) end 
# function count(fun, finger) end 
# function first(fun, outer_finger) end
# function next(fun, outer_finger, inner_finger) end
# function seek(fun, finger, value) end
# function get(fun, finger) end

# implementation for tuple of column vectors

struct Relation{T <: Tuple} 
  columns::T
end

struct RelationFinger{Column}
  lo::UInt64 # inclusive
  hi::UInt64 # exclusive
end

function gallop{T}(column::AbstractArray{T}, lo::UInt64, hi::UInt64, value::T, threshold::Int64)::UInt64
  @inbounds if (lo < hi) && cmp(column[lo], value) < threshold
    step = UInt64(1)
    while (lo + step < hi) && cmp(column[lo + step], value) < threshold
      lo = lo + step
      step = step << UInt64(1)
    end

    step = step >> UInt64(1)
    while step > UInt64(1)
      if (lo + step < hi) && cmp(column[lo + step], value) < threshold
        lo = lo + step
      end
      step = step >> UInt64(1)
    end

    lo += UInt64(1)
  end
  lo
end

function is_finite(::Type{Relation}) 
  true
end
  
function finger(fun::Relation)
  RelationFinger{0}(1, length(fun.columns[1]) + 1)
end

function count(fun::Relation, finger::RelationFinger) 
  # not actually correct, but will do for now
  finger.hi - finger.lo
end

function first(fun::Relation, outer_finger::RelationFinger{C}) where {C}
  C2 = C + 1
  column = fun.columns[C2]
  lo = outer_finger.lo
  value = column[lo]
  hi = gallop(column, lo, outer_finger.hi, value, 1)
  RelationFinger{C2}(lo, hi)
end

function next(fun::Relation, outer_finger::RelationFinger{C}, inner_finger::RelationFinger{C2}) where {C, C2}
  @assert C2 == C + 1
  column = fun.columns[C2]
  lo = inner_finger.hi
  if lo < outer_finger.hi
    value = column[lo]
    hi = gallop(column, lo, outer_finger.hi, value, 1)
    RelationFinger{C2}(lo, hi)
  else 
    RelationFinger{C2}(lo, outer_finger.hi)
  end
end

# TODO can push this into gallop, to search lo and hi at same time
#      or maybe use searchsorted if we can remove the cost of the subarray
function seek(fun::Relation, finger::RelationFinger{C}, value) where {C}
  C2 = C + 1
  column = fun.columns[C2]
  lo = gallop(column, finger.lo, finger.hi, value, 0)
  hi = gallop(column, lo+1, finger.hi, value, 1)
  RelationFinger{C2}(lo, hi)
end

function get(fun::Relation, finger::RelationFinger{C}) where {C}
  fun.columns[C][finger.lo]
end

function _join(n)
  quote
    @nexprs $n (i) -> count_i = count(funs[i], fingers[i])
    min_count = @ncall $n min (i) -> count_i
    @nif $n (i) -> count_i == min_count (i) -> begin
      join_inner(Val{i}, funs, fingers, f)
    end
  end
end

function _join_inner(min_i, n)
  inner_fingers = [Symbol("inner_finger_$i") for i in 1:n]
  body = quote 
    f(value, @ntuple $n (i) -> inner_finger_i)
  end
  for i in 1:n
    if i != min_i
      body = quote
        $(inner_fingers[i]) = seek(funs[$i], outer_fingers[$i], value)
        if count(funs[$i], $(inner_fingers[i])) > 0
          $body
        end
      end
    end
  end
  quote
    $(inner_fingers[min_i]) = first(funs[$min_i], outer_fingers[$min_i])
    while count(funs[$min_i], $(inner_fingers[min_i])) > 0
      value = get(funs[$min_i], $(inner_fingers[min_i])) 
      $body
      $(inner_fingers[min_i]) = next(funs[$min_i], outer_fingers[$min_i], $(inner_fingers[min_i]))
    end
  end
end

@generated function join(funs::Tuple, fingers::Tuple, f::Function)
  n = length(funs.parameters)
  _join(n)
end

@generated function join_inner(::Type{Val{MinI}}, funs::Tuple, outer_fingers::Tuple, f::Function) where {MinI}
  n = length(funs.parameters)
  _join_inner(MinI, n)
end

using BenchmarkTools

macroexpand(_join(2))
macroexpand(_join_inner(1,2))

function polynomial(xx, yy)
  const results_x = Int64[]
  const results_y = Int64[]
  const results_z = Int64[]

  const xx_finger = finger(xx)
  const yy_finger = finger(yy)
  join((xx, yy), (xx_finger, yy_finger), (i, fingers) -> begin
    (xx_finger, yy_finger) = fingers
    join((xx,), (xx_finger,), (x, fingers) -> begin
      (xx_finger,) = fingers
      join((yy,), (yy_finger,), (y, fingers) -> begin
        (yy_finger,) = fingers
        push!(results_x, x)
        push!(results_y, y)
        push!(results_z, (x * x) + (y * y) + (3 * x * y))
      end)
    end)
  end)
  
  results_z
end

@show @time polynomial(
  Relation((collect(0:10),collect(0:10))),
  Relation((collect(0:10), collect(reverse(0:10))))
)

# # @code_warntype polynomial([1,2],[1,4],[1,2],[1,-4])
# # @code_llvm polynomial([1,2],[1,4],[1,2],[1,-4])
# 


# @time polynomial(
# collect(0:1000000),
# collect(0:1000000),
# collect(0:1000000),
# collect(reverse(0:1000000))
# )

@show @benchmark polynomial(
$(Relation((collect(0:1000000),collect(0:1000000)))),
$(Relation((collect(0:1000000), collect(reverse(0:1000000)))))
)

end
