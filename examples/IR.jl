module IR

using Imp.Data
using Imp.Compiler

zz(x, y) = (x * x) + (y * y) + (3 * x * y)

polynomial_ast1 = Lambda(
  :poly1,
  [],
  SumProduct(
    Ring{Int64}(+,*,1,0),
    [
      FunCall(:xx, Any, [:i, :x]),
      FunCall(:yy, Any, [:i, :y]),
      FunCall(zz, Any, [:x, :y, :z]),
    ],
    [:z]
  )
)  

polynomial_ast2 = Lambda(
  :poly2,
  [],
  SumProduct(
    Ring{Int64}(+,*,1,0),
    [
      FunCall(:xx, Any, [:x, :x]),
      FunCall(:yy, Any, [:x, :y]),
      FunCall(zz, Any, [:x, :y, :z]),
    ],
    [:z]
  )
)

polynomial_ast3 = Lambda(
    :poly3,
    [],
    SumProduct(
      Ring{Int64}(+,*,1,0),
      [
        FunCall(:xx, Any, [:i, :x]),
        FunCall(:yy, Any, [:i, :y]),
        FunCall(*, Any, [:x, :x, :t1]),
        FunCall(*, Any, [:y, :y, :t2]),
        FunCall(*, Any, [Constant(3), :x, :y, :t3]),
        FunCall(+, Any, [:t1, :t2, :t3, :z])
      ],
      [:z]
    )
)

polynomial_ast4 = Lambda(
    :poly4,
    [:i],
    SumProduct(
      Ring{Int64}(+,*,1,0),
      [
        FunCall(:xx, Any, [:i, :x]),
        FunCall(:yy, Any, [:i, :y]),
        FunCall(*, Any, [:x, :x, :t1]),
        FunCall(*, Any, [:y, :y, :t2]),
        FunCall(*, Any, [Constant(3), :x, :y, :t3]),
        FunCall(+, Any, [:t1, :t2, :t3, :z])
      ],
      [:z]
    )
)

const xx = Relation((collect(0:100),collect(0:100)))
const yy = Relation((collect(0:100), collect(reverse(0:100))))
const big_xx = Relation((collect(0:1000000),collect(0:1000000)))
const big_yy = Relation((collect(0:1000000), collect(reverse(0:1000000))))

fun_type(fun) = typeof(eval(fun))
p1 = compile_function(polynomial_ast1, fun_type)
p2 = compile_function(polynomial_ast2, fun_type)
p3 = compile_function(polynomial_ast3, fun_type)
p4 = compile_function(polynomial_ast4, fun_type)
p5 = compile_relation(polynomial_ast4, fun_type)
p6 = compile_relation(polynomial_ast3, fun_type)

inputs = Dict(:xx => xx, :yy => yy, :zz => zz)
expected = @show sum(((x * x) + (y * y) + (3 * x * y) for (x,y) in zip(xx.columns[2], yy.columns[2])))
j1 = p1(inputs)
j2 = p2(inputs)
j3 = p3(inputs)
@assert j1() == expected
@assert j2() == expected
@assert j3() == expected
@assert Base.return_types(j1, ()) == [Int64]
@assert Base.return_types(j2, ()) == [Int64]
@assert Base.return_types(j3, ()) == [Int64]

j4 = p4(inputs)
@assert Base.return_types(j4, (Int64,)) == [Int64]
for x in 0:100
  y = 100-x
  z = (x * x) + (y * y) + (3 * x * y)
  @assert j4(x) == z
end

j5 = p5(inputs)
@assert Base.return_types(p5, (typeof(inputs),)) == [Data.Relation{Tuple{Vector{Int64},Vector{Int64}}}]
for x in 0:100
  y = 100-x
  z = (x * x) + (y * y) + (3 * x * y)
  @assert j5.columns[1][x+1] == x
  @assert j5.columns[2][x+1] == z
end

j6 = p6(inputs)
@assert Base.return_types(p6, (typeof(inputs),)) == [Data.Relation{Tuple{Vector{Int64}}}]
@assert j6.columns[1][1] == expected

# using BenchmarkTools
# big_inputs = Dict(:xx => big_xx, :yy => big_yy, :zz => zz)
# @show @benchmark p1(big_inputs)()
# @show @benchmark p2(big_inputs)()
# @show @benchmark p3(big_inputs)()

end
