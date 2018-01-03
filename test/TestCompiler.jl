module TestCompiler

using Base.Test

using Imp.Data
using Imp.Compiler

@testset "Compiler" begin

  zz(x, y) = (x * x) + (y * y) + (3 * x * y)
  const xx = Relation((collect(0:100),collect(0:100)))
  const yy = Relation((collect(0:100), collect(reverse(0:100))))
  inputs = Dict(:xx => xx, :yy => yy, :zz => zz)
  
  function fun_type(fun) 
    if fun == :xx
      typeof(xx)
    elseif fun == :yy
      typeof(yy)
    elseif isa(fun, Function)
      typeof(fun)
    else
      error()
    end
  end
  
  expected = sum(((x * x) + (y * y) + (3 * x * y) for (x,y) in zip(xx.columns[2], yy.columns[2])))
  
  @testset "poly1" begin 

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
    
    p1 = compile_function(polynomial_ast1, fun_type)
    j1 = p1(inputs)
    @inferred j1()
    @test j1() == expected
    
  end
    
  @testset "poly2 - repeated variables" begin 
  
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
    
    p2 = compile_function(polynomial_ast2, fun_type)  
    j2 = p2(inputs)
    @inferred j2()
    @test j2() == expected
    
  end
  
  @testset "poly3 - constants" begin
  
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
    
    p3 = compile_function(polynomial_ast3, fun_type)
    j3 = p3(inputs)
    @inferred j3()
    @test j3() == expected
    
  end
  
  @testset "poly4 - compile_function, 1 arg" begin

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

    p4 = compile_function(polynomial_ast4, fun_type)
    @inferred p4(inputs)
    j4 = p4(inputs)
    @inferred j4(1)
    for x in 0:100
      y = 100-x
      z = (x * x) + (y * y) + (3 * x * y)
      @test j4(x) == z
    end
    
  end

  @testset "poly5 - compile_relation, 1 arg" begin
  
    polynomial_ast5 = Lambda(
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
  
    p5 = compile_relation(polynomial_ast5, fun_type)
    @inferred p5(inputs)
    j5 = p5(inputs)
    for x in 0:100
      y = 100-x
      z = (x * x) + (y * y) + (3 * x * y)
      @test j5.columns[1][x+1] == x
      @test j5.columns[2][x+1] == z
    end
    
  end
  
  @testset "poly6 - compile_relation, 0 args" begin
    
    polynomial_ast6 = Lambda(
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
    
    p6 = compile_relation(polynomial_ast6, fun_type)
    @inferred p6(inputs)
    j6 = p6(inputs)
    @test j6.columns[1][1] == expected
    
  end

  # using BenchmarkTools
  # const big_xx = Relation((collect(0:1000000),collect(0:1000000)))
  # const big_yy = Relation((collect(0:1000000), collect(reverse(0:1000000))))
  # big_inputs = Dict(:xx => big_xx, :yy => big_yy, :zz => zz)
  # @show @benchmark p1(big_inputs)()
  # @show @benchmark p2(big_inputs)()
  # @show @benchmark p3(big_inputs)()

end

end
