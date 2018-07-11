const LoHi = UnitRange{Int64}

mutable struct Factor{ColumnIx, Columns <: NTuple{N, Vector} where N}
    columns::Columns # at least one column, at least one row
    bounds::LoHi
    focus::LoHi
end

Factor(columns) = Factor{0, typeof(columns)}(columns, 1:length(columns[1]), 1:length(columns[1]))

function Base.getproperty(factor::Factor{column_ix}, k::Symbol) where {column_ix}
    if k == :column_ix
        column_ix
    else
        getfield(factor, k)
    end
end

function factor_next_column(in_factor::Factor)::Factor
    Factor{in_factor.column_ix+1, typeof(in_factor.columns)}(in_factor.columns, 1:0, 1:0)
end

function factor_first!(in_factor::Factor, out_factor::Factor)
    out_factor.bounds = in_factor.focus
    out_factor.focus = in_factor.focus.start:(in_factor.focus.start-1)
end

function factor_next!(factor::Factor)::Union{Any, Nothing}
    column = factor.columns[factor.column_ix]
    focus = factor.focus
    bounds = factor.bounds
    focus.stop >= bounds.stop && return nothing
    start = focus.stop + 1
    value = column[start]
    stop = gallop(column, start, bounds.stop + 1, value, 1) - 1
    factor.focus = start:stop
    value
end

function factor_seek!(factor::Factor, value)::Bool
    column = factor.columns[factor.column_ix]
    focus = factor.focus
    bounds = factor.bounds
    start = gallop(column, focus.stop + 1, bounds.stop + 1, value, 0)
    # TODO start + 1 ?
    stop = gallop(column, start, bounds.stop + 1, value, 1) - 1
    factor.focus = start:stop
    start <= stop
end

function factor_count(factor::Factor)::Int64
    factor.bounds.stop - factor.bounds.start + 1
end

abstract type Query end

struct GenericJoin <: Query
    factor_ixes
    tail::Query
end

struct StagedGenericJoin{FactorIxes, InFactors, OutFactors, Tail}
    in_factors::InFactors
    out_factors::OutFactors
    tail::Tail
end

StagedGenericJoin(factor_ixes, in_factors, out_factors, tail) = StagedGenericJoin{factor_ixes, typeof(in_factors), typeof(out_factors), typeof(tail)}(in_factors, out_factors, tail)

function Base.getproperty(join::StagedGenericJoin{factor_ixes}, k::Symbol) where {factor_ixes}
    if k == :factor_ixes
        factor_ixes
    else
        getfield(join, k)
    end
end

function stage(join::GenericJoin, in_factors)
    out_factors = Any[in_factors...]
    for factor_ix in join.factor_ixes
        out_factors[factor_ix] = factor_next_column(in_factors[factor_ix])
    end
    out_factors = tuple(out_factors...)
    tail = stage(join.tail, out_factors)
    StagedGenericJoin(join.factor_ixes, in_factors, out_factors, tail)
end

@generated function execute(join::StagedGenericJoin{factor_ixes}) where {factor_ixes}
    quote
        in_factors = join.in_factors
        out_factors = join.out_factors
        tail = join.tail

        $(@splice factor_ix in factor_ixes quote
          factor_first!(in_factors[$factor_ix], out_factors[$factor_ix])
          end)

        (_, min_ix) = findmin(tuple($(@splice factor_ix in factor_ixes quote
                                 factor_count(out_factors[$factor_ix])
                                 end)))
        min_ix = $factor_ixes[min_ix]

        value = @match min_ix begin
            $(@splice factor_ix in [1,2,3] :(
                $factor_ix => factor_next!(out_factors[$factor_ix])
            ))
        end

        while true
            $(@splice factor_ix in factor_ixes quote
              if $factor_ix != min_ix
              factor_seek!(out_factors[$factor_ix], value) || @goto next
              end
              end)

            execute(tail)

            @label next
            next = @match min_ix begin
                $(@splice factor_ix in [1,2,3] :(
                    $factor_ix => factor_next!(out_factors[$factor_ix])
                ))
            end
            next == nothing && return
            value = next
        end
    end
end

function output(join::StagedGenericJoin)
    output(join.tail)
end

struct Select <: Query
    ixes
end

struct StagedSelect{Ixes, InFactors, Columns}
    in_factors::InFactors
    columns::Columns
end

StagedSelect(ixes, in_factors, columns) = StagedSelect{ixes, typeof(in_factors), typeof(columns)}(in_factors, columns)

function stage(select::Select, in_factors)
    ixes = select.ixes
    columns = tuple((Vector{eltype(in_factors[ix[1]].columns[ix[2]])}() for ix in ixes)...)
    StagedSelect(ixes, in_factors, columns)
end

@generated function execute(select::StagedSelect{ixes}) where {ixes}
    quote
        in_factors = select.in_factors
        columns = select.columns
        $(@splice (i, ix) in enumerate(ixes) quote
          let
          local column = columns[$i]
          local factor = in_factors[$ix[1]]
          local value = factor.columns[$ix[2]][factor.focus.start]
          push!(column, value)
          end
          end)
    end
end

function output(select::StagedSelect)
    select.columns
end

function run(expr::Query, factors)
    staged = stage(expr, factors)
    execute(staged)
    output(staged)
end

# --- basic tests ---

dep = Factor((["Eng", "Eng", "Sec"], ["Alice", "Bob", "Eve"]))
scale = Factor((["Eng", "Sec"], ["Hi", "Lo"]))

run(Select(((1,1), (1,2), (2,2))), (dep, scale))
run(GenericJoin((1,2), Select(((1,1), (1,2), (2,2)))), (dep, scale))
@show @time run(GenericJoin((1,2), GenericJoin((1,), Select(((1,1), (1,2), (2,2))))), (dep, scale))
@time run(GenericJoin((1,2), GenericJoin((1,), Select(((1,1), (1,2), (2,2))))), (dep, scale))

using DataFrames
using CSV
using Dates

Categorical = CSV.CategoricalArrays.CategoricalString{UInt32}
Categorical = String

@time begin
    df_holidays_events = CSV.read("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/holidays_events.csv", types=[Date, Categorical, Categorical, Categorical, Categorical, Bool], dateformat="yyyy-mm-dd", truestring="True", falsestring="False")
    df_items = CSV.read("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/items.csv", types=[Int64, Categorical, Int64, Bool], truestring="1", falsestring="0")
    df_oil = CSV.read("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/oil.csv", types=[Date, Union{Missing, Float64}], dateformat="yyyy-mm-dd")
    df_stores = CSV.read("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/stores.csv", types=[Int64, Categorical, Categorical, Categorical, Int64])
    df_test = CSV.read("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/test.csv", types=[Int64, Date, Int64, Int64, Union{Bool, Missing}], truestring="True", falsestring="False", dateformat="yyyy-mm-dd")
    df_train = CSV.read("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/train-mini.csv", types=[Int64, Date, Int64, Int64, Float64, Union{Bool, Missing}], truestring="True", falsestring="False", dateformat="yyyy-mm-dd")[1:1000, :]
    df_transactions = CSV.read("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/transactions.csv", types=[Date, Int64, Int64], dateformat="yyyy-mm-dd")
end

Factor(dataframe::DataFrames.DataFrame) = Factor(dataframe, 1:length(dataframe.columns))
Factor(dataframe::DataFrames.DataFrame, ixes) = Factor(tuple(sort(dataframe[ixes]).columns...))

using BenchmarkTools

# display(@benchmark DataFrames.join(df_train, df_items, on=[:item_nbr]))
df_out = DataFrames.join(df_train, df_items, on=[:item_nbr])

train = Factor(df_train, [4,1,2,3,5,6])
items = Factor(df_items)
query =
    GenericJoin((1,2),
    GenericJoin((1,),
    GenericJoin((1,),
    GenericJoin((1,),
    GenericJoin((1,),
    GenericJoin((1,),
    GenericJoin((2,),
    GenericJoin((2,),
    Select(((1,1),(1,2),(1,3),(1,4),(1,5),(1,6),(2,2),(2,3),(2,4))),
    ))))))))
display(@benchmark run(query, (train, items)))
out = run(query, (train, items))

# staged = stage(query, (train, items))
# display(@benchmark execute(staged))
# @code_warntype execute(staged)
# @code_warntype execute(staged.tail.tail.tail.tail.tail.tail.tail.tail)

df_test = Factor(df_out, [4,1,2,3,5,6,7,8,9])
test = Factor(out)
for i in 1:9
    @show i
    @assert replace(df_test.columns[i], missing=>nothing) == replace(test.columns[i], missing=>nothing)
end
