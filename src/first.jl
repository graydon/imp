const LoHi = UnitRange{Int64}

mutable struct Factor{Columns <: NTuple{N, Vector} where N}
    columns::Columns # at least one column, at least one row
    column_ix::Int64
    lohi::LoHi
end

Factor(columns) = Factor(columns, 0, 1:length(columns[1]))

function factor_down(factor::Factor)::Factor
    columns = tuple((column[factor.lohi] for column in factor.columns)...)
    column_ix = factor.column_ix + 1
    lohi = 1:(factor.lohi.stop - factor.lohi.start + 1)
    Factor(columns, column_ix, lohi)
end

function factor_first!(factor::Factor)::Any
    columns = factor.columns
    column_ix = factor.column_ix
    value = columns[column_ix][1]
    factor.lohi = searchsorted(columns[column_ix], value)
    value
end

function factor_next!(factor::Factor)::Union{Any, Nothing}
    columns = factor.columns
    column_ix = factor.column_ix
    factor.lohi.stop >= length(columns[column_ix]) && return nothing
    value = factor.columns[column_ix][factor.lohi.stop + 1]
    factor.lohi = searchsorted(columns[column_ix], value)
    value
end

function factor_seek!(factor::Factor, value, output=nothing)::Bool
    columns = factor.columns
    column_ix = factor.column_ix
    lohi = searchsorted(columns[column_ix], value)
    factor.lohi = lohi
    lohi.start <= lohi.stop
end

function factor_count(factor::Factor)::Int64
    factor.lohi.stop - factor.lohi.start + 1
end

struct GenericJoin
    factor_ixes
end

function foreach(f, join::GenericJoin, factors)
    factors = [factors...]
    for factor_ix in join.factor_ixes
        factors[factor_ix] = factor_down(factors[factor_ix])
    end
    
    (_, min_ix) = findmin([factor_count(factor) for factor in factors[[join.factor_ixes...]]])
    min_ix = join.factor_ixes[min_ix]
    
    value = factor_first!(factors[min_ix])
    while true
        for factor_ix in join.factor_ixes
            if factor_ix != min_ix
                factor_seek!(factors[factor_ix], value) || @goto next
            end
        end

        f(tuple(factors...))

        @label next
        next = factor_next!(factors[min_ix])
        next == nothing && return
        value = next
    end
end

struct Chain
    exprs::Tuple
end

function foreach(f, chain::Chain, factors)
    if isempty(chain.exprs)
        f(factors)
    else
        foreach(chain.exprs[1], factors) do factors
            foreach(f, Chain(chain.exprs[2:end]), factors)
        end
    end
end

function select(factor, ix)
    factor.columns[ix][factor.lohi.start]
end

function select(expr, factors, ixes)
    columns = tuple((Vector{eltype(factors[ix[1]].columns[ix[2]])}() for ix in ixes)...)
    foreach(expr, factors) do factors
        for (i, ix) in enumerate(ixes)
            push!(columns[i], select(factors[ix[1]], ix[2]))
        end
    end
    columns
end

function Imp.count(expr, factors)
    total = Ref(0)
    foreach(expr, factors) do factors
        total[] += 1
    end
    total[]
end

# --- basic tests ---

dep = Factor((["Eng", "Eng", "Sec"], ["Alice", "Bob", "Eve"]))
scale = Factor((["Eng", "Sec"], ["Hi", "Lo"]))

select(Chain(()), (dep, scale), ((1,1), (1,2), (2,2)))
select(Chain((GenericJoin((1,2)),)), (dep, scale), ((1,1), (1,2), (2,2)))
select(Chain((GenericJoin((1,2)), GenericJoin((1,)))), (dep, scale), ((1,1), (1,2), (2,2)))

@show @time count(Chain((GenericJoin((1,2)), GenericJoin((1,)))), (dep, scale))
@time count(Chain((GenericJoin((1,2)), GenericJoin((1,)))), (dep, scale))

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

display(@benchmark DataFrames.join(df_train, df_items, on=[:item_nbr]))
df_out = @time DataFrames.join(df_train, df_items, on=[:item_nbr])

train = Factor(df_train, [4,1,2,3,5,6])
items = Factor(df_items)
query = Chain((
    GenericJoin((1,2)),
    GenericJoin((1,)),
    GenericJoin((1,)),
    GenericJoin((1,)),
    GenericJoin((1,)),
    GenericJoin((1,)),
    GenericJoin((2,)),
    GenericJoin((2,)),
))
display(@benchmark select(query, (train, items), ((1,1),(1,2),(1,3),(1,4),(1,5),(1,6),(2,2),(2,3),(2,4))))
out = @time select(query, (train, items), ((1,1),(1,2),(1,3),(1,4),(1,5),(1,6),(2,2),(2,3),(2,4)))

df_test = Factor(df_out, [4,1,2,3,5,6,7,8,9])
test = Factor(out)
for i in 1:9
    @assert replace(df_test.columns[i], missing=>nothing) == replace(test.columns[i], missing=>nothing)
end
