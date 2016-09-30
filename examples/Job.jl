module Job

using Data
using Query
using JobData
using Base.Test
using BenchmarkTools
import DataFrames
import SQLite

function q1a()
  @query begin 
    info_type.info(it, "top 250 rank")
    movie_info_idx.info_type(mi, it)
    movie_info_idx.movie(mi, t)
    title.title(t, title)
    title.production_year(t, production_year)
    movie_companies.movie(mc, t)
    movie_companies.note(mc, note)
    @when !contains(note, "(as Metro-Goldwyn-Mayer Pictures)") &&
      (contains(note, "(co-production)") || contains(note, "(presents)"))
    movie_companies.company_type(mc, ct)
    company_type.kind(ct, "production companies")
    return (note::String, title::String, production_year::Int64)
  end
end

function q2a()
  @query begin
    keyword.keyword(k, "character-name-in-title")
    movie_keyword.keyword(mk, k)
    movie_keyword.movie(mk, t)
    title.title(t, title)
    movie_companies.movie(mc, t)
    movie_companies.company(mc, cn)
    company_name.country_code(cn, "[de]")
    return (title::String,)
  end
end

# "Denish" typo is also in original benchmarks
function q3a()
  infos = Set(["Sweden", "Norway", "Germany", "Denmark", "Swedish", "Denish", "Norwegian", "German"])
  @query begin 
    @when contains(keyword, "sequel")
    keyword.keyword(k, keyword)
    movie_keyword.keyword(mk, k)
    movie_keyword.movie(mk, t)
    title.title(t, title)
    title.production_year(t, production_year)
    @when production_year > 2005
    movie_info.movie(mi, t)
    movie_info.info(mi, info)
    @when info in infos
    return (title::String,)
  end
end

function q4a()
  @query begin
    @when contains(keyword, "sequel")
    keyword.keyword(k, keyword)
    movie_keyword.keyword(mk, k)
    movie_keyword.movie(mk, t)
    title.title(t, title)
    title.production_year(t, production_year)
    @when production_year > 2005
    movie_info_idx.movie(mi, t)
    movie_info_idx.info_type(mi, it)
    info_type.info(it, "rating")
    movie_info_idx.info(mi, info)
    @when info > "5.0"
    return (info::String, title::String)
  end
end

function query_names()
  query_names = []
  for num in 1:4
    for char in "abcdef"
      query_name = "$num$char"
      if isdefined(Symbol("q$query_name"))
        push!(query_names, query_name)
      end
    end
  end
  query_names
end

function test()
  for query_name in query_names()
    results_imp = eval(Symbol("q$(query_name)"))()
    query = rstrip(readline("../job/$(query_name).sql"))
    query = query[1:(length(query)-1)] # drop ';' at end
    query = replace(query, "MIN", "")
    query = "copy ($query) to '/tmp/results.csv' with CSV DELIMITER ',';"
    run(`sudo -u postgres psql -c $query`)
    frame = DataFrames.readtable(open("/tmp/results.csv"), header=false, eltypes=[eltype(c) for c in results_imp.columns])
    num_columns = length(results_imp)
    results_pg = Relation(tuple((frame[ix].data for ix in 1:num_columns)...), num_columns)
    (imp_only, pg_only) = Data.diff(results_imp, results_pg)
    @show query_name
    @test imp_only == pg_only # ie both empty - but @test will print both otherwise
  end
end

function bench_imp()
  medians = []
  for query_name in query_names()
    @show q
    trial = @show @benchmark $(eval(Symbol("q$(query_name)")))()
    push!(medians, median(trial.times) / 1000000)
  end
  medians
end

function bench_sqlite()
  db = SQLite.DB("../imdb/imdb.sqlite")
  SQLite.execute!(db, "PRAGMA cache_size = -1000000000;")
  SQLite.execute!(db, "PRAGMA temp_store = memory;")
  medians = []
  for query_name in query_names()
    query = rstrip(readline("../job/$(query_name).sql"))
    @time SQLite.query(db, query)
    trial = @show @benchmark SQLite.query($db, $query)
    push!(medians, @show (median(trial.times) / 1000000))
  end
  medians
end

function bench_pg()
  medians = []
  for query_name in query_names()
    query = rstrip(readline("../job/$(query_name).sql"))
    query = query[1:(length(query)-1)] # drop ';' at end
    bench = "explain analyze $query"
    cmd = `sudo -u postgres psql -c $bench`
    times = Float64[]
    @show q
    @show @benchmark push!($times, parse(Float64, match(r"Execution time: (\S*) ms", readstring($cmd))[1]))
    push!(medians, @show median(times))
  end
  medians
end

end
