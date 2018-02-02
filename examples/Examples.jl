module Examples

include("LegacyParser.jl")
include("Chinook.jl")
include("Graph.jl")
include("Polynomial.jl")
include("JobData.jl") 
include("Job.jl")

function test()
  Chinook.test()
  Graph.test()
  Polynomial.test()
  Job.test()
end

test()

end
