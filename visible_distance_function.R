# Distance visible

# Using equation in Haney, J.C., Fristrup, K.M., and Lee, D.S. (1992). Geometry of Visual Recruitment by Seabirds to Ephemeral Foraging Flocks. Ornis Scandinavica (Scandinavian Journal of Ornithology) 23, 49–62.

vis.dist.fun <- function(flight.height = 1, object.height = 1){
  (3.838*(flight.height^0.5))+(3.838*(object.height^0.5))   
}


vis.dist.fun(1, 50)
vis.dist.fun(50, 50)
