rm(list = ls())

# Load and install necessary packages
requiredPackages <- c("igraph", "ggplot2", "ggthemes", "esquisse")

for (pac in requiredPackages) {
    if(!require(pac,  character.only=TRUE)){
        install.packages(pac, repos="http://cran.rstudio.com")
        library(pac,  character.only=TRUE)
    } 
}
rm(pac)
rm(requiredPackages)


# Set WD and load data
wd = getwd()
if(grepl("nora", wd)) {
    setwd("~/Documents/18-19/IR/LABS/05/")
} else {
    # Set Carolina Working directory
    setwd("~/Documents/FEUP/5A/1S/IR/Information-Retrieval/05")
}
rm(wd)

##################
PLOT = F

simulateSIS <- function(g, beta, gamma, p, ts) {
  
  graph = as_edgelist(g)
  
  E = nrow(graph)
  N = length(V(g))
  
  infected = rep(FALSE, N)
  infected.start = sample(N, p, prob=NULL)
  infected[infected.start] = TRUE
  
  set.seed(123)
  #rands = runif(ts * E * 4, 0, 1)
  rands = sample(seq(from = 0, to = 1, by=0.01), ts * E * 4, replace=TRUE)
  nrInfected = c()
  
  for(t in seq(ts)){
      
      visited = rep(FALSE, N)
      nrInfected[t] = length(which(infected == TRUE)) 
      
      if(PLOT){
          vertex_attr(g, "infected", index = V(g)) <- (as.numeric(infected) + 5)
          plot(g, vertex.color = vertex_attr(g,"infected"), main=paste("t = ", t), layout=layout.circle)
      }
      
      tmp.infected = infected
      for(x in seq(E)){
          
          pair = graph[x, ]
          
          u = pair[1] ## idx of u
          v = pair[2] ## idx of v
          
          
          r.idx = t*x
          
          if(infected[u]){
              
              ## Infect others
              if(!infected[v] & beta > rands[r.idx]){
                  tmp.infected[v] <- TRUE
              }
              
              ## Recover yourself
              if(!visited[u] & gamma > rands[r.idx+1]){
                  tmp.infected[u] <- FALSE
              }
          }
          
          
          if(infected[v]){
              
              ## Infect others
              if(!infected[u] & beta > rands[r.idx+2]){
                  tmp.infected[u] <- TRUE
              }
              
              ## Recover yourself
              if(!visited[v] & gamma > rands[r.idx+3]){
                  #cat("Gamma" , gamma, "rand", rands[r.idx+3], r.idx+3, "\n")
                  tmp.infected[v] <- FALSE
              }
          }
          
          visited[u] = T
          visited[v] = T
      }
      infected = tmp.infected
  }
  return(nrInfected)
}

plotEvolution  <- function(nrInfected, N){
    nrSusceptible = N - nrInfected
    
    plot(seq(ts),nrSusceptible, type='l', ylim = c(0, N), col="blue")
    lines(nrInfected, col="red")
}




beta = 0.8
gamma = 0.4
p = 5

ts = 30
n = 1000

er.graph <- erdos.renyi.game(n, 0.5)
full.graph <- make_full_graph(n, directed=F)
ba.graph <-barabasi.game(n, 1, directed=F)
star.graph <- make_star(n, mode="undirected")
ws.graph <- watts.strogatz.game(1,n,4,0.5)


nrInfected.er.graph = simulateSIS(er.graph, beta, gamma, p, ts)
nrInfected.full.graph = simulateSIS(full.graph, beta, gamma, p, ts)
nrInfected.ba.graph = simulateSIS(ba.graph, beta, gamma, p, ts)
nrInfected.star.graph = simulateSIS(star.graph, beta, gamma, p, ts)
nrInfected.ws.graph = simulateSIS(ws.graph, beta, gamma, p, ts)


plotEvolution(nrInfected.er.graph, n)
plotEvolution(nrInfected.full.graph, n)
plotEvolution(nrInfected.ba.graph, n)
plotEvolution(nrInfected.star.graph, n)
plotEvolution(nrInfected.ws.graph, n)




# Threshold

threshold.er = spectrum(er.graph)$values
threshold.full = spectrum(full.graph)$values
threshold.ba = spectrum(ba.graph)$values
threshold.star = spectrum(star.graph)$values
threshold.ws = spectrum(ws.graph)$values

beta = 0.005
#gamma = 0.05

gamma = threshold.er * beta; gamma

p = 5

ts = 40
n = 1000


nrInfected.er.graph = simulateSIS(er.graph, beta, gamma, p, ts)
nrInfected.full.graph = simulateSIS(full.graph, beta, gamma, p, ts)
nrInfected.ba.graph = simulateSIS(ba.graph, beta, gamma, p, ts)
nrInfected.star.graph = simulateSIS(star.graph, beta, gamma, p, ts)
nrInfected.ws.graph = simulateSIS(ws.graph, beta, gamma, p, ts)


plotEvolution(nrInfected.er.graph, n)
plotEvolution(nrInfected.full.graph, n)
plotEvolution(nrInfected.ba.graph, n)
plotEvolution(nrInfected.star.graph, n)
plotEvolution(nrInfected.ws.graph, n)

