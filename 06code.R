rm(list = ls())

# Load and install necessary packages
requiredPackages <- c("igraph", "ggplot2", "ggthemes", "esquisse", "data.table")

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
    setwd("~/Documents/18-19/CSN/LABS/06/")
} else {
    # Set Carolina Working directory
    setwd("")
}
rm(wd)

##################
PLOT = F
RUN_FROM_SCRATCH = F
source("functions.R")

###########
n = 1000

####### Task 1


er.graph <- erdos.renyi.game(n, 0.5)
full.graph <- make_full_graph(n, directed=F)
ba.graph <-barabasi.game(n, 1, directed=F)
star.graph <- make_star(n, mode="undirected")
ws.graph <- watts.strogatz.game(1,n,4,0.5)


graphs = list(er.graph, full.graph, ba.graph, star.graph, ws.graph)
computeSummaryTable(graphs)

if(RUN_FROM_SCRATCH){

    beta = 0.05
    gamma = 0.15
    p = 10
    ts = 100

    nrInfected.er.graph = simulateSIS(er.graph, beta, gamma, p, ts)
    nrInfected.full.graph = simulateSIS(full.graph, beta, gamma, p, ts)
    nrInfected.ba.graph = simulateSIS(ba.graph, beta, gamma, p, ts)
    nrInfected.star.graph = simulateSIS(star.graph, beta, gamma, p, ts)
    nrInfected.ws.graph = simulateSIS(ws.graph, beta, gamma, p, ts)
    
    
    if(PLOT){
      plotEvolution(nrInfected.er.graph, n, "Erdos-Renyi infected/susceptible evolution")
      plotEvolution(nrInfected.full.graph, n, "Fully connected g. infected/susceptible evolution")
      plotEvolution(nrInfected.ba.graph, n, "Barabasi-Albert infected/susceptible evolution")
      plotEvolution(nrInfected.star.graph, n, "Star infected/susceptible evolution")
      plotEvolution(nrInfected.ws.graph, n, "Watts-Strogatz infected/susceptible evolution")
    }
    write.csv2(nrInfected.er.graph, "./data/er_infected.csv")
    write.csv2(nrInfected.full.graph, "./data/full_infected.csv")
    write.csv2(nrInfected.ba.graph, "./data/ba_infected.csv")
    write.csv2(nrInfected.star.graph, "./data/star_infected.csv")
    write.csv2(nrInfected.ws.graph, "./data/ws_infected.csv")
} else {
    nrInfected.er.graph <- read.csv2("./data/er_infected.csv"); nrInfected.er.graph <- nrInfected.er.graph$x
    nrInfected.full.graph <- read.csv2("./data/full_infected.csv"); nrInfected.full.graph <- nrInfected.full.graph$x
    nrInfected.ba.graph <- read.csv2("./data/ba_infected.csv"); nrInfected.ba.graph <- nrInfected.ba.graph$x
    nrInfected.star.graph <- read.csv2("./data/star_infected.csv"); nrInfected.star.graph <- nrInfected.star.graph$x
    nrInfected.ws.graph <- read.csv2("./data/ws_infected.csv"); nrInfected.ws.graph <- nrInfected.ws.graph$x
}

if(PLOT) {
    plot(seq(length(nrInfected.er.graph)), nrInfected.er.graph, type='l', ylim = c(0, n), col="blue", main="Evolutions comparison", ylab = "# infected nodes", xlab="t")
    lines(nrInfected.full.graph, col="red")
    lines(nrInfected.ba.graph, col="green")
    lines(nrInfected.star.graph, col="orchid")
    lines(nrInfected.ws.graph, col="skyblue")
    grid()
    legend("bottomright", legend = c("Erdo-Renyi", "Full graph", "Barabasi-Albert", "Star", "Watts-Strogatz"),
           lty = 1, lwd = 2,col = c("blue", "red", "green", "orchid", "skyblue"))

}



# Task 2 Threshold

if(F){
threshold.er = spectrum(er.graph)$values
threshold.full = spectrum(full.graph)$values
threshold.ba = spectrum(ba.graph)$values
threshold.star = spectrum(star.graph)$values
threshold.ws = spectrum(ws.graph)$values

beta.2 = 0.05
gamma.2 = threshold.er * beta.2; gamma.2

gamma.2 = 0.4
(beta.2 = gamma.2 /threshold.full); 
beta.2 = beta.2 - 0.000

p.2 = 5
ts.2 = 40

n = 1000

nrInfected.er.graph.2 = simulateSIS(er.graph, beta.2, gamma.2, p.2, ts.2)
nrInfected.full.graph.2 = simulateSIS(full.graph, beta.2, gamma.2, p.2, ts.2)
nrInfected.ba.graph.2 = simulateSIS(ba.graph, beta.2, gamma.2, p.2, ts.2)
nrInfected.star.graph.2 = simulateSIS(star.graph, beta.2, gamma.2, p.2, ts.2)
nrInfected.ws.graph.2 = simulateSIS(ws.graph, beta.2, gamma.2, p.2, ts.2)

if(PLOT){
  plotEvolution(nrInfected.er.graph.2, n, "Erdos-Renyi infected/susceptible evolution")
  plotEvolution(nrInfected.full.graph.2, n, "Fully connected g. infected/susceptible evolution")
  plotEvolution(nrInfected.ba.graph.2, n, "Barabasi-Albert infected/susceptible evolution")
  plotEvolution(nrInfected.star.graph.2, n, "Star infected/susceptible evolution")
  plotEvolution(nrInfected.ws.graph.2, n, "Watts-Strogatz infected/susceptible evolution")
}
}

