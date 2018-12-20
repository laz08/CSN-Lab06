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
    setwd("C:/Users/Carolina/Documents/FEUP/5A/1S/CSN/Lab/CSN-Lab06")
}
rm(wd)

PLOT = F
source("functions.R")

n = 100
full.graph <- make_full_graph(n, directed=F)
threshold.full = spectrum(full.graph)$values

gamma.2 = 0.4
(beta.2 = gamma.2 /threshold.full); 
beta.2 = beta.2 - 0.001
beta.2 = beta.2 + 0.05


p.2 = 4
ts.2 = 100

nrInfected.full.graph.2 = simulateSIS(full.graph, beta.2, gamma.2, p.2, ts.2)
plotEvolution(nrInfected.full.graph.2, n, "Fully connected g. infected/susceptible evolution")
plotEvolution2(nrInfected.full.graph.2, n, "Fully connected g. infected/susceptible evolution")

