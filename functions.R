
computeSummaryTable <- function(graphs){
    
    graphsNames = c("Erdo-Renyi", "Full graph", "Barabasi-Albert", "Star", "Watts-Strogatz")
    table <- data.table("Graph" = character(),
                        "N" = numeric(),
                        "E" = numeric(),
                        "k" = numeric(),
                        "delta" = numeric(),
                        "diameter" = numeric(),
                        stringsAsFactors = FALSE)
    
    for (x in 1:length(graphsNames)){
        
        g = graphs[[x]]
        gName = graphsNames[x]
        
        E = length(E(g))
        N = length(V(g))
        k = 2*E/N
        delta = 2*E/(N * (N-1))
        diameter = diameter(g, directed = FALSE, unconnected = TRUE, weights = NULL)
        
        
        table <- rbind(table, list(gName, N, E, round(k, 2), round(delta, 2), round(diameter, 2)))
    }
    return(table)
}

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

plotEvolution  <- function(nrInfected, N, title){
    nrSusceptible = N - nrInfected
    ts = seq(length(nrInfected))
    plot(ts,nrSusceptible, type='l', ylim = c(0, N), col="blue", main=title, ylab = "# nodes", xlab="t")
    lines(nrInfected, col="red")
    grid()
    legend("topright", legend = c("Infected", "Susceptible"),
           lty = 1, lwd = 2,col = c("red", "blue"))
}