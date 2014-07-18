library(ergm)
library(ergm.userterms)

## unipartite graph
net <- network.initialize(50, bipartite=FALSE, directed=FALSE)
net%v%"age" <- sample(c(15:50), size=network.size(net), replace=TRUE)

ergm(net~edges+absdiff("age"),
     target.stats=c(25, 0.1*25))

## bipartite graph

## Initialize bipartite network
bi.net <- network.initialize(50, bipartite=25, directed=FALSE)
## Assign ages
bi.net%v%"age" <- sample(c(15:50), size=network.size(net), replace=TRUE)

## Fit ergm with edges and "absdoff" term
bi.edge.absdiff.model <- ergm(bi.net~edges+absdiff("age"),
                              target.stats=c(25, 1*25)
                              )
## Simulate from ergm
bi.edge.absdiff.model.net <- simulate(bi.edge.absdiff.model)

## Examine edgelist
el.bi.edge.absdiff.model.net <- as.edgelist(bi.edge.absdiff.model.net)

## Examine difference between edges of partners
sum((bi.net%v%"age")[el.bi.edge.absdiff.model.net[,1]]-
    (bi.net%v%"age")[el.bi.edge.absdiff.model.net[,2]]
    )
