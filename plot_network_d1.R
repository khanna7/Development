## plot a network to show at HIVMC

rm(list=ls())
##library(statnet)
library(ergm)
library(network)

##library(tergm)

load("estimation_uganda_d4.RData")
plot(n0)
plot(n0, displayisolates=FALSE)
plot(n0, cex=0.7)
plot(n0, cex=0.7, vertex.col="sex")
plot(n0, cex=0.7, vertex.col="sex",
     displayisolates=FALSE)

plot.network(n0)
plot.network(n0, displayisolates=FALSE)

net.time1 <- simulate(n0,
                      formation=formation, dissolution=dissolution,
                      coef.form=theta.form, coef.diss=theta.diss,
                      ##time.slices=1000,d
                      time.slices=2,
                      constraints=constraints,
                      monitor=~edges+b1degree(0:5)+b2degree(0:5),
                      control=control.simulate.network(MCMC.burnin=10000)
                      )

xn.ergm.fit <- ergm(n0 ~ edges+b1degree(0:1) + b2degree(0:1),
                    target.stats=target.stats
                    )

simulate.n0 <- simulate(xn.ergm.fit, nsim=10)

plot(simulate.n0$networks[[1]])

plot(simulate.n0[[1]])

start.net <- simulate.n0[[1]]


plot(start.net, cex=0.7, vertex.col="sex",
     displayisolates=FALSE)

plot(start.net, cex=0.7, vertex.col="sex",
     displayisolates=FALSE)

plot(start.net, cex=0.7, vertex.col='bipartite',
     displayisolates=FALSE)

plot(start.net, cex=0.7, vertex.col=2,
     displayisolates=FALSE)

plot(start.net, cex=0.7, vertex.col=1:num.male,
     displayisolates=FALSE)

plot(start.net, cex=0.7, vertex.col="sex",
     displayisolates=FALSE)

plot(start.net, cex=0.7, vertex.col="sex",
     displayisolates=TRUE)



## Scale n0 by a factor
scale.factor <- 10

      	num.male.scaled <- 2500/scale.factor
	num.female.scaled <- 2500 /scale.factor
        N.scaled <- num.male.scaled+num.female.scaled

  #### Network Related Statistics

        ## Degree Distribution, Partnership Duration
        ## and Number of Partnerships
      	## male.deg.dist <- c(0.48,0.41,0.08,0.03) ## steve's example from 2012 class
        ## female.deg.dist <- c(0.40, 0.55, 0.04, 0.01) ## steve's example from 2012 class

        male.deg.dist <- c(34.8, 52.3, 9.8, 3.1)/100 ## Phase 2 data
        female.deg.dist <- c(42.0, 55.7, 2.3, 0)/100 ## Phase 2 data

	## duration <- 100
        size.of.timestep <- 14 ## each time step is 14 days
        duration <- 4303/size.of.timestep
	diagnostics <- T

        female.deg.counts <- female.deg.dist*num.female
	male.deg.counts <- male.deg.dist*num.male
	female.deg.tot <- (0*female.deg.counts[1] + 1*female.deg.counts[2] +
                           2*female.deg.counts[3] + 3*female.deg.counts[4])
 	male.deg.tot <- (0*male.deg.counts[1] + 1*male.deg.counts[2] +
                         2*male.deg.counts[3] + 3*male.deg.counts[4])

       male.deg.counts.scaled <- male.deg.counts/scale.factor
       ## to match the degree totals for men and women, reduce the number of
       ## isolates in the women, and increase the number of women with 1 partner
        female.deg.dist.matched <- c(22.0, 75.7, 2.3, 0)/100
        female.deg.counts.matched <- female.deg.dist.matched*num.female
        female.deg.tot.matched <- (0*female.deg.counts.matched[1] +
                                   1*female.deg.counts.matched[2] +
                                   2*female.deg.counts.matched[3] +
                                   3*female.deg.counts.matched[4])
        female.deg.counts.matched.scaled <- female.deg.counts.matched/scale.factor
        female.deg.tot.matched.scaled <- female.deg.tot.matched/scale.factor
   ### Set up model

	formation <- ~edges+b1degree(0:1)+b2degree(0:1)
	formation.n0 <- update.formula(formation,n0~.)
	dissolution <- ~offset(edges)
	theta.diss <- log(duration-1)
	##target.stats <- c(female.deg.tot, female.deg.counts[1:2],
                            ## male.deg.counts[1:2])
        target.stats <- c(female.deg.tot.matched, male.deg.counts[1:2],
                          female.deg.counts.matched[1:2])

        target.stats.scaled <- c(female.deg.tot.matched.scaled, male.deg.counts.scaled[1:2],
                          female.deg.counts.matched.scaled[1:2])

        constraints	<- ~.

        ## Initialize Network
        n0.scaled <- network.initialize(N.scaled, bipartite=num.male.scaled, directed=F)

        n0.scaled %v% "sex" <- rep(c("male", "female"),
                                   c(num.male.scaled, num.female.scaled)
                                   )


xn.scaled.ergm.fit <- ergm(n0.scaled ~ edges+b1degree(0:1) + b2degree(0:1),
                    target.stats=target.stats.scaled
                    )

simulate.n0.scaled <- simulate(xn.scaled.ergm.fit, nsim=10)

plot(simulate.n0.scaled[[1]])

plot(simulate.n0.scaled[[1]], vertex.col="sex")

pdf(file="n0.netsize500")
plot(simulate.n0.scaled[[1]], vertex.col="sex", displayisolates=FALSE)
dev.off()
