## 11 November 2013: Simulate from unconstrained ZA network, with age mixing containing
## both diagonoal and b1factor and b2factor terms.

## 10 November 2013: Simulate from phase 2 data

## 9 November 2013: Check network structure for network in
## "estimation_za_d8d.RData"

rm(list=ls())

library(ergm)
library(tergm)
library(networkDynamic)

##load("estimation_za_phase2_entr18_max_deg_unconstrained_d8d.RData")
load("estimation_za_phase2_entr18_max_deg_unconstrained_d8d_trial.RData")
                                        # 13Nov13: age mixing without diagonals

      nw <- fit$network
      formation <- fit$formula
      dissolution <- ~offset(edges)
      timesteps <- 10

for (time in 2:timesteps) {   
        set.seed(Sys.time()) # sam has no problem getting stochastically different results 
                           # without this line

      ## Relational Change
      cat("Completing time step", time,
          "of", timesteps, "\n")

        nw <- simulate(nw,
                 formation = formation, 
                 dissolution = dissolution,
                 coef.form = theta.form, 
                 coef.diss = theta.diss,
                 constraints = constraints,
                 #time.start = ts,
                 time.start = time,
                 #start.time = time,
                 time.slices = 1,
                 #monitor = stats.form,
                 control = control.simulate.network(MCMC.burnin=10000))

      cat("Entering transmission step at time", time,
          "of", timesteps, "\n")

      cat("Total number of edges is ", network.edgecount(nw), "\n") #15Aug13
      cat("Number of alive edges is ", network.edgecount(network.extract(nw, at=time)),
          "\n") #15Aug13

        cat("\n")
      }

    net.10 <- network.collapse(nw, at=10)
    degreedist(net.10)
    network.edgecount(net.10)
    mixingmatrix(net.10, "age.cat")$matrix/
  sum(mixingmatrix(net.10, "age.cat")$matrix)
