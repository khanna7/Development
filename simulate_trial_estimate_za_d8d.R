## 9 November 2013: Check network structure for network in
## "estimation_za_d8d.RData"

     load(file="estimation_za_d8d_men5_women4.RData")
                                        # max deg of men is 5 and women is 4
     # load(file="estimation_za_d8d.RData")
                                        # max degree of men and women is 5
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
