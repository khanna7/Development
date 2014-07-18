## 24 May 2013: Assign probability of unprotected intercourse in transmission code

## 23 May 2013: Adapted to Uganda data

## 7 April 2013: Women's ART status does not seem to update
## appropriately upon ART initiation

## 5 April 2013: Seem to require "set.seed" function inside for loop to give
## stochastically different results, just as during the migration project
## last year.

## toggle list errors may be due to something in the vital dynamics -- looks
## like a lot more men are dying due to non-HIV deaths than women

## 4 April 2013: Toggle list errors have surfaced again.

## 1 April 2013: Add parameters for biology

## 20 March 2013: Rewrite estimation file so it workds with simulation.
## Currently getting error: ``Error in toggleList[, 1] : incorrect number of dimensions"
## File works with Steve's estimation object from NMID course, so I believe the problem
## must exist with something about how the initial network is estimated.
## Check estimation file with what Steve has from NMID course. 
## Problem seems to be with the number of toggles (as indicated
## by the error above) -- when partnership duration is set at 2000 days,
## the number of toggles is too few, hence the error. When partnership duration is set
## at 500, simulation worked until time step 10.

## 26 Feb 2013: Start writing simulation file.

rm(list=ls())

     ## STATNET-related files and packages
       library(ergm)
       library(tergm)
       source("../simulate.networkDynamic.R")

     ## SIMULATION-related files and packages
       source("common.functions_d2.R")
       source("update.vital.dynamics_d3.R")
       source("update.biological.parameters_d2.R")
       source("transmission_d3.R")

     ## Load object
     ## for debugging (using NMID course) 
     ##load("hetdeg4.RData") # for debugging
     ## nw <- hetdeg4$fit$network 
     ## formation <- hetdeg4$fit$formula
     ## theta.form <- hetdeg4$theta.form
     ## theta.diss <- hetdeg4$theta.diss
     ## constraints <- ~.
     ## dissolution <- ~offset(edges)

     ## for my simulation 
        ## load("estimation_d4.RData")
        load("estimation_uganda_d1.RData")
        nw <- fit$network
        formation <- fit$formula

    ##formation <- formation.n0

     dissolution <- ~offset(edges)

     ## theta.form, theta.diss and constraints loaded from RData object above
     popsize <- network.size(nw)
     popsize.m <- nw %n% 'bipartite' ## ASK: opposite to what steve has 
     popsize.f <- popsize - popsize.m
     prop.m <- popsize.m/popsize
     prop.f <- 1-prop.m
     prev.i <- mean(nw %v% 'inf.status')
     prev.i.f <- mean((nw %v% 'inf.status')[1:popsize.m])
                 ## ASK: again opp. to what steve has
     prev.i.m <- mean((nw %v% 'inf.status')[(popsize.m+1):popsize])

  ## Other Needed Parameters for timesteps
     timesteps <- 100 ## timesteps <- 250
     start.time <- 1

  ## Set some values for time 1
     inci <- inci.f <- inci.m <- NA
     num.deaths.aids <- num.deaths.natural <- num.births <- NA

  ## Set initial infection (done in the estimation portion of the file)

     ## dur.inf <- length(beta.per.month)
     ## status <- rbinom(popsize,1,exp.init.prev.i)
     ## nw <- set.vertex.attribute(nw,"status",status)
     ## inf.time <- rep(NA,popsize)
     ## inf.time[status==1] <- sample(1:(-dur.inf+2),sum(status),replace=T)
     ## nw <- set.vertex.attribute(nw,"inf.time",inf.time)
     ## prev.i <- mean(nw %v% 'status')
     ## prev.i.f <- mean((nw %v% 'status')[1:popsize.f])
     ## prev.i.m <- mean((nw %v% 'status')[(popsize.f+1):popsize])

  ## Record initial network statistics

     nw.start <- network.copy(nw)
     nw.stats <- matrix(NA, timesteps, 13)
     nw.stats[1,] <- summary(nw~edges+b1degree(0:5)+b2degree(0:5)) 

  ## Start with all vertices and edges active
     nw <- activate.vertices(nw,onset=1,terminus=Inf)
     nw <- activate.edges(nw,onset=1,terminus=Inf)



#####################################################
### Other Needed Parameters
#####################################################

  ## Biological Parameters
     ## (times in daily units,
     ## viral load in log units)
     dur.inf <- 3301 ## check -- 3301 vs 2932
     peak.viral.load <- 6.17
     time.infection.to.peak.viremia <- 14 
     time.infection.to.viral.set.point <- 121
     viral.set.point.men <- 4.42
     viral.set.point.women <- 4.2
     set.point.viral.load <- 4.3 ## avg of above two
     time.infection.to.late.stage <- 1877
     late.stage.viral.load <- 5.05 ## but this is late stage VL, 5 months before death
     time.to.full.supp <- 4*30
     undetectable.vl <- log(50, base=10)

     ## CD4 Count
     ## (units for CD4 cells/mm3,
     ##  for time, per day)

     cd4.at.infection.male <- 518 
     cd4.at.infection.female <- 570
				
     untreated.cd4.time.to.350.men <- 3.3*365
     untreated.cd4.time.to.350.men <- 4.2*365

     untreated.cd4.daily.decline.men <- 0.139
     untreated.cd4.daily.decline.women <- 0.144

     treated.daily.cd4.increase <- 15
     treated.time.of.increase.of.cd4 <- 3*365 # 3 years

  ## Demographic Parameters
     asmr.male <- seq(1e-3, by=2e-4, length=8)
     asmr.female <- seq(1e-3, by=2e-4, length=8)
## for the 5 age groups:
## 15-19, 20-24, 25-29, 30-34,
## 35-39, 40-44, 45-49, 50-54   
## think of these as per day (non-HIV related)
## propbability of dying for a person
## of a given age
     phi <- 0.001*5 ## proportion of pregnant women??

  ## Epidemiologic Parameters
     ## CD4
     eligible.cd4 <- 350
     baseline.cd4.at.art.initiation.men <- 100 # should be about 100
     baseline.cd4.at.art.initiation.women <- 100 # should be about 100
     cd4.recovery.time <- 3*365 ## CD4 recovery for 3 years
     per.day.untreated.cd4.decline <- 20 # change
     per.day.cd4.recovery <- 10 # change
     ## min.chronic.infectivity <- 0.00497 # changed belowl
     min.chronic.infectivity <- 0.00497/2.89 # changed to include infection at log 2
     acute.mult <- 4.98
     late.mult <- 3.49

     ## acute.length <- 1:90 ## modify
     ## chronic.length <- 91:4000 ## modify
     ## late.length <- 4001:5000 ## modify

     acute.length <- 1:10 ## modify
     chronic.length <- 11:20 ## modify
     late.length <- 21:100 ## modify


  ## Infection parameters
     preg.mult <- 2.5 ## check
     circum.mult <- 0.60 ## check

#####################################################

#####################################################
### Time Loop
#####################################################

##    debug(simulate)
##    debug(update.biological.parameters)
##    mtrace(update.vital.dynamics)
##    debug(update.vital.dynamics)
##    debug(update.biological.parameters)
##    debug(compute.cd4.count)
  debug(assign.infectivity)
## debug(transmission)

    for (time in 2:timesteps) {   
      set.seed(Sys.time())



      ## Relational Change

      cat("Completing time step", time,
          "of", timesteps, "\n")
      ## Change in population
      ## transmission = f()
      ## Book-keeping

      nw <- simulate(nw,
                     formation=formation,
                     dissolution=dissolution,
                     coef.form=theta.form,
                     coef.diss=theta.diss,
                     constraints=constraints,
                     start.time=time, time.slices=1,
                     control=control.simulate.networkDynamic(MCMC.burnin=10000),
                     maxchanges=1e6
                     )

      cat("Entering transmission step at time", time,
          "of", timesteps, "\n")


      ## Change in population
      ## transmission = f()
      ## Book-keeping

      ## UPDATE VITAL DYNAMICS, AND UPDATE NETWORK
      ## function name changed from "update.network"
      nw <- update.vital.dynamics(nw, verbose=TRUE,
                                  dur.inf=dur.inf,
                                  asmr.male=asmr.male,
                                  asmr.female=asmr.female,
                                  phi=phi
                                  )


       ## UPDATE VITAL DYNAMICS, AND UPDATE NETWORK
      nw <-
        update.biological.parameters(nw, verbose=TRUE,
                                     scenario="baseline",
                                     dur.inf=dur.inf,
                                     eligibile.cd4=eligible.cd4,
                                     baseline.cd4.at.art.initiation.men =
                                     baseline.cd4.at.art.initiation.men,
                                     baseline.cd4.at.art.initiation.women=
                                     baseline.cd4.at.art.initiation.women,
                                     baseline.time.art.initiation.preg.women=23*7,
                                     cd4.at.infection.male=cd4.at.infection.male,
                                     cd4.at.infection.female=
                                     cd4.at.infection.female,
                                     per.day.untreated.cd4.decline=
                                     per.day.untreated.cd4.decline,
                                     cd4.recovery.time=cd4.recovery.time,
                                     per.day.cd4.recovery=per.day.cd4.recovery,
                                     acute.length=acute.length,
                                     chronic.length=chronic.length, 
                                     late.length=late.length, 
                                     time.infection.to.peak.viremia=14
                                     )

      

      ## ## MODEL TRANSMISSION
      ##    ## first assign infectivity
      ##    ## based on data in Hughes et al (2012) and 
      ##    ## modeling assumptions about ratio of infectivity for 
      ##    ## acute and late stages

         nw <- assign.infectivity(nw, verbose=TRUE,
                                  min.chronic.infectivity=min.chronic.infectivity,
                                  acute.mult=acute.mult,
                                  late.mult=late.mult
                                  )

      ##    ## now model transmission of infection

         nw <- transmission(nw,
                            preg.mult=preg.mult,
                            circum.mult=circum.mult,
                            verbose=TRUE
                            )


    ## Update edges coef, give feedback

       ##theta.form[1] <-  theta.form[1] + log(popsize[time-1]) - log(popsize[time])
       cat("Finished timestep ",time," of ",timesteps,".\n",sep="")

        cat("\n")

    }


save.image(file="simulation.RData")

####################################
### Diagnostics
#####################################
sim.stats <- attributes(hetdeg.diag.sim)$stats[1:timesteps,]
apply(sim.stats, 2, mean)

