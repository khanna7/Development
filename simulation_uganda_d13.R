#####################################################
### Progression of Versions
#####################################################

## 30 Jul 2013: Added "source("assign.infectivity") since this
## function is in its own file now (moved from
## "common.functions_d8.R."

## 25 Jul 2013: Viral load trajectory for  women who have ceased scART and are
## past pre-ART viral load in "compute.viral.load_d3.R".

## 25 Jul 2013: Add function call for updating viral load trajectory
## for women who have reached pre-ART viral load.  Ultimately this change
## was made in "compute.viral.load_d2.R" -- no separate function was needed.

## 12 Jul 2013: Add scenario, baseline.art.coverate.rate and
## baseline.preg.coverage.rate arguments to "update.vital.dynamics" function, and
## assign respective attributes for these parameters.
## Also made appropriate changes in "update.vital.dynamics_d6" and created
## "update.treatment_d2."

## 11 Jul 2013: Added "undectable.vl" as an argument.

## 11 Jul 2013: Removed the "per.day.untreated.cd4.decline" argument from
## "update.treatment" function -- since CD4 counts in infected people are now
## updated using a model that accouts for age and sex (gender).

## 10 Jul 2013: Breakup "update.biological.parameters" into separate functions for
## treatment, CD4 count and viral load.

## 9 Jul 2013: Add prop.f as an argument to "update.vital.dynamics" to
## make sure sex for new arrivals is assigned in proportion to sex distribution
## at the beginning

## 7 July 2013: Clean up file for runs. Debug all functions that are called.

## 7 July 2013: Change ASMR to realistic values

## 6 July 2013: Implement model with infection-based mortality rates --
## ASMR for HIV-negative individuals
## CD4-based for HIV-positive individuals

## 5 July 2013: Made corrections to adding changes in viral load for different
## types of treatment

## 12 June 2013: Add attribute for slope of viral load trajectory
## for art types 1, 2 and 3,
## and populate this attribute at the initiation of ART.

## 11 June 2013: Add details for increase
## in viral load and decline in CD4 county with
## ART status 2 (sc ART for pregnant women under Option A), or
## ART status 3 (cessation of sc ART for pregnant women under Option A)

## 10 June 2013: Sourced file ("common.functions_d7)
## with adjusted infectivities for HIV-positive pregnant women 

## 7 June 2013: Now that we two art types -- 1 for regular ART, 2 for
## scART in pregnant women -- viral load trajectories for treated individuals
## will vary separately

## use median of number of sex acts per time step from Rakai data,
## instead of mean.

## 5 June 2013: Call function for assigning pregnancy. File
## "simulation_uganda_d6.R" contains an attempt at a
## testing-driven-development (TDD) program that did not work. 

## 28 May 2013: Adapt with other files (update vital dynamics, update biological
## parameters, and transmission adjusted for size of timestep

## 27 May 2013: Adapt with revised definition of timestep (as 14 days)
## "min.chronic.infectivity.unadj" (i.e. based on 1 timestep = 1
## day might be higher by 1 order of magnitude. currently have it as 0.00497). --
## yes, should be 0.000497.

## 26 May 2013: Re-adapt

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

#####################################################

#####################################################
### Top Matter
#####################################################

  rm(list=ls())
      load("estimation_uganda_d5.RData")
  ## STATNET-Related Files and Packages
      library(ergm)
      library(tergm)
      source("../simulate.networkDynamic.R")

  ## SIMULATION-Related Files and Packages
      #source("common.functions_d7.R")
       source("common.functions_d8.R")
       source("update.vital.dynamics_d6.R")
       #source("assign.pregnancy_d2.R")
       source("assign.pregnancy_d3.R")
              # 25Jul13: populate "time.since.art.cessation" in "d3" 
       source("update.treatment_d2.R") # with coverage accounted for
       source("compute.cd4.count_d1.R")

       source("compute.viral.load_d3.R")             
       source("transmission_d5.R")

  ## Starting Network 
      # load("estimation_uganda_d5.RData")


#####################################################

#####################################################
### Starting Network Objects
#####################################################

  ## Fit, formula, population features
      nw <- fit$network
      formation <- fit$formula
      dissolution <- ~offset(edges)
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
      timesteps <- 100 ## 27 May '13: this mean 100*14 days now
      start.time <- 1

  ## Set some values for time 1
      inci <- inci.f <- inci.m <- NA
      num.deaths.aids <- num.deaths.natural <- num.births <- NA


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
     time.infection.to.peak.viremia <- time.infection.to.peak.viral.load
     time.to.full.supp <- 4*30/size.of.timestep
     undetectable.vl <- log(50, base=10)

     ## CD4 Count
     ## (units for CD4 cells/mm3,
     ##  for time, per day)

        cd4.at.infection.male <- 518 
        cd4.at.infection.female <- 570
				
     ## untreated.cd4.time.to.350.men <- 3.3*365
     ## untreated.cd4.time.to.350.men <- 4.2*365

        untreated.cd4.time.to.350.men <- 3.3*365/size.of.timestep # changed due to timestep
        untreated.cd4.time.to.350.men <- 4.2*365/size.of.timestep # changed due to timestep

     ## untreated.cd4.daily.decline.men <- 0.139 ## not needed here, already defined
     ## untreated.cd4.daily.decline.women <- 0.144

     ## treated.daily.cd4.increase <- 15
     ## treated.time.of.increase.of.cd4 <- 3*365 # 3 years


  ## Demographic Parameters

      ## Mortality
      ## 7Jul13: Adjusted to realistic values
      asmr.perperson.perday <- c(6.87671232876712E-006,
                                 1.31232876712329E-005,
                                 1.93424657534247E-005,
                                 2.66027397260274E-005,
                                 3.7013698630137E-005,
                                 4.59452054794521E-005,
                                 5.29315068493151E-005,
                                 5.68493150684932E-005
                                 )

      asmr.perperson.pertimestep <- asmr.perperson.perday*size.of.timestep
      asmr.male <- asmr.perperson.pertimestep
      asmr.female <- asmr.perperson.pertimestep

      ## for the 5 age groups:
      ## 15-19, 20-24, 25-29, 30-34,
      ## 35-39, 40-44, 45-49, 50-54   
      ## think of these as per day (non-HIV related)
      ## propbability of dying for a person
      ## of a given age

      ## Births
      phi <- 0.001*5 ## mean parameter for poisson process

      ## Biological and Treatment Parameters
      eligible.cd4 <- 350
      baseline.cd4.at.art.initiation.men <- 100 # should be about 100
      baseline.cd4.at.art.initiation.women <- 100 # should be about 100
      ## baseline.cd4.at.art.initiation.men <- 50 # MADE UP for debugging
      ## baseline.cd4.at.art.initiation.women <- 570 # MADE UP for debugging

      cd4.recovery.time <- 3*365/size.of.timestep ## CD4 recovery for 3 years
      per.day.untreated.cd4.decline <- 0.14
                 ## rename this parameter to untreated.cd4.perstep.decline,
                 ## then it does not need to be defined here
      per.day.cd4.recovery <- 15/30 ## rate of 15 cells/month
      min.chronic.infectivity.unadj <- 0.000497/2.89
                ## changed to include infection at log 2
      num.sex.acts.per.timestep <- 2.4*size.of.timestep/7

      optA.sc.art.vl.perstep.dec <- 1.1/((40-23)*7)*size.of.timestep
                ## decline is 1.1 log over 17 weeks(from first visit to delivery)
                ## Per day decline, therefore, is 1.1/((40-23)*7)
                ## Per time step decline, therefore, is given by expr. above
      optA.sc.art.cd4.perstep.rec <- 50/((40-23)*7)*size.of.timestep
                ## recovery is 50 cells/mm3 over 17 weeks
                ## (from first visit to delivery)
                ## same logic as above applies
      acute.mult <- 4.98
      late.mult <- 3.49


       ## Infection parameters
       preg.mult <- 2.5 ## check
       circum.mult <- 0.60 ## check

#####################################################

#####################################################
### Coverage-Related Information
### Populate "art.covered" based on the scenario being
### modeled
#####################################################
 
  ## ## Scneario
  ##    scenario <- "baseline"
  ##    bl.coverage <- 0.4

  ##    if (scenario == "baseline") {
  ##      art.covered <- rbinom(N, 1, 0.4)
  ##    }

  ## ## Populate attribute   
  ##    n0%v%"art.covered" <- art.covered
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
  ##    debug(assign.infectivity)
  ##    debug(transmission)
  ##    debug(assign.pregnancy)
  ##    debug(compute.cd4.count.sexage.acct)

  scenario <- "baseline"
  baseline.preg.coverage.rate <- baseline.preg.art.coverage.rate

  
  for (time in 2:timesteps) {   
        set.seed(Sys.time()) # sam has no problem getting stochastically different results 
                           # without this line

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
                     start.time=time,
                     time.slices=1, # 3Jun13: discussion with sam --
                                    # simulate over 1 time step every
                                    # time you hit the timestep in the loop
                                    # (as per the code in Steve's summer 2012 course)
                     control=control.simulate.networkDynamic(MCMC.burnin=10000),
                     maxchanges=1e6
                     )

      cat("Entering transmission step at time", time,
          "of", timesteps, "\n")

      cat("Number of edges is ", network.edgecount(nw))

      ## Change in population
      ## transmission = f()
      ## Book-keeping

      ## UPDATE VITAL DYNAMICS, AND UPDATE NETWORK
      ## function name changed from "update.network"
      nw <- update.vital.dynamics(nw, verbose=TRUE,
                                  dur.inf=dur.inf,
                                  asmr.male=asmr.male,
                                  asmr.female=asmr.female,
                                  phi=phi,
                                  size.of.timestep=size.of.timestep,
                                  prop.f=prop.f,
                                  circum.rate=circum.rate,#12Jul13
                                  scenario=scenario,
                                  baseline.art.coverage.rate=
                                  baseline.art.coverage.rate,
                                  baseline.preg.coverage.rate=
                                  baseline.preg.coverage.rate,
                                  )



    ##   ## assign pregnancy
      nw <- assign.pregnancy(nw, verbose=TRUE,
                             full.term=40/14, 
                             preg.prob=
                             ##0.8,
                             1,
                             min.preg.interval=15*30/14,
                             optA.vl.reduction=1.1,
                             sc.art.postcess.ret.bl=12
                             )


        nw <- update.treatment(
                               nw, verbose=TRUE,
                                     scenario="baseline",
                                     dur.inf=dur.inf,
                                     eligibile.cd4=eligible.cd4,
                                     baseline.cd4.at.art.initiation.men =
                                     baseline.cd4.at.art.initiation.men,
                                     baseline.cd4.at.art.initiation.women=
                                     baseline.cd4.at.art.initiation.women,
                                     ##baseline.time.art.initiation.preg.women=
                                     ##23*7,
                                     baseline.f.ges.visit=
                                     ##23*7/14,
                                     0,
                                     optA.thres=350,
                                     cd4.at.infection.male=
                                     cd4.at.infection.male,
                                     cd4.at.infection.female=
                                     cd4.at.infection.female,
                                     ## per.day.untreated.cd4.decline=
                                     ## per.day.untreated.cd4.decline,
                                     cd4.recovery.time=cd4.recovery.time,
                                     per.day.cd4.recovery=per.day.cd4.recovery,
                                     acute.length=acute.length,
                                     chronic.length=chronic.length, 
                                     late.length=late.length, 
                                     time.infection.to.peak.viral.load=14,
                                     size.of.timestep=size.of.timestep, #28May13
                                     ## optA.sc.art.vl.perstep.dec, #5Jul13
                                     ## optA.sc.art.vl.perstep.inc, #5Jul13
                                     optA.vl.reduction=1.1, ##5Jul13
                                     full.term=40*7/14,
                                     undetectable.vl=log(50, base=10),
                                     time.to.full.supp=time.to.full.supp
           )

    ## nw <- compute.cd4.count.sexage.acct(nw, verbose=FALSE,
    ##                                            cd4.at.infection.male=
    ##                                            cd4.at.infection.male,
    ##                                            cd4.at.infection.female=
    ##                                            cd4.at.infection.female,
    ##                                            per.day.untreated.cd4.decline=
    ##                                            per.day.untreated.cd4.decline,
    ##                                            cd4.recovery.time=cd4.recovery.time,
    ##                                            per.day.cd4.recovery=per.day.cd4.recovery,
    ##                                            optA.sc.art.cd4.perstep.rec=
    ##                                            optA.sc.art.cd4.perstep.rec,
    ##                                            size.of.timestep=size.of.timestep #28May13
    ##                                            )
 
        nw <- compute.vl.mp.art4(nw, verbose=TRUE, 
                                     time.infection.to.peak.viremia,
                                     peak.viral.load,
                                     time.infection.to.viral.set.point,
                                     set.point.viral.load,
                                     time.infection.to.late.stage,
                                     late.stage.viral.load,
                                     dur.inf,
                                     time.to.full.supp,
                                     undetectable.vl,
                                     sc.art.postcess.ret.bl=12, #25Jul13
                                     size.of.timestep#, # 28May13
                                     ## optA.sc.art.cd4.perstep.rec,
                                     ##    #11Jun13: CD4 recovery w scART
                                     ## optA.sc.art.vl.perstep.inc
                                     ##    #11Jun13: vl decline w scART
                                     )
 
      ## MODEL TRANSMISSION
      ## first assign infectivity
      ## based on data in Hughes et al (2012) and 
      ## modeling assumptions about ratio of infectivity for 
      ## acute and late stages

      nw <- assign.infectivity(nw, verbose=TRUE,
                               min.chronic.infectivity.unadj=
                               ## 0.2,
                               min.chronic.infectivity.unadj,
                               num.sex.acts.per.timestep=
                               num.sex.acts.per.timestep, #common.functions.d3"
                               acute.mult=acute.mult,
                               late.mult=late.mult,
                               preg.mult=preg.mult,#10Jun13
                               )
        
      ## now model transmission of infection

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

#####################################################
### Diagnostics
#####################################################

  sim.stats <- attributes(hetdeg.diag.sim)$stats[1:timesteps,]
  apply(sim.stats, 2, mean)

#####################################################

