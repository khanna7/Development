#####################################################
### Progression of Versions
#####################################################0

## 26 Sep 2013: Run this version with fertility rates increased by
## 15% for each age category.

## 16 Sep 2013:
## a. Source version "assign.pregnancy_d5d1.R" -- immaculate conception
##    without correction, but with the loop over all women, not just women
##    in relationships as was previously.

## 13 Sep 2013:
## a. Source file for recording pregnancy information =
##    "assign.pregnancy_d5b.R"
## b. Source file for immaculate conception -- we will implement with and
##    without correction for proportion of all women who meet preg criteria.
##    Version "d22d" is WITH correction, version "d22e" is WITHOUT correction.

## 3 Sep 2013: Runs for WIP presentation.

## 3 Sep 2013: Test binomial structure to compute probability of getting pregnant

## 2 Sep 2013: Based on "simulation_uganda_d21g.R." Written to
## test assignment of pregnancy pribability if number of births per
## 1000 women are speified.
##    i. empirical number of births by age per 1000,
##   ii. proportion of still births (as a number between 0 and 1).
##  iii. multiplier for reduction of probability of
##       pregnancy on account of positive HIV infection

## 31 Aug 2013: Repeat 64-bit simulation, this time with an estimation object
## on 64-bit computer. 

## 29 Aug 2013:
## i. Recreate the memory error on account of high growth rate.
## ii. Save the network only at certain time intervals (though I am not
## saving the network at all now) -- not sure if this will be the fix we need.
## iiia. Instead of doing ii. Try only working with the active network at any timestep.
## iiib. I tried (iiia.) with "retain.all.vertices=TRUE" Try it with
## "retain.all.vertices=FALSE"
## iv. Suppress all "verbose" output.
## v. Simulate with "male" changed to 0 and "female" changed to 1.
## vi. Remove some of the larger objects in R, and follow it up by gc()

## 27 Aug 2013: Comment out "dur.inf" from update.treatment next iteration, since this is not a global
## constant /net/home/khanna7/Projects/Home-Based/Code/Development/anymore.

## 26 Aug 2013: I never built in exits at age 55!!! Do that NOW!!! -- This was NOT
## the problem. The problem was that with infectivies receving a mortality based on
## combination of age and sex, exits were not happening at age 55. That is now fixed.

## 23 Aug 2013: Added code for recording demographic data in
## "update.vital.dynamics_d9."

## 22 Aug 2013: Add age-based duration of infection at time of infection
## Corrections in "estimation_uganda_d7.R"
## "transmission_d9", "update.vital.dynamics_d9.R",
## "compute.viral.load_d4.R", "update.treatment_d9.R"

## 22 Aug 2013: Simulate from a model with 15% prevalence at outset.

## 20 Aug 2013: Simulate from a model with 10% prevalence at outset.

## 16 Aug 2013: a. Fix birth rate to get growth of 2-3% a year.

## 15aug13: a. Sourced "update.transmission_d5.R" with corrected coding for
## status of ART initiators. Had fixed the error yesterday but sourced wrong version
## of "update.treatment" -- d4.

## b. Source "update.transmission_d8.R" to record information for various attribute 
## on network extracted at current timestep.

## c. Changed output of number of edges. Output on screen showed total number of edges.
## Need to see number of "alive" edges.

## d. Investigate infectivity of pregnant women.

## e. Source d4 of "assign.infectivity" tp adjust for pregnancy in a different place.

## 13 Aug 2013: changed simulate function. Everything compatible with
## statnet version 3.1 and tergm version 3.1.1.

## 13 Aug 2013:
## a. Test with "estimation_uganda_d6.R" -- constant parameters
## have been separated.

## b. Source version 8 of "update.vital.dynamics" and version 7 of "transmission."
## These make sure that ART coverage indicators ("art.coverage" and
## "preg.coverage") are assigned 0, 1 values at
## point of infection. When new nodes enter the population, their coverage indicators
## are NA.

## 9 Aug 2013: Move parameters to parameters file.

## 8 Aug 2013: Sourced "update.vital.dynamics_d7" (instead of d6). d7
## limits time-of-infection-based mortality to individuals not on any ART.

## 5 Aug 2013: Try different birth-rates to fix growth in population at 2-3%
## per year

## 31 July 2013: Rounding "time.infection.to.set.point.viral.load" and
## "time.infection.to.late.stage.viral.load" to an integer value fixed the probelm
## with no one in the chornic stage having a viral load of 4.2 (chronic infectives
## were showing a viral load of 5.26). This is because I am using the %in% function
## on the time since infection data to compute an individual's 
## viral load.

## 31Jul2013: Slow down birth-rate

## 30Jul2013: Incorporate new changes:
## Explicit call to "assign.infectivity_d*.R" file.
## changed arguments in "update.treatment" and
## "assign.infectivity" functions.

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
      # load("estimation_uganda_d6.RData") 
      # load("estimation_uganda_d6a.RData") #20Aug13:Simulate 10% infection at start
      # load("estimation_uganda_d6b.RData") #22Aug13:Simulate 15% infection at start
      #  load("estimation_uganda_d7.RData") #age-based duration of infection at time of inf.
      # load("estimation_uganda_d8.RData") #age-based duration of infection at time of inf.
      # load("estimation_uganda_d8_initprev15.RData")
      # load("estimation_uganda_d8_rmvstringatt.RData") #29Aug13
      # load("estimation_uganda_d8d_rmvstringatt_64bit.RData") #31Aug2013
        load("estimation_uganda_d8_10pc_32bit.RData") #3Sep13

## STATNET-Related Files and Packages
      ##library(ergm)
      ## library(statnet)#30Aug2013: difference from 32-bit,
                                        # i am using ergm here instead of statnet
      # library(ergm)
      library(tergm)
      library(ergm) #2Sep 2013
      library(network) #2Sep2013
      library(networkDynamic) #13Aug13
      ##source("../simulate.networkDynamic.R") #13Aug13: Commented out

  ## SIMULATION-Related Files and Packages
      #source("common.functions_d7.R")
       source("common.functions_d8.R")
       #source("update.vital.dynamics_d6.R")
         #source("update.vital.dynamics_d7.R")
       # source("update.vital.dynamics_d10.R") # 26Aug13: added max survival
        source("update.vital.dynamics_d10a.R") # 29Aug13: 0 and 1 to repr. males and feml.
       #source("assign.pregnancy_d2.R")
       #source("assign.pregnancy_d3.R")
              # 25Jul13: populate "time.since.art.cessation" in "d3"
       ## source("assign.pregnancy_d5.R")
       # source("assign.pregnancy_d5a.R") #3Sep13: Check binomial formula for pregnancy
       # source("assign.pregnancy_d5b.R") #13Sep13: Record informatio on num of pregns.
       # source("assign.pregnancy_d5d.R") #13Sep13: Immaculate conception WO correction
         source("assign.pregnancy_d5d1.R") #16Sep13: Correct loop. 
       ##source("update.treatment_d3.R") # with coverage accounted for in "d2"
       #source("update.treatment_d4.R")
       #source("update.treatment_d5.R") # 15aug13: corrected coding for status of
                                        # art initiators
       source("update.treatment_d6.R") # 22Aug13

       source("compute.cd4.count_d1.R")

       source("compute.viral.load_d4.R")
       # source("assign.infectivity_d2.R") #30Jul13
       source("assign.infectivity_d4.R") #30Jul13
       #source("transmission_d5.R") #"d6" has more information recording,
                                    # and output as csv 
       # source("transmission_d7.R") #15Aug13: source version "d8."
       source("transmission_d9.R")

  ## Starting Network 
      # load("estimation_uganda_d5.RData")
      
       source("params_d2.R") # ultimately take this out of here -- should
                                        # only be sourced in estimation file
#####################################################

#####################################################
### Material for saving
#####################################################

date <- "26Sep13_10pc_3phi_wocorrn_inc_fert_rate_1300steps_run2"
         # check if epidemic dies after 1000 timesteps

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
      timesteps <- 1300 ## 27 May '13: this mean 100*14 days now
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

#######################################################
### 29 Aug 2013: to save memory
#######################################################
rm(hetdeg.diag.sim)
rm(fit)
rm(sim.df)
rm(sim.stats)
gc()

#######################################################
  
  for (time in 2:timesteps) {   
        set.seed(Sys.time()) # sam has no problem getting stochastically different results 
                           # without this line

      ## Relational Change
      cat("Completing time step", time,
          "of", timesteps, "\n")
      ## Change in population
      ## transmission = f()
      ## Book-keeping

      ## nw <- simulate(nw,
      ##                formation=formation,
      ##                dissolution=dissolution,
      ##                coef.form=theta.form,
      ##                coef.diss=theta.diss,
      ##                constraints=constraints,
      ##                start.time=time,
      ##                time.slices=1, # 3Jun13: discussion with sam --
      ##                               # simulate over 1 time step every
      ##                               # time you hit the timestep in the loop
      ##                               # (as per the code in Steve's summer 2012 course)
      ##                control=control.simulate.networkDynamic(MCMC.burnin=10000),
      ##                maxchanges=1e6
      ##                )

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
        
      ## Change in population
      ## transmission = f()
      ## Book-keeping

      ## UPDATE VITAL DYNAMICS, AND UPDATE NETWORK
      ## function name changed from "update.network"
      nw <- update.vital.dynamics(nw, verbose=FALSE,
                                  max.survival=max.survival, #26Aug13
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
                             # preg.prob= 0.1, ## CHECK!!!
                             ##0.8, #2Sep13: commented out 
                             min.preg.interval=15*30/14,
                             optA.vl.reduction=1.1,
                             sc.art.postcess.ret.bl=12,
                             #num.births.per1k.byage, #2Sep13
                             num.births.per1k.byage=
                             num.births.per1k.byage.15pcinc,
                             prop.stillbirth, #2Sep13
                             inf.preg.red #2Sep13
                             )


        nw <- update.treatment(
                               nw, verbose=FALSE,
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
                                     23*7/14,
                                     ##0,
                                     optA.thres=350,
                                     cd4.at.infection.male=
                                     cd4.at.infection.male,
                                     cd4.at.infection.female=
                                     cd4.at.infection.female,
                                     ## per.day.untreated.cd4.decline=
                                     ## per.day.untreated.cd4.decline,
                                     cd4.recovery.time=cd4.recovery.time,
                                     per.day.cd4.recovery=per.day.cd4.recovery,
                                     #30Jul13: commented out below
                                     ## acute.length=acute.length,
                                     ## chronic.length=chronic.length, 
                                     ## late.length=late.length, 
                                     time.infection.to.peak.viral.load=
                                     time.infection.to.peak.viral.load,
                                     size.of.timestep=size.of.timestep, #28May13
                                     ## optA.sc.art.vl.perstep.dec, #5Jul13
                                     ## optA.sc.art.vl.perstep.inc, #5Jul13
                                     optA.vl.reduction=1.1, ##5Jul13
                                     full.term=40*7/14,
                                     undetectable.vl=log(50, base=10),
                                     time.to.full.supp=time.to.full.supp
                               )

    nw <- compute.cd4.count.sexage.acct(nw, verbose=FALSE,
                                               cd4.at.infection.male=
                                               cd4.at.infection.male,
                                               cd4.at.infection.female=
                                               cd4.at.infection.female,
                                               per.day.untreated.cd4.decline=
                                               per.day.untreated.cd4.decline,
                                               cd4.recovery.time=cd4.recovery.time,
                                               per.day.cd4.recovery=per.day.cd4.recovery,
                                               optA.sc.art.cd4.perstep.rec=
                                               optA.sc.art.cd4.perstep.rec,
                                               size.of.timestep=size.of.timestep #28May13
                                               )
 
        nw <- compute.vl.mp.art4(nw, verbose=FALSE, 
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

      nw <- assign.infectivity(nw, verbose=FALSE,
                               min.chronic.infectivity.unadj=
                               ## 0.2,
                               min.chronic.infectivity.unadj,
                               num.sex.acts.per.timestep=
                               num.sex.acts.per.timestep, #common.functions.d3"
                               acute.mult=acute.mult,
                               late.mult=late.mult,
                               preg.mult=preg.mult,#10Jun13
                               #30Jul13: Added arguments below
                               acute.length=acute.length,
                               chronic.length=chronic.length,
                               late.length=late.length
                               )
        
      ## now model transmission of infection
        ##browser()
        
        nw <- transmission(nw, verbose=FALSE,
                           preg.mult=preg.mult,
                           circum.mult=circum.mult, #13Aug13: Add new arguments
                           scenario="baseline", # 13Aug13: Add these arguments
                           baseline.art.coverage.rate=baseline.art.coverage.rate,
                           baseline.preg.coverage.rate=baseline.preg.coverage.rate,
                           dur.inf.by.age=dur.inf.by.age #22Aug2013
                           )

        ##browser()
        ## nw <- network.extract(nw, at=time, retain.all.vertices=TRUE)
                                                #29Aug13: (iiia.) above
        ##   nw <- network.extract(nw, at=time, retain.all.vertices=FALSE)
                                        #29Aug13: (iiib.) above
        
      ## Update edges coef, give feedback

         ## gc() #29Aug13
         ## gcinfo(TRUE)
        ## browser()
        z <- sapply(ls(), function(x) object.size(get(x))) #29Aug13: memory
        print(as.matrix(rev(sort(z))[1:10]))

        if (time == 5){
          rm(nw.start) #29Aug13: remove more unnecessary objects
          rm(n0)
          cleanMem()
        }
        
      ##theta.form[1] <-  theta.form[1] + log(popsize[time-1]) - log(popsize[time])
      cat("Finished timestep ",time," of ",timesteps,".\n",sep="")
      cat("\n")

    }


  save.image(paste(file="simulation.", date, ".RData", sep="")) #5 Aug 2013

#####################################################
### Diagnostics
#####################################################

  ## sim.stats <- attributes(hetdeg.diag.sim)$stats[1:timesteps,] #29Aug13: save memory
  ## apply(sim.stats, 2, mean)

#####################################################

