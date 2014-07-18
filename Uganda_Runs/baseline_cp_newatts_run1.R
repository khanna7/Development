#####################################################
### Progression of Versions
#####################################################

## 28 Feb 2014: Was waiting for room on the cluster,
## and doing other things. Can simulate now. 

## 25 Feb 2014: Adapt for Uganda Runs

## 24 Feb 2014: Rerun with new attributes for
## post-cessation CD4 and viral load trajectories in
## Option A and Option B.
## Relevant details from
## "burnin_fieldmortality_phi004_fixartsign_incdur700_chungvl_newatts.R" --
## Main needed modification to functions below:
## Add both arguments "optB.cess.time" and
## "optB.vl.postcess.ret.bl"
## to "assign.pregnancy" and only the latter
## to "compute.cd4.count" functions

## 14 Feb 2014: Fixed Option B bug in transmission code.

## 7 Feb 2014: Adapt for Option B

## 5 Feb 2014: Had sourced wrong parameter file,
## correct it now

## 3 Feb 2014: With ART model fixed.

## 23 Jan 2014: Modify for South Africa

## 21 Jan 2014: Run interventions on burnin models
## with Fielding estimates for pre-ART mortality. 

## 18 Dec 2013: Adapt for Uganda intervention runs using
## "burnin.14Dec_UG_phi004_1100ts_run2.RData"

## 14 Dec 2013:
## a. Adapt to run bl scenario with realistic implementation

## 4 Dec 2013: Adapt revised South Afrcia version for
## redoing Udanga burnins. See changes from 3 Dec 2013
## and 2 Dec 2013 for list of changes.

## 3 Dec 2013:
## a. fixed cap for viral load of pregnant women of ART again.
## version "../compute.viral.load_d5_d2.R" now.

## b. Also source "../assign.pregnancy_d6d1.R" for recording number of pregnancies

## c. Differentiate prevalence at recruitment for men and women.

## d. Revised (a) again because
## ``Cap is only necessary for people in chronic stage, or later. Currently
## it was applied to people in peak stage going to late stage. Change cap to only
## apply to chronic stage and later.''
## See "../compute.viral.load_d5_d3.R"

## e. Load estimation object
## "estimation_za_phase2_entr18_max_deg_unconstrained_params_rev_numpreg_d8d.RData"
## that contains initialized "num.of.pregnancies" attribute.

## 2 Dec 13: test if fix for vl of post-sc ART women is working 
## and also check time of birth and death

## 25 Nov 2013: Adjust "baseline.art.coverage.rate" for adherence. We will do this by using
## a multiplicative model for coverage and adherence -- details
## in "../update.params_uganda_d3.R" and see email dated 22 Nov 2013
## from Sarah.

## 18 Nov 2013: load latest estimation object -- rerun simulations

## 12 Nov 2013: South Africa runs

## 11 Nov 2013: Source update vital dynamics file with age 18 at start.
## load estimation object with age 18 --
## estimation_uganda_d10a_agemix_diag_b1fact_b2fact_minage18.RData

## 8 Nov 2013: Change name of data "dur.inf.by.age" to "given.dur.inf.by.age"
## to avoid confusion with the attribute which appears later


## 7 Nov 2013:
## a. Getting error: Error in if (viral.load.today[i] < 2) { :
##  missing value where TRUE/FALSE needed -- has to be fixed via
## "vl.art.traj.slope" -- see note dated 1 Aug 2013.
## To fix, we need arguments "undetectable.vl" and "time.to.full.supp"
## We assume all new entrants are infected, untreated, and have viral
## load 0 on day of entry. We need to assign them a numerical 
## "vl.art.traj.slope" based on these assumptions -- which is what
## we do here. 

## we also need "dur.inf.by.age" here

## 3 Nov 2013: Try to fix low prevalence in longterm
## a. Here try age mixing model with only diagonal entries specified, but growth
## rate is what we have been using phi/5*3 where phi=0.001*5

## 1 Nov 2013:
## a. Debug Option A
## b. Recompile in this folder, with "male" and "female" replaced by 0 and 1 respectively.
## c. Corrected bug in ("../update.treatment_d6_wpmtctb+.R")
## with ("../update.treatment_d7_wpmtctb+.R")

## 22 Oct 2013: Run on 64-bit MOSIX3 with estimation object also compiled in
## 64-bit. 

## 4 Oct 2013: Simulate with partnership durations restricted to those
## reported between 15 and 55 year olds. 

## 1 Oct 2013: Simulate from age mixing matrix with only diagonal entries
## and structural zeros specified.

## 30 Sep 2013: This is from version "simulation_uganda_d22e."
## We have a. age mixing now,
## b. increased fertility rates for pregnant women to get pregnancies in the
## correct ballpark
## c. pregnancy model without "correction factor" i.e. inverse of ratio of
## all women in an age group to women who meet pregnancy criteria.
## d. Also change version of "update_vital_dynamics" file being sourced
## to 11a.R so the "age.cat" attribute is updated for everyone, and
## assigned equal to 1 for new entries.

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


## STATNET-Related Files and Packages
      library(tergm)
      library(ergm) #2Sep 2013
      library(network) #2Sep2013
      library(networkDynamic) #13Aug13
 
  ## SIMULATION-Related Files and Packages

      
       load("burnin.21Feb_UG_phi004_newatts_run1.RData") #25Feb14

       source("../common.functions_fieldingmortality_d9.R")
       source("../update.vital.dynamics_newinfected_entryage18_d13a.R") #14Jan14
       source("../assign.pregnancy_d8d1.R") #22Feb14
       source("../update.treatment_d11.R") #22Feb14
       source("../compute.cd4.count_d3.R") #22Feb14
       source("../compute.viral.load_d7_d3.R") #22Feb14
       source("../assign.infectivity_d4.R") #30Jul13
       source("../transmission_d10.R")

  ## Starting Network
       source("../params_uganda_d3.R")
#####################################################

#####################################################
### Material for saving
#############################################0########

   date <- "28Feb_UG_bl_cp_run1"

#####################################################


#####################################################
### Starting Network Objects
#####################################################

  ## Fit, formula, population features
      nw <- network.collapse(nw, at=1041)

  ## Other Needed Parameters for timesteps
     time1 <- 1042
     time2 <- (time1-1)+260

  ## Record initial network statistics
      nw.start <- network.copy(nw)
      nw.stats <- matrix(NA, timesteps, 13)
      nw.stats[1,] <- summary(nw~edges+b1degree(0:5)+b2degree(0:5)) 

  ## Start with all vertices and edges active
#####################################################
### Time Loop
#####################################################

  scenario <- "baseline"

#######################################################
### 29 Aug 2013: to save memory
#######################################################
rm(hetdeg.diag.sim)
rm(fit)
rm(sim.df)
rm(sim.stats)
gc()

#######################################################
  
    for (time in time1:time2) {
        set.seed(Sys.time()) # sam has no problem getting stochastically different results 
                           # without this line

      ## Relational Change
      cat("Completing time step", time,
          #"of", timesteps, "\n") #14Dec13
          "of", time2, "\n")

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
        

      ## UPDATE VITAL DYNAMICS, AND UPDATE NETWORK
      ## function name changed from "update.network"
      nw <- update.vital.dynamics(nw, verbose=FALSE,
                                  max.survival=max.survival, #26Aug13
                                  asmr.male=asmr.male,
                                  asmr.female=asmr.female,
                                  phi=phi.std4,
                                  size.of.timestep=size.of.timestep,
                                  prop.f=prop.f,
                                  circum.rate=circum.rate,#12Jul13
                                  baseline.art.coverage.rate=
                                  baseline.art.coverage.rate,
                                  baseline.preg.coverage.rate=
                                  baseline.preg.coverage.rate,
                                  ## recruit.inf.prop=
                                  ## 3.00/100, #3Dec13
                                  recruit.inf.prop.male=recruit.inf.prop.male,#3Dec13
                                  recruit.inf.prop.female=recruit.inf.prop.female,#3Dec13
                                  undetectable.vl=undetectable.vl,
                                  time.to.full.supp= #7Nov13:todebug
                                  time.to.full.supp,
                                  given.dur.inf.by.age=
                                  given.dur.inf.by.age
                                  )

      ## assign pregnancy
      nw <- assign.pregnancy(nw, verbose=FALSE,
                             full.term=full.term, 
                             min.preg.interval=min.preg.interval,
                             optA.vl.reduction=optA.vl.reduction,
                             sc.art.postcess.ret.bl=sc.art.postcess.ret.bl,
                             num.births.per1k.byage=
                             num.births.per1k.byage.15pcinc,
                             prop.stillbirth=prop.stillbirth, 
                             inf.preg.red=inf.preg.red,
                             scenario=scenario, #21Jan14
                             optB.cess.time=
                             optB.cess.time, #24Feb14
                             optB.vl.postcess.ret.bl=
                             optB.vl.postcess.ret.bl
                             #24Feb14
                             )

        nw <- update.treatment(
                               nw, verbose=FALSE,
                               scenario=scenario,
                               dur.inf=dur.inf,
                               eligibile.cd4=eligible.cd4,
                               baseline.cd4.at.art.initiation.men = 
                               baseline.cd4.at.art.initiation.men,
                               baseline.cd4.at.art.initiation.women=
                               baseline.cd4.at.art.initiation.women,
                               baseline.f.ges.visit=
                               baseline.f.ges.visit,
                               #0.1,
                               optA.thres=optA.thres,
                               cd4.at.infection.male=
                               cd4.at.infection.male,
                               cd4.at.infection.female=
                               cd4.at.infection.female,
                               cd4.recovery.time=cd4.recovery.time,
                               per.day.cd4.recovery=per.day.cd4.recovery,
                               time.infection.to.peak.viral.load=
                               time.infection.to.peak.viral.load,
                               size.of.timestep=size.of.timestep, 
                               optA.vl.reduction=optA.vl.reduction, 
                               full.term=full.term,
                               undetectable.vl=undetectable.vl,
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
                                            size.of.timestep=size.of.timestep, #28May13
                                            sc.art.postcess.ret.bl=
                                            sc.art.postcess.ret.bl, #24Feb14
                                            optB.vl.postcess.ret.bl=
                                            optB.vl.postcess.ret.bl #24Feb14
                                            )
 
        nw <- compute.vl.mp.art4(nw, verbose=FALSE, 
                                 time.infection.to.peak.viral.load=
                                 time.infection.to.peak.viral.load,
                                 peak.viral.load=peak.viral.load,
                                 time.infection.to.viral.set.point=
                                 time.infection.to.viral.set.point,
                                 set.point.viral.load=
                                 set.point.viral.load,
                                 time.infection.to.late.stage=
                                 time.infection.to.late.stage,
                                 late.stage.viral.load=
                                 late.stage.viral.load,
                                 dur.inf=
                                 dur.inf,
                                 time.to.full.supp=
                                 time.to.full.supp,#7Nov13:debug
                                 undetectable.vl=
                                 undetectable.vl,
                                 sc.art.postcess.ret.bl=sc.art.postcess.ret.bl, #25Jul13
                                 size.of.timestep=size.of.timestep, # 28May13
                                 )
 
      ## MODEL TRANSMISSION
      nw <- assign.infectivity(nw, verbose=FALSE,
                               min.chronic.infectivity.unadj=
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
                           preg.susc.mult=preg.susc.mult,
                           circum.mult=circum.mult, #13Aug13: Add new arguments
                           scenario=scenario, # 13Aug13: Add these arguments
                           baseline.art.coverage.rate=baseline.art.coverage.rate,
                           baseline.preg.coverage.rate=baseline.preg.coverage.rate,
                           given.dur.inf.by.age=given.dur.inf.by.age, #22Aug2013,
                           eligible.cd4=eligible.cd4, #30Oct13
                           baseline.f.ges.visit=#0.1 #30Oct13
                           baseline.f.ges.visit
                           )

        z <- sapply(ls(), function(x) object.size(get(x))) #29Aug13: memory
        print(as.matrix(rev(sort(z))[1:10]))

        if ("n0" %in% ls()){
          rm(n0)
          cleanMem()
        }
        
        if ("nw.start" %in% ls()){
          rm(nw.start)
          cleanMem()
        }

        if ("nw.stats" %in% ls()){
          rm(nw.stats)
          cleanMem()
        }

        if (time %% 10 == 0){ #8Oct2013: Crash-retrieval mechanism
          crash.save.obj <- paste("nw", date , ".RData", sep="")
          save(nw, file=crash.save.obj)
          rm(crash.save.obj)
          cleanMem()
        }
        
      ##theta.form[1] <-  theta.form[1] + log(popsize[time-1]) - log(popsize[time])

      # cat("Memory size is ", memory.size(), ".\n", sep="")

      cat("Finished timestep ",time," of ",timesteps,".\n",sep="")
      cat("\n")

    }

  save.image(paste(file="interv.", date, ".RData", sep="")) #5 Aug 2013

#####################################################
### Diagnostics
#####################################################

  ## sim.stats <- attributes(hetdeg.diag.sim)$stats[1:timesteps,] #29Aug13: save memory
  ## apply(sim.stats, 2, mean)

#####################################################

