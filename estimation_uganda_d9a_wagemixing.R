#####################################################
### Progression of versions
#####################################################

## 9 Nov 2013: a. Estimation works. Either specify only isolates for men and women, 
## or specify degrees 1, 2 and 3 for men and women and leave isolates out. That
## is what I have done. Simulated from this network in "simulate_trial_estimate_za_d8d.R"
## to check mean statistics. Added constraint of max degree = 5 for both men and women. 

## b. Also tried constraining max degree of men to 5 and women to 4 at the same
## time via ~bdmaxout=c(5,4) -- but that didn't work.

## c. Now I will try this model with age mixing. 

## 8 Nov 2013: Try estimating network model again
## Revise target stats

## 17 Sep 2013: Estimate model for ZA

## 3 Sep 2013: Re-estimate, 32bit, 10% starting prevalence
## for WIP presentation

## 29Aug13: Change the data-type for sex -- from "male" to 0
## and from "female" to 1.

## 26 Aug 2013: Fix the "age.at.infection" attribute.

## 23 Aug 2013: Also record age at time of infection

## 22 Aug 2013: Add age-based duration of infection at time of infection
## Corrections in "estimation_uganda_d7.R"
## "transmission_d9", "update.vital.dynamics_d9.R",
## "compute.viral.load_d4.R", "update.treatment_d9.R",
## "simulation_uganda_d20.R"

## 20 Aug 2013: Make initial prevalence 15%.

#13 Aug 2013: Everything compatible with
## statnet version 3.1 and tergm version 3.1.1.

#13 Aug 2013: updated statnet to from CRAN version 3.0.1

## 11 Aug 2013: Update order of changes below:
## a. ART Coverage Indicator at time of infection. 
## b. Time Since Infection (drawn from uniform distribution)
## c. Time of Infection <- 0 - Time Since Infection
## d. Assign CD4 count -- following ART-naive trajectory for all infecteds.
## e. ART Status Indicator: For all infecteds who have ART Coverage = 1,
## assign ART Status=1
## f. Update CD4 count for those on ART -- from time since ART initiation.
## g. Assign VL -- again for men and women who are
## on ART
## h. VL will have to account for ART initiation.


## 9 Aug 2013: Move all constants to a separate "parameters" file.
## Order of new attributes:
## a. Time Since Infection (drawn from uniform distribution)
## b. Time of Infection <- 0 - Time Since Infection
## c. ART Coverage Indicator
## d. ART Status Indicator: For all infecteds who have ART Coverage = 1,
## assign ART Status=1
## e. Assign CD4 count -- for men and women who have are
## on ART, CD4 computation will include ART effect.
## f. Assign VL -- again for men and women who are
## on ART
## VL will have to account for ART initiation.

## 8 Aug 2013: Assign coverage indicator only to infected indivuals.
## Assign CD4 count, stage of infection, viral load, infectivity.

## 26 Jul 2013: Add time of infection attribute

## 25 Jul 2013: Add "time.since.art.cessation" attribute for women who go
## off scART

## 25 Jul 2013: Corrected spelling of pregnancy in
## n0%v%"time.since.curr.prenancy"
## n0%v%"time.since.last.prenancy"

## 7 July 2013: Clean up this file. Add "art.covered" to list of attribuetes.

## 12 June 2013: Add new attributes for computing slope of viral load decrease.

## The data we have report
## a. time to full suppression for regular ART
## b. change of 1.1 log between initiation and treatment for sc ART (Option A)
## c. coming back to baseline VL in 8-12 weeks,
##    after cessation of sc ART (Option A)
## can call this attribute "vl.traj.slope"

## 6 June 2013: A better way to parameterize "time.since.last.pregnancy" may
## be that everyone who has time.since.last.pregnancy=NA or > min.preg.interval
## is eligible to get pregnant. This is because currently we have
## all women given a value of (min.preg.interval + 1) at the start so they
## are eligible to get pregnant. However, this creates a problem later when
## new women who enter the population are initially assigned a value
## of NA for time since last pregnancy. Therefore, it may be better to revise
## the condition for eligibility of pregnancy.

## Add new attribute "art.type" to differentiate between
## a. regular ART (Type 1)
## b. short course ART (Type 2)

## 5 June 2013: We need two separate attributes here --
## time-since-current-pregnancy to monitor when pregnancy reaches full term.
## and time-since-last-pregnancy to monitor how much time has passed
## since the last pregnancy before the woman is "eligible" to be pregnant
## again.
## At time step 0, assign all women a "time.since.last.pregnancy" of 15 months,
## so they are all eligible to be pregnant at the beginning.
## Also, change name of "pregnancy.status" to "curr.pregnancy.status"

## 3 June 2013: Empirical degree distribution of Ugandan men here was wrong -- corrected
## to Phase 2 data. We get a model that converges here (as we did in "estimation_uganda_d3.R").

## 27 May 2013: In this version we implement all the changes
## due changing the size of the timestep.

## 27 May 2013: Adapt to increase the size of the time-step. With time-steps of the
## order of 4000 days, we get the following error:
## Error in toggleList[, 1] : incorrect number of dimensions
## which (I think) means there are not enough "toggles" in the changeList. 

## 26 May 2013: Adapt once again for estimation of network with features for Uganda

## 8 April 2013: Rework to correct inf.status
## Add "infectivity.today" attribute.

## 7 April 2013: Rewrite the way "art.status" is initially assigned.
## For all uninfected people, "init.inf.status" should be NA
## For all infected people not on treatment, "init.inf.status" should be 0.
## For all infected people on treatment, "init.inf.status" should be 1.
## Make sure this structure is maintained throughout.

## For now, keep time since infection = 1 for all infected people, to avoid
## problems with dynamics of CD4 counts later. 

## 5 April 2013: Play with duration parameter
## With 100 duration, I was getting the program to run for all 250 time steps.
## Something changed after I added the new functions, specifically,
## the update on the biological parameters

## 4 April 2013: Added more
## a. pregnancy parameters (time of, time since)
## b. art parameters (time of)
## c. CD4 parameters (cd4.count.today)
## d. viral load parameters (viral.load.today)

## HSV related attributes are not necessary, since we are not modeling
## HSV dynamics, but simply adjusting infectivities based on HSV prevalence

## STILL HAVE TO CHECK RELATIONSHIP BETWEEN VIRAL LOAD and TIME SINCE INFECTION
## IN FINAL STEP

## 1 April 2013: Clean up CD4 functions -- move to "common.functions" file,
## Corrected values for infection stages

## 25 March 2013: Add time since initiation of ART

## 20 March 2013: Rewrite because result of estimate is not working with
## simulation file. I keep getting the following error from the simulate function:
## ``Error in toggleList[, 1] : incorrect number of dimensions''

## 26 Feb 2013: Add testing as a function of pregnancy,
## and effect on ART status, adherence,
## Type of treatment -- sc ART,
## 1 week after birth.
## Reduction in infectiousness
## Option A and effect on treatment.
## Attribute for type of treatment = f(Pregnancy Status, CD4).
## For men, just ART.
## For women, scART (Option A), ART,
## Time of Initiation of Treatment in Men, Non Pregnant Women = f(CD4).
## Time of Initiation of Treatment in Pregnant Women = f(Gestation).
## Time of Exiting Treatment in Men, Non-Pregnant Women = 
## Time of Exiting Treatment in Pregnant Women on Option A = 1 week after Delivery
## Infectivity as a Function of Pregnancy Status (and interaction of pregnancy with
## treatment)

## 19 Feb 2013: After conversation with Sam, realized that
## "ergm" with Carnegie approximation will work instead of the "tergm"
## function, which takes much longer to fit -- and did not seem
## to be working with b1degre and b2degree anyway.
## Take infection status (in addition to ASFR)
## into account while defining pregnancy.

## 12 Feb 2013: Carnegie approximation does not seem to
## work for really long durations. If partnership duration is 100,
## the mean stats are right on. If partnership duration is 2000, mean stats
## for the edges term are off, and also for the mean partnership duration.
## Solution: Timeslices needs to be higher

## 8 Feb 2013: Start coding to get network right
## Follow Google's Style Guide
## Learn about Unit-Testing
## Remember the set.vertex.attribute function
#####################################################


rm(list=ls())
##library(ergm)

##library(statnet)#13 Aug 2013
library(network)# 3 Sep 2013
library(ergm)#3 Sep 2013
library(tergm)
library(networkDynamic)
## source("../simulate.networkDynamic.R") #13Aug13: comment out network dynamic here
source("common.functions_d8.R")

#source("compute.cd4.count_d1.R")
#source("params_d1.R")
source("params_za_d2.R") #22Aug13 age based duration of infection

#####################################################
### Set up population
### and initialize network
#####################################################


  ### Set up model
  formation <- ~edges+
                #b1degree(0:1)+
                #b1degree(0)+
                #b1degree(1:2)+
                b1degree(1:3)+
                #b2degree(0:1)
                #b2degree(0)
                #b2degree(1:2)
                b2degree(1:3)
  formation.n0 <- update.formula(formation, n0~.)
  dissolution <- ~offset(edges)
  theta.diss <- log(duration-1)
  # target.stats <- c(female.deg.tot.matched, male.deg.counts[1:2],
  # female.deg.counts.matched[1:2])
  target.stats <- c(n.edges,
                    #male.deg.edgecount[1:2],
                    #male.deg.edgecount[1],
                    #male.deg.edgecount[2:3],
                    male.deg.edgecount[2:4],
                    #female.deg.edgecount[1:2]
                    #female.deg.edgecount[1]
                    #female.deg.edgecount[2:3]
                    female.deg.edgecount[2:4]
                    )

  target.stats.cs <- c(n.edges.cs,
                       #male.deg.edgecount.cs[1:2],
                       #male.deg.edgecount.cs[1],
                       #male.deg.edgecount.cs[2:3],
                       male.deg.edgecount.cs[2:4],
                       #female.deg.edgecount.cs[1:2]
                       #female.deg.edgecount.cs[1]
                       #female.deg.edgecount.cs[2:3]
                       female.deg.edgecount.cs[2:4]
                       )
  constraints <- ~.
  constraints <- ~bd(maxout=5)
  #constraints <- ~bd(maxout=c(5,4)) #doesn't work

#####################################################
### Initialize Network
#####################################################
  n0 <- network.initialize(N, bipartite=num.male, directed=F)
#####################################################


#####################################################
### Add Atributes
#####################################################


#####################################################


#####################################################
### Fit cross-sectional model to formation,
### and apply Carnegie correction
#####################################################

  fit <- ergm(formation.n0,
              #target.stats=target.stats,
              target.stats=target.stats.cs,
              constraints=constraints,
              maxit=100,
              verbose=FALSE
              )

  theta.form <- fit$coef 
  theta.form[1] <- theta.form[1] - theta.diss

  hetdeg.diag.sim <- simulate(n0,
                              formation=formation, dissolution=dissolution,
                              coef.form=theta.form, coef.diss=theta.diss,
                              ##time.slices=1000,
                              time.slices=2e4,
                              constraints=constraints,
                              monitor=~edges+b1degree(0:5)+b2degree(0:5),
                              control=control.simulate.network(MCMC.burnin=10000)
                              )
  ## Part 1 (From Class) 
     sim.stats <- attributes(hetdeg.diag.sim)$stats[101:2e4,]
     stats.means <- colMeans(sim.stats)##mean(sim.stats)
     stats.sd <- apply(sim.stats,2,sd)##sd(sim.stats)##apply(sim.stats,2,sd)
     ## 9 Nov 2013: VERY IMPORTANT!!!!!!!!!
     ## The mean statistics from here are wrong -- simulate from this network to
     ## see waht mean statistics are -- in "simulate_trial_estimate_za_d8d.R"


  ## Part 2 (From Class)
     sim.df <- as.data.frame(hetdeg.diag.sim)
     partnership.duration.mean <- mean(sim.df$duration)
     partnership.duration.sd <- sd(sim.df$duration)
     partnership.duration.expected <- exp(theta.diss)+1

     save(hetdeg.diag.sim, file="hetdeg.diag.sim_za.RData")
                                        # max degree of men and women is 5
  ## save(hetdeg.diag.sim, file="hetdeg.diag.sim_za_men5_women4.RData")
                                        # max deg of men is 5 and women is 4

##   save(hetdeg.diag.sim.durstep, file="hetdeg.diag.sim.durstep.RData")
#####################################################
### Diagnostics
#####################################################

       save.image(file="estimation_za_d8d.RData")
                                        # max degree of men and women is 5
    ## save.image(file="estimation_za_d8d_men5_women4.RData")
                                        # max deg of men is 5 and women is 4
