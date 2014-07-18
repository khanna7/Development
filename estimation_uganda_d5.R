rm(list=ls())
##library(ergm)

library(tergm)
source("../simulate.networkDynamic.R")
source("common.functions_d8.R")

#####################################################
### Progression of versions
#####################################################

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

#####################################################
### Set up population
### and initialize network
#####################################################


  ## Basic Population Set UP
  num.male <- 2500
  num.female <- 2500 
  N <- num.male+num.female

  ## BEHAVIOR
  ## Network Related Statistics
  ## Degree Distribution, Partnership Duration
  ## and Number of Partnerships
  male.deg.dist <- c(34.8, 52.3, 9.8, 3.1)/100 ## Phase 2 data
  female.deg.dist <- c(42.0, 55.7, 2.3, 0)/100 ## Phase 2 data

  size.of.timestep <- 14 ## each time step is 14 days
  duration <- 4303/size.of.timestep
  diagnostics <- T

  female.deg.counts <- female.deg.dist*num.female
  male.deg.counts <- male.deg.dist*num.male
  female.deg.tot <- (0*female.deg.counts[1] + 1*female.deg.counts[2] +
                     2*female.deg.counts[3] + 3*female.deg.counts[4])
  male.deg.tot <- (0*male.deg.counts[1] + 1*male.deg.counts[2] +
                   2*male.deg.counts[3] + 3*male.deg.counts[4])

  ## to match the degree totals for men and women, reduce the number of
  ## isolates in the women, and increase the number of women with 1 partner
  female.deg.dist.matched <- c(22.0, 75.7, 2.3, 0)/100
  female.deg.counts.matched <- female.deg.dist.matched*num.female
  female.deg.tot.matched <- (0*female.deg.counts.matched[1] +
                             1*female.deg.counts.matched[2] +
                             2*female.deg.counts.matched[3] +
                             3*female.deg.counts.matched[4])

  ### Set up model
  formation <- ~edges+b1degree(0:1)+b2degree(0:1)
  formation.n0 <- update.formula(formation,n0~.)
  dissolution <- ~offset(edges)
  theta.diss <- log(duration-1)
  target.stats <- c(female.deg.tot.matched, male.deg.counts[1:2],
  female.deg.counts.matched[1:2])

  constraints <- ~.

#####################################################
### Initialize Network
#####################################################
  n0 <- network.initialize(N, bipartite=num.male, directed=F)
#####################################################

#####################################################
### Add Atributes
#####################################################

  ## BEHAVIORAL ATTRIBUTES
     ## ADDED ABOVE

  ## DEMOGRAPHIC ATTRIBUTES 

      ## Sex, age, circumcision status, 
      ## pregnancy status, 
      ## other pregnany related parameters

      ## Sex (uniformly distributed -- in accordance with census data)
      n0 %v% "sex" <- rep(c("male", "female"),
                          c(num.male, num.female)
                          )
      male.id <- which(n0%v%"sex"=="male")
      female.id <- which(n0%v%"sex"=="female")

      ## Age (in accordance with proportions from census data)
      min.age <- 15
      max.age <- 54
      age.distribution <- c(0.25468095,
                            0.20238352,
                            0.155598745,
                            0.119586275,
                            0.095660855,
                            0.07367249,
                            0.058677315,
                            0.04014263)

      age.classes <- c(seq(15, 19, by=1),
                       seq(20, 24, by=1),
                       seq(25, 29, by=1),
                       seq(30, 34, by=1),
                       seq(35, 39, by=1),
                       seq(40, 44, by=1),
                       seq(45, 49, by=1),
                       seq(50, 54, by=1)
                       ) # create a vector of all ages of interest

      prob.for.each.age <- unlist(lapply(age.distribution, function(x) rep(x, 5)))

      ## then assign the probability for each age class to be selected to the numeric
      ## values for that class
      age <- sample(age.classes, size=N, replace=TRUE, prob=prob.for.each.age)
      n0 %v% "age" <- age

      ## checks for age
      length(intersect(which(age>=15), which(age<20)))/N # first age class
      length(intersect(which(age>=50), which(age<55)))/N # last age class

      ## circumcision status
      circum.rate <- 96/(851+96) ## No 851; Yes 96; Refusal 1; NA 0,
                               ## see file "data_report.pdf"
      circum.status <- rbinom(N/2, 1, circum.rate)
      n0 %v% "circum.status" <- c(circum.status, rep(NA, N/2))

      ## pregnancy status
      set.vertex.attribute(n0, "curr.pregnancy.status", NA, male.id)
      set.vertex.attribute(n0, "curr.pregnancy.status", 0, female.id)
      ## defeault pregnancy status is NA,
      ## overwritten with age specific fertility rates for women below

      asfr.15to19 <- 0.000472603 ## Age-Specific Fertility Rate
      asfr.20to24 <- 0.000916164 ## Tailor each to ZA, UG
      asfr.25to29 <- 0.000836712
      asfr.30to34 <- 0.000691233
      asfr.35to39 <- 0.000476164
      asfr.40to44 <- 0.000207945
      asfr.45to49 <- 9.50685E-05

 
      ## will have to move this to after infection status?
      ## to account for lower fertility rate in HIV diagnosed women?

      fem.age.15to19 <- intersect(which((n0%v%"age") < 20 ),
                                  which((n0%v%"sex") == "female")) 
      fem.age.20to24 <- intersect(intersect(which((n0%v%"age") >= 20),
                                            which((n0%v%"age") < 25)),
                                  which((n0%v%"sex") == "female"))
      fem.age.25to29 <- intersect(intersect(which((n0%v%"age") >= 25),
                                        which((n0%v%"age") < 30)),
                                  which((n0%v%"sex") == "female"))
      fem.age.30to34 <- intersect(intersect(which((n0%v%"age") >= 30),
                                            which((n0%v%"age") < 35)),
                                  which((n0%v%"sex") == "female"))
      fem.age.35to39 <- intersect(intersect(which((n0%v%"age") >= 35),
                                            which((n0%v%"age") < 40)),
                                  which((n0%v%"sex") == "female"))
      fem.age.40to44 <- intersect(intersect(which((n0%v%"age") >= 40),
                                            which((n0%v%"age") < 45)),
                                  which((n0%v%"sex") == "female"))
      fem.age.45to49 <- intersect(intersect(which((n0%v%"age") >= 45),
                                            which((n0%v%"age") < 50)),
                                  which((n0%v%"sex") == "female"))

      set.vertex.attribute(n0, "curr.pregnancy.status",
                           rbinom(length(fem.age.15to19), 1, asfr.15to19),
                           fem.age.15to19)
      set.vertex.attribute(n0, "curr.pregnancy.status", 
                           rbinom(length(fem.age.20to24), 1, asfr.20to24),
                           fem.age.20to24)
      set.vertex.attribute(n0, "curr.pregnancy.status", 
                           rbinom(length(fem.age.25to29), 1, asfr.25to29),
                           fem.age.25to29)
      set.vertex.attribute(n0, "curr.pregnancy.status", 
                           rbinom(length(fem.age.30to34), 1, asfr.30to34),
                           fem.age.30to34)
      set.vertex.attribute(n0, "curr.pregnancy.status", 
                           rbinom(length(fem.age.35to39), 1, asfr.35to39),
                           fem.age.35to39)
      set.vertex.attribute(n0, "curr.pregnancy.status", 
                           rbinom(length(fem.age.40to44), 1, asfr.40to44),
                           fem.age.40to44)
      set.vertex.attribute(n0, "curr.pregnancy.status", 
                           rbinom(length(fem.age.45to49), 1, asfr.45to49),
                           fem.age.45to49)

      intersect(female.id, which(n0%v%"age" > 49))
      ## NA for pregnancy in women is
      ## for those above 49 years of age
                         
      ## time of, and time since current pregnancy
      ## time since last pregnancy
      n0%v%"time.of.curr.pregnancy" <- NA
      n0%v%"time.since.curr.pregnancy" <- NA #25Jul2013: Correct spelling of pregnancy
      n0%v%"time.since.last.pregnancy" <- NA # was prenancy earlier

      pregnany.status <- n0%v%"curr.pregnancy.status"
      pregnant.yes <- which(n0%v%"curr.pregnancy.status" == 1)

      set.vertex.attribute(n0, "time.since.curr.pregnancy", 0,
                           pregnant.yes)
      set.vertex.attribute(n0, "time.of.curr.pregnancy", 0,
                           pregnant.yes)
      ## initially, time since last pregnancy was set
      ## to be slightly greater than the "min.preg.interval."
      ## But for reasons described above, we changed these back to NA.


  ## BIOLOGICAL ATTRIBUTES 

      ## Infection Status 
      init.hiv.prev <- 0.06 # set to about 6% for Uganda
      init.inf.status <- rbinom(network.size(n0), 1, init.hiv.prev)
      set.vertex.attribute(n0, "inf.status", init.inf.status)
      inf.status.men <- init.inf.status[male.id]
      inf.status.women <- init.inf.status[female.id]

      ## Time Since Infection 
      duration.of.infection <- 3300 ## in days, modify later
      init.infected <- which(init.inf.status == 1)

      time.since.infection <- rep(NA, N)
      ## 7 April 2013, changed to avoid problems with dynamics of CD4 counts later
      ## all time since infection for positives set to 0 initially
      time.since.infection[init.infected] <- 0
      set.vertex.attribute(n0, "time.since.infection", time.since.infection
                                                                   )
      ## 26Jul13: Add time of infection attribute
      time.of.infection <- time.since.infection #At time 0, both are equal
      time.of.infection[init.infected] <- 0
      set.vertex.attribute(n0, "time.of.infection", time.of.infection)


      ## infectivity for infected individuals
      infectivity.today <- rep(NA, N)
      min.chronic.infectivity <- 0.00497/2.89
      set.vertex.attribute(n0, "infectivity.today", NA)
      set.vertex.attribute(n0, "infectivity.today", min.chronic.infectivity,
                           init.infected
                           )


  ## Time of Infection 
      acute.length <- 1:floor(121/size.of.timestep) ## in daily time units
      chronic.length <- ceiling(121/size.of.timestep):floor(1877/size.of.timestep)
      late.length <- ceiling(1877/size.of.timestep):floor(3300/size.of.timestep)

      ## Stage of Infection
             ## Classification necessary for deciding infectivity
             ## for chronically infected
      stage.of.infection <- classify.stage.numeric.rewrite(time.since.infection)
      n0%v%"stage.of.infection" <- stage.of.infection

      ## CD4 Counts
             ## Set to 518 for men and 570 for women
             ## For positives, this will change as we step through time loop.
             ## Relevant parameters
      cd4.at.infection.male      <- 518 #cells/mm3
      cd4.at.infection.female      <- 570 #cells/mm3
      untreated.cd4.daily.decline <- 0.14 # (for men and women)
      untreated.cd4.perstep.decline <-      untreated.cd4.daily.decline*size.of.timestep

      ## Initialize "cd4.count.today"
      cd4.count.today <- rep(NA, N)
      cd4.count.today[male.id] <- cd4.at.infection.male
      cd4.count.today[female.id] <- cd4.at.infection.female
      set.vertex.attribute(n0, "cd4.count.today", cd4.count.today)


      ## Viral Load Today
        ## Set to 0 for everyone initially
        ## Relevant Parameters
        ## time.infection.to.peak.viral.load,
        ## peak.viral.load,
        ## time.infection.to.viral.set.point,
        ## set.point.viral.load,
        ## time.infection.to.late.stage,
        ## dur.inf,
       ## late.stage.viral.load

      ## List viral load parameters, adjusted for size of timestep
      time.infection.to.peak.viral.load <- floor(14/size.of.timestep)
      peak.viral.load <- 6.17
      time.infection.to.viral.set.point <- floor(121/size.of.timestep)
      set.point.viral.load <- 4.2
      time.infection.to.late.stage <- floor(1877/size.of.timestep)
      dur.inf <- floor(3300/size.of.timestep)
      late.stage.viral.load <- 5.05 ## (max?)

      ## At time step 0, everyone has viral load 0.
      viral.load.today <- rep(0, N)

      ## Assign viral loads
        ## 12Jun13: Assign "vl.art.traj.slope" is to calculate change in
        ## viral load given different ART regimens
      n0%v%"viral.load.today" <- viral.load.today
      n0%v%"vl.art.traj.slope" <- rep(NA, N)

  ## TREATMENT (ART) ATTRIBUTES 

      ## ART Status (Whether currently on treatment)
        ## This is an inidividual-level attribute indicating 
       ### whether someone is on ART on a given day. 
      art.status <- rep(NA, N)
      art.status[which(init.inf.status==1)] <- 0
      ## Initially all positives have ART status 0
     set.vertex.attribute(n0, "art.status", art.status)

     ## Time since, time of ART initiation
        ## both set to NA at start
      time.since.art.initiation <- rep(NA, N)
      time.of.art.initiation <- time.since.art.initiation

      ## ART type
         ## Set to NA for everyone at start
      art.type <- rep(NA, N)
      n0%v%"time.since.art.initiation" <- time.since.art.initiation
      n0%v%"time.of.art.initiation" <- time.of.art.initiation
      n0%v%"art.type" <- art.type

      n0%v%"time.since.art.cessation" <- NA
      n0%v%"time.of.art.cessation" <- NA
                                        #25Jul13 -- primarily for preg. women going
                                        # of scART

  ## ART coverage
     ## so we can control how many individuals will receive ART
     ## will depend on scenario being modeled
     ## set to NA initially
     ## combines coverage, uptake, adherence 
      ## n0%v%"art.covered" <- NA
      baseline.art.coverage.rate <- 0.40
      reg.art.covered <- rbinom(N, 1, baseline.art.coverage.rate)
      n0%v%"reg.art.covered" <- reg.art.covered

      baseline.preg.art.coverage.rate <- 0.43 # check this
      preg.art.covered <- rbinom(num.female, 1, baseline.preg.art.coverage.rate)
      n0%v%"preg.art.covered" <- c(rep(NA, num.male), preg.art.covered)

#####################################################
### Fit cross-sectional model to formation,
### and apply Carnegie correction
#####################################################

  fit <- ergm(formation.n0,
              target.stats=target.stats,
              constraints=constraints)
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
   ## Save simulation object -- takes a long time to run (~20 mins with
   ## time.slices=2e4)

   ## hetdeg.diag.sim.durstep <- simulate(n0,
   ##                            formation=formation, dissolution=dissolution,
   ##                            coef.form=theta.form, coef.diss=theta.diss,
   ##                            ##time.slices=1000,
   ##                            time.slices=floor(duration/3),
   ##                            constraints=constraints,
   ##                            monitor=~edges+b1degree(0:5)+b2degree(0:5),
   ##                            control=control.simulate.network(MCMC.burnin=10000)
   ##                            )
   ## time.slices parameter chosen to get roughly the mean number of specified
   ## edges in "hetdeg.diag.sim.durstep" -- and this object can then be used in
   ## testing

   save(hetdeg.diag.sim, file="hetdeg.diag.sim.RData")
##   save(hetdeg.diag.sim.durstep, file="hetdeg.diag.sim.durstep.RData")
#####################################################
### Diagnostics
#####################################################

  ## Part 1 (From Class) 
     sim.stats <- attributes(hetdeg.diag.sim)$stats[101:2e4,]
     stats.means <- colMeans(sim.stats)##mean(sim.stats)
     stats.sd <- apply(sim.stats,2,sd)##sd(sim.stats)##apply(sim.stats,2,sd)

  ## Part 2 (From Class)
     sim.df <- as.data.frame(hetdeg.diag.sim)
     partnership.duration.mean <- mean(sim.df$duration)
     partnership.duration.sd <- sd(sim.df$duration)
     partnership.duration.expected <- exp(theta.diss)+1

  ## Part 3 (From Class)
  ## Back-calculate the expected values for
  ## b1degree(2:3) and b2degree(2:3), and compile
  ## was unnecessary -- so took out

#####################################################
## Save Object
#####################################################
  save.image(file="estimation_uganda_d5.RData")
#####################################################

