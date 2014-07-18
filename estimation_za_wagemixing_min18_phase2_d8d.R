#####################################################
### Progression of versions
#####################################################

## 11 Nov 2013: Model estimation works with:
## a. num of edges
## b. degree distributions of men and women (specified deg 1 and deg 2 for both)
## c. constrain max degree to 4 for men and women
## d. age mixing specified via: diagonal entries, row sums (1 and 2 left out),
## column sums (1 and 2 left out). looks pretty good, except in the upper triangle.

## 10 Nov 2013: Estimate ZA network from Phase 2 data.

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

library(network)# 3 Sep 2013
library(ergm)#3 Sep 2013
library(tergm)
library(networkDynamic)

source("common.functions_d8.R")
source("params_za_d2.R") #22Aug13 age based duration of infection

#####################################################
### Set up population
### and initialize network
#####################################################
  seq.of.age.mix <- 1:16
  to.specify <- c(1, 6, 11, 16) 
  age.leave.out <- seq.of.age.mix[-c(to.specify)]

  ### Set up model
  formation <- ~edges+
                b1degree(1:2)+
                b2degree(1:2)+
                b1starmix(k=1,
                          attrname="age.cat",
                          base=c(age.leave.out),
                          diff=TRUE)+
                 b1factor("age.cat", base=c(1:2))+
                 b2factor("age.cat", base=c(1:2))
  
  formation.n0 <- update.formula(formation, n0~.)
  dissolution <- ~offset(edges)
  theta.diss <- log(duration-1)

  age.mixing.all.stat <- matrix(c(0.176, 0.010, 0.000, 0.000,
                                0.119, 0.274, 0.011, 0.000,
                                0.007, 0.092, 0.149, 0.005,
                                0.005, 0.019, 0.057, 0.077
                                ), nrow=4, ncol=4
                              ) #30Sep13

  age.mixing.all.stat <- age.mixing.all.stat*n.edges

  ##       [,1]    [,2]    [,3]    [,4]
  ## [1,] 456.72 308.805  18.165  12.975
  ## [2,]  25.95 711.030 238.740  49.305
  ## [3,]   0.00  28.545 386.655 147.915
  ## [4,]   0.00   0.000  12.975 199.815

  age.mix.spec.diag <- c(age.mixing.all.stat[c(1, 6, 11, 16)])

  age.b1factor <- apply(age.mixing.all.stat, 2, sum) #sum of all male ties (each column)
  age.b1factor.spec <- age.b1factor[3:4]

  age.b2factor <- apply(age.mixing.all.stat, 1, sum) #sum of all female ties (each row)
  age.b2factor.spec <- age.b2factor[3:4]

  target.stats <- c(n.edges,
                    male.deg.edgecount[2:3],
                    female.deg.edgecount[2:3],
                    age.mix.spec.diag,
                    age.b1factor.spec,
                    age.b2factor.spec
                    )

  constraints <- ~bd(maxout=4)

#####################################################
### Initialize Network
#####################################################
  n0 <- network.initialize(N, bipartite=num.male, directed=F)
#####################################################


#####################################################
### Add Atributes
#####################################################

  ## Sex (uniformly distributed -- in accordance with census data)
     n0 %v% "sex" <- rep(c("male", "female"),
                          c(num.male, num.female)
                          )
     male.id <- which(n0%v%"sex"=="male")
     female.id <- which(n0%v%"sex"=="female")

  ## Age
     prob.for.each.age <- unlist(lapply(age.distribution, function(x) rep(x, 5)))
     prob.for.each.age <- prob.for.each.age[-c(1:3)]
                          ##12Nov13: adjust age structure for
                          ## starting age 18, so delete entries corresponding to ages 
                          ## 15, 16 and 17 in prob.for.each.age
     age <- sample(age.classes, size=N, replace=TRUE, prob=prob.for.each.age)
     n0 %v% "age" <- age

  ## checks for age
     length(intersect(which(age>=15), which(age<20)))/N # first age class
     length(intersect(which(age>=18), which(age<20)))/N ==
     length(intersect(which(age>=15), which(age<20)))/N # check if min age has been
                                                     # adjusted to 18.
     length(intersect(which(age>=50), which(age<55)))/N # last age class

     age.cat <- assign.age.cat(age, cutoffs=c(25, 35, 45, 55))
     n0%v%"age.cat" <- age.cat

  ## circumcision status
     circum.status <- rbinom(N/2, 1, circum.rate)
     n0 %v% "circum.status" <- c(circum.status, rep(NA, N/2))

  ## pregnancy status
     set.vertex.attribute(n0, "curr.pregnancy.status", NA, male.id)
     set.vertex.attribute(n0, "curr.pregnancy.status", 0, female.id)
     ## defeault pregnancy status is NA,
     ## overwritten with age specific fertility rates for women below


      asfr.15to19 <- 0.000220822 ## Age-Specific Fertility Rate
      asfr.20to24 <- 0.000380822 ## Tailor each to ZA, UG
      asfr.25to29 <- 0.000388493 ## 12Nov13: See J25-31 in 
      asfr.30to34 <- 0.000289315 ## "fertility rate calculations" in dropbox folder
      asfr.35to39 <- 0.000184658
      asfr.40to44 <- 7.42466E-05
      asfr.45to49 <- 2.41096E-05

      asfr.15to19 <- asfr.15to19*size.of.timestep ## 12Nov13: multiply by 14 to adjust from 
      asfr.20to24 <- asfr.20to24*size.of.timestep ## day to timestep
      asfr.25to29 <- asfr.25to29*size.of.timestep
      asfr.30to34 <- asfr.30to34*size.of.timestep
      asfr.35to39 <- asfr.35to39*size.of.timestep
      asfr.40to44 <- asfr.40to44*size.of.timestep
      asfr.45to49 <- asfr.45to49*size.of.timestep

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
      init.inf.status <- rbinom(network.size(n0), 1, init.hiv.prev)
      set.vertex.attribute(n0, "inf.status", init.inf.status)
      inf.status.men <- init.inf.status[male.id]
      inf.status.women <- init.inf.status[female.id]
      init.infected <- which(init.inf.status == 1)
      init.infected.male <- intersect(init.infected, male.id)
      init.infected.female <- intersect(init.infected, female.id)
      set.vertex.attribute(n0, "inf.status", 1, v=init.infected)

      ## ART coverage indicator
         ## so we can control how many individuals will receive ART
         ## will depend on scenario being modeled
         ## set to NA initially
         ## combines coverage, uptake, adherence 
         ## n0%v%"art.covered" <- NA
      reg.art.covered <- rbinom(length(init.infected),
                                1, baseline.art.coverage.rate)
      set.vertex.attribute(n0, "art.covered", reg.art.covered, v=init.infected)

      preg.art.covered <- rbinom(init.infected.female,
                                 1, baseline.preg.art.coverage.rate)
      ## set.vertex.attribute(n0, "preg.art.covered", preg.art.covered,
      ##                      v=init.infected.female)

      set.vertex.attribute(n0, "preg.covered", preg.art.covered, # 1Nov13
                           v=init.infected.female)

      ## Time Since Infection 
         time.since.infection <- rep(NA, N)
         ## 8 Aug 2013: Modify to draw time.since.infection from a distribution
         ## 7 April 2013, changed to avoid problems with dynamics of CD4 counts later
         ## all time since infection for positives set to 0 initially
         ## time.since.infection[init.infected] <- 0
         time.since.infection[init.infected] <- sample(c(0:floor(duration.of.infection/
                                                                 size.of.timestep)),
                                                       size=length(init.infected),
                                                       replace=TRUE)
                                                   
         set.vertex.attribute(n0, "time.since.infection", time.since.infection)

         time.of.infection <- 0 - (n0%v%"time.since.infection")
         set.vertex.attribute(n0, "time.of.infection", time.of.infection)

      ## 26Aug13: Add age at time of of infection --
      ## "age.at.infection"

      ## 23Aug13: Add age at time of of infection --
      ## "age.at.infection"

      ## 22Aug13: Assign age-based duration of infection

         n0%v%"age.at.infection" <- rep(NA, N)
         age.at.infection <- age-(time.since.infection*size.of.timestep/365)
         age.at.infection[which(age.at.infection < 15)] <- 15 # added for consistency
         set.vertex.attribute(n0, "age.at.infection", age[init.infected],
                              v=init.infected)
         summary(n0%v%"age.at.infection")

         n0%v%"dur.inf.by.age" <- rep(NA, N)
         duration.of.inf <- n0%v%"dur.inf.by.age"

         ## for (i in 1:length(duration.of.inf)){
         ##   if (init.inf.status[i] == 1){
         ##     if (age[i] <= 24){
         ##       duration.of.inf[i] <- dur.inf.by.age[1]
         ##     } else if (age[i] > 24 && age[i] <= 34){
         ##       duration.of.inf[i] <- dur.inf.by.age[2]
         ##     } else if (age[i] > 34 && age[i] <= 44){
         ##       duration.of.inf[i] <- dur.inf.by.age[3]
         ##     } else if (age[i] > 44){
         ##       duration.of.inf[i] <- dur.inf.by.age[4]
         ##     }
         ##   }
         ## }

         for (i in 1:length(duration.of.inf)){
           if (init.inf.status[i] == 1){
             if (age.at.infection[i] <= 24){
               duration.of.inf[i] <- given.dur.inf.by.age[1] #11Nov13:
               ## change name to "given.dur.inf.by.age" to avoid errors later.
             } else if (age.at.infection[i] > 24 && age.at.infection[i] <= 34){
               duration.of.inf[i] <- given.dur.inf.by.age[2]
             } else if (age.at.infection[i] > 34 && age.at.infection[i] <= 44){
               duration.of.inf[i] <- given.dur.inf.by.age[3]
             } else if (age.at.infection[i] > 44){
               duration.of.inf[i] <- given.dur.inf.by.age[4]
             }
           }
         }

         set.vertex.attribute(n0, "dur.inf.by.age", duration.of.inf)

        xtabs(~factor(n0%v%"age.at.infection", exclude=NULL)+
               factor(n0%v%"dur.inf.by.age", exclude=NULL)
              )

        xtabs(~factor(n0%v%"age.at.infection", exclude=NULL)+
               factor(n0%v%"time.since.infection", exclude=NULL)
              )

      ## Assign CD4 counts at ART-naive level.
         cd4.count.today.male <- rep(518, num.male)
         cd4.count.today.female <- rep(570, num.female)
         cd4.count.today <- c(cd4.count.today.male, cd4.count.today.female)

         ## Code below taken from function "compute.cd4.count.sexage.acct"
         ## "in compute.cd4.count_d1.R"
         b1.ref <- 23.53
         b2.african <- -0.76
         b3.female <- 1.11
         b4.cd4.ref <- -1.49
         b5.african <- 0.34
         b6.age.15to29 <- 0
         b6.age.30to39 <- -0.1
         b6.age.40to49 <- -0.34
         b6.age.50ormore <- -0.63

         for (i in 1:length(male.id)){ # for male
           if (i %in% init.infected.male){ # we only update CD4 of infecteds
             if (age[i] %/% 10 < 3){ ## need quotient operator
               cd4.count.today[i] <- (b1.ref+b2.african+
                                      (time.since.infection[i]/365*size.of.timestep)*
                                      (b4.cd4.ref+b5.african+b6.age.15to29))^2
             } else if (age[i] %/% 10 == 3){
               cd4.count.today[i] <- (b1.ref+b2.african+
                                      (time.since.infection[i]/365*size.of.timestep)*
                                      (b4.cd4.ref+b5.african+b6.age.30to39))^2
             } else if (age[i] %/% 10 == 4){
               cd4.count.today[i] <- (b1.ref+b2.african+
                                      (time.since.infection[i]/365*size.of.timestep)*
                                      (b4.cd4.ref+b5.african+b6.age.40to49))^2
             } else if (age[i] %/% 10 >= 5){
               cd4.count.today[i] <- (b1.ref+b2.african+
                                      (time.since.infection[i]/365*size.of.timestep)*
                                      (b4.cd4.ref+b5.african+b6.age.50ormore))^2
             } 
           }
         }

         for (i in (min(female.id):max(female.id))){ # for female
           if (i %in% init.infected.female){ # we only update CD4 of infecteds
             if (age[i] %/% 10 < 3){ ## need quotient operator
               cd4.count.today[i] <- (b1.ref+b2.african+b3.female+
                                      (time.since.infection[i]/365*size.of.timestep)*
                                      (b4.cd4.ref+b5.african+b6.age.15to29))^2
             } else if (age[i] %/% 10 == 3){
               cd4.count.today[i] <- (b1.ref+b2.african+b3.female+
                                      (time.since.infection[i]/365*size.of.timestep)*
                                      (b4.cd4.ref+b5.african+b6.age.30to39))^2
             } else if (age[i] %/% 10 == 4){
               cd4.count.today[i] <- (b1.ref+b2.african+b3.female+
                                      (time.since.infection[i]/365*size.of.timestep)*
                                      (b4.cd4.ref+b5.african+b6.age.40to49))^2
             } else if (age[i] %/% 10 >= 5){
               cd4.count.today[i] <- (b1.ref+b2.african+b3.female+
                                      (time.since.infection[i]/365*size.of.timestep)*
                                      (b4.cd4.ref+b5.african+b6.age.50ormore))^2
             } 
           }
         }

         n0%v%"cd4.count.today" <- cd4.count.today

        ## Update ART status for those who are below ART initiation level and covered.
           ## Start by assigning an ART status of NA for uninfected,
           ## 0 for infected
           ## for men and women below ART initiation levels,
           ## if they are ART covered, update ART status.

        art.status <- rep(NA, num.male) # everyone
        art.status[init.infected] <- 0 # initially infected
        reg.art.coverage <- n0%v%"art.covered" # regular ART status

        for (i in 1:length(male.id)){ # update ART status for covered men
          if ( (cd4.count.today[i] <= baseline.cd4.at.art.initiation.men) &&
              (reg.art.coverage[i] == 1)
              ){
            art.status[i] <- 1
          }
        }

        for (i in (min(female.id)):max(female.id)){ # update ART status for covered women
          if ( (cd4.count.today[i] <= baseline.cd4.at.art.initiation.men) &&
              (reg.art.coverage[i] == 1)
              ){
            art.status[i] <- 1
          }
        }

       table(art.status, useNA="always")
       n0%v%"art.status" <- art.status

        ## Update ART time since art initiation
           ## Start by assigning "time.since.art.initiation" of
           ## NA for not on ART,
           ## calculate "time.since.art.initiation" for those on ART
       time.since.art.initiation <- rep(NA, N)
       on.art <- which(art.status == 1)

       if (length(on.art) > 0){ # only update time since infection for those on ART
         for (i in 1:length(on.art)){

           if  (on.art[i] %in% male.id){
             time.since.art.initiation[on.art[i]] <-
               floor(
               (baseline.cd4.at.art.initiation.men- 
                cd4.count.today[on.art[i]])/
                  (per.day.cd4.recovery*size.of.timestep)
                     )
           }
           if  (on.art[i] %in% female.id){
             time.since.art.initiation[on.art[i]] <-
               floor(
               (baseline.cd4.at.art.initiation.women- 
                cd4.count.today[on.art[i]])/
                  (per.day.cd4.recovery*size.of.timestep)
                     )
                   
           }
         }
       }

       n0%v%"time.since.art.initiation" <- time.since.art.initiation

        ## Update ART type (for those on ART)
           n0%v%"art.type" <- NA
           set.vertex.attribute(n0, "art.type", 1, on.art)
                                        # all on ART are initially assigned reg.ART
                                
        ## Update CD4 counts for those on ART
           ## We have list of those on ART
           ## and how long they have been on ART
           ## multiply by CD4 recovery in every time step, and
           ## we get their updated CD4 count.

       if (length(on.art) > 0){ # only update art-naive CD4 counts for those on ART
         for (i in 1:length(on.art)){
           if  (on.art[i] %in% male.id){
             cd4.count.today[on.art[i]] <- baseline.cd4.at.art.initiation.men
           }
           if  (on.art[i] %in% female.id){
             cd4.count.today[on.art[i]] <- baseline.cd4.at.art.initiation.women
           }
         }
       }

      n0%v%"cd4.count.today" <- cd4.count.today
         
      ## Now compute viral loads -- "viral.load.today"
 
         ## First compute viral load in all infecteds as ART-naive individuals
            ## At time step 0, everyone has viral load 0.

      viral.load.today <- rep(0, N)

      ## Code Taken from function "compute.vl.mp.ar4" in "compute.viral.load_d3.R"

      for (i in 1:N){

        if(i %in% init.infected){

          if ( (!is.na(time.since.infection[i])) && 
              (time.since.infection[i] <= time.infection.to.peak.viral.load)
                ){

             viral.load.today[i] <- mean(c(0, peak.viral.load))

           }  else if ( (time.since.infection[i] %in%
                        (time.infection.to.peak.viral.load+1):
                        (time.infection.to.viral.set.point))
                      )  {
               
               viral.load.today[i] <- peak.viral.load -
                                           ((peak.viral.load - set.point.viral.load) *
                                           (time.since.infection[i] -
                                            time.infection.to.peak.viral.load)/
                                           (time.infection.to.viral.set.point -
                                            time.infection.to.peak.viral.load))*
                                           1/2 # so we arrive at the midpoint
               
           } else if ( (time.since.infection[i] %in%
                        (time.infection.to.viral.set.point+1):
                        (time.infection.to.late.stage))
                      ) {
             viral.load.today[i] <- set.point.viral.load

           }  else if ( (!is.na(time.since.infection[i])) &&
                      (time.since.infection[i] > time.infection.to.late.stage)
                    ){
           
             viral.load.today[i] <- set.point.viral.load +
               ((late.stage.viral.load - set.point.viral.load)*
                 (time.since.infection[i] - time.infection.to.late.stage)/
                   ( (dur.inf-1) - time.infection.to.late.stage ))*1/2
             # so we arrive at midpoint

           }
        }
      }

        ## Then decrease viral load in those who are on ART
           ## For those on ART we will first have to assign
           ## "vl.art.traj.slope"

           vl.art.traj.slope <- rep(NA, N)

           if (length(on.art) > 0){ # only update art-naive viral loads for those on ART
             for (i in 1:length(on.art)){

               vl.art.traj.slope[on.art[i]] <- 
                 abs((undetectable.vl - viral.load.today[on.art[i]])/time.to.full.supp)

               viral.load.today[on.art[i]] <- 
                 viral.load.today[on.art[i]] -
                   vl.art.traj.slope[on.art[i]]*time.since.art.initiation[on.art[i]]

             }
           }

       n0%v%"viral.load.today" <- viral.load.today
       n0%v%"vl.art.traj.slope" <- rep(NA, N)

    ## Stage of Infection
       ## Classification necessary for deciding infectivity
       ## for chronically infected
       stage.of.infection <- classify.stage.numeric.rewrite(time.since.infection)
       n0%v%"stage.of.infection" <- stage.of.infection

    ## Infectivity today "infectivity.today"
       n0%v%"infectivity.today" <- NA #may not need to be assigned at this stage



## 1 Nov 2013: Recode "male" to 0 and "female" to 1
called.male <- which(n0%v%"sex" == "male")
called.female <- which(n0%v%"sex" == "female")

## 1 Nov 2013
n0 <- set.vertex.attribute(n0, "sex", 0, v=called.male)
n0 <- set.vertex.attribute(n0, "sex", 1, v=called.female)


      #################################################




#####################################################


#####################################################
### Fit cross-sectional model to formation,
### and apply Carnegie correction
#####################################################

  fit <- ergm(formation.n0,
              target.stats=target.stats,
              constraints=constraints,
              maxit=500,
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

#####################################################
### Diagnostics
#####################################################

       save.image(file="estimation_za_phase2_entr18_d8d.RData")
                                        # max degree of men and women is 5
    ## save.image(file="estimation_za_d8d_men5_women4.RData")
                                        # max deg of men is 5 and women is 4
