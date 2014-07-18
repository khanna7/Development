#####################################################
### Progression of versions
#####################################################

## 26 Aug 2013: Test deaths by age.

## 22 Aug 2013: Unit test latest version of vital dynamics,
## with age-based duration of infection.
## Also remember, upgrade to "statnet_3.1-0"
## is now complete -- take out source("network.dynamic") and
## change arguments to simulate function.

## 9 Jul 2013: Tests seem to pass. Some points to consider:
## a. Should "vertex.names" attribute be updated for arrivals?
## b. Currently joint mortality on account of CD4 and age is hard-coded.
## Change that.
## c. Well placed now to figure out point about extracting network
## at a particular time step in every function
## d. Do we need lists for popsize, prev, inci or is the current coding
## fine?

## 8 Jul 2013: Test "update.vital.dynamics_d6.R" functions

#####################################################

rm(list=ls())

#####################################################
### Top Matter
#####################################################

library(ergm)
library(tergm)
library(testthat)
## source("../simulate.networkDynamic.R")
source("common.functions_d8.R")

source("update.vital.dynamics_d10.R")
##load("estimation_uganda_d5.RData")
##load(file="hetdeg.diag.sim.durstep.RData")

#####################################################

#####################################################
### Check "assign.mortality" functions
#####################################################

  size.of.timestep <- 14
  phi <- 0

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

  ## intervals are left-closed


#####################################################

#####################################################
### Check other portions of this file. 
#####################################################

## Make fake network
  num.male <- 5
  num.female <- 5 
  N <- num.male+num.female

  formation <- ~edges
  formation.net <- update.formula(formation,
                                  net~.)
  duration <- 1
  dissolution <- ~offset(edges)
  theta.diss <- log(duration-1)
  target.stats <- 5
  constraints <- ~.

  net <- network.initialize(N, bipartite=num.male,
                            directed=FALSE)

  fit.fake <- ergm(formation.net,
                   target.stats=target.stats,
                   constraints=constraints)
  theta.form <- fit.fake$coef 
  theta.form[1] <- theta.form[1] - theta.diss


  fit.fake.net <- fit.fake$network

  male.id.curr <- nwmodes(fit.fake.net, 1)
  female.id.curr <- nwmodes(fit.fake.net, 2)

  prop.f <- length(female.id.curr)/(length(male.id.curr)+length(female.id.curr))   
  popsize <- network.size(fit.fake.net) 
  popsize.m <- fit.fake.net %n% 'bipartite' ## ASK: opposite to what steve has 
  popsize.f <- popsize - popsize.m
  prop.m <- popsize.m/popsize
  prop.f <- 1-prop.m
  prev.i <- mean(fit.fake.net %v% 'inf.status')
  prev.i.f <- mean((fit.fake.net %v% 'inf.status')[1:popsize.m])
     ## ASK: again opp. to what steve has
  prev.i.m <- mean((fit.fake.net %v% 'inf.status')[(popsize.m+1):popsize])
  inci <- inci.f <- inci.m <- NA
  num.deaths.aids <- num.deaths.natural <- num.births <- NA


#####################################################
### 26Aug13: Test removal on account of age
#####################################################

##debug(update.vital.dynamics)
  fit.fake.net <- fit.fake$network

## Needed attributes

  fit.fake.net%v%"inf.status" <- 0
  fit.fake.net%v%"art.status" <- NA
  fit.fake.net%v%"cd4.count.today" <- c(rep(518, 5), rep(570, 5))
  fit.fake.net%v%"age" <- sample(c(50:55), size=N, replace=TRUE)
  fit.fake.net%v%"time.since.infection" <- NA
  fit.fake.net%v%"dur.inf.by.age" <- NA

  fit.fake.net <- activate.vertices(fit.fake.net, onset=1, terminus=Inf)
  fit.fake.net <- activate.edges(fit.fake.net, onset=1, terminus=Inf)

  date <- "26Aug13_test_death_by_age_"

  max.survival <- 55

 for (time in 2:30){

  cat("Completing timestep", time, "\n")
  
  fit.fake.net <- simulate(fit.fake.net,
                           formation=formation,
                           dissolution=dissolution,
                           coef.form=theta.form,
                           coef.diss=theta.diss,
                           constraints=constraints,
                           # start.time=2,
                           time.start=time,
                           time.slices=1,
                                        # 3Jun13: discussion with sam --
                                        # simulate over 1 time step every
                                        # time you hit the timestep in the loop
                                        # (as per the code in Steve's summer 2012 course)
                           
                           control=control.simulate.network(MCMC.burnin=10000)
                           )

  fit.fake.net <- update.vital.dynamics(fit.fake.net, verbose=TRUE,
                                        max.survival=max.survival,
                                        #dur.inf=dur.inf,
                                        ##total lifespan for infected individuals
                                        asmr.male=asmr.male,
                                        asmr.female=asmr.female,
                                        phi=phi,
                                        size.of.timestep=size.of.timestep,
                                        prop.f=prop.f
                                        )

  ## fit.fake.net%v%age <- (fit.fake.net%v%age)+(size.of.timestep/365)
  cat("\n")

}


fit.fake.net%v%"age"

fit.fake.net.30 <- network.extract(fit.fake.net, at=30)
fit.fake.net.30%v%"age"

## looking at attribute values of previous timesteps is useless.

#####################################################

