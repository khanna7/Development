#####################################################
### Progression of versions
#####################################################

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

source("update.vital.dynamics_d9.R")
##load("estimation_uganda_d5.RData")
##load(file="hetdeg.diag.sim.durstep.RData")

#####################################################

#####################################################
### Check "assign.mortality" functions
#####################################################

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


  ## for inf.status==0

     ## males
assign.mortality.male(male.curr.age=15,
                                      asmr.male=asmr.male,
                                      male.inf.status=0,
                                      male.cd4.today=518,
                     male.art.status=0
                                      )

assign.mortality.male(male.curr.age=25,
                                      asmr.male=asmr.male,
                                      male.inf.status=0,
                                      male.cd4.today=518,
                     male.art.status=0
                                      )


assign.mortality.male(male.curr.age=35,
                                      asmr.male=asmr.male,
                                      male.inf.status=0,
                                      male.cd4.today=350,
                                      male.art.status=0
                                      )


expect_equal(assign.mortality.male(male.curr.age=35,
                                      asmr.male=asmr.male,
                                      male.inf.status=0,
                                      male.cd4.today=350,
                                      male.art.status=0
                                      ),
             assign.mortality.male(male.curr.age=35,
                                      asmr.male=asmr.male,
                                      male.inf.status=0,
                                      male.cd4.today=350,
                                      male.art.status=NA
                                      ),
             )

    ## females
assign.mortality.female(female.curr.age=15,
                                      asmr.female=asmr.female,
                                      female.inf.status=0,
                                      female.cd4.today=518,
                     female.art.status=0
                                      )

assign.mortality.female(female.curr.age=25,
                                      asmr.female=asmr.female,
                                      female.inf.status=0,
                                      female.cd4.today=518,
                     female.art.status=0
                                      )


assign.mortality.female(female.curr.age=35,
                                      asmr.female=asmr.female,
                                      female.inf.status=0,
                                      female.cd4.today=350,
                                      female.art.status=0
                                      )


expect_equal(assign.mortality.female(female.curr.age=35,
                                      asmr.female=asmr.female,
                                      female.inf.status=0,
                                      female.cd4.today=350,
                                      female.art.status=0
                                      ),
             assign.mortality.female(female.curr.age=35,
                                      asmr.female=asmr.female,
                                      female.inf.status=0,
                                      female.cd4.today=350,
                                      female.art.status=NA
                                      ),
             )



  ## for inf.status=1
     ## males
     assign.mortality.male(male.curr.age=15,
                                      asmr.male=asmr.male,
                                      male.inf.status=1,
                                      male.cd4.today=518,
                           male.art.status=NA
                                      )

     assign.mortality.male(male.curr.age=15,
                                      asmr.male=asmr.male,
                                      male.inf.status=1,
                           male.cd4.today=518,
                           male.art.status=1
                                      )


     assign.mortality.male(male.curr.age=15,
                                      asmr.male=asmr.male,
                                      male.inf.status=1,
                                      male.cd4.today=350,
                           male.art.status=1
                                      )

     assign.mortality.male(male.curr.age=15,
                                      asmr.male=asmr.male,
                                      male.inf.status=1,
                                      male.cd4.today=200,
                           male.art.status=1
                                      )

     assign.mortality.male(male.curr.age=15,
                                      asmr.male=asmr.male,
                                      male.inf.status=1,
                                      male.cd4.today=99,
                           male.art.status=1
                                      )

     ## females
     assign.mortality.female(female.curr.age=15,
                                      asmr.female=asmr.female,
                                      female.inf.status=1,
                                      female.cd4.today=518,
                           female.art.status=NA
                                      )

     assign.mortality.female(female.curr.age=15,
                                      asmr.female=asmr.female,
                                      female.inf.status=1,
                           female.cd4.today=518,
                           female.art.status=1
                                      )


     assign.mortality.female(female.curr.age=15,
                                      asmr.female=asmr.female,
                                      female.inf.status=1,
                                      female.cd4.today=350,
                           female.art.status=1
                                      )

     assign.mortality.female(female.curr.age=15,
                                      asmr.female=asmr.female,
                                      female.inf.status=1,
                                      female.cd4.today=200,
                           female.art.status=1
                                      )

     assign.mortality.female(female.curr.age=15,
                                      asmr.female=asmr.female,
                                      female.inf.status=1,
                                      female.cd4.today=99,
                           female.art.status=1
                                      )

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

## Needed attributes

  fit.fake.net%v%"inf.status" <- rep(c(1,0), 5)
  fit.fake.net%v%"art.status" <- 0
  fit.fake.net%v%"cd4.count.today" <- rep(c(100, 518), 5)
  fit.fake.net%v%"age" <- 30
  fit.fake.net%v%"time.since.infection" <- rep(c(4, NA), 5)
  fit.fake.net%v%"dur.inf.by.age" <- rep(c(5, NA), 5) #22Aug13
  
## male and female attributes
  ## differentiate men and women
  male.id.curr <- nwmodes(fit.fake.net, 1)
  female.id.curr <- nwmodes(fit.fake.net, 2)

  ## mark out separate attributes by sex
  male.inf.status <- get.vertex.attribute(fit.fake.net,
                                          "inf.status")[male.id.curr]
  female.inf.status <- get.vertex.attribute(fit.fake.net,
                                            "inf.status")[female.id.curr]
  male.cd4.today <- get.vertex.attribute(fit.fake.net,
                                         "cd4.count.today")[male.id.curr]
  female.cd4.today <- get.vertex.attribute(fit.fake.net,
                                           "cd4.count.today")[female.id.curr]
  male.art.status <- get.vertex.attribute(fit.fake.net,
                                          "art.status")[male.id.curr]
  female.art.status <- get.vertex.attribute(fit.fake.net,
                                            "art.status")[female.id.curr]         

## needed parameters
   verbose <- TRUE
   dur.inf <- 5
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

   ## these parameters are specified in the simulation file
   ## they are populated at [time] (eg popsize[time]) in "update.vital.dynamics"
   ## maybe they should be set up as lists and the first element should contain
   ## info for the starting network
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


## simulate fake network
   fit.fake.net <- activate.vertices(fit.fake.net, onset=1, terminus=Inf)
   fit.fake.net <- activate.edges(fit.fake.net, onset=1, terminus=Inf)

#####################################################

#####################################################
### test removal on account of AIDS
#####################################################

##debug(update.vital.dynamics)

 for (time in 2:4){
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
                                              #dur.inf=dur.inf,
                                              ##total lifespan for infected individuals
                                              asmr.male=asmr.male,
                                              asmr.female=asmr.female,
                                              phi=phi,
                                              size.of.timestep=size.of.timestep,
                                              prop.f=prop.f
                                              )
  cat("\n")

}




#####################################################

#####################################################
### test removal on account of natural mortality
#####################################################

  fit.fake.net <- fit.fake$network

## Needed attributes

  fit.fake.net%v%"inf.status" <- 0
  fit.fake.net%v%"art.status" <- NA
  fit.fake.net%v%"cd4.count.today" <- 700
  fit.fake.net%v%"age" <- 30
  fit.fake.net%v%"time.since.infection" <- NA
  
## male and female attributes
  ## differentiate men and women
  male.id.curr <- nwmodes(fit.fake.net, 1)
  female.id.curr <- nwmodes(fit.fake.net, 2)

  ## mark out separate attributes by sex
  male.inf.status <- get.vertex.attribute(fit.fake.net,
                                          "inf.status")[male.id.curr]
  female.inf.status <- get.vertex.attribute(fit.fake.net,
                                            "inf.status")[female.id.curr]
  male.cd4.today <- get.vertex.attribute(fit.fake.net,
                                         "cd4.count.today")[male.id.curr]
  female.cd4.today <- get.vertex.attribute(fit.fake.net,
                                           "cd4.count.today")[female.id.curr]
  male.art.status <- get.vertex.attribute(fit.fake.net,
                                          "art.status")[male.id.curr]
  female.art.status <- get.vertex.attribute(fit.fake.net,
                                            "art.status")[female.id.curr]         

## needed parameters
   dur.inf <- 1e3
   size.of.timestep <- 14
   asmr.male <- asmr.male
   asmr.female <- asmr.female

## simulate fake network
   fit.fake.net <- activate.vertices(fit.fake.net, onset=1, terminus=Inf)
   fit.fake.net <- activate.edges(fit.fake.net, onset=1, terminus=Inf)

## time loop
for (time in 2:2){
  fit.fake.net <- simulate.networkDynamic(fit.fake.net,
                                              formation=formation,
                                              dissolution=dissolution,
                                              coef.form=theta.form,
                                              coef.diss=theta.diss,
                                              constraints=constraints,
                                              start.time=2,
                                              time.slices=1,
                                        # 3Jun13: discussion with sam --
                                        # simulate over 1 time step every
                                        # time you hit the timestep in the loop
                                        # (as per the code in Steve's summer 2012 course)
                      control=control.simulate.networkDynamic(MCMC.burnin=10000),
                                              maxchanges=1e6
                                              )

  fit.fake.net <- update.vital.dynamics(fit.fake.net, verbose=TRUE,
                                              dur.inf=dur.inf,
                                              ##total lifespan for infected individuals
                                              asmr.male=asmr.male,
                                              asmr.female=asmr.female,
                                              phi=phi,
                                              size.of.timestep=size.of.timestep,
                                              prop.f=prop.f
                                              )
  cat("\n")

}

 ## Expect error here because all actors should have non-AIDS deaths --
 ## we get that result as expected.

#####################################################

#####################################################
### test removal on account of age
#####################################################

  fit.fake.net <- fit.fake$network

## Needed attributes

  fit.fake.net%v%"inf.status" <- 0
  fit.fake.net%v%"art.status" <- NA
  fit.fake.net%v%"cd4.count.today" <- 700
  fit.fake.net%v%"age" <- rep(c(53, 55), 5)
  fit.fake.net%v%"time.since.infection" <- NA
  
## male and female attributes
  ## differentiate men and women
  male.id.curr <- nwmodes(fit.fake.net, 1)
  female.id.curr <- nwmodes(fit.fake.net, 2)

  ## mark out separate attributes by sex
  male.inf.status <- get.vertex.attribute(fit.fake.net,
                                          "inf.status")[male.id.curr]
  female.inf.status <- get.vertex.attribute(fit.fake.net,
                                            "inf.status")[female.id.curr]
  male.cd4.today <- get.vertex.attribute(fit.fake.net,
                                         "cd4.count.today")[male.id.curr]
  female.cd4.today <- get.vertex.attribute(fit.fake.net,
                                           "cd4.count.today")[female.id.curr]
  male.art.status <- get.vertex.attribute(fit.fake.net,
                                          "art.status")[male.id.curr]
  female.art.status <- get.vertex.attribute(fit.fake.net,
                                            "art.status")[female.id.curr]         

## needed parameters
   dur.inf <- 1e3
   size.of.timestep <- 14
   asmr.male <- asmr.perperson.pertimestep
   asmr.female <- asmr.perperson.pertimestep

## simulate fake network
   fit.fake.net <- activate.vertices(fit.fake.net, onset=1, terminus=Inf)
   fit.fake.net <- activate.edges(fit.fake.net, onset=1, terminus=Inf)

## time loop
for (time in 2:2){
  fit.fake.net <- simulate.networkDynamic(fit.fake.net,
                                              formation=formation,
                                              dissolution=dissolution,
                                              coef.form=theta.form,
                                              coef.diss=theta.diss,
                                              constraints=constraints,
                                              start.time=2,
                                              time.slices=1,
                                        # 3Jun13: discussion with sam --
                                        # simulate over 1 time step every
                                        # time you hit the timestep in the loop
                                        # (as per the code in Steve's summer 2012 course)
                      control=control.simulate.networkDynamic(MCMC.burnin=10000),
                                              maxchanges=1e6
                                              )

  fit.fake.net <- update.vital.dynamics(fit.fake.net, verbose=TRUE,
                                              dur.inf=dur.inf,
                                              ##total lifespan for infected individuals
                                              asmr.male=asmr.male,
                                              asmr.female=asmr.female,
                                              phi=phi,
                                              size.of.timestep=size.of.timestep,
                                              prop.f=prop.f
                                              )
  cat("\n")

}

#####################################################
### test removal on account of CD4 ang age
#####################################################

  fit.fake.net <- fit.fake$network

## Needed attributes

  fit.fake.net%v%"inf.status" <- 1
  fit.fake.net%v%"art.status" <- 0
  fit.fake.net%v%"cd4.count.today" <- 20
  fit.fake.net%v%"age" <- 30
  fit.fake.net%v%"time.since.infection" <- 10
  
## male and female attributes
  ## differentiate men and women
  male.id.curr <- nwmodes(fit.fake.net, 1)
  female.id.curr <- nwmodes(fit.fake.net, 2)

  ## mark out separate attributes by sex
  male.inf.status <- get.vertex.attribute(fit.fake.net,
                                          "inf.status")[male.id.curr]
  female.inf.status <- get.vertex.attribute(fit.fake.net,
                                            "inf.status")[female.id.curr]
  male.cd4.today <- get.vertex.attribute(fit.fake.net,
                                         "cd4.count.today")[male.id.curr]
  female.cd4.today <- get.vertex.attribute(fit.fake.net,
                                           "cd4.count.today")[female.id.curr]
  male.art.status <- get.vertex.attribute(fit.fake.net,
                                          "art.status")[male.id.curr]
  female.art.status <- get.vertex.attribute(fit.fake.net,
                                            "art.status")[female.id.curr]         

## needed parameters
   dur.inf <- 1e3
   size.of.timestep <- 14
   asmr.male <- asmr.perperson.pertimestep
   asmr.female <- asmr.perperson.pertimestep

## simulate fake network
   fit.fake.net <- activate.vertices(fit.fake.net, onset=1, terminus=Inf)
   fit.fake.net <- activate.edges(fit.fake.net, onset=1, terminus=Inf)

## time loop
for (time in 2:10){
  fit.fake.net <- simulate.networkDynamic(fit.fake.net,
                                              formation=formation,
                                              dissolution=dissolution,
                                              coef.form=theta.form,
                                              coef.diss=theta.diss,
                                              constraints=constraints,
                                              start.time=2,
                                              time.slices=1,
                                        # 3Jun13: discussion with sam --
                                        # simulate over 1 time step every
                                        # time you hit the timestep in the loop
                                        # (as per the code in Steve's summer 2012 course)
                      control=control.simulate.networkDynamic(MCMC.burnin=10000),
                                              maxchanges=1e6
                                              )

  fit.fake.net <- update.vital.dynamics(fit.fake.net, verbose=TRUE,
                                              dur.inf=dur.inf,
                                              ##total lifespan for infected individuals
                                              asmr.male=asmr.male,
                                              asmr.female=asmr.female,
                                              phi=phi,
                                              size.of.timestep=size.of.timestep,
                                              prop.f=prop.f
                                              )
  cat("\n")

}

#####################################################
### test number of births
#####################################################

  fit.fake.net <- fit.fake$network

## Needed attributes

  fit.fake.net%v%"inf.status" <- 0
  fit.fake.net%v%"art.status" <- 0
  fit.fake.net%v%"cd4.count.today" <- 20
  fit.fake.net%v%"age" <- 30
  fit.fake.net%v%"time.since.infection" <- 10
  
## male and female attributes
  ## differentiate men and women
  male.id.curr <- nwmodes(fit.fake.net, 1)
  female.id.curr <- nwmodes(fit.fake.net, 2)

  ## mark out separate attributes by sex
  male.inf.status <- get.vertex.attribute(fit.fake.net,
                                          "inf.status")[male.id.curr]
  female.inf.status <- get.vertex.attribute(fit.fake.net,
                                            "inf.status")[female.id.curr]
  male.cd4.today <- get.vertex.attribute(fit.fake.net,
                                         "cd4.count.today")[male.id.curr]
  female.cd4.today <- get.vertex.attribute(fit.fake.net,
                                           "cd4.count.today")[female.id.curr]
  male.art.status <- get.vertex.attribute(fit.fake.net,
                                          "art.status")[male.id.curr]
  female.art.status <- get.vertex.attribute(fit.fake.net,
                                            "art.status")[female.id.curr]         

## needed parameters
   dur.inf <- 1e3
   size.of.timestep <- 14
   asmr.male <- rep(0, 8)
   asmr.female <- rep(0, 8)
   phi <- 1

## simulate fake network
   fit.fake.net <- activate.vertices(fit.fake.net, onset=1, terminus=Inf)
   fit.fake.net <- activate.edges(fit.fake.net, onset=1, terminus=Inf)

## time loop
for (time in 2:2){
  fit.fake.net <- simulate.networkDynamic(fit.fake.net,
                                              formation=formation,
                                              dissolution=dissolution,
                                              coef.form=theta.form,
                                              coef.diss=theta.diss,
                                              constraints=constraints,
                                              start.time=2,
                                              time.slices=1,
                                        # 3Jun13: discussion with sam --
                                        # simulate over 1 time step every
                                        # time you hit the timestep in the loop
                                        # (as per the code in Steve's summer 2012 course)
                      control=control.simulate.networkDynamic(MCMC.burnin=10000),
                                              maxchanges=1e6
                                              )

  fit.fake.net <- update.vital.dynamics(fit.fake.net, verbose=TRUE,
                                              dur.inf=dur.inf,
                                              ##total lifespan for infected individuals
                                              asmr.male=asmr.male,
                                              asmr.female=asmr.female,
                                              phi=phi,
                                              size.of.timestep=size.of.timestep,
                                              prop.f=prop.f
                                              )
  cat("\n")

}

## check attributes of arrivals
   att.list <- list.vertex.attributes(fit.fake.net)
   get.vertex.attribute(fit.fake.net[2])
   fit.fake.net%v%"age"
   fit.fake.net%v%"art.status"
   fit.fake.net%v%"cd4.count.today"
   fit.fake.net%v%"curr.pregnancy.status"
   fit.fake.net%v%"inf.status"
   fit.fake.net%v%"time.since.art.initiation"
   fit.fake.net%v%"vertex.names"

#####################################################

#####################################################
### check updating of age
#####################################################

phi <- 0.1

for (time in 2:10){
  fit.fake.net <- simulate.networkDynamic(fit.fake.net,
                                              formation=formation,
                                              dissolution=dissolution,
                                              coef.form=theta.form,
                                              coef.diss=theta.diss,
                                              constraints=constraints,
                                              start.time=2,
                                              time.slices=1,
                                        # 3Jun13: discussion with sam --
                                        # simulate over 1 time step every
                                        # time you hit the timestep in the loop
                                        # (as per the code in Steve's summer 2012 course)
                      control=control.simulate.networkDynamic(MCMC.burnin=10000),
                                              maxchanges=1e6
                                              )

  fit.fake.net <- update.vital.dynamics(fit.fake.net, verbose=TRUE,
                                              dur.inf=dur.inf,
                                              ##total lifespan for infected individuals
                                              asmr.male=asmr.male,
                                              asmr.female=asmr.female,
                                              phi=phi,
                                              size.of.timestep=size.of.timestep,
                                              prop.f=prop.f
                                              )
  cat(fit.fake.net%v%"age", "\n")

}

#####################################################
