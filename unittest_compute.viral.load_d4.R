#####################################################
### Progression of versions
#####################################################

## 22 Aug 2013: To test changes on account of age-based duration of infection
## at time of infection.

## a. Remember to change in accordance with "statnet_3.0."

## Thinking so far (formal test not written yet):
## Time from infection to late stage is 134 timesteps
## and duration of infection for oldest group is 146.
## So the denominator of the viral load computation will not be negative:
## viral.load.today[i] <- viral.load.today[i] + #22Aug13
##                      (late.stage.viral.load - set.point.viral.load)/
##                        (dur.inf.by.age[i] - time.infection.to.late.stage)


## 26 Jul 2013: There are 3 groups with diff. viral load
## trajectories: 
## a. Preg. women on scART.
## b. Preg. women who have stopped scART.
## c. Everyone else.

## 10 Jul 2013: Test viral load functions using 
## fake network objects.
#####################################################

#####################################################
### Top Matter
#####################################################

rm(list=ls())

library(ergm)
library(tergm)
library(testthat)
## source("../simulate.networkDynamic.R")
source("common.functions_d8.R")

## source("compute.viral.load_d1.R")
## source("compute.viral.load_d3.R")
source("compute.viral.load_d4.R") 

#####################################################
## Make fake network
#####################################################

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

## needed parameters
size.of.timestep=14
#time.infection.to.peak.viral.load <- floor(14/size.of.timestep)
time.infection.to.peak.viral.load <- floor(5)
peak.viral.load <- 6.17
#time.infection.to.viral.set.point <- floor(121/size.of.timestep)
time.infection.to.viral.set.point <- 10
set.point.viral.load <- 4.2
#time.infection.to.late.stage <- floor(1877/size.of.timestep)
time.infection.to.late.stage <- floor(15)
#dur.inf <- 3300/size.of.timestep
## dur.inf <- 27
late.stage.viral.load <- 5.05 ## (max?)
time.to.full.supp <- 4*30/size.of.timestep
undetectable.vl <- log(50, base=10)
optA.sc.art.vl.perstep.dec <- 1.1/((40-23)*7)*size.of.timestep
sc.art.postcess.ret.bl=12
  

##optA.sc.art.vl.perstep.inc #5Jul13


## Needed attributes
      fit.fake.net%v%"inf.status" <- 1
      fit.fake.net%v%"vl.art.traj.slope" <- 0.1
      fit.fake.net%v%"time.since.infection" <- 0
      fit.fake.net%v%"art.status" <- 0
      fit.fake.net%v%"art.type" <- NA
      fit.fake.net%v%"time.since.art.initiation" <- NA
      fit.fake.net%v%"viral.load.today" <- 0
      fit.fake.net%v%"age" <- 30
  
## simulate fake network
   fit.fake.net <- activate.vertices(fit.fake.net, onset=1, terminus=Inf)
   fit.fake.net <- activate.edges(fit.fake.net, onset=1, terminus=Inf)

   cat("Number of edges is", network.edgecount(fit.fake.net), "\n")

#####################################################

#####################################################
### test increase in viral load for just-infected
### individuals
#####################################################

##debug(update.vital.dynamics)

for (time in 2:20){
  cat("Completing timestep ", time, "\n")
  
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

    cat("After simulate.networkDynamic, number of edges is",
      network.edgecount(fit.fake.net), "\n")
  
  fit.fake.net<- compute.vl.mp.art4(fit.fake.net, verbose=TRUE,
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
                                    time.to.full.supp,
                                    undetectable.vl=
                                    undetectable.vl,
                                    size.of.timestep=
                                    size.of.timestep,
                                    optA.sc.art.vl.perstep.dec=
                                    optA.sc.art.vl.perstep.dec,
                                    optA.sc.art.vl.perstep.inc=
                                    optA.sc.art.vl.perstep.inc
                                    )

                                    cat("Viral loads are ",
                                        fit.fake.net%v%"viral.load.today",
                                        "\n"
                                        )
  
      fit.fake.net%v%"time.since.infection" <- (fit.fake.net%v%"time.since.infection")+1
      fit.fake.net%v%"time.since.art.initiation" <- (fit.fake.net%v%"time.since.art.initiation")+1
      fit.fake.net%v%"age" <- (fit.fake.net%v%"age")+size.of.timestep/365

  cat("\n")


}

 ### check all specified attributes
      fit.fake.net%v%"viral.load.today"

      fit.fake.net%v%"inf.status"
      fit.fake.net%v%"art.status"
      fit.fake.net%v%"art.type"
      fit.fake.net%v%"time.since.infection"
      fit.fake.net%v%"time.since.art.initiation"
      fit.fake.net%v%"age"

#####################################################

#####################################################
### Check what happens when everyone is put on ART
#####################################################

      time.to.full.supp <- 10
    
      fit.fake.net <- fit.fake$network

      fit.fake.net%v%"inf.status" <- 1
      fit.fake.net%v%"vl.art.traj.slope" <- 0.1
      fit.fake.net%v%"time.since.infection" <- 0
      fit.fake.net%v%"art.status" <- 0
      fit.fake.net%v%"art.type" <- NA
      fit.fake.net%v%"time.since.art.initiation" <- NA
      fit.fake.net%v%"viral.load.today" <- 0
      fit.fake.net%v%"age" <- 30


## simulate fake network
  fit.fake.net <- activate.vertices(fit.fake.net, onset=1, terminus=Inf)
  fit.fake.net <- activate.edges(fit.fake.net, onset=1, terminus=Inf)

##undebug(compute.vl.mp.art4)

for (time in 2:30){
  cat("Completing timestep ", time, "\n")
  
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

  fit.fake.net<- compute.vl.mp.art4(fit.fake.net, verbose=TRUE,
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
                                    time.to.full.supp,
                                    undetectable.vl=
                                    undetectable.vl,
                                    size.of.timestep=
                                    size.of.timestep,
                                    optA.sc.art.vl.perstep.dec=
                                    optA.sc.art.vl.perstep.dec,
                                    optA.sc.art.vl.perstep.inc=
                                    optA.sc.art.vl.perstep.inc
                                    )

  cat("Viral loads are ",
      fit.fake.net%v%"viral.load.today",
      "\n")

    if (time == 3){
        fit.fake.net%v%"art.status" <- 1
        fit.fake.net%v%"art.type" <- 1
        fit.fake.net%v%"time.since.art.initiation" <- 0
      }

      fit.fake.net%v%"time.since.infection" <- (fit.fake.net%v%"time.since.infection")+1
      fit.fake.net%v%"time.since.art.initiation" <-
        (fit.fake.net%v%"time.since.art.initiation")+1
      fit.fake.net%v%"age" <- (fit.fake.net%v%"age")+size.of.timestep/365

  cat("\n")


}

    ### check all specified attributes
      fit.fake.net%v%"viral.load.today"

      fit.fake.net%v%"inf.status"
      fit.fake.net%v%"art.status"
      fit.fake.net%v%"art.type"
      fit.fake.net%v%"time.since.infection"
      fit.fake.net%v%"time.since.art.initiation"
      fit.fake.net%v%"age"

#####################################################

#####################################################
### Check what happens when women receive
## short course ART (Option A), others do not receive treatment
#####################################################
      sc.art.postcess.ret.bl=5
      time.to.full.supp <- 10

      fit.fake.net <- fit.fake$network

      fit.fake.net%v%"inf.status" <- 1
      fit.fake.net%v%"vl.art.traj.slope" <- 0.1
      fit.fake.net%v%"time.since.infection" <- 0
      fit.fake.net%v%"art.status" <- 0
      fit.fake.net%v%"art.type" <- NA
      fit.fake.net%v%"time.since.art.initiation" <- NA
      fit.fake.net%v%"viral.load.today" <- 0
      fit.fake.net%v%"age" <- 30


## simulate fake network
  fit.fake.net <- activate.vertices(fit.fake.net, onset=1, terminus=Inf)
  fit.fake.net <- activate.edges(fit.fake.net, onset=1, terminus=Inf)

##undebug(compute.vl.mp.art4)

for (time in 2:50){
  cat("Completing timestep ", time, "\n")
  
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

  fit.fake.net<- compute.vl.mp.art4(fit.fake.net, verbose=TRUE,
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
                                    time.to.full.supp,
                                    undetectable.vl=
                                    undetectable.vl,
                                    sc.art.postcess.ret.bl=
                                    sc.art.postcess.ret.bl, #26Jul13
                                    size.of.timestep=
                                    size.of.timestep,
                                    optA.sc.art.vl.perstep.dec=
                                    optA.sc.art.vl.perstep.dec,
                                    optA.sc.art.vl.perstep.inc=
                                    optA.sc.art.vl.perstep.inc
                                    )

  cat("Viral loads are ",
      fit.fake.net%v%"viral.load.today",
      "\n")

    if (time == 3){ ## Test initiation of scART in women
        fit.fake.net%v%"art.status" <- c(rep(0,5), rep(1,5))
        fit.fake.net%v%"art.type" <- c(rep(NA,5), rep(2,5))
        fit.fake.net%v%"time.since.art.initiation" <- c(rep(NA,5), rep(0, 5))
      }

  if (time == 31){ ## Test cessation of scART in women
        fit.fake.net%v%"art.status" <- c(rep(0,10))
        fit.fake.net%v%"art.type" <- c(rep(NA,5), rep(3,5))
      }

      fit.fake.net%v%"time.since.infection" <- (fit.fake.net%v%"time.since.infection")+1
      fit.fake.net%v%"time.since.art.initiation" <-
        (fit.fake.net%v%"time.since.art.initiation")+1
      fit.fake.net%v%"age" <- (fit.fake.net%v%"age")+size.of.timestep/365

  cat("\n")


}

    ### check all specified attributes
      fit.fake.net%v%"viral.load.today"

      fit.fake.net%v%"inf.status"
      fit.fake.net%v%"art.status"
      fit.fake.net%v%"art.type"
      fit.fake.net%v%"time.since.infection"
      fit.fake.net%v%"time.since.art.initiation"
      fit.fake.net%v%"age"

      fit.fake.net%v%"stage.of.infection"
#####################################################

#####################################################
### 22 Aug 2013: To test changes on account of age-based duration of infection
### at time of infection.
#####################################################

  fit.fake.net <- fit.fake$network

  fit.fake.net%v%"inf.status" <- 1
  fit.fake.net%v%"vl.art.traj.slope" <- 0.1
  fit.fake.net%v%"time.since.infection" <- 0
  fit.fake.net%v%"art.status" <- 0
  fit.fake.net%v%"art.type" <- NA
  fit.fake.net%v%"time.since.art.initiation" <- NA
  fit.fake.net%v%"viral.load.today" <- 0
  fit.fake.net%v%"age" <- 30
  fit.fake.net%v%"dur.inf.by.age" <- 25 #22Aug2013

for (time in 2:40){
  cat("Completing timestep ", time, "\n")

 fit.fake.net <- simulate(fit.fake.net, #22Aug2013
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

  
  fit.fake.net<- compute.vl.mp.art4(fit.fake.net, verbose=TRUE,
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
                                    time.to.full.supp,
                                    undetectable.vl=
                                    undetectable.vl,
                                    size.of.timestep=
                                    size.of.timestep,
                                    optA.sc.art.vl.perstep.dec=
                                    optA.sc.art.vl.perstep.dec,
                                    optA.sc.art.vl.perstep.inc=
                                    optA.sc.art.vl.perstep.inc
                                    )

  cat("Viral loads are ",
      fit.fake.net%v%"viral.load.today",
      "\n")

    ## if (time == 3){
    ##     fit.fake.net%v%"art.status" <- 1
    ##     fit.fake.net%v%"art.type" <- 1
    ##     fit.fake.net%v%"time.since.art.initiation" <- 0
    ##   }

      fit.fake.net%v%"time.since.infection" <- (fit.fake.net%v%"time.since.infection")+1
      fit.fake.net%v%"time.since.art.initiation" <-
        (fit.fake.net%v%"time.since.art.initiation")+1
      fit.fake.net%v%"age" <- (fit.fake.net%v%"age")+size.of.timestep/365

  cat("\n")


}
