#####################################################
### Progression of versions
#####################################################

## 31 Jul 2013: How does CD4 for post-scART women change? 

## 10 Jul 2013: Test CD4 functions using fake network
## objects.
#####################################################

#####################################################
### Top Matter
#####################################################

rm(list=ls())

library(ergm)
library(tergm)
library(testthat)
source("../simulate.networkDynamic.R")
source("common.functions_d7.R")
source("compute.cd4.count_d1.R")

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
  size.of.timestep <- 14
  cd4.at.infection.male <- 518 
  cd4.at.infection.female <- 570
  per.day.untreated.cd4.decline <- 0.14
  cd4.recovery.time <- 3*365/size.of.timestep ## CD4 recovery for 3 years
  per.day.cd4.recovery <- 15/30 ## rate of 15 cells/month
  optA.sc.art.vl.perstep.dec <- 1.1/((40-23)*7)*size.of.timestep
  optA.sc.art.cd4.perstep.rec <- 50/((40-23)*7)*size.of.timestep

## Needed attributes
      fit.fake.net%v%"inf.status" <- 0
      fit.fake.net%v%"art.status" <- 0
      fit.fake.net%v%"art.type" <- NA
      fit.fake.net%v%"time.since.infection" <- NA
      fit.fake.net%v%"time.since.art.initiation" <- NA
      fit.fake.net%v%"cd4.count.today" <- c(rep(518, 5), rep(570, 5))
      fit.fake.net%v%"age" <- 30
  
## simulate fake network
   fit.fake.net <- activate.vertices(fit.fake.net, onset=1, terminus=Inf)
   fit.fake.net <- activate.edges(fit.fake.net, onset=1, terminus=Inf)

#####################################################

#####################################################
### test initial CD4 counts
#####################################################

##debug(update.vital.dynamics)

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

   test.init.cd4 <- compute.cd4.count.sexage.acct(fit.fake.net, verbose=TRUE,
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
  

  cat("\n")


}

    ### check all specified attributes
      test.init.cd4%v%"cd4.count.today"

      test.init.cd4%v%"inf.status"
      test.init.cd4%v%"art.status"
      test.init.cd4%v%"art.type"
      test.init.cd4%v%"time.since.infection"
      test.init.cd4%v%"time.since.art.initiation"
      test.init.cd4%v%"age"

#####################################################

#####################################################
### test decline in treatment-naive infectives
### with same ages
#####################################################

  fit.fake.net <- fit.fake$network

  fit.fake.net%v%"inf.status" <- 1
  fit.fake.net%v%"art.status" <- 0
  fit.fake.net%v%"art.type" <- NA
  fit.fake.net%v%"time.since.infection" <- 0
  fit.fake.net%v%"time.since.art.initiation" <- NA
  fit.fake.net%v%"cd4.count.today" <- c(rep(518, 5), rep(570, 5))
  fit.fake.net%v%"age" <- 30

  fit.fake.net <- activate.vertices(fit.fake.net, onset=1, terminus=Inf)
  fit.fake.net <- activate.edges(fit.fake.net, onset=1, terminus=Inf)

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

  fit.fake.net <- compute.cd4.count.sexage.acct(fit.fake.net, verbose=TRUE,
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

      fit.fake.net%v%"time.since.infection" <- (fit.fake.net%v%"time.since.infection")+1

  cat("\n")


}

    fit.fake.net%v%"cd4.count.today"

#####################################################

#####################################################
### test decline in treatment-naive infectives
### with different ages
#####################################################

  fit.fake.net <- fit.fake$network

  fit.fake.net%v%"inf.status" <- 1
  fit.fake.net%v%"art.status" <- 0
  fit.fake.net%v%"art.type" <- NA
  fit.fake.net%v%"time.since.infection" <- 0
  fit.fake.net%v%"time.since.art.initiation" <- NA
  fit.fake.net%v%"cd4.count.today" <- c(rep(518, 5), rep(570, 5))
  fit.fake.net%v%"age" <- rep(c(15, 20, 30, 40, 50), 2)

  fit.fake.net <- activate.vertices(fit.fake.net, onset=1, terminus=Inf)
  fit.fake.net <- activate.edges(fit.fake.net, onset=1, terminus=Inf)

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

  fit.fake.net <- compute.cd4.count.sexage.acct(fit.fake.net, verbose=TRUE,
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

      fit.fake.net%v%"time.since.infection" <- (fit.fake.net%v%"time.since.infection")+1

  cat("\n")


}

    fit.fake.net%v%"cd4.count.today"

#####################################################

#####################################################
### test decline in treatment-naive infectives
### with same ages
### treatment introduced at timestep 6.
### Regular Art (type 1) for everyone
#####################################################

  fit.fake.net <- fit.fake$network

  fit.fake.net%v%"inf.status" <- 1
  fit.fake.net%v%"art.status" <- 0
  fit.fake.net%v%"art.type" <- NA
  fit.fake.net%v%"time.since.infection" <- 0
  fit.fake.net%v%"time.since.art.initiation" <- NA
  fit.fake.net%v%"cd4.count.today" <- c(rep(518, 5), rep(570, 5))
  fit.fake.net%v%"age" <- rep(c(15, 20, 30, 40, 50), 2)

  fit.fake.net <- activate.vertices(fit.fake.net, onset=1, terminus=Inf)
  fit.fake.net <- activate.edges(fit.fake.net, onset=1, terminus=Inf)

  for (time in 2:20){

    cat("Completing timestep", time, "\n")

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

  fit.fake.net <- compute.cd4.count.sexage.acct(fit.fake.net, verbose=TRUE,
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

    if (time == 10){
        fit.fake.net%v%"art.status" <- 1
        fit.fake.net%v%"art.type" <- 1
        fit.fake.net%v%"time.since.art.initiation" <- 0
      }
    
  cat("\n")

      fit.fake.net%v%"time.since.infection" <- (fit.fake.net%v%"time.since.infection")+1
      fit.fake.net%v%"time.since.art.initiation" <- (fit.fake.net%v%"time.since.art.initiation")+1
      fit.fake.net%v%"age" <- (fit.fake.net%v%"age")+size.of.timestep/365
    

}
    ## check CD4 counts
    fit.fake.net%v%"cd4.count.today"

#####################################################
