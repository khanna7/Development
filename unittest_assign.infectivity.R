###########################################################
## 30 Jul 2013: Moved "assign.infectivity" to its own
## file from "common.functions_d8.R."
###########################################################

rm(list=ls())

library(ergm)
library(tergm)
library(testthat)
source("../simulate.networkDynamic.R")
source("common.functions_d8.R")
source("assign.infectivity_d2.R")

#####################################################
## Make fake network
#####################################################

  num.male <- 25
  num.female <- 25 
  N <- num.male+num.female

  formation <- ~edges
  formation.net <- update.formula(formation,
                                  net~.)
  duration <- 5
  dissolution <- ~offset(edges)
  theta.diss <- log(duration-1)
  target.stats <- 25
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

   min.chronic.infectivity.unadj <- 0.000497/2.89
   size.of.timestep <- 14
   num.sex.acts.per.timestep <- 2.4*size.of.timestep/7
   acute.mult <- 4.98 
   late.mult <- 3.49
   preg.mult <- 2.5 ## check
   acute.length <- 1:floor(121/size.of.timestep) ## in daily time units
   chronic.length <- ceiling(121/size.of.timestep):floor(1877/size.of.timestep)
   late.length <- ceiling(1877/size.of.timestep):floor(3301/size.of.timestep)

  peak.viral.load <- 6.17
  set.point.viral.load <- 4.2
  late.stage.viral.load <- 5.05
  dur.inf <- 3300/size.of.timestep



##   dur.inf <- 3301/size.of.timestep

## Needed attributes
  fit.fake.net%v%"inf.status" <- 1
  fit.fake.net%v%"viral.load.today" <- 5
  fit.fake.net%v%"stage.of.infection" <- NA
  fit.fake.net%v%"time.since.infection" <- seq(0, 3301/size.of.timestep,
                                               length.out=50)
  fit.fake.net%v%"infectivity.today" <- 5
  fit.fake.net%v%"curr.pregnancy.status" <- 5
  
## simulate fake network
   fit.fake.net <- activate.vertices(fit.fake.net, onset=1, terminus=Inf)
   fit.fake.net <- activate.edges(fit.fake.net, onset=1, terminus=Inf)


   cat("Number of edges is", network.edgecount(fit.fake.net), "\n")

#####################################################

#####################################################
 ## First test assignment of "stage of infection"
 ## (viral load parameter set to 5 is meaningless in this test)
#####################################################

for (time in 2:3){
  cat("Completing timestep ", time, "\n")
  
  fit.fake.net <- simulate(fit.fake.net,
                           formation=formation,
                           dissolution=dissolution,
                           coef.form=theta.form,
                           coef.diss=theta.diss,
                           constraints=constraints,
                           start.time=time,
                           time.slices=1,
                                        # 3Jun13: discussion with sam --
                                        # simulate over 1 time step every
                                        # time you hit the timestep in the loop
                                        # (as per the code in Steve's summer 2012 course)
                           control=control.simulate.networkDynamic(MCMC.burnin=10000),
                           maxchanges=1e6
                           )

  net.time <- network.extract(fit.fake.net, at=time,
                              retain.all.vertices=T)
  
  cat("After simulate.networkDynamic, number of edges is",
      network.edgecount(net.time), "\n")
  
  fit.fake.net <- assign.infectivity(fit.fake.net, verbose=TRUE,
                                     min.chronic.infectivity.unadj=
                                     min.chronic.infectivity.unadj, 
                                     num.sex.acts.per.timestep=
                                     num.sex.acts.per.timestep,
                                     acute.mult=acute.mult,
                                     late.mult=late.mult,
                                     preg.mult=preg.mult, #10Jun13
                                     acute.length=acute.length,#30Jul13
                                     chronic.length=chronic.length, 
                                     late.length=late.length ##,
##                                     dur.inf=dur.inf
                                     )
  
}

 ### check all specified attributes

      cbind(fit.fake.net%v%"time.since.infection",
            fit.fake.net%v%"stage.of.infection"
            )

#####################################################

#####################################################
  ## Test for acute, chronic and late infecteds
  ## with no modification
#####################################################

  fit.fake.net <- fit.fake$network

  fit.fake.net <- activate.vertices(fit.fake.net, onset=1, terminus=Inf)
  fit.fake.net <- activate.edges(fit.fake.net, onset=1, terminus=Inf)


   cat("Number of edges is", network.edgecount(fit.fake.net), "\n")

  num.not.infected.men <- 5
  num.not.infected.women <- 5

  fit.fake.net%v%"inf.status" <- c(rep(0, num.not.infected.men),
                                   rep(1, num.male-num.not.infected.men),
                                   rep(0, num.not.infected.men),
                                   rep(1, num.female-num.not.infected.men)
                                   )

  # first "num.not.infected.men" and
  # first "num.not.infected.women" are not infected

  fit.fake.net%v%"stage.of.infection" <- NA

  fit.fake.net%v%"time.since.infection" <- c(rep(NA, num.not.infected.men),
                                             rep(max(acute.length)/2,
                                                 7),
                                             rep(max(acute.length+1),
                                                 7),
                                             rep(max(chronic.length+1),
                                                 6),
                                             rep(NA, num.not.infected.women),
                                             rep(max(acute.length/2),
                                                 7),
                                             rep(max(acute.length+1),
                                                 7),
                                             rep(max(chronic.length)+1,
                                                 6)
                                             )

  fit.fake.net%v%"viral.load.today" <- c(rep(0, num.not.infected.men),
                                         rep(peak.viral.load-1,
                                             7),
                                         rep(set.point.viral.load,
                                             7),
                                         rep(late.stage.viral.load,
                                             6),
                                         rep(0, num.not.infected.women),
                                         rep(peak.viral.load-1,
                                             7),
                                         rep(set.point.viral.load,
                                             7),
                                         rep(late.stage.viral.load,
                                             6)
                                         )


 ## infected's are equally distributed in between the three infection stages.

  fit.fake.net%v%"infectivity.today" <- NA
  fit.fake.net%v%"curr.pregnancy.status" <- NA

 for (time in 2:3){
  cat("Completing timestep ", time, "\n")
  
  fit.fake.net <- simulate(fit.fake.net,
                           formation=formation,
                           dissolution=dissolution,
                           coef.form=theta.form,
                           coef.diss=theta.diss,
                           constraints=constraints,
                           start.time=time,
                           time.slices=1,
                                        # 3Jun13: discussion with sam --
                                        # simulate over 1 time step every
                                        # time you hit the timestep in the loop
                                        # (as per the code in Steve's summer 2012 course)
                           control=control.simulate.networkDynamic(MCMC.burnin=10000),
                           maxchanges=1e6
                           )

  net.time <- network.extract(fit.fake.net, at=time,
                              retain.all.vertices=T)
  
  cat("After simulate.networkDynamic, number of edges is",
      network.edgecount(net.time), "\n")
  
  fit.fake.net <- assign.infectivity(fit.fake.net, verbose=TRUE,
                                     min.chronic.infectivity.unadj=
                                     min.chronic.infectivity.unadj, 
                                     num.sex.acts.per.timestep=
                                     num.sex.acts.per.timestep,
                                     acute.mult=acute.mult,
                                     late.mult=late.mult,
                                     preg.mult=preg.mult, #10Jun13
                                     acute.length=acute.length,#30Jul13
                                     chronic.length=chronic.length, 
                                     late.length=late.length ##,
##                                     dur.inf=dur.inf
                                     )
  
}

### check all specified attributes

      cbind(fit.fake.net%v%"inf.status",
            fit.fake.net%v%"time.since.infection",
            fit.fake.net%v%"stage.of.infection",
            fit.fake.net%v%"viral.load.today",
            fit.fake.net%v%"infectivity.today"
            )

#####################################################

#####################################################
  ## Test impact of pregnancy on infectivity of
  ## pregnant women
#####################################################

  fit.fake.net <- fit.fake$network

  fit.fake.net <- activate.vertices(fit.fake.net, onset=1, terminus=Inf)
  fit.fake.net <- activate.edges(fit.fake.net, onset=1, terminus=Inf)


   cat("Number of edges is", network.edgecount(fit.fake.net), "\n")

  num.not.infected.men <- 5
  num.not.infected.women <- 5

  fit.fake.net%v%"inf.status" <- c(rep(0, num.not.infected.men),
                                   rep(1, num.male-num.not.infected.men),
                                   rep(0, num.not.infected.men),
                                   rep(1, num.female-num.not.infected.men)
                                   )

  # first "num.not.infected.men" and
  # first "num.not.infected.women" are not infected

  fit.fake.net%v%"stage.of.infection" <- NA

  fit.fake.net%v%"time.since.infection" <- c(rep(NA, num.not.infected.men),
                                             rep(max(acute.length)/2,
                                                 7),
                                             rep(max(acute.length+1),
                                                 7),
                                             rep(max(chronic.length+1),
                                                 6),
                                             rep(NA, num.not.infected.women),
                                             rep(max(acute.length/2),
                                                 7),
                                             rep(max(acute.length+1),
                                                 7),
                                             rep(max(chronic.length)+1,
                                                 6)
                                             )

  fit.fake.net%v%"viral.load.today" <- c(rep(0, num.not.infected.men),
                                         rep(peak.viral.load-1,
                                             7),
                                         rep(set.point.viral.load,
                                             7),
                                         rep(late.stage.viral.load,
                                             6),
                                         rep(0, num.not.infected.women),
                                         rep(peak.viral.load-1,
                                             7),
                                         rep(set.point.viral.load,
                                             7),
                                         rep(late.stage.viral.load,
                                             6)
                                         )


 ## infected's are equally distributed in between the three infection stages.

  fit.fake.net%v%"infectivity.today" <- NA

  fit.fake.net%v%"curr.pregnancy.status" <- c(rep(NA, num.male),
                                              sample(c(0,1), size=num.female,
                                                     prob=c(1/2, 1/2),
                                                     replace=TRUE)
                                              
                                              )
 for (time in 2:3){
  cat("Completing timestep ", time, "\n")
  
  fit.fake.net <- simulate(fit.fake.net,
                           formation=formation,
                           dissolution=dissolution,
                           coef.form=theta.form,
                           coef.diss=theta.diss,
                           constraints=constraints,
                           start.time=time,
                           time.slices=1,
                                        # 3Jun13: discussion with sam --
                                        # simulate over 1 time step every
                                        # time you hit the timestep in the loop
                                        # (as per the code in Steve's summer 2012 course)
                           control=control.simulate.networkDynamic(MCMC.burnin=10000),
                           maxchanges=1e6
                           )

  net.time <- network.extract(fit.fake.net, at=time,
                              retain.all.vertices=T)
  
  cat("After simulate.networkDynamic, number of edges is",
      network.edgecount(net.time), "\n")
  
  fit.fake.net <- assign.infectivity(fit.fake.net, verbose=TRUE,
                                     min.chronic.infectivity.unadj=
                                     min.chronic.infectivity.unadj, 
                                     num.sex.acts.per.timestep=
                                     num.sex.acts.per.timestep,
                                     acute.mult=acute.mult,
                                     late.mult=late.mult,
                                     preg.mult=preg.mult, #10Jun13
                                     acute.length=acute.length,#30Jul13
                                     chronic.length=chronic.length, 
                                     late.length=late.length ##,
##                                     dur.inf=dur.inf
                                     )
  
}

### check all specified attributes

      cbind(fit.fake.net%v%"inf.status",
            fit.fake.net%v%"time.since.infection",
            fit.fake.net%v%"stage.of.infection",
            fit.fake.net%v%"viral.load.today",
            fit.fake.net%v%"curr.pregnancy.status",
            fit.fake.net%v%"infectivity.today"
            )

#####################################################
