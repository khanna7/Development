#####################################################
### Progression of versions
#####################################################

## 22 Aug 2013: Test if assigning age-based duration of infection assigns
## correct age-based life expectancy at time of infection. 
## Also update simulate function to be compatible with "statnet_3.0."

## 31 Jul 2013: Important note from 26Jul13 -- noticed while unit-testing the 
## transmission function. We have a network with 25 men and 25 women, 
## and a mean of 25 edges. If I specify a duration of 1, after simulate, I 
## can get as many as 600 edges (though there is a large variation).  If the duration
## parameter is raised to 5, the number of edges in simulation has much lower variation.
## If the duration parameter is raised to 25, then, we get a "toggleList" error --
## which I think implies that the average duration is so high that ties cannot be turned
## over and the toggle list remains empty.

## 30 Jul 2013: Test to see modification on account
## of pregancy, circumcision

## 26 Jul 2013: Test transmission function.
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
source("transmission_d9.R") 

#####################################################
## Make fake network
#####################################################

  num.male <- 25
  num.female <- 25 
  N <- num.male+num.female

  formation <- ~edges
  formation.net <- update.formula(formation,
                                  net~.)
  duration <- 1
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
 preg.mult <- 0
 circum.mult <- 0

##optA.sc.art.vl.perstep.inc #5Jul13


## Needed attributes
  inf.status <- c(rep(0, num.male/2), rep(1, num.male/2),
                  rep(0, num.female/2), rep(1, num.female/2))

  fit.fake.net%v%"inf.status" <- inf.status
  fit.fake.net%v%"circum.status" <- NA
  fit.fake.net%v%"curr.pregnancy.status" <- c(rep(NA, num.male), rep(0, num.female))
  fit.fake.net%v%"art.status" <- 0
  fit.fake.net%v%"infectivity.today" <- 1
  
## simulate fake network
   fit.fake.net <- activate.vertices(fit.fake.net, onset=1, terminus=Inf)
   fit.fake.net <- activate.edges(fit.fake.net, onset=1, terminus=Inf)


   cat("Number of edges is", network.edgecount(fit.fake.net), "\n")

#####################################################

#####################################################
### test transmission across serodiscordant partnerships
#####################################################

##debug(transmission)

inci <- 0
inci.m <- 0
inci.f <- 0

for (time in 2:30){
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
  
  fit.fake.net<- transmission(fit.fake.net, verbose=TRUE,
                              preg.mult=preg.mult,
                              circum.mult=circum.mult
                              )
                              
}

 ### check all specified attributes

      fit.fake.net%v%"inf.status"

#####################################################

#####################################################
### test how change in circumcision affects
### transmission 
#####################################################

  fit.fake.net <- fit.fake$network

  ## Conditions:
  ## All men are susceptible and circumcised,
  ## all women are infected
  ## Circumcision has a 100% protective effect
  ## We should observe transmission in none of the partnerships
  circum.mult <- 0

  ## new attributes for testing impact of circumcision
  fit.fake.net%v%"inf.status" <- c(rep(0, num.male),
                                 rep(1, num.female))

  fit.fake.net%v%"circum.status" <- c(rep(1, num.male),
                                      rep(NA, num.female)
                                      )

  ## old attributes
  fit.fake.net%v%"curr.pregnancy.status" <- c(rep(NA, num.male), rep(0, num.female))
  fit.fake.net%v%"art.status" <- 0
  fit.fake.net%v%"infectivity.today" <- 1


  fit.fake.net <- activate.vertices(fit.fake.net, onset=1, terminus=Inf)
  fit.fake.net <- activate.edges(fit.fake.net, onset=1, terminus=Inf)

  cat("Number of edges is", network.edgecount(fit.fake.net), "\n")

  
inci <- 0
inci.m <- 0
inci.f <- 0

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
  
  fit.fake.net<- transmission(fit.fake.net, verbose=TRUE,
                              preg.mult=preg.mult,
                              circum.mult=circum.mult
                              )
                              
}

 ### check all specified attributes

      sum(fit.fake.net%v%"inf.status")
   ## tests worked -- no transmissions observed if "circum.mult" is changed to 0.
   ## if "circum.mult" is changed to 1, all men in partnerships get infected.
   ## we can test this via
   which(fit.fake.net%v%"inf.status" == 0) # list uninfected women
   unique(sort((as.edgelist(fit.fake.net))[,1])) # list women in partnerships
                                                 # (only once if they are in multiple)
  ## (these two lines copied from below -- to test effect of "preg.mult")

#####################################################
### test how change in pregnancy affects
### transmission
### NB Test for the effect of increased infectivity on account of pregnancy
### of infected women is in "unittest_assigninfectivity.R."
### Test below is to check the effect of pregnancy in terms of
### increased risk in susceptible women  
#####################################################

  fit.fake.net <- fit.fake$network

  ## Conditions:
  ## All men are infected,
  ## all women are susceptible and pregnant.
  ## Pregnancy has a 100% protective effect
  ## We should observe transmission in none of the partnerships

  preg.mult <- 1
  circum.mult <- 0

  ## new attributes for testing impact of circumcision
  fit.fake.net%v%"inf.status" <- c(rep(1, num.male),
                                 rep(0, num.female))

  fit.fake.net%v%"curr.pregnancy.status" <- c(rep(NA, num.male),
                                              rep(1, num.female)
                                              )

  ## old attributes
  fit.fake.net%v%"circum.status" <- NA
  fit.fake.net%v%"art.status" <- 0
  fit.fake.net%v%"infectivity.today" <- 1


  fit.fake.net <- activate.vertices(fit.fake.net, onset=1, terminus=Inf)
  fit.fake.net <- activate.edges(fit.fake.net, onset=1, terminus=Inf)

  cat("Number of edges is", network.edgecount(fit.fake.net), "\n")

  
inci <- 0
inci.m <- 0
inci.f <- 0

for (time in 2:2){
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
  
  fit.fake.net <- transmission(fit.fake.net, verbose=TRUE,
                               preg.mult=preg.mult,
                               circum.mult=circum.mult
                              )
                              
}

 ### check all specified attributes

      sum(fit.fake.net%v%"inf.status")
   ## tests worked -- no transmissions observed if "preg.mult" is changed to 0.
   ## if "preg.mult" is changed to 1,
   ## all women in partnerships are infected. we can test this via
   which(fit.fake.net%v%"inf.status" == 0) # list uninfected women
   unique(sort((as.edgelist(fit.fake.net))[,2])) # list women in partnerships
                                                 # (only once if they are in multiple)


#####################################################
## 22 Aug 2013: Test assigned age-based life expectancy at
## point of infection
#####################################################

  fit.fake.net <- fit.fake$network
  orig.inf.status <- inf.status
  fit.fake.net%v%"inf.status" <- orig.inf.status
  fit.fake.net%v%"circum.status" <- NA
  fit.fake.net%v%"curr.pregnancy.status" <- c(rep(NA, num.male), rep(0, num.female))
  fit.fake.net%v%"art.status" <- 0
  fit.fake.net%v%"infectivity.today" <- 1
  fit.fake.net%v%"age" <- sample(15:55, size=network.size(fit.fake.net),
                                 replace=TRUE)
                                 
  ## Conditions:
  ## All men are infected,
  ## all women are susceptible and pregnant.
  ## Pregnancy has a 100% protective effect
  ## We should observe transmission in none of the partnerships

  preg.mult <- 0
  circum.mult <- 0
  dur.inf.by.age <- seq(1000, 4000, length=4)
  baseline.art.coverage.rate <- 1
  baseline.preg.coverage.rate <- 1
  date <- "22Aug_unittest"

  ## new attributes for testing impact of circumcision
  ## fit.fake.net%v%"inf.status" <- c(rep(1, num.male),
  ##                                rep(0, num.female))

  ## fit.fake.net%v%"curr.pregnancy.status" <- c(rep(NA, num.male),
  ##                                             rep(1, num.female)
  ##                                             )

  fit.fake.net <- activate.vertices(fit.fake.net, onset=1, terminus=Inf)
  fit.fake.net <- activate.edges(fit.fake.net, onset=1, terminus=Inf)

  cat("Number of edges is", network.edgecount(fit.fake.net), "\n")

  
  inci <- 0
  inci.m <- 0
  inci.f <- 0

 for (time in 2:2){
  cat("Completing timestep ", time, "\n")
  
  nw <- simulate(fit.fake.net, #22Aug2013
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



  
  net.time <- network.extract(fit.fake.net, at=time,
                              retain.all.vertices=T)
  
  cat("After simulate.networkDynamic, number of edges is",
      network.edgecount(net.time), "\n")

   fit.fake.net <- transmission(nw, verbose=TRUE,
                                preg.mult=preg.mult,
                                circum.mult=circum.mult, #13Aug13: Add new arguments
                                scenario="baseline", # 13Aug13: Add these arguments
                                baseline.art.coverage.rate=baseline.art.coverage.rate,
                                baseline.preg.coverage.rate=baseline.preg.coverage.rate,
                                dur.inf.by.age=dur.inf.by.age #22Aug2013
                                )

}

 ### check all specified attributes

      sum(fit.fake.net%v%"inf.status")
      fit.fake.net%v%"age"
      fit.fake.net%v%"inf.status"
      fit.fake.net%v%"dur.inf.by.age"

      new.inf <- intersect(which(orig.inf.status == 0),
                           which(fit.fake.net%v%"inf.status" == 1)
                           )
 
      (fit.fake.net%v%"age")[new.inf]
      (fit.fake.net%v%"dur.inf.by.age")[new.inf]
 
      xtabs(~factor((fit.fake.net%v%"age")[new.inf], exclude=NULL)+
            factor((fit.fake.net%v%"dur.inf.by.age")[new.inf], exclude=NULL)
            )

      (fit.fake.net%v%"dur.inf.by.age")[new.inf]
      (fit.fake.net%v%".age")[new.inf]
