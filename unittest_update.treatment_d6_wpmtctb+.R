#####################################################
### Progression of versions
#####################################################

## 22 Oct 2013: Debug the PMTCTB+ code

## 27 Aug 2013: Debug latest treatment file -- with special attention
## to proportion of infecteds who are on ART at equilibrium. Shouldn't
## be about the specified number ~ 43%.

## 30 July 2013: Moved assignment of infection stage to "assign.infectivity:
## function. Revise arguments here by removing acute, chronic and late
## arguments.

## 11 July 2013: Added "undetectable.vl" and "time.to.full.supp" as arguments. 
## Currently stage of infection is assigned here. Maybe make it its own function.
## Not necessary to pass full network object here.

## 11 July 2013: Test updating of treatment
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
## source("update.treatment_d1.R")
## source("update.treatment_d3.R") #30Jul13
#source("update.treatment_d6.R") #27Aug13
source("update.treatment_d6_wpmtctb+.R") #2Oct2013

#####################################################
## Make fake network
#####################################################

## needed parameters
size.of.timestep=14
#scenario="baseline"
scenario="pmtct.b+"
dur.inf=27#made up
eligible.cd4=350
baseline.cd4.at.art.initiation.men=100
baseline.cd4.at.art.initiation.women=100
baseline.f.ges.visit= 0#23*7/14
optA.thres=350
cd4.at.infection.male=518
cd4.at.infection.female=570
## per.day.untreated.cd4.decline=
## per.day.untreated.cd4.decline
cd4.recovery.time=3*365/size.of.timestep ## CD4 recovery for 3 yearscd4.recovery.time
per.day.cd4.recovery=15/30 ## rate of 15 cells/month
acute.length=1:floor(121/size.of.timestep) ## in daily time unitsacute.length
chronic.length=ceiling(121/size.of.timestep):floor(1877/size.of.timestep)
late.length=ceiling(1877/size.of.timestep):floor(3300/size.of.timestep)
time.infection.to.peak.viral.load=floor(5) #made up
optA.vl.reduction=1.1 ##5Jul13
full.term <- 40*7/14
undetectable.vl <- log(50, base=10)
time.to.full.supp <- 4*30/size.of.timestep
##  #time.infection.to.peak.viral.load <- floor(14/size.of.timestep)
## time.infection.to.peak.viral.load <- floor(5)
## peak.viral.load <- 6.17
## #time.infection.to.viral.set.point <- floor(121/size.of.timestep)
## time.infection.to.viral.set.point <- 10
## set.point.viral.load <- 4.2
## #time.infection.to.late.stage <- floor(1877/size.of.timestep)
## time.infection.to.late.stage <- floor(15)
## #dur.inf <- 3300/size.of.timestep
## dur.inf <- 27
## late.stage.viral.load <- 5.05 ## (max?)
## time.to.full.supp <- 4*30/size.of.timestep
## undetectable.vl <- log(50, base=10)
## optA.sc.art.vl.perstep.dec <- 1.1/((40-23)*7)*size.of.timestep
## ##optA.sc.art.vl.perstep.inc #5Jul13

  ## fake network
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
      fit.fake.net%v%"inf.status" <- 1
      fit.fake.net%v%"time.since.infection" <- 0
      fit.fake.net%v%"cd4.count.today" <- 99
      fit.fake.net%v%"curr.pregnancy.status" <- 0
      fit.fake.net%v%"vl.art.traj.slope" <- 0.1
      fit.fake.net%v%"art.status" <- 0
      fit.fake.net%v%"art.type" <- NA
      fit.fake.net%v%"time.since.art.initiation" <- NA
      fit.fake.net%v%"stage.of.infection" <- NA
      fit.fake.net%v%"viral.load.today" <- rep(c(2:6), 2)
  
## simulate fake network
   fit.fake.net <- activate.vertices(fit.fake.net, onset=1, terminus=Inf)
   fit.fake.net <- activate.edges(fit.fake.net, onset=1, terminus=Inf)

#####################################################

  fit.fake.net <- fit.fake$network

#####################################################
## 27 Aug 2013: Test proportion of infected on ART
## at equilibrium
#####################################################

## Needed attributes
      fit.fake.net%v%"inf.status" <- 1
      fit.fake.net%v%"time.since.infection" <- 0
      fit.fake.net%v%"cd4.count.today" <- 99
      fit.fake.net%v%"curr.pregnancy.status" <- 0
      fit.fake.net%v%"vl.art.traj.slope" <- 0.1
      fit.fake.net%v%"art.status" <- 0
      fit.fake.net%v%"art.type" <- NA
      fit.fake.net%v%"time.since.art.initiation" <- NA
      fit.fake.net%v%"stage.of.infection" <- NA
      fit.fake.net%v%"viral.load.today" <- rep(c(2:6), 2)

## simulate fake network
   fit.fake.net <- activate.vertices(fit.fake.net, onset=1, terminus=Inf)
   fit.fake.net <- activate.edges(fit.fake.net, onset=1, terminus=Inf)


#####################################################
### test going on treatment when CD4 counts are below
### baseline initiation values
### Also monitor viral-load trajectories after ART
## is initiated
#####################################################

##debug(update.vital.dynamics)

  fit.fake.net <- fit.fake$network

## parameters to be revised
  baseline.cd4.at.art.initiation.men=100
  baseline.cd4.at.art.initiation.women=100

## Needed attributes
      fit.fake.net%v%"inf.status" <- 1
      fit.fake.net%v%"time.since.infection" <- 0
      fit.fake.net%v%"cd4.count.today" <- 99
      fit.fake.net%v%"curr.pregnancy.status" <- 0
      fit.fake.net%v%"vl.art.traj.slope" <- 0.1
      fit.fake.net%v%"art.status" <- 0
      fit.fake.net%v%"art.type" <- NA
      fit.fake.net%v%"time.since.art.initiation" <- NA
      fit.fake.net%v%"stage.of.infection" <- NA
      fit.fake.net%v%"viral.load.today" <- rep(c(2:6), 2)

      fit.fake.net%v%"art.covered" <- rep(c(0,1), 5)
  
## simulate fake network
   fit.fake.net <- activate.vertices(fit.fake.net, onset=1, terminus=Inf)
   fit.fake.net <- activate.edges(fit.fake.net, onset=1, terminus=Inf)

for (time in 2:30){
  cat("Completing timestep ", time, "\n")
  
  fit.fake.net <- simulate(fit.fake.net,
                           formation=formation,
                           dissolution=dissolution,
                           coef.form=theta.form,
                           coef.diss=theta.diss,
                           constraints=constraints,
                           # start.time=2,
                           time.start=time,
                           time.slices=1,
                           control=control.simulate.network(MCMC.burnin=10000)
                           )

  fit.fake.net <- update.treatment(fit.fake.net,
                                   verbose=TRUE,
                                   scenario="pmtct.b+",
                                   dur.inf=dur.inf,
                                   eligibile.cd4=eligible.cd4,
                                   baseline.cd4.at.art.initiation.men =
                                   baseline.cd4.at.art.initiation.men,
                                   baseline.cd4.at.art.initiation.women=
                                   baseline.cd4.at.art.initiation.women,
                                   ##baseline.time.art.initiation.preg.women=
                                     ##23*7,
                                   baseline.f.ges.visit=
                                   23*7/14,
                                     ##0,
                                   optA.thres=350,
                                   cd4.at.infection.male=
                                   cd4.at.infection.male,
                                   cd4.at.infection.female=
                                   cd4.at.infection.female,
                                   ## per.day.untreated.cd4.decline=
                                   ## per.day.untreated.cd4.decline,
                                   cd4.recovery.time=cd4.recovery.time,
                                   per.day.cd4.recovery=per.day.cd4.recovery,
                                     #30Jul13: commented out below
                                     ## acute.length=acute.length,
                                     ## chronic.length=chronic.length, 
                                     ## late.length=late.length, 
                                   time.infection.to.peak.viral.load=
                                   time.infection.to.peak.viral.load,
                                   size.of.timestep=size.of.timestep, #28May13
                                     ## optA.sc.art.vl.perstep.dec, #5Jul13
                                     ## optA.sc.art.vl.perstep.inc, #5Jul13
                                   optA.vl.reduction=1.1, ##5Jul13
                                   full.term=40*7/14,
                                   undetectable.vl=log(50, base=10),
                                   time.to.full.supp=time.to.full.supp
                                   )

  
      cat("ART states are", fit.fake.net%v%"art.status", "\n")
  
      cat("Viral load-trajectory slopes are ", fit.fake.net%v%"vl.art.traj.slope", "\n")
      cat("Viral loads are ", fit.fake.net%v%"viral.load.today", "\n")

      fit.fake.net%v%"time.since.infection" <- (fit.fake.net%v%"time.since.infection")+1
      fit.fake.net%v%"time.since.art.initiation" <- (fit.fake.net%v%"time.since.art.initiation")+1
      fit.fake.net%v%"age" <- (fit.fake.net%v%"age")+size.of.timestep/365

  cat("\n")


}

 ### check all specified attributes
      fit.fake.net%v%"art.status"
      fit.fake.net%v%"art.type"
      fit.fake.net%v%"time.since.art.initiation"
      fit.fake.net%v%"time.of.art.initiation"

      fit.fake.net%v%"viral.load.today"
      fit.fake.net%v%"inf.status"
      fit.fake.net%v%"time.since.infection"
      fit.fake.net%v%"age"

#####################################################

#####################################################
### make sure treatment is not initiated at CD4 
### counts above the baseline initiation values
#####################################################

  fit.fake.net <- fit.fake$network
## Needed attributes
      fit.fake.net%v%"inf.status" <- 1
      fit.fake.net%v%"time.since.infection" <- 0
      fit.fake.net%v%"cd4.count.today" <- 101
      fit.fake.net%v%"curr.pregnancy.status" <- 0
      fit.fake.net%v%"vl.art.traj.slope" <- 0.1
      fit.fake.net%v%"art.status" <- 0
      fit.fake.net%v%"art.type" <- NA
      fit.fake.net%v%"time.since.art.initiation" <- NA
      fit.fake.net%v%"stage.of.infection" <- NA
  
## simulate fake network
   fit.fake.net <- activate.vertices(fit.fake.net, onset=1, terminus=Inf)
   fit.fake.net <- activate.edges(fit.fake.net, onset=1, terminus=Inf)

for (time in 2:2){
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

  fit.fake.net <- update.treatment(fit.fake.net, verbose=TRUE,
                                     scenario="baseline",
                                     dur.inf=dur.inf,
                                     eligibile.cd4=eligible.cd4,
                                     baseline.cd4.at.art.initiation.men =
                                     baseline.cd4.at.art.initiation.men,
                                     baseline.cd4.at.art.initiation.women=
                                     baseline.cd4.at.art.initiation.women,
                                     ##baseline.time.art.initiation.preg.women=
                                     ##23*7,
                                     baseline.f.ges.visit=
                                     ##23*7/14,
                                     0,
                                     optA.thres=350,
                                     cd4.at.infection.male=
                                     cd4.at.infection.male,
                                     cd4.at.infection.female=
                                     cd4.at.infection.female,
                                     ## per.day.untreated.cd4.decline=
                                     ## per.day.untreated.cd4.decline,
                                     cd4.recovery.time=cd4.recovery.time,
                                     per.day.cd4.recovery=per.day.cd4.recovery,
                                     acute.length=acute.length,
                                     chronic.length=chronic.length, 
                                     late.length=late.length, 
                                     time.infection.to.peak.viral.load=14,
                                     size.of.timestep=size.of.timestep, #28May13
                                     ## optA.sc.art.vl.perstep.dec, #5Jul13
                                     ## optA.sc.art.vl.perstep.inc, #5Jul13
                                        optA.vl.reduction=optA.vl.reduction, ##5Jul13
                                   full.term=full.term,
                                   undetectable.vl=undetectable.vl,
                                   time.to.full.supp=time.to.full.supp
           )

      fit.fake.net%v%"time.since.infection" <- (fit.fake.net%v%"time.since.infection")+1
      fit.fake.net%v%"time.since.art.initiation" <- (fit.fake.net%v%"time.since.art.initiation")+1
      fit.fake.net%v%"age" <- (fit.fake.net%v%"age")+size.of.timestep/365

      cat("Viral load-trajectory slopes are ", fit.fake.net%v%"vl.art.traj.slope", "\n")
      cat("Viral loads are ", fit.fake.net%v%"viral.load.today", "\n")
  
      cat("\n")


}

 ### check all specified attributes
      fit.fake.net%v%"art.status"
      fit.fake.net%v%"art.type"
      fit.fake.net%v%"time.since.art.initiation"
      fit.fake.net%v%"time.of.art.initiation"

      fit.fake.net%v%"viral.load.today"
      fit.fake.net%v%"inf.status"
      fit.fake.net%v%"time.since.infection"
      fit.fake.net%v%"age"

#####################################################
### Test Option A for pregnant women
### with CD4<350, do they start regular ART?
#####################################################

      fit.fake.net <- fit.fake$network
## Needed attributes
      # general
      fit.fake.net%v%"inf.status" <- 1
      fit.fake.net%v%"time.since.infection" <- 0
      fit.fake.net%v%"cd4.count.today" <- 101
      fit.fake.net%v%"vl.art.traj.slope" <- 0.1
      fit.fake.net%v%"art.status" <- 0
      fit.fake.net%v%"art.type" <- NA
      fit.fake.net%v%"time.since.art.initiation" <- NA
      fit.fake.net%v%"stage.of.infection" <- NA
      # pregnancy
      fit.fake.net%v%"curr.pregnancy.status" <- c(rep(NA, num.male), rep(1, num.female))
      fit.fake.net%v%"time.since.curr.pregnancy" <- c(rep(NA, num.male),
                                                      rep(1, num.female))
      ## these women are ineligible based on CD4, but eligible based on
      ## pregnancy status

## simulate fake network
   fit.fake.net <- activate.vertices(fit.fake.net, onset=1, terminus=Inf)
   fit.fake.net <- activate.edges(fit.fake.net, onset=1, terminus=Inf)

for (time in 2:2){
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

  fit.fake.net <- update.treatment(fit.fake.net, verbose=TRUE,
                                     scenario="baseline",
                                     dur.inf=dur.inf,
                                     eligibile.cd4=eligible.cd4,
                                     baseline.cd4.at.art.initiation.men =
                                     baseline.cd4.at.art.initiation.men,
                                     baseline.cd4.at.art.initiation.women=
                                     baseline.cd4.at.art.initiation.women,
                                     ##baseline.time.art.initiation.preg.women=
                                     ##23*7,
                                     baseline.f.ges.visit=
                                     ##23*7/14,
                                     0,
                                     optA.thres=350,
                                     cd4.at.infection.male=
                                     cd4.at.infection.male,
                                     cd4.at.infection.female=
                                     cd4.at.infection.female,
                                     ## per.day.untreated.cd4.decline=
                                     ## per.day.untreated.cd4.decline,
                                     cd4.recovery.time=cd4.recovery.time,
                                     per.day.cd4.recovery=per.day.cd4.recovery,
                                     acute.length=acute.length,
                                     chronic.length=chronic.length, 
                                     late.length=late.length, 
                                     time.infection.to.peak.viral.load=14,
                                     size.of.timestep=size.of.timestep, #28May13
                                     ## optA.sc.art.vl.perstep.dec, #5Jul13
                                     ## optA.sc.art.vl.perstep.inc, #5Jul13
                                        optA.vl.reduction=optA.vl.reduction, ##5Jul13
                                   full.term=full.term,
                                   undetectable.vl=undetectable.vl,
                                   time.to.full.supp=time.to.full.supp
           )

      fit.fake.net%v%"time.since.infection" <- (fit.fake.net%v%"time.since.infection")+1
      fit.fake.net%v%"time.since.art.initiation" <- (fit.fake.net%v%"time.since.art.initiation")+1
      fit.fake.net%v%"age" <- (fit.fake.net%v%"age")+size.of.timestep/365

  cat("\n")


}

 ### check all specified attributes
      fit.fake.net%v%"art.status"
      fit.fake.net%v%"art.type"
      fit.fake.net%v%"time.since.art.initiation"
      fit.fake.net%v%"time.of.art.initiation"

      fit.fake.net%v%"viral.load.today"
      fit.fake.net%v%"inf.status"
      fit.fake.net%v%"time.since.infection"
      fit.fake.net%v%"age"

#####################################################
### Test Option A for pregnant women
### with CD4>350, do they start scART?
#####################################################

      fit.fake.net <- fit.fake$network
## Needed attributes
      # general
      fit.fake.net%v%"inf.status" <- 1
      fit.fake.net%v%"time.since.infection" <- 0
      fit.fake.net%v%"cd4.count.today" <- c(rep(101, num.male), rep(400, num.female))
      fit.fake.net%v%"vl.art.traj.slope" <- 0.1
      fit.fake.net%v%"art.status" <- 0
      fit.fake.net%v%"art.type" <- NA
      fit.fake.net%v%"time.since.art.initiation" <- NA
      fit.fake.net%v%"stage.of.infection" <- NA
      # pregnancy
      fit.fake.net%v%"curr.pregnancy.status" <- c(rep(NA, num.male), rep(1, num.female))
      fit.fake.net%v%"time.since.curr.pregnancy" <- c(rep(NA, num.male),
                                                      rep(1, num.female))
      ## these women are ineligible based on CD4, but eligible based on
      ## pregnancy status

## simulate fake network
   fit.fake.net <- activate.vertices(fit.fake.net, onset=1, terminus=Inf)
   fit.fake.net <- activate.edges(fit.fake.net, onset=1, terminus=Inf)

for (time in 2:2){
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

  fit.fake.net <- update.treatment(fit.fake.net, verbose=TRUE,
                                     scenario="baseline",
                                     dur.inf=dur.inf,
                                     eligibile.cd4=eligible.cd4,
                                     baseline.cd4.at.art.initiation.men =
                                     baseline.cd4.at.art.initiation.men,
                                     baseline.cd4.at.art.initiation.women=
                                     baseline.cd4.at.art.initiation.women,
                                     ##baseline.time.art.initiation.preg.women=
                                     ##23*7,
                                     baseline.f.ges.visit=
                                     ##23*7/14,
                                     0,
                                     optA.thres=350,
                                     cd4.at.infection.male=
                                     cd4.at.infection.male,
                                     cd4.at.infection.female=
                                     cd4.at.infection.female,
                                     ## per.day.untreated.cd4.decline=
                                     ## per.day.untreated.cd4.decline,
                                     cd4.recovery.time=cd4.recovery.time,
                                     per.day.cd4.recovery=per.day.cd4.recovery,
                                     acute.length=acute.length,
                                     chronic.length=chronic.length, 
                                     late.length=late.length, 
                                     time.infection.to.peak.viral.load=14,
                                     size.of.timestep=size.of.timestep, #28May13
                                     ## optA.sc.art.vl.perstep.dec, #5Jul13
                                     ## optA.sc.art.vl.perstep.inc, #5Jul13
                                        optA.vl.reduction=optA.vl.reduction, ##5Jul13
                                   full.term=full.term,
                                   undetectable.vl=undetectable.vl,
                                   time.to.full.supp=time.to.full.supp
           )

      fit.fake.net%v%"time.since.infection" <- (fit.fake.net%v%"time.since.infection")+1
      fit.fake.net%v%"time.since.art.initiation" <- (fit.fake.net%v%"time.since.art.initiation")+1
      fit.fake.net%v%"age" <- (fit.fake.net%v%"age")+size.of.timestep/365

  cat("\n")


}

 ### check all specified attributes
      fit.fake.net%v%"art.status"
      fit.fake.net%v%"art.type"
      fit.fake.net%v%"time.since.art.initiation"
      fit.fake.net%v%"time.of.art.initiation"

      fit.fake.net%v%"viral.load.today"
      fit.fake.net%v%"inf.status"
      fit.fake.net%v%"time.since.infection"
      fit.fake.net%v%"age"
