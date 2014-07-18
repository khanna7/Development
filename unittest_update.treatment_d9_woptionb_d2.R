#####################################################
### Progression of versions
#####################################################

## 20 Dec 2013: Test Option B code. 

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
source("update.treatment_d9_wpmtctb+_optionb_d2.R") #20Dec2013
source("compute.cd4.count_d2.R")
source("compute.viral.load_d6_d3.R")
source("assign.pregnancy_d7d1.R")


#####################################################
## Make fake network
#####################################################

## needed parameters
size.of.timestep=14
#scenario="baseline"
scenario="option.b"

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

min.preg.interval = 15*30/14
sc.art.postcess.ret.bl = 10
num.births.per1k.byage = rep(200, 7)
prop.stillbirth = 0.0001
inf.preg.red = 0.53
optB.cess.time = 12*30/14

##date <- paste("20Dec2013.debugoptionb.")
date <- paste("7Jan2014.debugoptionb.")

  ## fake network
  num.male <- 50
  num.female <- 50
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

  net%v%"sex" <- c(rep(0, 5), rep(1, 5))
  fit.fake <- ergm(formation.net,
                   target.stats=target.stats,
                   constraints=constraints)
  theta.form <- fit.fake$coef 
  theta.form[1] <- theta.form[1] - theta.diss
  fit.fake.net <- fit.fake$network




#####################################################


#####################################################

#####################################################
###################################################
### Check initiation of ART on a/c of
### PMTCT Option B
###################################################

  fit.fake.net <- fit.fake$network
## Needed attributes
      fit.fake.net%v%"sex" <- c(rep(0, num.male),
                                rep(0, num.female)
                                )

      fit.fake.net%v%"inf.status" <- 1
      fit.fake.net%v%"time.since.infection" <- 0
      fit.fake.net%v%"cd4.count.today" <- 1e3
      fit.fake.net%v%"vl.art.traj.slope" <- 0.1
      fit.fake.net%v%"stage.of.infection" <- NA

      fit.fake.net%v%"age" <- sample(c(18:45), N, replace=TRUE)
      fit.fake.net%v%"art.status" <- 0
      fit.fake.net%v%"art.type" <- NA
      fit.fake.net%v%"time.since.art.initiation" <- NA
      fit.fake.net%v%"curr.pregnancy.status" <- c(rep(0, num.male),
                                                  rep(1, num.female)
                                                    )
      fit.fake.net%v%"time.since.curr.pregnancy" <- c(rep(NA, num.male),
                                                      rep(25*7/14, num.female)
                                                      )

      fit.fake.net%v%"time.since.last.pregnancy" <- NA
      fit.fake.net%v%"preg.covered" <- 1

## simulate fake network
   fit.fake.net <- activate.vertices(fit.fake.net, onset=1, terminus=Inf)
   fit.fake.net <- activate.edges(fit.fake.net, onset=1, terminus=Inf)

for (time in 2:10){
  cat("Completing timestep ", time, "\n")
  
  fit.fake.net <- simulate(fit.fake.net,
                           formation=formation,
                           dissolution=dissolution,
                           coef.form=theta.form,
                           coef.diss=theta.diss,
                           constraints=constraints,
                           time.start=time,
                           time.slices=1,
                           control=control.simulate.network(MCMC.burnin=10000)
                                              )


     fit.fake.net <- assign.pregnancy(fit.fake.net, verbose=TRUE,
                                    full.term=full.term,
                                        # preg.prob, 
                                    min.preg.interval=min.preg.interval,
                                    optA.vl.reduction=optA.vl.reduction, #12Jun13
                                    sc.art.postcess.ret.bl=sc.art.postcess.ret.bl, #12Jun13
                                    num.births.per1k.byage=num.births.per1k.byage, #2Sep13
                                    prop.stillbirth=prop.stillbirth, #2Sep13
                                    inf.preg.red=inf.preg.red, #2Sep13
                                    optB.cess.time=optB.cess.time, #20Dec13
                                    scenario=scenario#20Dec13
                                    )

  fit.fake.net <- update.treatment(fit.fake.net, verbose=TRUE,
                                   scenario=scenario,
                                   dur.inf=dur.inf,
                                   eligibile.cd4=eligible.cd4,
                                   baseline.cd4.at.art.initiation.men = 
                                   baseline.cd4.at.art.initiation.men,
                                   baseline.cd4.at.art.initiation.women=
                                   baseline.cd4.at.art.initiation.women,
                                   baseline.f.ges.visit=
                                   baseline.f.ges.visit,
                                   optA.thres=optA.thres,
                                   cd4.at.infection.male=
                                   cd4.at.infection.male,
                                   cd4.at.infection.female=
                                   cd4.at.infection.female,
                                   cd4.recovery.time=cd4.recovery.time,
                                   per.day.cd4.recovery=per.day.cd4.recovery,
                                   time.infection.to.peak.viral.load=
                                   time.infection.to.peak.viral.load,
                                   size.of.timestep=size.of.timestep, 
                                   optA.vl.reduction=optA.vl.reduction, 
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

###################################################

###################################################
### Check cessation of ART
###################################################
      fit.fake.net <- fit.fake$network
## Needed attributes
      fit.fake.net%v%"sex" <- c(rep(0, num.male),
                                rep(0, num.female)
                                )

      fit.fake.net%v%"inf.status" <- 1
      fit.fake.net%v%"time.since.infection" <- 0
      fit.fake.net%v%"cd4.count.today" <- 1e3
      fit.fake.net%v%"vl.art.traj.slope" <- 0.1
      fit.fake.net%v%"stage.of.infection" <- NA

      fit.fake.net%v%"age" <- sample(c(18:45), N, replace=TRUE)
      fit.fake.net%v%"art.status" <- c(rep(0, num.male), 
                                       rep(1, num.female)
                                       )
      fit.fake.net%v%"art.type" <- c(rep(NA, num.male), 
                                     rep(4, num.female)
                                     )
      fit.fake.net%v%"time.since.art.initiation" <- NA

      fit.fake.net%v%"curr.pregnancy.status" <- 0
      fit.fake.net%v%"time.since.curr.pregnancy" <- NA

      fit.fake.net%v%"time.since.last.pregnancy" <- c(rep(NA, num.male),
                                                      rep(20*30/14, num.female)
                                                      )

      fit.fake.net%v%"preg.covered" <- 1

## simulate fake network
   fit.fake.net <- activate.vertices(fit.fake.net, onset=1, terminus=Inf)
   fit.fake.net <- activate.edges(fit.fake.net, onset=1, terminus=Inf)

for (time in 2:10){
  cat("Completing timestep ", time, "\n")
  
  fit.fake.net <- simulate(fit.fake.net,
                           formation=formation,
                           dissolution=dissolution,
                           coef.form=theta.form,
                           coef.diss=theta.diss,
                           constraints=constraints,
                           time.start=time,
                           time.slices=1,
                           control=control.simulate.network(MCMC.burnin=10000)
                                              )


     fit.fake.net <- assign.pregnancy(fit.fake.net, verbose=TRUE,
                                    full.term=full.term,
                                        # preg.prob, 
                                    min.preg.interval=min.preg.interval,
                                    optA.vl.reduction=optA.vl.reduction, #12Jun13
                                    sc.art.postcess.ret.bl=sc.art.postcess.ret.bl, #12Jun13
                                    num.births.per1k.byage=num.births.per1k.byage, #2Sep13
                                    prop.stillbirth=prop.stillbirth, #2Sep13
                                    inf.preg.red=inf.preg.red, #2Sep13
                                    optB.cess.time=optB.cess.time, #20Dec13
                                    scenario=scenario#20Dec13
                                    )

  fit.fake.net <- update.treatment(fit.fake.net, verbose=TRUE,
                                   scenario=scenario,
                                   dur.inf=dur.inf,
                                   eligibile.cd4=eligible.cd4,
                                   baseline.cd4.at.art.initiation.men = 
                                   baseline.cd4.at.art.initiation.men,
                                   baseline.cd4.at.art.initiation.women=
                                   baseline.cd4.at.art.initiation.women,
                                   baseline.f.ges.visit=
                                   baseline.f.ges.visit,
                                   optA.thres=optA.thres,
                                   cd4.at.infection.male=
                                   cd4.at.infection.male,
                                   cd4.at.infection.female=
                                   cd4.at.infection.female,
                                   cd4.recovery.time=cd4.recovery.time,
                                   per.day.cd4.recovery=per.day.cd4.recovery,
                                   time.infection.to.peak.viral.load=
                                   time.infection.to.peak.viral.load,
                                   size.of.timestep=size.of.timestep, 
                                   optA.vl.reduction=optA.vl.reduction, 
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

###################################################
