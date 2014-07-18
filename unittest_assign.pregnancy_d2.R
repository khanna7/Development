#####################################################
### Progression of versions
#####################################################

## 10 Jul 2013: Lost some matter at top, recreated the file
## upto line 102 today.

## 9 Jul 2013: Tested delivery upong completion of full term yesterday.
## Test pregnancy model today.

## 8 Jul 2013: Test assign.pregnancy function.
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

source("assign.pregnancy_d2.R")

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

## needed parameters
    full.term=40/14 
    preg.prob=1
    min.preg.interval=15*30/14
    optA.vl.reduction=1.1
    sc.art.postcess.ret.bl=12
    verbose <- TRUE

## Needed attributes
    fit.fake.net%v%"curr.pregnancy.status" <- 1
    fit.fake.net%v%"time.since.curr.pregnancy" <- full.term+1
    fit.fake.net%v%"time.of.curr.pregnancy" <- 0
    fit.fake.net%v%"time.since.last.pregnancy" <- NA
    fit.fake.net%v%"art.status" <- 0
    fit.fake.net%v%"art.type" <- NA
    fit.fake.net%v%"vl.art.traj.slope" <- NA
  
## simulate fake network
   fit.fake.net <- activate.vertices(fit.fake.net, onset=1, terminus=Inf)
   fit.fake.net <- activate.edges(fit.fake.net, onset=1, terminus=Inf)

#####################################################

#####################################################
### test removal on account of AIDS
#####################################################

##debug(update.vital.dynamics)

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

  preg.testnet.obj <- assign.pregnancy(fit.fake.net, verbose=TRUE,
                                       full.term=full.term,
                                       preg.prob=preg.prob,
                                       min.preg.interval=min.preg.interval,
                                       optA.vl.reduction=optA.vl.reduction, 
                                       sc.art.postcess.ret.bl=sc.art.postcess.ret.bl
                                       )

}

    ### test if all needed attributes are updated
    preg.testnet.obj%v%"curr.pregnancy.status"
    preg.testnet.obj%v%"time.since.curr.pregnancy"
    preg.testnet.obj%v%"time.of.curr.pregnancy"
    preg.testnet.obj%v%"time.since.last.pregnancy"
    preg.testnet.obj%v%"art.status"
    preg.testnet.obj%v%"art.type"
    preg.testnet.obj%v%"vl.art.traj.slope"
   ## suggests that all deliveries are modeled fine 
#####################################################

#####################################################
### Test pregnancy model
#####################################################

### Network objects and edgelist
fit.fake.net
fit.fake.el <- as.edgelist(network.extract(fit.fake.net,
                                           at = time,
                                           retain.all.vertices = T))

### Other needed data
curr.pregnancy.status <- fit.fake.net%v%"curr.pregnancy.status"
female.id.curr <- nwmodes(fit.fake.net, 2)
id.female.in.rel <- which(female.id.curr %in% fit.fake.el[,2])
female.in.rel <- female.id.curr[id.female.in.rel]


   ### Check network objects being used
   for (i in 1:length(female.in.rel)){
      cat(curr.pregnancy.status[female.in.rel[i]] == 0, "\n")
    } ## fake-network where all women are pregnant

     for (i in 1:length(female.in.rel)){
      cat((preg.testnet.obj%v%"curr.pregnancy.status")[female.in.rel[i]] == 0, "\n")
    } ## fake-network where all pregnancies were delivered

### Unit test I for pregnancy
min.preg.interval = -1
test.pregmodel.net <- assign.pregnancy(preg.testnet.obj,
                                       full.term=full.term,
                                       preg.prob=1,
                                       min.preg.interval=min.preg.interval,
                                       optA.vl.reduction=optA.vl.reduction,
                                       sc.art.postcess.ret.bl=sc.art.postcess.ret.bl
                                       ) ## prob=1 for pregnancy
  test.pregmodel.net%v%"curr.pregnancy.status"
  test.pregmodel.net%v%"time.since.curr.pregnancy"
  test.pregmodel.net%v%"time.of.curr.pregnancy"
  test.pregmodel.net%v%"time.since.last.pregnancy"
  test.pregmodel.net%v%"art.status"
  test.pregmodel.net%v%"art.type"
  test.pregmodel.net%v%"vl.art.traj.slope"

### Unit test II for pregnancy
test.pregmodel.net2 <- assign.pregnancy(preg.testnet.obj,
                                       full.term=full.term,
                                       preg.prob=0,
                                       min.preg.interval=min.preg.interval,
                                       optA.vl.reduction=optA.vl.reduction,
                                       sc.art.postcess.ret.bl=sc.art.postcess.ret.bl
                                       ) ## prob=0 for pregnancy
  test.pregmodel.net2%v%"curr.pregnancy.status"
  test.pregmodel.net2%v%"time.since.curr.pregnancy"
  test.pregmodel.net2%v%"time.of.curr.pregnancy"
  test.pregmodel.net2%v%"time.since.last.pregnancy"
  test.pregmodel.net2%v%"art.status"
  test.pregmodel.net2%v%"art.type"
  test.pregmodel.net2%v%"vl.art.traj.slope"

### Unit test III for pregnancy
test.pregmodel.net3 <- assign.pregnancy(preg.testnet.obj,
                                       full.term=full.term,
                                       preg.prob=0.5,
                                       min.preg.interval=min.preg.interval,
                                       optA.vl.reduction=optA.vl.reduction,
                                       sc.art.postcess.ret.bl=sc.art.postcess.ret.bl
                                       ) ## prob=0 for pregnancy
  test.pregmodel.net3%v%"curr.pregnancy.status"
  test.pregmodel.net3%v%"time.since.curr.pregnancy"
  test.pregmodel.net3%v%"time.of.curr.pregnancy"
  test.pregmodel.net3%v%"time.since.last.pregnancy"
  test.pregmodel.net3%v%"art.status"
  test.pregmodel.net3%v%"art.type"
  test.pregmodel.net3%v%"vl.art.traj.slope"

#####################################################
