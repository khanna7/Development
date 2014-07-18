rm(list=ls())
##library(ergm)

library(tergm)
source("../simulate.networkDynamic.R")
#####################################################
### Progression of versions
#####################################################

## 12 Feb 2013: Carnegie approximation does not seem to
## work for really long durations. If partnership duration is 100,
## the mean stats are right on. If partnership duration is 2000, mean stats
## for the edges term are off, and also for the mean partnership duration.

## 8 Feb 2013: Start coding to get network right
## Follow Google's Style Guide
## Learn about Unit-Testing
## Remember the set.vertex.attribute function
#####################################################

#####################################################
### Set up population
### and initialize network
#####################################################
N <- 500
num.male <- N/2
num.female <- N/2

n0 <- network.initialize(N,bipartite=num.male, directed=F)

#####################################################

#####################################################
### Add Atributes
#####################################################

###############
### Demorgaphy 
##############

## Sex, Age, Circumcision Status, 
## Pregnancy Status, 
## Number of Days Since Last Pregnancy, 
## Number of Days Since Last Delivery
## Currently Breastfeeding, 
## Number of Days Since Last Breastfeeding
## If Alive
## Number of Days Since Dropping Out of Treatment, 
## Number of Days Since Arrival into Population,
## Number of Days Since Departure from Population


## Sex
n0 %v% "sex" <- rep(c("male", "female"),
                    c(num.male, num.female)
                     )
male.id <- which(n0%v%"sex"=="male")
female.id <- which(n0%v%"sex"=="female")

## age
## (uniform for now)
min.age <- 15
max.age <- 54
age <- runif(N, min.age, max.age)
age <- as.integer(age)
n0 %v% "age" <- age


## circumcision status
circum.rate <- 0.125 ## Tailor to ZA, UG
circum.status <- rbinom(N/2, 1, circum.rate)
n0 %v% "circum.status" <- c(circum.status, rep(NA, N/2))

## pregnancy status
n0 %v% "pregnancy.status" <- NA
##defeault pregnancy status is NA,
## overwritten with age specific fertility rates for women below

asfr.15to19 <- 0.1 ## Age-Specific Fertility Rate
asfr.20to24 <- 0.1 ## Tailor each to ZA, UG
asfr.25to29 <- 0.1
asfr.30to34 <- 0.1
asfr.35to39 <- 0.1
asfr.40to44 <- 0.1
asfr.45to49 <- 0.1

## will have to move this to after infection status,
## to account for lower fertility rate in HIV diagnosed women

fem.age.15to19 <- intersect(which((n0%v%"age") <= 19 ),
                            which((n0%v%"sex") == "female"))
fem.age.20to24 <- intersect(intersect(which((n0%v%"age") >= 20),
                                      which((n0%v%"age") <= 24)),
                            which((n0%v%"sex") == "female"))
fem.age.25to29 <- intersect(intersect(which((n0%v%"age") >= 25),
                                      which((n0%v%"age") <= 29)),
                            which((n0%v%"sex") == "female"))
fem.age.30to34 <- intersect(intersect(which((n0%v%"age") >= 30),
                                      which((n0%v%"age") <= 39)),
                            which((n0%v%"sex") == "female"))
fem.age.35to39 <- intersect(intersect(which((n0%v%"age") >= 35),
                                      which((n0%v%"age") <= 39)),
                            which((n0%v%"sex") == "female"))
fem.age.40to44 <- intersect(intersect(which((n0%v%"age") >= 40),
                                      which((n0%v%"age") <= 44)),
                            which((n0%v%"sex") == "female"))
fem.age.45to49 <- intersect(intersect(which((n0%v%"age") >= 45),
                                      which((n0%v%"age") <= 49)),
                            which((n0%v%"sex") == "female"))


set.vertex.attribute(n0, "pregnancy.status",
                     rbinom(length(fem.age.15to20), 1, asfr.15to19),
                     fem.age.15to19)
set.vertex.attribute(n0, "pregnancy.status", 
                     rbinom(length(fem.age.20to24), 1, asfr.20to24),
                     fem.age.20to24)
set.vertex.attribute(n0, "pregnancy.status", 
                     rbinom(length(fem.age.25to29), 1, asfr.25to29),
                     fem.age.25to29)
set.vertex.attribute(n0, "pregnancy.status", 
                     rbinom(length(fem.age.30to34), 1, asfr.30to34),
                     fem.age.30to34)
set.vertex.attribute(n0, "pregnancy.status", 
                     rbinom(length(fem.age.35to39), 1, asfr.35to39),
                     fem.age.35to39)
set.vertex.attribute(n0, "pregnancy.status", 
                     rbinom(length(fem.age.40to44), 1, asfr.40to44),
                     fem.age.40to44)
set.vertex.attribute(n0, "pregnancy.status", 
                     rbinom(length(fem.age.45to49), 1, asfr.45to49),
                     fem.age.45to49)

intersect(female.id, which(n0%v%"age" > 49))
## NA for pregnancy in women is
## for those above 49 years of age
         
## number of days since last pregnancy
num%v%"num.days.last.preg" <- NA

## number of days since last delivery
num%v%"num.days.last.delivery" <- NA

## number of children
num%v%"num.days.last.delivery" <- NA

## currently breastfeeding --
## (if less than 6 months since last delive, currently breastfeeding)
num%v%"breast.feeding" <- NA

##number of days since last breastfeeding
num%v%"num.days.last.breast.feeding" <- NA

###############
### Biology 
##############

## Infection Status, 
init.hiv.prev <- 0.20 # randomly infect 20% of the population at the start?
init.inf.status <- rbinom(network.size(n0), 1, init.hiv.prev)
set.vertex.attribute(n0, "inf.status", init.inf.status,
                     )

## Time Since Infection, 
duration.of.infection <- 5000  ## in days, modify later
init.infected <- which(init.inf.status == 1)
time.since.infection <- sample(duration.of.infection, size=length(init.infected),
                               replace=TRUE)
set.vertex.attribute(n0, "inf.status", init.inf.status,
                     init.infected
                     )

## Stage of Infection, 
acute.length <- 1:90 ## modify
chronic.length <- 91:4000 ## modify
late.length <- 4001:5000 ## modify

time.and.stage <- cbind(time.since.infection, stage.of.infection)

classify.stage <- function(x){
  if (x %in% acute.length){
    stage.of.infection <- "Acute"
  } else  if (x %in% chronic.length){
    stage.of.infection <- "Chronic"
  } else  if (x %in% late.length){
    stage.of.infection <- "Late"
  }
  return(stage.of.infection)
}

stage.of.infection <- lapply(time.since.infection, classify.stage)
stage.of.infection <- unlist(stage.of.infection)

## Diagnosis Status,
male.testing.rate <- 0.50 # modify
female.testing.rate <- 0.50 # modify

infected.men <- intersect(init.infected, male.id)
infected.women <- intersect(init.infected, female.id)

diagnosis.status.men <- rbinom(infected.men, 1, male.testing.rate)
diagnosis.status.women <- rbinom(infected.women, 1, female.testing.rate)

diagnosis.status <- c(diagnosis.status.men, diagnosis.status.women)

set.vertex.attribute(n0, "diagnosis.status", diagnosis.status,
                     init.infected)

## Time since Diagnosis,
## if daily probability of 

## ART Access (Ever Accessed or Will Access Treatment)

## ART Status (Whether currently on treatment)

## ART Adherence (If negative, reset Treatment Status to 0)

## Viral Load, -- if on treatment, set to 0

## HSV Status -- calibrate using Phase 2 data

#####################################################
### Network Parameters
#####################################################

## Statistics: 
## Edgecount, Partnership Duration, Degree Distribution
##mean.degree <- 1 # modify
##n.edges <- N*mean.degree/

## male.deg.dist <- c(0.68, 0.27, 0.05, 0.00)
## female.deg.dist <- c(0.65, 0.30, 0.04, 0.01)
male.degree.dist <- c(0.48,0.41,0.08,0.03)
female.degree.dist <- c(0.40, 0.55, 0.04, 0.01)
##male.degree.dist <- c(0.2, 0.3) # modify
##female.degree.dist <- c(0.1, 0.65)

male.degreecounts <- male.degree.dist*num.male
female.degreecounts <- female.degree.dist*num.female

n.edges <- 0*female.degreecounts[1] + 1*female.degreecounts[2] + 2*female.degreecounts[3] + 3*female.degreecounts[4]

##0*male.degreecounts[1] + 1*male.degreecounts[2] + 2*male.degreecounts[3] + 3*male.degreecounts[4]

partnership.duration <- 2000 # modify

formation.meanstats <- c(n.edges, male.degreecounts[1:2],
                          female.degreecounts[1:2])

#####################################################

#####################################################
### Generate Network
#####################################################
        formation <- ~edges+b1degree(0:1)+b2degree(0:1)
	formation.n0 <- update.formula(formation,n0~.)
	dissolution <- ~offset(edges)
        theta.diss <- log(partnership.duration-1)
	target.stats <- c(formation.meanstats)
        constraints	<- ~.

	##n0 <- network.initialize(N,bipartite=num.male,directed=F)

        ## Cross-Sectional Network and Diagnostics
	fit <- ergm(formation.n0,target.stats=target.stats,
                    constraints=constraints)
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

	sim.stats <- attributes(hetdeg.diag.sim)$stats[101:5000,]
	stats.means <- colMeans(sim.stats)##mean(sim.stats)
	stats.sd <- apply(sim.stats,2,sd)##sd(sim.stats)##apply(sim.stats,2,sd)

	sim.df <- as.data.frame(hetdeg.diag.sim)
	partnership.duration.mean <- mean(sim.df$duration)
	partnership.duration.sd <- sd(sim.df$duration)
	partnership.duration.expected <- exp(theta.diss)+1

        ## Temporal Network and Diagnostics
        n0.stergm.500 <- stergm(n0,
                          formation = formation,
                          dissolution = dissolution,
                          targets = "formation",
                          target.stats = target.stats,
                          offset.coef.diss = theta.diss,
                          estimate = "EGMME",
                          control=control.stergm(SA.plot.progress=TRUE),
                          verbose=2
#		SA.phase2.levels.max=1)
)

        ## diagnostics on STERGM fit
        n0.stergm.500
        mcmc.diagnostics(n0.stergm.500) 
        summary(n0.stergm.500)
        n0.stergm.500 <- as.data.frame(n0.stergm.500)
#####################################################


## time of infection
n0 %v% "inf.time" <- rep(NA, N)
#set.vertex.attribute(n0, "inf.time", 0, c(2501,3751)) # 1 rural and urban female inf.
set.vertex.attribute(n0, "inf.time", 0, init.infected.women)


## time since infection
n0 %v% "time.infected" <- rep(NA, N)
#set.vertex.attribute(n0, "time.infected", 0, c(2501,3751)) # 1 rural and urban female inf.
##lifespan.inf.ind <- 616
lifespan.inf.ind <- 12+500+40 # changed after discussion w. steve
init.time.infected <- runif(length(c(init.infected.women)),
                            min=0, max=lifespan.inf.ind)
set.vertex.attribute(n0, "time.infected", init.time.infected,
                     c(init.infected.women)) # for testing


## infector.id
n0 %v% "infector.id" <- rep(NA, N)
#set.vertex.attribute(n0, "infector.id",
#                     0, c(2501,3751)) # 1 rural and urban female inf.
set.vertex.attribute(n0, "infector.id", 0, init.infected.women)

## migrant-male mixing
n0 %v% "migmalemix.rural" <- rep(c("A-X", "B-MMR.MIX",
                                   "C-Y", "B-MMR.MIX", "D-Z"),
                                 c(N.M.R, N.M.M, N.M.U, N.F.R, N.F.U)
                                 )

## migrant-male mixing
n0 %v% "migmalemix.urban" <- rep(c("1X", "2MMU.MIX",
                                   "3Y", "4Z", "2MMU.MIX"),
                                 c(N.M.R, N.M.M, N.M.U, N.F.R, N.F.U)
                                 )

#####################################################

#####################################################
## Behavior
#####################################################
duration <- 100
#####################################################

#####################################################
### Mean-Statistics for Network
#####################################################
mean.deg <- 1.2
n.edges <- N*mean.deg/2 # change mean degree to 1.2
MF.type.mix <- c(0,
                 rep(0,2),
                 rep(0,3),
                 (1/6*n.edges), rep(0,2), # term 8 left out
                 0, (1/3*n.edges), (1/6*n.edges), 0, 0)

meanstats  <- c(n.edges, MF.type.mix # no k-star terms in this version
                )
#####################################################

#####################################################
### Other Network Information
#####################################################
diss.network <- ~edges
gamma.network <- log(duration-1)

## constraints.network <- ~.
constraints.network <- ~.
constraints.degree <- ~bd(maxout=1) # not used
##dyninterval <- 1e4
dyninterval <- 1e6
maxit = 10
burnin.sim.network=25e3
#burnin=25e3
burnin=25e4 # after discssion with steve
#####################################################

#####################################################
## Initiate Network
#####################################################

san.obj <- n0 ~  edges+nodemix("type", base=c(8))

init.network <- san(san.obj, meanstats=meanstats,
                    interval=1000, # from ampham
                    MCMC.samplesize=1e4, # from ampham
                    burnin=burnin, # from ampham
                    maxit=maxit,
                    constraints=constraints.network)

#####################################################

#####################################################
### estimate  network
#####################################################
formula.network <- init.network ~ edges+nodemix("type", base=c(8))
                                        # no k-star terms

migration.fit.network <- ergm(formula=formula.network,
                              control=control.ergm(dyninterval=
                                dyninterval), # 10 jan '12: from steve
                              meanstats=meanstats,
                              interval=1000, # from ampham
                              MCMC.samplesize=1e4, # from ampham
                              burnin=burnin, # from ampham
                              maxit=maxit, # from ampham
                              verbose=T) # from ampham

#####################################################

#####################################################
### test -- simulation from cross-sectional network
### not needed
#####################################################

## simulate from cross-sectional network (to test if mean-stats make sense)
## sim.networks.take1<-simulate(migration.fit.network,
##                              nsim=100,statsonly=FALSE,sequential=TRUE)

## apply(sim.networks.take1$stats,2,mean)
#####################################################

#####################################################
### apply nicole's approximation
#####################################################
theta0 <- migration.fit.network$coef
theta0[1] <- theta0[1]-gamma.network
migration.fit.network$coef <- theta0

#####################################################
### Make offset
#####################################################
offset.edges.network <- -log(network.size(init.network))
theta.network <- c((migration.fit.network$coef[1]-# error was right here!!!!
                    offset.edges.network),
                   migration.fit.network$coef[-1], 
                   offset.edges.network
                   )
#####################################################

## Reset theta.network 22 April 2012 -- for k-star terms
## mig-malemix.urban: 7-star and 9-star because they are positive
## and 0 respectively -- decided to not do
## theta.network[29] <- -10
## theta.network[31] <- -10
##theta.network[16:31] <- -1e4

names(theta.network) <- c(names(ergm(formula.network, MPLEonly=T)$coef),
                          "offset.edges"
                          )

formula.network.w.offset <- update.formula(formula.network,
                                           ~.+offset(edges)
                                           )

## check attributes in estimated network via
## net <- migration.fit.network$network
## get.vertex.attribut(net, "")

#####################################################

#####################################################
### Save Object
#####################################################
##save.image(file="est_dur100_meandeg12.RData")
#####################################################


