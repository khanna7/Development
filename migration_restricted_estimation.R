rm(list=ls())
set.seed(Sys.time())
library(ergm)

## 13 Feb 2013:
## Copied from ~/Desktop/Documents/MastersThesis/Manuscript/Manuscript_Stochastic_Model/Code/Migration_Code/Restricted_DetMigrations/est_dur100_meandeg12.R

#####################################################
### Progression of versions
#####################################################

## 23 April 202: modified to have just duration 100 (not mixed durations)
## since we are not estimating k-star terms > order 3 now.

## 22 April 2012: Discussion w. Steve: Higher than 2-star for k-stars
## is not necessary

## 20 April 2012: Transactional Model: Mean degree 1.2
### But non-structural zero has to be left out of mixing matrix

## 16 April 2012: Shorter average partnership duration
## in urban area
## for migrant-men with urban women, average partnership duration
## is 25 100 weeks. for other categories, urban men -- urban women,
## rural men -- rural women and migrant men--rural women average duration
## is 25 weeks

## 2 March 2012: Organized in folder for final runs

## 30 Jan 2012: distribution of infecteds has to be redefined
## because we late-stage has been reduced to 9 months

## 27 Jan 2012:  Add new ergm.term to have a 'restricted' partnership
## structure for migrant-men.

## 20 Jan 2012: Have 'time.infected' (time since infection)
## for infected people be uniformly
## distributed between 0 and life-span of infected individuals (600 weeks)

### 13 Jan 2012: Fix mean degree after discussion with Steve
### Still have 10 urban and 10 rural infected at outset

## 10 Jan 2012: Fix dyn interval
## after discussion with Steve

## Need to move away from
## bipartite graphs I think to
## do births and deaths correctly
#####################################################

#####################################################
### Set up population
#####################################################
N <- 1000*5

N.M.R<- 1/8*N
N.M.M <- 1/4*N # Ditto migrant
N.M.U<- 1/8*N # Ditto urban
N.M<-N.M.R+N.M.M+N.M.U # Total number of males

N.F.R<- 1/4*N # Number of rural females
N.F.M<-0 # Ditto migrant
N.F.U<-1/4*N # Ditto urban
N.F<-N.F.R+N.F.M+N.F.U # Total number of females
#####################################################

#####################################################
### Construct the empty network.
#####################################################
n0<-network.initialize(N,bipartite=FALSE,directed=FALSE)

#####################################################
### Add Atributes
#####################################################

## Type
n0 %v% "type" <- rep(c("A-MR", "B-MM", "C-MU", "D-FR", "F-FU"),
                                        #s for correct ordering
                                        # left out female migrant cell, 5 FM
                                        # because
                                        # it was confusing
                                        # change from masters
                     c(N.M.R, N.M.M, N.M.U, N.F.R, N.F.U)
                     )
## In order these are
### Number-Male-Rural, Number-Male-Migrant, Number-Male-Urban,
### Number-Female-Rural, Number-Female-Migrant, Number-Female-Urban

## location
n0 %v% "location" <- c(rep(0, N.M.R), #0=rural
                       rep(0, N.M.M/2), #1=urban
                       rep(1, N.M.M/2),
                       rep(1, N.M.U),
                       rep(0, N.F.R),
                       rep(1, N.F.U)
                       )


## infection status
n0 %v% "inf.status" <- rep(0, N)
n.init.infected.women <- 200
to.be.infected.urbanwomen <- sample(2501:3750, n.init.infected.women/2)
to.be.infected.ruralwomen <- sample(3751:5000, n.init.infected.women/2)
init.infected.women <- c(to.be.infected.urbanwomen,
                         to.be.infected.ruralwomen)
set.vertex.attribute(n0, "inf.status", 1, init.infected.women)

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
n.edges <- N*mean.deg/2 # change mean degree to 1
MF.type.mix <- c(0,
                 rep(0,2),
                 rep(0,3),
                 (1/6*n.edges), rep(0,2), # term 8 left out
                 0, (1/3*n.edges), (1/6*n.edges), 0, 0)

kstar.mmr.mix.2star <- 0
kstar.mmu.mix.2star <- 0

meanstats  <- c(n.edges, MF.type.mix, 
                c(rep(kstar.mmr.mix.2star,1)), # only 2-star terms
                                        # are needed
                c(rep(kstar.mmu.mix.2star, 1))
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
maxit = 100
burnin.sim.network=25e3
#burnin=25e3
burnin=25e4 # after discssion with steve
#####################################################

#####################################################
## Initiate Network
#####################################################

san.obj <- n0 ~  edges+nodemix("type", base=c(8))+
  kstar(2, "migmalemix.rural")+ # more than 2-star terms not needed
  kstar(2, "migmalemix.urban")



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
formula.network <- init.network ~ edges+nodemix("type", base=c(8))+
  kstar(2, "migmalemix.rural")+ # only k 2-stars are needed
  kstar(2, "migmalemix.urban")

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
save.image(file="est_dur100_meandeg12.RData")
#####################################################
