rm(list=ls())
##library(ergm)

library(tergm)
source("../simulate.networkDynamic.R")
#####################################################
### Progression of versions
#####################################################

## 19 Feb 2013: After conversation with Sam, realized that
## "ergm" with Carnegie approximation will work instead of the "tergm"
## function, which takes much longer to fit -- and did not seem
## to be working with b1degre and b2degree anyway.
## Take infection status (in addition to ASFR)
## into account while defining pregnancy.

## 12 Feb 2013: Carnegie approximation does not seem to
## work for really long durations. If partnership duration is 100,
## the mean stats are right on. If partnership duration is 2000, mean stats
## for the edges term are off, and also for the mean partnership duration.
## Solution: Timeslices needs to be higher

## 8 Feb 2013: Start coding to get network right
## Follow Google's Style Guide
## Learn about Unit-Testing
## Remember the set.vertex.attribute function
#####################################################

#####################################################
### Set up population
### and initialize network
#####################################################
    N <- 5000
    num.male <- N/2
    num.female <- N/2

    male.deg.dist <- c(0.48,0.41,0.08,0.03)
    female.deg.dist <- c(0.40, 0.55, 0.04, 0.01)
    num.tot <- num.male + num.female

    male.deg.counts <- male.deg.dist*num.male
    female.deg.counts <- female.deg.dist*num.female

    female.deg.tot <- (0*female.deg.counts[1]) + (1*female.deg.counts[2]) +
                      (2*female.deg.counts[3]) + (3*female.deg.counts[4])
    male.deg.tot <- (0*male.deg.counts[1]) + (1*male.deg.counts[2]) +
                    (2*male.deg.counts[3]) + (3*male.deg.counts[4])

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
  ## If live
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
    ## defeault pregnancy status is NA,
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

## Move assigning pregnancy status to after infection status has been
## assigned.

    ## assign.pregnancy.status <- function(){
    ##   ## assign pregnancy as a function of relationship status
    ##   ## and infection status
    ##   el <- as.edgelist(n0)
    ##   women <- el[,1]
  

  
    set.vertex.attribute(n0, "pregnancy.status",
                         rbinom(length(fem.age.15to19), 1, asfr.15to19),
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
    n0%v%"num.days.last.preg" <- NA

    ## number of days since last delivery
    n0%v%"num.days.last.delivery" <- NA

    ## number of children
    n0%v%"num.days.last.delivery" <- NA

    ## currently breastfeeding --
    ## (if less than 6 months since last delive, currently breastfeeding)
    n0%v%"breast.feeding" <- NA

    ##number of days since last breastfeeding
    n0%v%"num.days.last.breast.feeding" <- NA

###############
### Biology 
##############

  ## Infection Status 
     init.hiv.prev <- 0.20 # randomly infect 20% of the population at the start?
     init.inf.status <- rbinom(network.size(n0), 1, init.hiv.prev)
     set.vertex.attribute(n0, "inf.status", init.inf.status,
                          )

     inf.status.men <- init.inf.status[male.id]
     inf.status.women <- init.inf.status[female.id]

  ## Time Since Infection 
     duration.of.infection <- 5000  ## in days, modify later
     init.infected <- which(init.inf.status == 1)
     time.since.infection.val <- sample(duration.of.infection, size=length(init.infected),
                                 replace=TRUE) 

     time.since.infection <- rep(NA, N)
     time.since.infection[which(init.inf.status == 1)] <- time.since.infection.val

     set.vertex.attribute(n0, "inf.status", init.inf.status,
                          init.infected
                          )
     set.vertex.attribute(n0, "time.since.infection", time.since.infection
                          )
  ## Stage of Infection 
  ## Classification necessary for deciding infectivity
  ## for chronically infected

     acute.length <- 1:90 ## modify
     chronic.length <- 91:4000 ## modify
     late.length <- 4001:5000 ## modify

     stage.of.infection <- rep(NA, N)
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

    stage.of.infection <- unlist(lapply(time.since.infection, classify.stage))

  ## Diagnosis Status,
     male.daily.testing.prob <- 0.50 # modify
     female.daily.testing.prob <- 0.50 # modify

     ##infected.men <- intersect(init.infected, male.id)
     ##infected.women <- intersect(init.infected, female.id)


     assign.diag.status <- function(inf.status){
       if (inf.status == 1){
         diag.status <- rbinom(1, 1, male.daily.testing.prob)
       } else {
         diag.status <- NA
       }
       return(diag.status)
     }

     diag.status.men <- unlist(lapply(inf.status.men, assign.diag.status))
     diag.status.women <- unlist(lapply(inf.status.women, assign.diag.status))

     diag.status <- c(diag.status.men, diag.status.women)

     diag.status <- unlist(diag.status)

     set.vertex.attribute(n0, "diag.status", diag.status)


     ## xtabs(~factor(art.adherence, exclude=NULL)+
     ##       factor(diag.status), exclude=NULL)

  ## Time since Diagnosis,
     ### if daily probability of 

  ## ART Access (Ever Accessed or Will Access Treatment)
    ### This attribute is an inidividual-level indicator 
    ### population who will not have access to ART. 
    art.prob.men <- 0.40
    art.prob.women <- 0.40
    art.access.men <- rbinom(length(male.id), 1, art.prob.men)
    art.access.women <- rbinom(length(male.id), 1, art.prob.women)

    art.access <- c(art.access.men, art.access.women)
    set.vertex.attribute(n0, "art.access", art.access.men,
                         male.id)
    set.vertex.attribute(n0, "art.access", art.access.women,
                         female.id)
 
  ## ART Adherence 
     ### This attribute is an inidividual-level indicator 
     ### for those who will not adhere to ART.
     ### indicator 0 or 1 for those who have access.
     ### NA for everyone else. 

    set.vertex.attribute(n0, "art.adherence", NA)
    art.adherence.prob.men <- 0.80
    art.adherence.prob.women <- 0.80

    set.adherence <- function(art.access, art.adherence.prob){
      if(art.access == 1){
        art.adherence.indicator <- rbinom(1, 1, art.adherence.prob)
      } else {
        art.adherence.indicator <- NA
      }
     return(art.adherence.indicator)
    }

    art.adherence.men <- unlist(lapply(art.access.men, set.adherence,
                                       art.adherence.prob=art.adherence.prob.men))
    art.adherence.women <- unlist(lapply(art.access.women, set.adherence,
                                         art.adherence.prob=art.adherence.prob.women))

    art.adherence.men <- unlist(art.adherence.men) # right data structure
    art.adherence.women <- unlist(art.adherence.women)

    art.adherence <- c(art.adherence.men, art.adherence.women)

    set.vertex.attribute(n0, "art.adherence", art.adherence)

    xtabs(~factor(art.adherence, exclude=NULL)+
          factor(art.access), exclude=NULL)
          

 ## ART Status (Whether currently on treatment
    ### This is an inidividual-level attribute indicating 
    ### whether someone is on ART on a given day. 
    ### Depends on diagnosis status,
    ### access, and adherence.

    art.status <- diag.status+art.adherence+art.access
    ## if art.status here is 3, that implies all 3 conditions are met:
    ## diagnosis, adherence, access. All other values imply  non-adherence.
    art.status[art.status<3] <- 0
    art.status[art.status==3] <- 1
    table(art.status, useNA="always")

    ## With current conditions, 15% of infecteds are on treatment
    ## 30% of diagnosed are on treatment.

    set.vertex.attribute(n0, "art.status", art.status)

    xtabs(~factor(art.adherence, exclude=NULL)+
          factor(art.status), exclude=NULL)

    xtabs(~factor(art.access, exclude=NULL)+
          factor(art.status), exclude=NULL)

    xtabs(~factor(diag.status, exclude=NULL)+
          factor(art.status), exclude=NULL)

  ## Viral Load,
     ### If uninfected, set to NA
     ### If infected and on treatment, set to "undetectable" (=0?)
     ### If infected dependent on time since infection.

     viral.load <- rep(NA, length=1:N)

     set.viral.load <- function(){
       if (inf.status == 0){
         viral.load <- NA
       } else if (inf.status == 1 && art.status == 0){
                 if(time.since.infection < 0) { } 
                 else if () {} ...
          else if (art.status == 1){
            viral.load <- 0
          }
               }
     }



  ## HSV Status -- calibrate using Phase 2 data
    hsv.inf.prob <- 0.30 ## Modify
    hsv.status <- rbinom(N, 1, hsv.inf.prob)
    set.vertex.attribute(n0, "hsv.status", hsv.status)

  ## CD4 Counts
     ### If uninfected, ---
     ### if infected, not on treatment, time since infection
     ### if infected, on treatment, then NA?

    cd4.untreated <- 520
    cd4.perday.decline.men <- (520-350)/(365*3.5)
    cd4.perday.decline.women <- (520-350)/(365*4)


    calculate.cd4.count.men <- function(inf.status){
      if (inf.status==0) {
        cd.count <- 520
      } else if (inf.status==1 && art.status==0){
        cd4.count <- cd4.count - (cd4.perday.decline.men*time.since.infection)
      }  else if (inf.status==1 && art.status==1){
        cd4.count <- cd4.count - (cd4.perday.decline.men*time.since.infection)
      }
      return (art.status)
    }
          

#####################################################
### Network Parameters
#####################################################

  ## Statistics: 
    ### Edgecount, Partnership Duration, Degree Distribution
    mean.degree <- 0.3 # modify
    n.edges <- N*mean.degree/2

    male.degree.dist <- c(0.48,0.41,0.08,0.03)
    female.degree.dist <- c(0.40, 0.55, 0.04, 0.01)

    male.degreecounts <- male.degree.dist*num.male
    female.degreecounts <- female.degree.dist*num.female

    partnership.duration <- 2000 # modify

    formation.meanstats <- c(n.edges,
                             male.degreecounts[1:2],
                             female.degreecounts[1:2]
                             )
#####################################################

#####################################################
### Generate Network
#####################################################
        formation <- ~edges+b1degree(0:1)+b2degree(0:1)
	formation.n0 <- update.formula(formation, n0~.)
	dissolution <- ~offset(edges)
        theta.diss <- log(partnership.duration-1)
	target.stats <- c(formation.meanstats)
        constraints	<- ~.

	n0 <- network.initialize(N,bipartite=num.male,directed=F)

        ## Cross-Sectional Network Fit with Carnegie Approxmiation
        ##  and Diagnostics
	fit <- ergm(formation.n0,target.stats=target.stats,
                    constraints=constraints,
                    iterations=100)
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

        save(hetdeg.diag.sim, file="hetdeg.diag.sim.RData")

        sim.stats <- attributes(hetdeg.diag.sim)$stats[101:5000,]
	stats.means <- colMeans(sim.stats)##mean(sim.stats)
	stats.sd <- apply(sim.stats,2,sd)##sd(sim.stats)##apply(sim.stats,2,sd)

	sim.df <- as.data.frame(hetdeg.diag.sim)
	partnership.duration.mean <- mean(sim.df$duration)
	partnership.duration.sd <- sd(sim.df$duration)
	partnership.duration.expected <- exp(theta.diss)+1

        ## Temporal Network and Diagnostics
##         network.stergm.fit <- stergm(n0,
##                                      formation = formation,
##                                      dissolution = dissolution,
##                                      targets = "formation",
##                                      target.stats = target.stats,
##                                      offset.coef.diss = theta.diss,
##                                      estimate = "EGMME",
##                                      control=control.stergm(SA.plot.progress=TRUE),
##                                      verbose=2
## #		SA.phase2.levels.max=1)
## )

##         mcmc.diagnostics(msm.fit)
#####################################################


