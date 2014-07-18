rm(list=ls())
##library(ergm)

library(tergm)
source("../simulate.networkDynamic.R")
source("common.functions_d2.R")

#####################################################
### Progression of versions
#####################################################

## 27 May 2013: Adapt to increase the size of the time-step. With time-steps of the
## order of 4000 days, we get the following error:
## Error in toggleList[, 1] : incorrect number of dimensions
## which (I think) means there are not enough "toggles" in the changeList. 

## 26 May 2013: Adapt once again for estimation of network with features for Uganda

## 8 April 2013: Rework to correct inf.status
## Add "infectivity.today" attribute.

## 7 April 2013: Rewrite the way "art.status" is initially assigned.
## For all uninfected people, "init.inf.status" should be NA
## For all infected people not on treatment, "init.inf.status" should be 0.
## For all infected people on treatment, "init.inf.status" should be 1.
## Make sure this structure is maintained throughout.

## For now, keep time since infection = 1 for all infected people, to avoid
## problems with dynamics of CD4 counts later. 

## 5 April 2013: Play with duration parameter
## With 100 duration, I was getting the program to run for all 250 time steps.
## Something changed after I added the new functions, specifically,
## the update on the biological parameters

## 4 April 2013: Added more
## a. pregnancy parameters (time of, time since)
## b. art parameters (time of)
## c. CD4 parameters (cd4.count.today)
## d. viral load parameters (viral.load.today)

## HSV related attributes are not necessary, since we are not modeling
## HSV dynamics, but simply adjusting infectivities based on HSV prevalence

## STILL HAVE TO CHECK RELATIONSHIP BETWEEN VIRAL LOAD and TIME SINCE INFECTION
## IN FINAL STEP

## 1 April 2013: Clean up CD4 functions -- move to "common.functions" file,
## Corrected values for infection stages

## 25 March 2013: Add time since initiation of ART

## 20 March 2013: Rewrite because result of estimate is not working with
## simulation file. I keep getting the following error from the simulate function:
## ``Error in toggleList[, 1] : incorrect number of dimensions''

## 26 Feb 2013: Add testing as a function of pregnancy,
## and effect on ART status, adherence,
## Type of treatment -- sc ART,
## 1 week after birth.
## Reduction in infectiousness
## Option A and effect on treatment.
## Attribute for type of treatment = f(Pregnancy Status, CD4).
## For men, just ART.
## For women, scART (Option A), ART,
## Time of Initiation of Treatment in Men, Non Pregnant Women = f(CD4).
## Time of Initiation of Treatment in Pregnant Women = f(Gestation).
## Time of Exiting Treatment in Men, Non-Pregnant Women = 
## Time of Exiting Treatment in Pregnant Women on Option A = 1 week after Delivery
## Infectivity as a Function of Pregnancy Status (and interaction of pregnancy with
## treatment)

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

  ### Basic Population Set UP     
      	num.male <- 2500
	num.female <- 2500 
        N <- num.male+num.female

  #### Network Related Statistics
        ## Degree Distribution, Partnership Duration
        ## and Number of Partnerships
      	male.deg.dist <- c(0.48,0.41,0.08,0.03)
        female.deg.dist <- c(0.40, 0.55, 0.04, 0.01)
	## duration <- 100
        duration <- 4303
	diagnostics <- T

        female.deg.counts <- female.deg.dist*num.female
	male.deg.counts <- male.deg.dist*num.male
	female.deg.tot <- (0*female.deg.counts[1] + 1*female.deg.counts[2] +
                           2*female.deg.counts[3] + 3*female.deg.counts[4])
	male.deg.tot <- (0*male.deg.counts[1] + 1*male.deg.counts[2] +
                         2*male.deg.counts[3] + 3*male.deg.counts[4])


       ## to match the degree totals for men and women, reduce the number of
       ## isolates in the women, and increase the number of women with 1 partner
        female.deg.dist.matched <- c(22.0, 75.7, 2.3, 0)/100
        female.deg.counts.matched <- female.deg.dist.matched*num.female
        female.deg.tot.matched <- (0*female.deg.counts.matched[1] +
                                   1*female.deg.counts.matched[2] +
                                   2*female.deg.counts.matched[3] +
                                   3*female.deg.counts.matched[4])

   ### Set up model

	formation <- ~edges+b1degree(0:1)+b2degree(0:1)
	formation.n0 <- update.formula(formation,n0~.)
	dissolution <- ~offset(edges)
	theta.diss <- log(duration-1)
	##target.stats <- c(female.deg.tot, female.deg.counts[1:2],
                            ## male.deg.counts[1:2])
        target.stats <- c(female.deg.tot.matched, male.deg.counts[1:2],
                          female.deg.counts.matched[1:2])

        constraints	<- ~.

        ## Initialize Network
        n0 <- network.initialize(N, bipartite=num.male, directed=F)

#####################################################
### Add Atributes
#####################################################

  ###############
  ### Demography 
  ##############

  ## Sex, Age, Circumci<sion Status, 
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
    age.distribution <- c(0.25468095,
                          0.20238352,
                          0.155598745,
                          0.119586275,
                          0.095660855,
                          0.07367249,
                          0.058677315,
                          0.04014263)
    ## mean of male-female distributions

    age.classes <- c(seq(15, 19, by=1),
                         seq(20, 24, by=1),
                         seq(25, 29, by=1),
                         seq(30, 34, by=1),
                         seq(35, 39, by=1),
                         seq(40, 44, by=1),
                         seq(45, 49, by=1),
                         seq(50, 54, by=1)
                         ) # create a vector of all ages of interest

    prob.for.each.age <- unlist(lapply(age.distribution, function(x) rep(x, 5)))

    ## then assign the probability for each age class to be selected to the numeric
    ## values for that class
    age <- sample(age.classes, size=N, replace=TRUE, prob=prob.for.each.age)
    n0 %v% "age" <- age

   ## checks for age
   length(intersect(which(age>=15), which(age<20)))/N # first age class
   length(intersect(which(age>=50), which(age<55)))/N # last age class

  ## circumcision status
    circum.rate <- 96/(851+96) ## No 851; Yes 96; Refusal 1; NA 0,
                               ## see file "data_report.pdf"
    circum.status <- rbinom(N/2, 1, circum.rate)
    n0 %v% "circum.status" <- c(circum.status, rep(NA, N/2))

  ## pregnancy status
    ###n0 %v% "pregnancy.status" <- NA
    set.vertex.attribute(n0, "pregnancy.status", NA, male.id)
    set.vertex.attribute(n0, "pregnancy.status", 0, female.id)
    ## defeault pregnancy status is NA,
    ## overwritten with age specific fertility rates for women below

    asfr.15to19 <- 0.000472603  ## Age-Specific Fertility Rate
    asfr.20to24 <- 0.000916164 ## Tailor each to ZA, UG
    asfr.25to29 <- 0.000836712
    asfr.30to34 <- 0.000691233
    asfr.35to39 <- 0.000476164
    asfr.40to44 <- 0.000207945
    asfr.45to49 <- 9.50685E-05

 
   ## will have to move this to after infection status,
   ## to account for lower fertility rate in HIV diagnosed women

    fem.age.15to19 <- intersect(which((n0%v%"age") < 20 ),
                                which((n0%v%"sex") == "female"))
    fem.age.20to24 <- intersect(intersect(which((n0%v%"age") >= 20),
                                          which((n0%v%"age") < 25)),
                                which((n0%v%"sex") == "female"))
    fem.age.25to29 <- intersect(intersect(which((n0%v%"age") >= 25),
                                          which((n0%v%"age") < 30)),
                                which((n0%v%"sex") == "female"))
    fem.age.30to34 <- intersect(intersect(which((n0%v%"age") >= 30),
                                          which((n0%v%"age") < 35)),
                                which((n0%v%"sex") == "female"))
    fem.age.35to39 <- intersect(intersect(which((n0%v%"age") >= 35),
                                          which((n0%v%"age") < 40)),
                            which((n0%v%"sex") == "female"))
    fem.age.40to44 <- intersect(intersect(which((n0%v%"age") >= 40),
                                          which((n0%v%"age") < 45)),
                                which((n0%v%"sex") == "female"))
    fem.age.45to49 <- intersect(intersect(which((n0%v%"age") >= 45),
                                          which((n0%v%"age") < 50)),
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

    ##number of days since last breastfeeding
    n0%v%"time.of.prenancy" <- NA
    n0%v%"time.since.prenancy" <- NA
    

###############
### Biology 
##############

  ## Infection Status 
     init.hiv.prev <- 0.06 # set to about 6% for Uganda
     init.inf.status <- rbinom(network.size(n0), 1, init.hiv.prev)
     set.vertex.attribute(n0, "inf.status", init.inf.status,
                          )

     inf.status.men <- init.inf.status[male.id]
     inf.status.women <- init.inf.status[female.id]

  ## Time Since Infection 
     duration.of.infection <- 3300  ## in days, modify later
     init.infected <- which(init.inf.status == 1)
     ## time.since.infection.val <- sample(duration.of.infection, size=length(init.infected),
     ##                                    replace=TRUE) 

     time.since.infection <- rep(NA, N)
     ## time.since.infection[which(init.inf.status == 1)] <- time.since.infection.val
     ## 7 April 2013, changed to avoid problems with dynamics of CD4 counts later
     ##timeme.since.infection[which(init.inf.status == 1)] <- time.since.infection.val <- 0
     time.since.infection[init.infected] <- 0

     set.vertex.attribute(n0, "time.since.infection", time.since.infection
                          )

      ## infectivity for infected individuals
      infectivity.today <- rep(NA, N)
      min.chronic.infectivity <- 0.00497/2.89
      set.vertex.attribute(n0, "infectivity.today", NA)
      set.vertex.attribute(n0, "infectivity.today", min.chronic.infectivity,
                           init.infected
                           )


  ## Time of Infection 
  ## Stage of Infection 
  ## Classification necessary for deciding infectivity
  ## for chronically infected

     acute.length <- 1:121 ## in daily time units
     chronic.length <- 121:1877
     late.length <- 1878:3300 

     ## stage.of.infection <- rep(NA, N)
     ## time.and.stage <- cbind(time.since.infection, stage.of.infection)

     ## classify.stage <- function(x){
     ##   if (x %in% acute.length){
     ##     stage.of.infection <- "Acute"
     ##   } else  if (x %in% chronic.length){
     ##     stage.of.infection <- "Chronic"
     ##   } else  if (x %in% late.length){
     ##     stage.of.infection <- "Late"
     ##   }
     ##   return(stage.of.infection)
     ## }

     ## stage.of.infection <- unlist(lapply(time.since.infection, classify.stage))

     stage.of.infection <- classify.stage.numeric.rewrite(time.since.infection)

     n0%v%"stage.of.infection" <- stage.of.infection


  ## Diagnosis Status,
  ##    male.daily.testing.prob <- 0.50 # modify
  ##    female.daily.testing.prob <- 0.50 # modify

  ##    ##infected.men <- intersect(init.infected, male.id)
  ##    ##infected.women <- intersect(init.infected, female.id)


  ##    assign.diag.status <- function(inf.status){
  ##      if (inf.status == 1){
  ##        diag.status <- rbinom(1, 1, male.daily.testing.prob)
  ##      } else {
  ##        diag.status <- NA
  ##      }
  ##      return(diag.status)
  ##    }

  ##    diag.status.men <- unlist(lapply(inf.status.men, assign.diag.status))
  ##    diag.status.women <- unlist(lapply(inf.status.women, assign.diag.status))

  ##    diag.status <- c(diag.status.men, diag.status.women)

  ##    diag.status <- unlist(diag.status)

  ##    set.vertex.attribute(n0, "diag.status", diag.status)


  ##    ## xtabs(~factor(art.adherence, exclude=NULL)+
  ##    ##       factor(diag.status), exclude=NULL)

  ## ## Time since Diagnosis,
  ##    ### if daily probability of 

  ## ## ART Access (Ever Accessed or Will Access Treatment)
  ##   ### This attribute is an inidividual-level indicator 
  ##   ### population who will not have access to ART. 

  ##   ### Add CD4 count as a determinant of eligibility
  ##   art.prob.men <- 0.40
  ##   art.prob.women <- 0.40

  ##   ## Consider calling "uptake" -- based on CD4 count
  ##   ## 
  ##   art.access.men <- rbinom(length(male.id), 1, art.prob.men)
  ##   art.access.women <- rbinom(length(male.id), 1, art.prob.women)

  ##   art.access <- c(art.access.men, art.access.women)
  ##   set.vertex.attribute(n0, "art.access", art.access.men,
  ##                        male.id)
  ##   set.vertex.attribute(n0, "art.access", art.access.women,
  ##                        female.id)
 
  ## ## ART Adherence (should be a static attribute -- some % of people are not going to
  ## ## treatment 
  ##    ### This attribute is an inidividual-level indicator 
  ##    ### for those who will not adhere to ART.
  ##    ### indicator 0 or 1 for those who have access.
  ##    ### NA for everyone else. 

  ##   set.vertex.attribute(n0, "art.adherence", NA)
  ##   art.adherence.prob.men <- 0.80
  ##   art.adherence.prob.women <- 0.80

  ##   set.adherence <- function(art.access, art.adherence.prob){
  ##     if(art.access == 1){
  ##       art.adherence.indicator <- rbinom(1, 1, art.adherence.prob)
  ##     } else {
  ##       art.adherence.indicator <- NA
  ##     }
  ##    return(art.adherence.indicator)
  ##   }

  ##   art.adherence.men <- unlist(lapply(art.access.men, set.adherence,
  ##                                      art.adherence.prob=art.adherence.prob.men))
  ##   art.adherence.women <- unlist(lapply(art.access.women, set.adherence,
  ##                                        art.adherence.prob=art.adherence.prob.women))

  ##   art.adherence.men <- unlist(art.adherence.men) # right data structure
  ##   art.adherence.women <- unlist(art.adherence.women)

  ##   art.adherence <- c(art.adherence.men, art.adherence.women)

  ##   set.vertex.attribute(n0, "art.adherence", art.adherence)

  ##   xtabs(~factor(art.adherence, exclude=NULL)+
  ##         factor(art.access), exclude=NULL)
          

 ## ART Status (Whether currently on treatment
    ### This is an inidividual-level attribute indicating 
    ### whether someone is on ART on a given day. 
    ### Depends on diagnosis status,
    ### access, and adherence.

    ##art.status <- diag.status+art.adherence+art.access
    ## if art.status here is 3, that implies all 3 conditions are met:
    ## diagnosis, adherence, access. All other values imply  non-adherence.

   ##art.status[art.status<3] <- 0 ## 7 April 2013, see above
    ## art.status[init.inf.status==1] <- 0
    ## art.status[art.status==3] <- 1 ## diagnosed, adherent and access, 8 April 2013
    ## table(art.status, useNA="always")

    ## With current conditions, 15% of infecteds are on treatment
    ## 30% of diagnosed are on treatment.

    art.status <- rep(NA, N)
    art.status[which(init.inf.status==1)] <- 0

    set.vertex.attribute(n0, "art.status", art.status)

    ## xtabs(~factor(art.adherence, exclude=NULL)+
    ##       factor(art.status), exclude=NULL)

    ## xtabs(~factor(art.access, exclude=NULL)+
    ##       factor(art.status), exclude=NULL)

    ## xtabs(~factor(diag.status, exclude=NULL)+
    ##       factor(art.status), exclude=NULL)

 ## Time since ART initiation
    ### Initially, for all people on ART set=0?
        on.art <- which(art.status==1)
        time.since.art.initiation <- rep(NA, N)
        time.since.art.initiation[on.art] <- 0
        time.of.art.initiation <- time.since.art.initiation
        ## At time step 0, time.of.art.initiation and
        ## time.since.art.initiation have same values
        n0%v%"time.since.art.initiation" <- time.since.art.initiation
        n0%v%"time.of.art.initiation" <- time.of.art.initiation

  ## Viral Load,
     ### If uninfected, set to NA
     ### If infected and on treatment and adherent,
     ### depends on time since art initiation
     ### If infected and untreated, or,
     ### on treatment but not adherent, then
     ### same as in treatment-naive

     ## viral.load <- rep(NA, length=1:N)

     ## set.viral.load <- function(){
     ##   if (inf.status == 0){
     ##     viral.load <- NA
     ##   } else if (inf.status == 1 && art.status == 0){
     ##             if(time.since.infection < 0) { } 
     ##             else if () {} ...
     ##      else if (art.status == 1){
     ##        viral.load <- 0
     ##      }
     ##           }
     ## }



  ## HSV Status -- calibrate using Phase 2 data
  ## or account for HSV by making infectivity a weighted avg of
  ## infectivities for HSV+ and HSV- individuals.
    ## hsv.inf.prob <- 0.30 ## Modify
    ## hsv.status <- rbinom(N, 1, hsv.inf.prob)
    ## set.vertex.attribute(n0, "hsv.status", hsv.status)

  ## CD4 Counts
     ## Relevant parameters
     cd4.at.infection.male  <- 518 #cells/mm3
     cd4.at.infection.female  <- 570 #cells/mm3
     untreated.cd4.daily.decline <- 0.14 # (for men and women)

     ## Loop to compute CD4 Count based on time since infection
     ## Men
     cd4.count.today <- rep(NA, N)
     cd4.count.today[male.id] <- cd4.at.infection.male
     cd4.count.today[female.id] <- cd4.at.infection.female

     for (i in 1:length(male.id)) {

       if (init.inf.status[i] == 1) { # untreated individuals
         cd4.count.today[i] <-
           cd4.at.infection.male - # this is a constant value
             time.since.infection[i]*untreated.cd4.daily.decline

       } 
     } ## this doesn't quite make sense since CD4 counts can fall below 0
       ## maybe cap off with a while loop

     ## Women
     for (i in (min(female.id) : max(female.id)) ) {

         if (init.inf.status[i] == 1) { # untreated individuals
           cd4.count.today[i] <-
             cd4.at.infection.female - # this is a constant value
               time.since.infection[i]*untreated.cd4.daily.decline
         }
       } ## this doesn't quite make sense since CD4 counts can fall below 0
         ## maybe cap off with a while loop
       
     set.vertex.attribute(n0, "cd4.count.today", cd4.count.today)


  ## Viral Load Today
     ## Relevant Parameters
     time.infection.to.peak.viral.load <- 14
     peak.viral.load <- 6.17
     time.infection.to.viral.set.point <- 121
     set.point.viral.load <- 4.2
     time.infection.to.late.stage <- 1877
     dur.inf <- 3300
     late.stage.viral.load <- 5.05 ## (max?)

     ## At time step 0, viral load today
     viral.load.today <- rep(0, N)
     ## viral.load.today


     ## Loop to assign viral load based on time sice infection

     for (i in 1:length(viral.load.today)){
       if (init.inf.status[i] == 1){
           if (time.since.infection[i] <= time.infection.to.peak.viral.load){

             viral.load.today[i] <- peak.viral.load*time.since.infection[i]/
               time.infection.to.peak.viral.load ## assumes viral load 0 at infection
             
           } else if ( (time.since.infection[i] %in%
                        (time.infection.to.peak.viral.load+1):
                        (time.infection.to.viral.set.point))
                      )  {
             viral.load.today[i] <- peak.viral.load -
               (peak.viral.load - set.point.viral.load) *
                 (time.since.infection[i] - time.infection.to.peak.viral.load)/
                   (time.infection.to.viral.set.point - time.infection.to.peak.viral.load)

           } else if ( (time.since.infection[i] %in%
                        (time.infection.to.viral.set.point+1):
                        (time.infection.to.late.stage))
                      ) {
             viral.load.today[i] <- set.point.viral.load
             
           } else if ( (time.since.infection[i] > time.infection.to.late.stage)
                    ){
           
             viral.load.today[i] <- set.point.viral.load +
               (late.stage.viral.load - set.point.viral.load)*
                 (time.since.infection[i] - time.infection.to.late.stage)/
                   ( (dur.inf-1) - time.infection.to.late.stage )
           } ## check this one what happens to viral load in the last stage
       }
     }

     ## Assign viral loads
        n0%v%"viral.load.today" <- viral.load.today

#####################################################
### Fit cross-sectional model to formation,
### and apply Carnegie correction
#####################################################

     fit <- ergm(formation.n0,
                    target.stats=target.stats,
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
   ## Save simulation object -- takes a long time to run (~20 mins with
     ## time.slices=2e4)

     save(hetdeg.diag.sim, file="hetdeg.diag.sim.RData")

#####################################################
### Diagnostics
#####################################################

  ## Part 1 (From Class) 
     sim.stats <- attributes(hetdeg.diag.sim)$stats[101:2e4,]
     stats.means <- colMeans(sim.stats)##mean(sim.stats)
     stats.sd <- apply(sim.stats,2,sd)##sd(sim.stats)##apply(sim.stats,2,sd)

  ## Part 2 (From Class)
     sim.df <- as.data.frame(hetdeg.diag.sim)
     partnership.duration.mean <- mean(sim.df$duration)
     partnership.duration.sd <- sd(sim.df$duration)
     partnership.duration.expected <- exp(theta.diss)+1

  ## Part 3 (From Class)
  ## Back-calculate the expected values for
  ## b1degree(2:3) and b2degree(2:3), and compile
     unclaimed.b1nodes <- num.male - sum(target.stats[2:3])
     unclaimed.b1edges <- target.stats[1] - target.stats[3]
     unclaimed.b2nodes <- num.female - sum(target.stats[4:5])
     unclaimed.b2edges <- target.stats[1] - target.stats[5]
     expected.b1deg2 <- 3*unclaimed.b1nodes -  unclaimed.b1edges ## don't understand
     expected.b1deg3 <- unclaimed.b1edges - 2*unclaimed.b1nodes ## don't understand
     expected.b2deg2 <- 3*unclaimed.b2nodes -  unclaimed.b2edges ## don't understand
     expected.b2deg3 <- unclaimed.b2edges - 2*unclaimed.b2nodes ## don't understand
     stats.expected <- c(target.stats[1:3], expected.b1deg2, expected.b1deg3, rep(0,2),
                         target.stats[4:5], expected.b2deg2,
                         expected.b2deg3, rep(0,2)
                         )

#####################################################
## Save Object
#####################################################
    save.image(file="estimation_uganda_d2.RData")
#####################################################

