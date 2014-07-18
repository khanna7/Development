## 21 Feb 2014:
## a. Change name of "vl.at.art.init" to
## "vl.at.art.initiation"
## b. Record viral load at art cessation:
## "vl.at.art.cessation"

## 20 Feb 2014: Record CD4 at ART cessation

## 19 Feb 2014: Differentiate "art.type" attribute for
## Option A-ceased and Option B-ceased women.
## a. Let's call the Option A-ceased attribute 3,
##    let's call the Option B-ceased attribute 5. 
## b. Add argument in function to 
## c. For women ceasing Option B, current "vl.art.traj.slope" is
## vl.art.traj.slope[i] <- optA.vl.reduction/sc.art.postcess.ret.bl
## Changed this to
## vl.art.traj.slope[i] <- abs(4.2 - viral.load.today[i])/optB.vl.postcess.ret.bl #19Feb14
## Above isn't good enough
## d. Have added "vl.at.art.init" attribute, so above
## we do not have to use 4.2 above.

## 20 Dec 2013:
## Option B update: Adapt for cessation of regular ART under
## PMTCT Option B, 1 year after delivery
## a. Need 2 more arguments in function,
## "scenario" (so we know under what model scenario to terminate reg. ART)
## b. "optB.cess.time" (so we know when to terminate reg. ART)

## 28 Oct 2013: Update attribute of number of pregnancies
## (Implement a from list below)
## (Now implement c from list below)

## New things to be added throughout the code:
## a. number of pregnancies
## b. CD4 count when ART is supposed to be initiated for pregnant women
## c. ART status at termination of pregnancy
## d. In summary data, record number of eligible people at every time step.
## e. Differentiate pregnancy multiplier for susceptible women -- 1.7
## implemented in version "transmission_d10.R"

## 23 Oct 2013: Also record total number of pregnancies from this file

## 16 Sep 2013:
## a. Mistake here -- the loop for assigning new pregnancies is still
## only over women in relationships. Need to change to all women.
## b. Also restrict this to cross-sectional network at every timestep? --
## doing this by simply using the "network.extract" function here does not work --
## especially if the crossectional object is also named "nw" -- because then all
## the information in the network outside of the current timestep is lost.

## 13 Sep 2013:
## a. Count number of new pregnancies (already implemented in "d5b").
## b. Implement immaculate conception here -- WITHOUT correction for
##    proportion of women who meet pregnancy criteria. 

## 3 Sep 2013:
## a. Should the daily probability for getting pregnant be
## computed using the binomial formula or a linear approximation.

## Let p_y = prob. of getting pregnant over a year and
## let beta.d = prob. of getting pregnant per day.

## Then, is 1-(1-beta.d)^365=p_y OR is beta.d = p_y/365.

## 2 Sep 2013:
## a. Test pregnancy probability code written on 31 August
## b. Add arguments for
##    i. empirical number of births by age per 1000,
##   ii. proportion of still births (as a number between 0 and 1).
##  iii. multiplier for reduction of probability of
##       pregnancy on account of positive HIV infection

## 31 Aug 2013: Add code for stratifying pregnancy by age, adjusting for
## infection status, and rate of still birth.

## 5 Aug 2013: Stratify incidence of pregnancy by age

## 25 Jul 2013: Populate "time.since.art.cessation" attribute for women
## who stop ART treatment

## 12 Jun 2013: Compute viral load trajectory for women
## after cessation of sc ART. The change will be equal to
## "optA.vl.reduction" and will take place in the time specified
## for return to baseline -- "sc.art.ret.bl"

## 11 Jun 2013: Adjust treatment status for women
## on sc ART (Option A) after delivery:
## After delivery
## art.status == 0
## art.type == 3 -- defined as post-cessation of sc ART (Option A)

## 7 Jun 2013: Adjust preg. probability in accordance with ASFR.

## 6 June 2013: A better way to parameterize "time.since.last.pregnancy" may
## be that everyone who has time.since.last.pregnancy=NA or > min.preg.interval
## is eligible to get pregnant. This is because currently we have
## all women given a value of (min.preg.interval + 1) at the start so they
## are eligible to get pregnant. However, this creates a problem later when
## new women who enter the population are initially assigned a value
## of NA for time since last pregnancy. Therefore, it may be better to revise
## the condition for eligibility of pregnancy.

## 5 June 2013: Assign pregnancy status to women.

#####################################################
### Model transmission  
#####################################################

  ### Extract partnership network

assign.pregnancy <-
  function(nw, verbose,
           full.term,
           # preg.prob, #2Sep2013: commented  out
           min.preg.interval,
           optA.vl.reduction, #12Jun13
           sc.art.postcess.ret.bl, #12Jun13
           num.births.per1k.byage, #2Sep13
           prop.stillbirth, #2Sep13
           inf.preg.red, #2Sep13
           optB.cess.time, #20Dec13
           scenario,#20Dec13
           optB.vl.postcess.ret.bl, #19Feb14
           ...
           ) {

    ## 16Sep 2013: b -- restrict to alive network?? 
    ## nw <- network.extract(nw, at = time,
    ##                       retain.all.vertices = T)
    ## browser()                                        
    male.id.curr <- nwmodes(nw, 1)
    female.id.curr <- nwmodes(nw, 2)

    curr.pregnancy.status <- nw%v%"curr.pregnancy.status"
    time.since.curr.pregnancy <- nw%v%"time.since.curr.pregnancy"
    time.of.curr.pregnancy <- nw%v%"time.of.curr.pregnancy"
    time.since.last.pregnancy <- nw%v%"time.since.last.pregnancy"
    art.status <- nw%v%"art.status"
    art.type <- nw%v%"art.type"
    vl.art.traj.slope <- nw%v%"vl.art.traj.slope"
    num.of.pregnancies <- nw%v%"num.of.pregnancies" #28Oct13
    vl.at.art.initiation     <- nw%v%"vl.at.art.initiation" #19Feb14
    
    time.of.art.cessation <- nw%v%"time.of.art.cessation" #25Jul13
    time.since.art.cessation <- nw%v%"time.since.art.cessation" #25Jul13
    cd4.count.today <- nw%v%"cd4.count.today"
    cd4.at.art.cessation <- nw%v%"cd4.at.art.cessation" #20Feb14
    viral.load.today <- nw%v%"viral.load.today" #21Feb14
    vl.at.art.cessation <- nw%v%"vl.at.art.cessation" #21Feb14
    

    if(verbose){
      cat("Pregnant women are ", which(curr.pregnancy.status==1), "\n")
      cat("Number of not-pregnant women is ",
          length(which(curr.pregnancy.status==0)),
          "\n")
    }

    for(i in 1:length(curr.pregnancy.status)){

      ## maintain pregnancy status for pregnancies 
      ## not carried to term yet
      ## and change for pregnancies that go to term

      if (!is.na(curr.pregnancy.status[i]) && (curr.pregnancy.status[i] == 1)){
        ## delivery upon completion of full term
        if (time.since.curr.pregnancy[i] > full.term ) {
          curr.pregnancy.status[i] <- 1-curr.pregnancy.status[i]
          time.since.curr.pregnancy[i] <- NA
          time.since.last.pregnancy[i] <- 0

          set.vertex.attribute(nw, "art.status.at.preg.term",
                               art.status[i],
                               i) # 28Oct13: Implement (c) above --
                                  # art status at termination of pregnancy

            if (!is.na(art.type[i]) && art.type[i] == 2) { ## for women receiving 
              art.status[i] <- 0 # update ART-status for  sc ART (Option A) women
              art.type[i]   <- 3 # update ART type, for post-sc ART
              vl.art.traj.slope[i] <- optA.vl.reduction/sc.art.postcess.ret.bl
                               # 12Jun13
              time.of.art.cessation[i] <- time
              time.since.art.cessation[i] <- 0
                               # 25Jul13: Populate "time.since.art.cessation"
              cd4.at.art.cessation[i] <- cd4.count.today[i] #20Feb14
              vl.at.art.cessation[i] <- viral.load.today[i] #21Feb14
            
          }

          
        }

      }

    }

    ## 20Dec13: PMTCT Option B
    ## browser()
    if (scenario == "option.b"){
    ##<  browser()
      for(i in 1:length(curr.pregnancy.status)){
        ## 20Dec13: Stop reg ART for women with PMTCT Option B
        if (!is.na(curr.pregnancy.status[i]) && (curr.pregnancy.status[i] == 0)){
          ## delivery upon completion of full term
            if (!is.na(art.type[i]) && art.type[i] == 4) { ## 20Dec13: women on Opt B ART 

              if (time.since.last.pregnancy[i] > optB.cess.time){
                art.status[i] <- 0 # update ART-status for  sc ART (Option A) women
                art.type[i]   <- 5
                                        # 19Feb14: update ART type, post-Option B cessation (was set equiv. to Option A earlier)
                ## vl.art.traj.slope[i] <- optA.vl.reduction/sc.art.postcess.ret.bl
                vl.art.traj.slope[i] <- abs(vl.at.art.initiation[i] - viral.load.today[i])/optB.vl.postcess.ret.bl #19Feb14
                time.of.art.cessation[i] <- time
                time.since.art.cessation[i] <- 0
                                        # 25Jul13: Populate "time.since.art.cessation"
                cd4.at.art.cessation[i] <- cd4.count.today[i] #20Feb14
                vl.at.art.cessation[i] <- viral.load.today[i] #21Feb14
              }
            }
          }
      }
    }

    ## browser()
    
    nw.el <- as.edgelist(network.extract(nw,
                                         at = time,
                                         retain.all.vertices = T))

    id.female.in.rel <- which(female.id.curr %in% nw.el[,2])
    female.in.rel <- female.id.curr[id.female.in.rel]


   ########################################################################   ### 31 Aug 2013: Add code for still birth ---
   ########################################################################

   ## number of women in different age groups
      age <- nw%v%"age"

      ## browser()
       
    
  ## Now think about how to populate prob-vector.

        ## probi = (w_i/1-v)*Fi*1/fi where
           ## where w_i: number of births in age-group i (from data)
           ##       v overall proportion of still-births
           ##       F_i: total number of women in age group i
           ##       f_i: women meeting pregnancy criteria in age group i.
           ## f should be a composite of women who are in relationships,
           ## and who have crossed the minimum amt of time
           ## reqd to be pregnant
    
        F1 = length(which(age[female.id.curr] <= 19))
        F2 = length(intersect(which(age[female.id.curr]>19), which(age[female.id.curr] <= 24)))
        F3 = length(intersect(which(age[female.id.curr]>24), which(age[female.id.curr] <= 29)))
        F4 = length(intersect(which(age[female.id.curr]>29), which(age[female.id.curr] <= 34)))
        F5 = length(intersect(which(age[female.id.curr]>34), which(age[female.id.curr] <= 39)))
        F6 = length(intersect(which(age[female.id.curr]>39), which(age[female.id.curr] <= 44)))        
        F7 = length(intersect(which(age[female.id.curr]>44), which(age[female.id.curr] <= 49)))

        ## The f vector should be a composite of
           ## women who are in relationships,
           ## and who have crossed the minimum amt of time
           ## reqd to be pregnant

        ## f1 = length(which(age[female.in.rel] <= 19))
        ## f2 = length(intersect(which(age[female.in.rel]>19), which(age[female.in.rel] <= 24)))
        ## f3 = length(intersect(which(age[female.in.rel]>24), which(age[female.in.rel] <= 29)))
        ## f4 = length(intersect(which(age[female.in.rel]>29), which(age[female.in.rel] <= 34)))
        ## f5 = length(intersect(which(age[female.in.rel]>34), which(age[female.in.rel] <= 39)))
        ## f6 = length(intersect(which(age[female.in.rel]>39), which(age[female.in.rel] <= 44)))        
        ## f7 = length(intersect(which(age[female.in.rel]>44), which(age[female.in.rel] <= 49)))        

    lt.preg.thres <- which(time.since.last.pregnancy >
                               min.preg.interval)
                                        # women who have crossed threshold
                                        # for time since last pregnancy
    never.preg.fem <- intersect(which(is.na(time.since.last.pregnancy)),
                                                   female.id.curr)
                                        # women who have never
                                        # been pregnant

    curr.not.pregnant <- which(curr.pregnancy.status != 1)
    
    preg.crit.true <- intersect(c(lt.preg.thres,
                                  never.preg.fem),
                                ## female.in.rel, #13Sep13: implement imm. conception
                                                  # intersect3 function not needed
                                 curr.not.pregnant
                                )
    ## union of women have crossed min. threshold for time since last
    ## pregnancy with women who have never been pregnant,
    ## intersect with women who are in relationships
    ## intersect with women who are currently not pregnant
    ## thus we have women who are in relationships AND
    ## have never been pregnant OR
    ## crossed the threshold of time since last pregnancy AND
    ## are currently not pregnant

    f1 = length(which(age[preg.crit.true] <= 19))
    f2 = length(intersect(which(age[preg.crit.true]>19),
                          which(age[preg.crit.true] <= 24)))
    f3 = length(intersect(which(age[preg.crit.true]>24),
                          which(age[preg.crit.true] <= 29)))
    f4 = length(intersect(which(age[preg.crit.true]>29),
                          which(age[preg.crit.true] <= 34)))
    f5 = length(intersect(which(age[preg.crit.true]>34),
                          which(age[preg.crit.true] <= 39)))
    f6 = length(intersect(which(age[preg.crit.true]>39),
                          which(age[preg.crit.true] <= 44)))    
    f7 = length(intersect(which(age[preg.crit.true]>44),
                          which(age[preg.crit.true] <= 49)))    
    
    F <- c(F1, F2, F3, F4, F5, F6, F7)
    f <- c(f1, f2, f3, f4, f5, f6, f7)

    cat("Number of women in each age category ", F, "\n")
    cat("Number of women matching preg criteria in each age category ",
        f, "\n")
    
        ## Now probi = (w_i/1-v)*Fi*1/fi where 1 <= i <= 7.
           ## prob1 = (num.births.per1k.byage[1]/1-v)*F1*1/f1
           ##         # where num.births.per1k.byage = w
           ##         # prop.stillbirth = v
           ## prob2 = (num.births.per1k.byage[2]/1-prop.stillbirth)*F2*1/f2
           ## prob3 = (num.births.per1k.byage[3]/1-prop.stillbirth)*F2*1/f3
           ## prob4 = (num.births.per1k.byage[4]/1-prop.stillbirth)*F4*1/f4
           ## prob5 = (num.births.per1k.byage[5]/1-prop.stillbirth)*F5*1/f5
           ## prob6 = (num.births.per1k.byage[6]/1-prop.stillbirth)*F6*1/f6
           ## prob7 = (num.births.per1k.byage[7]/1-prop.stillbirth)*F7*1/f7

    # prob.year = (num.births.per1k.byage/(1-prop.stillbirth))*F*1/f
    prob.year = (num.births.per1k.byage/(1-prop.stillbirth)) #13Sep13: WO correction

    cat("Annual pregnancy rates per 1000 are are ", prob.year, "\n")
        ## F and f are vectors
        ## prob here is per 1000 women who meet preg. criteria per year

    prob.year = prob.year/1000
        ## prob per woman who meets preg. criteria per year
            ## 3Sep13: Now we want to compute the daily probability of getting pregnant
        ##         1-(1-beta.daily)^n = prob.y
        ##         where "prob.y" is probability of getting pregnant in a year
        ##                "n" is 365 days = 365/size.of.timestep as measured in timesteps
        ##                "beta.daily" is daily probability of getting pregnant
        ##         prob = 1 - exp(1/n*log(1-prob.y))
    ## browser()
    ## prob = prob/365*size.of.timestep
    ## 3Sep2013: Above assumes linear relationship between annual prob of getting pregnant
    ## and daily probability of getting pregnant.
    ## Another option is to model this relation using the above binomial formula. 
    ## It turns out that these are not different if annual probability of pregnanty <= 20%
    ## See "check_pregnancy_formula.R."
    
    ## prob.year[which(prob.year > 1)] <- 1 #13Sep13: commented out

    ## 3Sep13: If the f-vector gets small enough
    ## then the prob.year becomes > 1 -- which doesn't make sense.
    ## therefore, must cap all prob.year entries at 1 -- artificially
    timesteps.in.year <- round(365/size.of.timestep)
    cat("Annual pregnancy probabilities are ", prob.year, "\n")
    prob = 1 - (exp(1/timesteps.in.year*log(1-prob.year))) #here prob = daily probability
    ## prob per woman who meets pregnancy criteria per year


    ## for (i in 1:length(female.in.rel)){
      
    ##    if (curr.pregnancy.status[female.in.rel[i]] == 0 &&
    ##        (is.na(time.since.last.pregnancy[female.in.rel[i]]) || 
    ##         time.since.last.pregnancy[female.in.rel[i]] >
    ##         min.preg.interval)
    ##             ) {

    preg.prob <- nw%v%"preg.prob"
    preg.crit.age1 <- intersect(which(age <= 19),
                                preg.crit.true)
    preg.crit.age2 <- intersect3(which(age > 19),
                                 which(age <= 24),
                                 preg.crit.true)
    preg.crit.age3 <- intersect3(which(age > 24),
                                 which(age <= 29),
                                 preg.crit.true)
    preg.crit.age4 <- intersect3(which(age > 29),
                                 which(age <= 34),
                                 preg.crit.true)
    preg.crit.age5 <- intersect3(which(age > 34),
                                 which(age <= 39),
                                 preg.crit.true)
    preg.crit.age6 <- intersect3(which(age > 39),
                                 which(age <= 44),
                                 preg.crit.true)
    preg.crit.age7 <- intersect3(which(age > 44),
                                 which(age <= 49),
                                 preg.crit.true)
    female.age8 <- intersect(which(age > 49),
                             female.id.curr)
    
    preg.prob[preg.crit.age1] <- prob[1]
    preg.prob[preg.crit.age2] <- prob[2]
    preg.prob[preg.crit.age3] <- prob[3]
    preg.prob[preg.crit.age4] <- prob[4]
    preg.prob[preg.crit.age5] <- prob[5]
    preg.prob[preg.crit.age6] <- prob[6]
    preg.prob[preg.crit.age7] <- prob[7]
    preg.prob[female.age8]    <- 0
    
    ## Now adjust for infectivity
    inf.status <- nw%v%"inf.status"

    infected <- which(inf.status == 1) 
    preg.prob[infected] <- preg.prob[infected]*inf.preg.red

    nw%v%"preg.prob" <- preg.prob
    
   ######################################################################## 

    num.new.pregnancies <- 0 #13Sep13
    
    ## for (i in 1:length(female.in.rel)){
    ## browser()
    for (i in 1:length(female.id.curr)){
       ## model pregnancy as a stochastic process in women who meet criteria
       ## browser()
      
      ## if (curr.pregnancy.status[female.in.rel[i]] == 0 &&
      ##     (is.na(time.since.last.pregnancy[female.in.rel[i]]) || 
      ##      time.since.last.pregnancy[female.in.rel[i]] > min.preg.interval)
      ##     ) {

      
      if (female.id.curr[i] %in% preg.crit.true) {
        curr.pregnancy.status[female.id.curr[i]] <-
          rbinom(1, 1, preg.prob[female.id.curr[i]])

        if (!is.na(curr.pregnancy.status[female.id.curr[i]])){
                                        #3Sep13: to protect comparison
                                        # with NA's
          if (curr.pregnancy.status[female.id.curr[i]] == 1){
            time.since.curr.pregnancy[female.id.curr[i]] <- 0
            time.of.curr.pregnancy[female.id.curr[i]] <- time
            time.since.last.pregnancy[female.id.curr[i]] <- NA
            ## 2Sep2013: Set "time.since.last.pregnancy = NA"
            ## for women who are currently pregnant
            num.new.pregnancies <- num.new.pregnancies+1 #13Sep13

            ##28Oct13: update attribute of number of pregnancies
            num.of.pregnancies[female.id.curr[i]] <-
              (num.of.pregnancies[female.id.curr[i]])+1 #28Oct13
          }
          
        ## time since last pregnancy does not need to change here --
        ## it can keep incrementing through the loop?
        
          if (verbose==TRUE && curr.pregnancy.status[female.id.curr[i]]
              == 1){
          cat("Newly pregnant women are ", female.id.curr[i], "\n")
           }   
        }
      }
    }

    nw%v%"curr.pregnancy.status" <- curr.pregnancy.status
    nw%v%"time.since.curr.pregnancy" <- time.since.curr.pregnancy
    nw%v%"time.of.curr.pregnancy" <- time.of.curr.pregnancy
    nw%v%"time.since.last.pregnancy" <- time.since.last.pregnancy
    nw%v%"art.status" <- art.status ## 11 June 2013: update art states for 
    nw%v%"art.type" <- art.type ## women after cessation of ART
    nw%v%"vl.art.traj.slope" <- vl.art.traj.slope
    nw%v%"time.since.art.cessation" <- time.since.art.cessation
    nw%v%"num.of.pregnancies" <- num.of.pregnancies #28Oct13
    nw%v%"cd4.at.art.cessation" <- cd4.at.art.cessation #20Feb14
    nw%v%"vl.at.art.cessation" <- vl.at.art.cessation #21Feb14
    nw%v%"time.of.art.cessation" <- time.of.art.cessation #21Feb14
    
    ## update time since pregnancy where other pregnancy related attributes
    ## are updated

    preg.data <- paste(date, ".preg", ".csv", sep="")

    ###########################################################
    ## 23 Oct 2013: Record number of new pregnancies
    ###########################################################    
    net <- network.collapse(nw, at=time)
    tot.num.pregnancies <- length(which(net%v%"curr.pregnancy.status" == 1))

    ###########################################################
    
    write.table(cbind(time,
                      num.new.pregnancies,
                      tot.num.pregnancies
                      ),
                   file=preg.data, ## to note total number of new infections
                   append=TRUE,
                   col.names=FALSE,
                   row.names=FALSE
                   )

            

    return(nw)
  }
