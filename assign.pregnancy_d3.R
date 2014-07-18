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
  function(nw, verbose=TRUE,
           full.term,
           preg.prob,
           min.preg.interval,
           optA.vl.reduction, #12Jun13
           sc.art.postcess.ret.bl, #12Jun13
           ...
           ) {

    male.id.curr <- nwmodes(nw, 1)
    female.id.curr <- nwmodes(nw, 2)

    curr.pregnancy.status <- nw%v%"curr.pregnancy.status"
    time.since.curr.pregnancy <- nw%v%"time.since.curr.pregnancy"
    time.of.curr.pregnancy <- nw%v%"time.of.curr.pregnancy"
    time.since.last.pregnancy <- nw%v%"time.since.last.pregnancy"
    art.status <- nw%v%"art.status"
    art.type <- nw%v%"art.type"
    vl.art.traj.slope <- nw%v%"vl.art.traj.slope"

    time.of.art.cessation <- nw%v%"time.of.art.cessation" #25Jul13
    time.since.art.cessation <- nw%v%"time.since.art.cessation" #25Jul13

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

            if (!is.na(art.type[i]) && art.type[i] == 2) { ## for women receiving 
              art.status[i] <- 0 # update ART-status for  sc ART (Option A) women
              art.type[i]   <- 3 # update ART type, for post-sc ART
              vl.art.traj.slope[i] <- optA.vl.reduction/sc.art.postcess.ret.bl
                               # 12Jun13
              time.of.art.cessation[i] <- time
              time.since.art.cessation[i] <- 0
                               # 25Jul13: Populate "time.since.art.cessation"
            
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

    for (i in 1:length(female.in.rel)){
       ## model pregnancy as a stochastic process in women who meet criteria
       ## browser()
      
      if (curr.pregnancy.status[female.in.rel[i]] == 0 &&
          (is.na(time.since.last.pregnancy[female.in.rel[i]]) || 
           time.since.last.pregnancy[female.in.rel[i]] > min.preg.interval)
          ) {
         ## getting pregnant also conditional on time since last pregnancy. 
        
        curr.pregnancy.status[female.in.rel[i]] <- rbinom(1, 1, preg.prob)
        
        time.since.curr.pregnancy[female.in.rel[i]] <- 0
        time.of.curr.pregnancy[female.in.rel[i]] <- time
        ## time since last pregnancy does not need to change here --
        ## it can keep incrementing through the loop?
        
        if (verbose==TRUE && curr.pregnancy.status[female.in.rel[i]] == 1){
          cat("Newly pregnant women are ", female.in.rel[i], "\n")
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
    
    ## update time since pregnancy where other pregnancy related attributes
    ## are updated

    return(nw)
  }
