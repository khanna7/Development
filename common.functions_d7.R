###########################################################
### Biological Functions
###########################################################

## 8Jul2013: Add "art.status" argument to "assign.mortality.male" and
## "assign.mortality.female."

## 7Jul2013: Changed name of "time.infection.to.peak.viremia"
## to "time.infection.to.peak.viral.load."

## 6 July 2013: Rewrite "assign.asmr.male" and "assign.asmr.female" functions
## to implement age-based mortality for HIV-negatives and CD4 based mortality
## for infecteds

## 5 July 2013: Implemented viral load with with 4 trajectories:
## treatment-naive, regular ART, sc ART for pregnant women (ongoing),
## sc ART for pregnant women (post-cessation).

## 11 June 13: Describe effect on scART for pregnant women
## Can write separate function(s) for
## increase in viral load and decrease in CD4 count
## after cessation of sc ART (Option A).

## 10 June 2013: Need to describe impact of ART treatment type "2" --
## Option A for pregnant women with CD4 < 350,
## for its impact on CD4 count and viral load.
## The treatment status of such women can be described as "3" when they are off
## treatment. (art.status 1 is regular treatment,
## 2 is Option A before delivery for CD4<350,
## 3 is when Option A treatment is terminated.)

## 10 June 2013: Account for change in infectivity of HIV-positive
## women on account of pregnancy. This is a nodal level attribute of pregnancy.

## 7 June 2013: min.chronic.infectivity.unadj: "unadj" refers to
## number of sex acts
## and HSV prevalence estimates are variable, we are currently in the high end,
## but to reduce overall transmissions, reducing HSC prevalence might
## be possible too.

## 4 June 2013: Try test-based development, removing the networks from being inserted as
## an argument inside of main functions

## 29 May: Rewrite "compute.viral load" so that at the end of each time step,
## the viral load is the average of the viral loads at the two endpoints,
## rather than the value of the right endpoint.

## 29 May 2013: Rewrite "compute.cd4.counts" function

## 28 May 2013: Adjust cd4 count and viral load for size of timestep.

## 27 May 2013: Adjust "infectivity.today" to account for size of timestep.
## and add "num.sex.acts.per.timestep" argument

## 24 May 2013: To fix "assign.infectivity" function.

### Classify infection  state

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

classify.stage.numeric <- function(x){
  if (x %in% acute.length){
    stage.of.infection <- 0
  } else  if (x %in% chronic.length){
    stage.of.infection <- 1
  } else  if (x %in% late.length){
    stage.of.infection <- 2
  }
  return(stage.of.infection)
}

classify.stage.numeric.rewrite <- function(time.since.infection){
  ## less preferable, since it 
  ## takes the entire vector "time.since.infection" as input --
  ## but should be usable with apply, too

  stage.of.infection <- rep(NA, length(time.since.infection))

  for (i in 1:length(time.since.infection)){

    if (time.since.infection[i] %in% acute.length){
      stage.of.infection[i] <- 0

    } else if (time.since.infection[i] %in% chronic.length){
      stage.of.infection[i] <- 1

    } else if (time.since.infection[i] %in% late.length){
      stage.of.infection[i] <- 2

    }

  }
  return(stage.of.infection)
}


## Compute Untreated Viral Load

  compute.viral.load <-
  function (nw, verbose, 
            time.infection.to.peak.viral.load,
            peak.viral.load,
            time.infection.to.viral.set.point,
            set.point.viral.load,
            time.infection.to.late.stage,
            late.stage.viral.load,
            dur.inf,
            time.to.full.supp,
            undetectable.vl,
            size.of.timestep # 28May13
            ) {

     ## Loop to assign viral load based on time sice infection
     ## this piece of code does not have to be changed because
     ## we have changed the length of the relevant parameters here
     ## on account of size of the timestep --
     ## a.time.infection.to.peark.viral.load;
     ## b. time.infection.to.viral.set.point
     ## c. time.infection.to.late.stage
     ## d. dur.inf


  ## Top Matter  
     inf.status <- nw%v%"inf.status"
     art.status <- nw%v%"art.status"
     time.since.infection <- nw%v%"time.since.infection" 
     time.since.art.initiation <- nw%v%"time.since.art.initiation" 
     viral.load.today <- nw%v%"viral.load.today"
     
  ## Male and Female IDs    
     male.id.curr <- nwmodes(nw, 1)
     female.id.curr <- nwmodes(nw, 2)

     ## 7 Jun 13: check following lines
     ## male.viral.load.today <- (nw%v%"cd4.count.today")[male.id.curr]
     ## female.viral.load.today <- (nw%v%"cd4.count.today")[female.id.curr]

     for (i in 1:length(time.since.infection)){

       if (!is.na(art.status[i]) && art.status[i] == 0) { # untreated individuals --
                                 # men and women are not considered separately 

           if (time.since.infection[i] <= time.infection.to.peak.viral.load){

             viral.load.today[i] <- peak.viral.load*time.since.infection[i]/
               time.infection.to.peak.viral.load ## assumes viral load 0 at infection


             if (verbose){
               cat("Untreated actors going to peak viral.load are: ", i, "\n")
             }
             
           } else if ( (time.since.infection[i] %in%
                        (time.infection.to.peak.viral.load+1):
                        (time.infection.to.viral.set.point))
                      )  {
               
               viral.load.today[i] <- peak.viral.load -
               (peak.viral.load - set.point.viral.load) *
                 (time.since.infection[i] - time.infection.to.peak.viral.load)/
                   (time.infection.to.viral.set.point - time.infection.to.peak.viral.load)
               
             if (verbose){
               cat("Untreated actors going to set-point viral.load are: ", i, "\n")
             }
             
           } else if ( (time.since.infection[i] %in%
                        (time.infection.to.viral.set.point+1):
                        (time.infection.to.late.stage))
                      ) {
             viral.load.today[i] <- set.point.viral.load

             if (verbose){
               cat("Untreated actors at set-point viral.load are: ", i, "\n")
             }
             
           } else if ( (time.since.infection[i] > time.infection.to.late.stage)
                    ){
           
             viral.load.today[i] <- set.point.viral.load +
               (late.stage.viral.load - set.point.viral.load)*
                 (time.since.infection[i] - time.infection.to.late.stage)/
                   ( (dur.inf-1) - time.infection.to.late.stage )

             if (verbose){
               cat("Untreated actors at final stage are: ", i, "\n")
             }


           } ## check this one what happens to viral load in the last stage


         }  else if (!is.na(art.status[i]) && art.status[i] == 1) {
                                      # treated individuals --
                                      # men and women are not considered separately

           if (viral.load.today[i] >= undetectable.vl){

             if ( !is.na(time.since.art.initiation[i]) &&
                 (time.since.art.initiation[i] < time.to.full.supp)){
               ##viral.load.current <- viral.load.today[i]
               viral.load.today[i] <- viral.load.today[i] - ((viral.load.today[i] -
                                       undetectable.vl)/time.to.full.supp)
             
               if (verbose){
                 cat("Treated actors, not yet suppressed, are: ", i, "\n")
               }

             }
               
         } else if ((!is.na(time.since.art.initiation[i])) &&
                    (time.since.art.initiation[i] >= time.to.full.supp)){
           viral.load.today[i] <- undetectable.vl

           if (verbose){
               cat("Treated actors, fully suppressed, are: ", i, "\n")
             }
         }
       }
     }

     ## Update viral load
        nw %v% "viral.load.today" <- viral.load.today

     ## Return network object
        return(nw)
   }

compute.viral.load.midpt <- function(nw, verbose, 
            time.infection.to.peak.viral.load,
            peak.viral.load,
            time.infection.to.viral.set.point,
            set.point.viral.load,
            time.infection.to.late.stage,
            late.stage.viral.load,
            dur.inf,
            time.to.full.supp,
            undetectable.vl,
            size.of.timestep # 28May13
            ) {

       ## Loop to assign viral load based on time sice infection
     ## this piece of code does not have to be changed because
     ## we have changed the length of the relevant parameters here
     ## on account of size of the timestep --
     ## a.time.infection.to.peark.viral.load;
     ## b. time.infection.to.viral.set.point
     ## c. time.infection.to.late.stage
     ## d. dur.inf
     ## e. take midpoints instead of right endpoints to compute viral loads over
     ## 14-day intervals

  ## Top Matter  
     inf.status <- nw%v%"inf.status"
     art.status <- nw%v%"art.status"
     time.since.infection <- nw%v%"time.since.infection" 
     time.since.art.initiation <- nw%v%"time.since.art.initiation" 
     viral.load.today <- nw%v%"viral.load.today"
     
  ## Male and Female IDs    
     male.id.curr <- nwmodes(nw, 1)
     female.id.curr <- nwmodes(nw, 2)

     
       for (i in 1:length(time.since.infection)){

       if (!is.na(art.status[i]) && art.status[i] == 0) { # untreated individuals --
                                 # men and women are not considered separately 

           if (time.since.infection[i] <= time.infection.to.peak.viral.load){

             viral.load.today[i] <- mean(c(0, peak.viral.load))

             if (verbose){
               cat("Untreated actors going to peak viral.load are: ", i, "\n")
             }
             
           }  else if ( (time.since.infection[i] %in%
                        (time.infection.to.peak.viral.load+1):
                        (time.infection.to.viral.set.point))
                      )  {
               
               viral.load.today[i] <- peak.viral.load -
                                           ((peak.viral.load - set.point.viral.load) *
                                           (time.since.infection[i] -
                                            time.infection.to.peak.viral.load)/
                                           (time.infection.to.viral.set.point -
                                            time.infection.to.peak.viral.load))*
                                           1/2 # so we arrive at the midpoint

               
             if (verbose){
               cat("Untreated actors going to set-point viral.load are: ", i, "\n")
             }
             
           } else if ( (time.since.infection[i] %in%
                        (time.infection.to.viral.set.point+1):
                        (time.infection.to.late.stage))
                      ) {
             viral.load.today[i] <- set.point.viral.load

             if (verbose){
               cat("Untreated actors at set-point viral.load are: ", i, "\n")
             }
             
           }  else if ( (time.since.infection[i] > time.infection.to.late.stage)
                    ){
           
             viral.load.today[i] <- set.point.viral.load +
               ((late.stage.viral.load - set.point.viral.load)*
                 (time.since.infection[i] - time.infection.to.late.stage)/
                   ( (dur.inf-1) - time.infection.to.late.stage ))*1/2
             # so we arrive at midpoint

             if (verbose){
               cat("Untreated actors at final stage are: ", i, "\n")
             }


           } ## check this one what happens to viral load in the last stage


         } else if (!is.na(art.status[i]) && art.status[i] == 1) {
                                      # untreated individuals --
                                      # men and women are not considered separately

           if (viral.load.today[i] >= undetectable.vl){
            ## 29May13: should this go to midpoint too??
             if ( !is.na(time.since.art.initiation[i]) &&
                 (time.since.art.initiation[i] < time.to.full.supp)){
               viral.load.current <- viral.load.today[i]
               viral.load.today[i] <- viral.load.today[i] - (viral.load.today[i] -
                                       undetectable.vl)/time.to.full.supp
             
               if (verbose){
                 cat("Treated actors, not yet suppressed, are: ", i, "\n")
               }
             }

             } else if ((!is.na(time.since.art.initiation[i])) &&
                    (time.since.art.initiation[i] >= time.to.full.supp)){
           viral.load.today[i] <- undetectable.vl

           if (verbose){
               cat("Treated actors, fully suppressed, are: ", i, "\n")
             }
         }
       }
     }

     ## Update viral load
        nw %v% "viral.load.today" <- viral.load.today

     ## Return network object
        return(nw)
   }

compute.vl.mp.art2 <- function(nw, verbose, 
                               time.infection.to.peak.viral.load,
                               peak.viral.load,
                               time.infection.to.viral.set.point,
                               set.point.viral.load,
                               time.infection.to.late.stage,
                               late.stage.viral.load,
                               dur.inf,
                               time.to.full.supp,
                               undetectable.vl,
                               size.of.timestep, # 28May13
                               sc.art.vl.dec, # 7Jun13: vl decline w scART
                               sc.art.cd4.rec,
                               ...
                               ) {

     ## 12June13: Rewrite now to compute individual slopes for viral load
     ## trajectories in treated individuals.

     ## 10Jun13: Modify to include decline in CD4 count and increase in
     ## viral load after cessation of treatment in pregnant women (Option A)
     ## Also model CD4 and viral load trajectories for these individuals
     ## while on treatment

     ## Loop to assign viral load based on time sice infection
     ## this piece of code does not have to be changed because
     ## we have changed the length of the relevant parameters here
     ## on account of size of the timestep --
     ## a.time.infection.to.peark.viral.load;
     ## b. time.infection.to.viral.set.point
     ## c. time.infection.to.late.stage
     ## d. dur.inf
     ## e. take midpoints instead of right endpoints to compute viral loads over
     ## 14-day intervals

  ## Top Matter  
     inf.status <- nw%v%"inf.status"
     art.status <- nw%v%"art.status"
     art.type <- nw%v%"art.type"
     time.since.infection <- nw%v%"time.since.infection" 
     time.since.art.initiation <- nw%v%"time.since.art.initiation" 
     viral.load.today <- nw%v%"viral.load.today"
     vl.art.traj.slope <- nw%v%"vl.art.traj.slope"
     
  ## Male and Female IDs    
     male.id.curr <- nwmodes(nw, 1)
     female.id.curr <- nwmodes(nw, 2)
     
     
     for (i in 1:length(time.since.infection)){
       
       if (!is.na(art.status[i]) && art.status[i] == 0) {
                                        # untreated individuals --
                                 # men and women are not considered separately 

           if (time.since.infection[i] <= time.infection.to.peak.viral.load){

             viral.load.today[i] <- mean(c(0, peak.viral.load))

             if (verbose){
               cat("Untreated actors going to peak viral load are: ", i, "\n")
             }
             
           }  else if ( (time.since.infection[i] %in%
                        (time.infection.to.peak.viral.load+1):
                        (time.infection.to.viral.set.point))
                      )  {
               
               viral.load.today[i] <- peak.viral.load -
                                           ((peak.viral.load - set.point.viral.load) *
                                           (time.since.infection[i] -
                                            time.infection.to.peak.viral.load)/
                                           (time.infection.to.viral.set.point -
                                            time.infection.to.peak.viral.load))*
                                           1/2 # so we arrive at the midpoint

               
             if (verbose){
               cat("Untreated actors going to set-point viral load are: ", i, "\n")
             }
             
           } else if ( (time.since.infection[i] %in%
                        (time.infection.to.viral.set.point+1):
                        (time.infection.to.late.stage))
                      ) {
             viral.load.today[i] <- set.point.viral.load

             if (verbose){
               cat("Untreated actors at set-point viral load are: ", i, "\n")
             }
             
           }  else if ( (time.since.infection[i] > time.infection.to.late.stage)
                    ){
           
             viral.load.today[i] <- set.point.viral.load +
               ((late.stage.viral.load - set.point.viral.load)*
                 (time.since.infection[i] - time.infection.to.late.stage)/
                   ( (dur.inf-1) - time.infection.to.late.stage ))*1/2
             # so we arrive at midpoint

             if (verbose){
               cat("Untreated actors at final stage are: ", i, "\n")
             }


           } ## check this one what happens to viral load in the last stage


         } else if (!is.na(art.status[i]) && art.status[i] == 1) {
                                      # untreated individuals --
                                      # men and women are not considered separately

           if (viral.load.today[i] >= undetectable.vl){
            ## 29May13: should this go to midpoint too??
             if ( !is.na(time.since.art.initiation[i]) &&
                 (time.since.art.initiation[i] < time.to.full.supp)){
               viral.load.current <- viral.load.today[i]
               viral.load.today[i] <- viral.load.today[i] - vl.art.traj.slope[i]
               ## 6Jul13: Above should be minus sign -- vl.art.traj.slop is
               ## subtracted from viral.load.today[i]

               if (verbose){
                 cat("Treated actors, not yet suppressed, are: ", i, "\n")
               }
             }

             } else if ((!is.na(time.since.art.initiation[i])) &&
                    (time.since.art.initiation[i] >= time.to.full.supp)){
           viral.load.today[i] <- undetectable.vl

           if (verbose){
               cat("Treated actors, fully suppressed, are: ", i, "\n")
             }
         }
       }
     }

     ## Update viral load
        nw %v% "viral.load.today" <- viral.load.today

     ## Return network object
        return(nw)
   }

compute.vl.mp.art3 <- function(nw, verbose, 
                               time.infection.to.peak.viral.load,
                               peak.viral.load,
                               time.infection.to.viral.set.point,
                               set.point.viral.load,
                               time.infection.to.late.stage,
                               late.stage.viral.load,
                               dur.inf,
                               time.to.full.supp,
                               undetectable.vl,
                               size.of.timestep, # 28May13
                               optA.sc.art.vl.perstep.dec, #5Jul13
                               optA.sc.art.vl.perstep.inc, #5Jul13
                               # optA.daily.vl.reduction=1.1, ##5Jul13
                               ...
                               ) {

     ## 5 Jul 2013: make corrections for option A viral load reduction
  
     ## 12June13: Rewrite now to compute individual slopes for viral load
     ## trajectories in treated individuals.

     ## 10Jun13: Modify to include decline in CD4 count and increase in
     ## viral load after cessation of treatment in pregnant women (Option A)
     ## Also model CD4 and viral load trajectories for these individuals
     ## while on treatment

     ## Loop to assign viral load based on time sice infection
     ## this piece of code does not have to be changed because
     ## we have changed the length of the relevant parameters here
     ## on account of size of the timestep --
     ## a.time.infection.to.peark.viral.load;
     ## b. time.infection.to.viral.set.point
     ## c. time.infection.to.late.stage
     ## d. dur.inf
     ## e. take midpoints instead of right endpoints to compute viral loads over
     ## 14-day intervals

  ## Top Matter  
     inf.status <- nw%v%"inf.status"
     art.status <- nw%v%"art.status"
     art.type <- nw%v%"art.type"
     time.since.infection <- nw%v%"time.since.infection" 
     time.since.art.initiation <- nw%v%"time.since.art.initiation" 
     viral.load.today <- nw%v%"viral.load.today"
     vl.art.traj.slope <- nw%v%"vl.art.traj.slope"
     
  ## Male and Female IDs    
     male.id.curr <- nwmodes(nw, 1)
     female.id.curr <- nwmodes(nw, 2)
     
     ##browser()         
     for (i in 1:length(time.since.infection)){
      ##if(viral.load.today[i] >= 0){
       if ((is.na(art.status[i]) && is.na(art.type[i])) ||
           (art.status[i] == 0 && is.na(art.type[i]))
           ## if both art.status and art.type are NA,
           ## or if art.status is 0 and art.type is NA
           ){ 
                                        # untreated individuals --
                                        # men and women are not considered separately 

           if ( (!is.na(time.since.infection[i])) && 
                (time.since.infection[i] <= time.infection.to.peak.viral.load)
                ){

             viral.load.today[i] <- mean(c(0, peak.viral.load))

             if (verbose){
               cat("Untreated actors going to peak viral load are: ", i, "\n")
             }
             
           }  else if ( (time.since.infection[i] %in%
                        (time.infection.to.peak.viral.load+1):
                        (time.infection.to.viral.set.point))
                      )  {
               
               viral.load.today[i] <- peak.viral.load -
                                           ((peak.viral.load - set.point.viral.load) *
                                           (time.since.infection[i] -
                                            time.infection.to.peak.viral.load)/
                                           (time.infection.to.viral.set.point -
                                            time.infection.to.peak.viral.load))*
                                           1/2 # so we arrive at the midpoint

               
             if (verbose){
               cat("Untreated actors going to set-point viral load are: ", i, "\n")
             }
             
           } else if ( (time.since.infection[i] %in%
                        (time.infection.to.viral.set.point+1):
                        (time.infection.to.late.stage))
                      ) {
             viral.load.today[i] <- set.point.viral.load

             if (verbose){
               cat("Untreated actors at set-point viral load are: ", i, "\n")
             }
             
           }  else if ( (!is.na(time.since.infection[i])) &&
                      (time.since.infection[i] > time.infection.to.late.stage)
                    ){
           
             viral.load.today[i] <- set.point.viral.load +
               ((late.stage.viral.load - set.point.viral.load)*
                 (time.since.infection[i] - time.infection.to.late.stage)/
                   ( (dur.inf-1) - time.infection.to.late.stage ))*1/2
             # so we arrive at midpoint

             if (verbose){
               cat("Untreated actors at final stage are: ", i, "\n")
             }


           } ## check this one what happens to viral load in the last stage


         } else if (art.status[i] == 1 && art.type[i] == 1) {
                                      # individuals on regular ART -- 
                                      # men and women are not considered separately

           if (viral.load.today[i] >= undetectable.vl){
            ## 29May13: should this go to midpoint too??
             if ( !is.na(time.since.art.initiation[i]) &&
                 (time.since.art.initiation[i] < time.to.full.supp)){
               viral.load.current <- viral.load.today[i]
               viral.load.today[i] <- viral.load.today[i] + vl.art.traj.slope[i]
             
               if (verbose){
                 cat("Treated actors on regular ART,
                      not yet suppressed, are: ", i, "\n")
               }
             }
             } else if ((!is.na(time.since.art.initiation[i])) &&
                    (time.since.art.initiation[i] >= time.to.full.supp)){
               viral.load.today[i] <- undetectable.vl

               if (verbose){
               cat("Treated actors, fully suppressed, are: ", i, "\n")
             }
         }
       } else if (art.status[i] == 1 && art.type[i] == 2) {
                                      # individuals on regular ART -- 
                                      # men and women are not considered separately

           if (viral.load.today[i] >= undetectable.vl){
            ## 29May13: should this go to midpoint too??

               viral.load.current <- viral.load.today[i]
               viral.load.today[i] <- viral.load.today[i] - optA.sc.art.vl.perstep.dec
             
               if (verbose){
                 cat("Treated actors on scART,
                      not yet suppressed, are: ", i, "\n")
               }
             }
         } else if (art.status[i] == 1 && art.type[i] == 2) {
                                      # individuals on regular ART -- 
                                      # men and women are not considered separately

           if (viral.load.today[i] >= undetectable.vl){
            ## 29May13: should this go to midpoint too??

               viral.load.current <- viral.load.today[i]
               viral.load.today[i] <- viral.load.today[i] + optA.sc.art.vl.perstep.inc
             
               if (verbose){
                 cat("Treated actors off scART,
                      not yet suppressed, are: ", i, "\n")
               }
             }
         }
     ##}
    }
     ## Update viral load
        nw %v% "viral.load.today" <- viral.load.today

     ## Return network object
        return(nw)
   }

## Describe increase in viral load and
## decrease in CD4 counts after cessation of scART

optA.post.cess.scart <- function(nw,
                                 optA.postcess.cd4.perstep.dec,
                                 optA.postcess.vl.perstep.inc,
                                 ...
                                 ){

     ## 11Jun13: describe decrease in CD4 counts,
     ## increase in viral load, post-cessation
     cd4.count.today <- nw%v%"cd4.count.today"
     viral.load.today <- nw%v%"viral.load.today"
     art.status <- nw%v%"art.status"
     art.type <- nw%v%"art.type"
     curr.pregnancy.status <- nw%v%"curr.pregnancy.status"
     time.since.curr.pregnancy <- nw%v%"time.of.curr.pregnancy"
     time.since.infection <- nw%v%"time.since.infection"


     

   }

###########################################################
### Network Related Functions
###########################################################

    nwmodes <- function(nw, mode) {
      if (mode == 1) out <- 1:(nw %n% 'bipartite')
      if (mode == 2) out <- ((nw %n% 'bipartite')+1):(nw %n% 'n')
      return(out)
    }


###########################################################
### Demographic Functions
###########################################################

    assign.asmr.male <- function(male.curr.age, asmr.male){

      ## male.curr.age is a vector of all men in the population 
      ## asmr.male is a vector of non-AIDS related mortality in the population
      ## asmr.male is a vector of 8 individuals
      
      asmr.male.out <- male.curr.age

      for (i in 1:length(male.curr.age)){
        if (male.curr.age[i] >= 15 && male.curr.age[i] < 20){
          asmr.male.out[i] <- asmr.male[1]
        } else if (male.curr.age[i] >= 20 && male.curr.age[i] < 25){
          asmr.male.out[i] <- asmr.male[2]
        } else if (male.curr.age[i] >= 25 && male.curr.age[i] < 30){
          asmr.male.out[i] <- asmr.male[3]
        } else if (male.curr.age[i] >= 30 && male.curr.age[i] < 35){
          asmr.male.out[i] <- asmr.male[4]
        } else if (male.curr.age[i] >= 35 && male.curr.age[i] < 40){
          asmr.male.out[i] <- asmr.male[5] 
        } else if (male.curr.age[i] >= 40 && male.curr.age[i] < 45){
          asmr.male.out[i] <- asmr.male[6] 
        } else if (male.curr.age[i] >= 45 && male.curr.age[i] < 50){
          asmr.male.out[i] <- asmr.male[7] 
        } else if (male.curr.age[i] >= 50 && male.curr.age[i] < 55){
          asmr.male.out[i] <- asmr.male[8]
        }  else if (male.curr.age[i] >= 55){
          asmr.male.out[i] <- 1
        }
      }
      return(asmr.male.out)
    }

      assign.asmr.female <- function(female.curr.age, asmr.female){

        ## female.curr.age is a vector of all men in the population 
        ## asmr.female is a vector of non-AIDS related mortality in the population
        ## asmr.female is a vector of 8 individuals
      
        asmr.female.out <- female.curr.age
        
        for (i in 1:length(female.curr.age)){
          if (female.curr.age[i] >= 15 && female.curr.age[i] < 20){
            asmr.female.out[i] <- asmr.female[1]
          } else if (female.curr.age[i] >= 20 && female.curr.age[i] < 25){
            asmr.female.out[i] <- asmr.female[2]
          } else if (female.curr.age[i] >= 25 && female.curr.age[i] < 30){
            asmr.female.out[i] <- asmr.female[3]
          } else if (female.curr.age[i] >= 30 && female.curr.age[i] < 35){
            asmr.female.out[i] <- asmr.female[4]
          } else if (female.curr.age[i] >= 35 && female.curr.age[i] < 40){
            asmr.female.out[i] <- asmr.female[5] 
          } else if (female.curr.age[i] >= 40 && female.curr.age[i] < 45){
            asmr.female.out[i] <- asmr.female[6] 
          } else if (female.curr.age[i] >= 45 && female.curr.age[i] < 50){
            asmr.female.out[i] <- asmr.female[7] 
          } else if (female.curr.age[i] >= 50 && female.curr.age[i] < 55){
            asmr.female.out[i] <- asmr.female[8] 
          } else if (female.curr.age[i] >= 55){
            asmr.female.out[i] <- 1
          }
        }
        return(asmr.female.out)
      }


    assign.mortality.male <- function(male.curr.age, asmr.male,
                                      male.inf.status,
                                      male.cd4.today,
                                      male.art.status){

      ## 6Jul13: implement age-based mortality for negatives,
      ## and cd4 based mortality for positives
      
      ## male.curr.age is a vector of all men in the population 
      ## asmr.male is a vector of non-AIDS related mortality in the population
      ## asmr.male is a vector of 8 individuals

      
      asmr.male.out <- male.curr.age

      for (i in 1:length(male.curr.age)){

        if (male.inf.status[i] == 0){
          if (male.curr.age[i] >= 15 && male.curr.age[i] < 20){
            asmr.male.out[i] <- asmr.male[1]
          } else if (male.curr.age[i] >= 20 && male.curr.age[i] < 25){
            asmr.male.out[i] <- asmr.male[2]
          } else if (male.curr.age[i] >= 25 && male.curr.age[i] < 30){
            asmr.male.out[i] <- asmr.male[3]
          } else if (male.curr.age[i] >= 30 && male.curr.age[i] < 35){
            asmr.male.out[i] <- asmr.male[4]
          } else if (male.curr.age[i] >= 35 && male.curr.age[i] < 40){
            asmr.male.out[i] <- asmr.male[5] 
          } else if (male.curr.age[i] >= 40 && male.curr.age[i] < 45){
            asmr.male.out[i] <- asmr.male[6] 
          } else if (male.curr.age[i] >= 45 && male.curr.age[i] < 50){
            asmr.male.out[i] <- asmr.male[7] 
          } else if (male.curr.age[i] >= 50 && male.curr.age[i] < 55){
            asmr.male.out[i] <- asmr.male[8]
          }  else if (male.curr.age[i] >= 55){
            asmr.male.out[i] <- 1
          }
        } else if (male.inf.status[i] == 1){
             if (is.na(male.art.status[i]) || male.art.status[i] == 0){
               asmr.male.out[i] <- 38.6/(100*365)
             } else if (!is.na(male.art.status[i]) && male.art.status[i] > 0){
               if (male.cd4.today[i] %/% 100 ==0 ){ #quotient operator is needed
                 asmr.male.out[i] <- 12.8/(100*365)
               } else if (male.cd4.today[i] %/% 100 == 1 ){
                 asmr.male.out[i] <- 5.4/(100*365)
               } else if (male.cd4.today[i] %/% 100 == 2 ){
                 asmr.male.out[i] <- 2.7/(100*365)
               } else if (male.cd4.today[i] %/% 100 > 2 ){
                 asmr.male.out[i] <- 2.0/(100*365)
               }
             }
           }
      }
      stopifnot(all(asmr.male.out <= 1))
      return(asmr.male.out)
    }

      assign.mortality.female <- function(female.curr.age, asmr.female,
                                          female.inf.status,
                                          female.cd4.today,
                                          female.art.status){

        ## 6Jul13: implement age-based mortality for negatives,
        ## and cd4 based mortality for positives
        
        ## female.curr.age is a vector of all men in the population 
        ## asmr.female is a vector of non-AIDS related mortality in the population
        ## asmr.female is a vector of 8 individuals
      
        asmr.female.out <- female.curr.age
        
        for (i in 1:length(female.curr.age)){
          if (female.inf.status[i] == 0){
            if (female.curr.age[i] >= 15 && female.curr.age[i] < 20){
              asmr.female.out[i] <- asmr.female[1]
            } else if (female.curr.age[i] >= 20 && female.curr.age[i] < 25){
              asmr.female.out[i] <- asmr.female[2]
            } else if (female.curr.age[i] >= 25 && female.curr.age[i] < 30){
              asmr.female.out[i] <- asmr.female[3]
            } else if (female.curr.age[i] >= 30 && female.curr.age[i] < 35){
              asmr.female.out[i] <- asmr.female[4]
            } else if (female.curr.age[i] >= 35 && female.curr.age[i] < 40){
              asmr.female.out[i] <- asmr.female[5] 
            } else if (female.curr.age[i] >= 40 && female.curr.age[i] < 45){
              asmr.female.out[i] <- asmr.female[6] 
            } else if (female.curr.age[i] >= 45 && female.curr.age[i] < 50){
              asmr.female.out[i] <- asmr.female[7] 
            } else if (female.curr.age[i] >= 50 && female.curr.age[i] < 55){
              asmr.female.out[i] <- asmr.female[8] 
            } else if (female.curr.age[i] >= 55){
              asmr.female.out[i] <- 1
            }
          } else if (female.inf.status[i] == 1){
             if (is.na(female.art.status[i]) || female.art.status[i] == 0){
               asmr.female.out[i] <- 38.6/(100*365)
             } else if (!is.na(female.art.status[i]) && female.art.status[i] > 0){
               if (female.cd4.today[i] %/% 100 ==0 ){ ## need quotient operator
                 asmr.female.out[i] <- 12.8/(100*365)
               } else if (female.cd4.today[i] %/% 100 == 1 ){
                 asmr.female.out[i] <- 5.4/(100*365)
               } else if (female.cd4.today[i] %/% 100 == 2 ){
                 asmr.female.out[i] <- 2.7/(100*365)
               } else if (female.cd4.today[i] %/% 100 > 2 ){
                 asmr.female.out[i] <- 2.0/(100*365)
               }
             }
           }
        }
        ## cat(which(asmr.female.out > 1))
        ## cat(asmr.female.out[asmr.female.out > 1])
        stopifnot(all(asmr.female.out <= 1))
        return(asmr.female.out)
      }


###########################################################
### Epidemiological Functions
###########################################################

### Assign infectivity based on viral load

    assign.infectivity <-
  # 10Jun13: Account for effect of pregnancy
  function(nw, verbose,
           ## function describes extrapolated 
           ## infectivity at lowest viral load of log 2
           ## Hughes et al. give minimum infectivity at viral load of log 3.
           ## infection stages coded numerically
           min.chronic.infectivity.unadj, # changed in "common.functions_d3"
           num.sex.acts.per.timestep,
           acute.mult,
           late.mult,
           preg.mult, #10Jun13
           ...) {

  ## Top Matter
     inf.status <- nw%v%"inf.status"
     art.status <- nw%v%"art.status"
     viral.load.today <- nw%v%"viral.load.today"
     stage.of.infection <- nw%v%"stage.of.infection"
     time.since.infection <- nw%v%"time.since.infection"
     infectivity.today <- nw%v%"infectivity.today"
     curr.pregnancy.status <- nw%v%"curr.pregnancy.status"
     for (i in 1:network.size(nw)) {

       if (inf.status[i] == 1){
         ## 24 May '13: Add conditional for stage.of.infection not be ing NA
          if (!is.na(stage.of.infection[i])){
            ## browser()
            if (stage.of.infection[i] == 1) {
                                        # for chronically infected
             ## if (viral.load.today[i] == 3) {
             if (viral.load.today[i] < 2) {
               infectivity.today[i] <- 0
               infectivity.today[i] <- 1-(1-infectivity.today[i])^num.sex.acts.per.timestep
             }
           
           else if (viral.load.today[i] == 2) {
             infectivity.today[i] <- min.chronic.infectivity.unadj
             infectivity.today[i] <- 1-(1-infectivity.today[i])^num.sex.acts.per.timestep
           } else if (viral.load.today[i] > 2) {
             infectivity.today[i] <- min.chronic.infectivity.unadj*2.89^
             (viral.load.today[i] - 2)
             infectivity.today[i] <- 1-(1-infectivity.today[i])^num.sex.acts.per.timestep
           }

           if (verbose){
             cat("Chronically Infected are ", i,
                 "with viral load", viral.load.today[i],                 
                 "with infectivity", infectivity.today[i],
                 "\n")

           }
           } else if (stage.of.infection[i] == 0) {
                                        # for acutely infected
             if (viral.load.today[i] < 2) {
               infectivity.today[i] <- 0
               infectivity.today[i] <- 1-(1-infectivity.today[i])^num.sex.acts.per.timestep
             }  else if (viral.load.today[i] == 2) {

               infectivity.today[i] <- min.chronic.infectivity.unadj*acute.mult
               infectivity.today[i] <- 1-(1-infectivity.today[i])^num.sex.acts.per.timestep
                                  ## set as 0.00497
           } else if (viral.load.today[i] > 2) {
             infectivity.today[i] <- min.chronic.infectivity.unadj*2.89^
             (viral.load.today[i] - 2)*acute.mult 
             infectivity.today[i] <- 1-(1-infectivity.today[i])^num.sex.acts.per.timestep
           }

           if (verbose){
             cat("Acutely Infected are ", i,
                 "with viral load", viral.load.today[i],
                 "with infectivity", infectivity.today[i],
                 "\n")
           }
         } else if (stage.of.infection[i] == 2){
                                        # for late infected
           if (viral.load.today[i] < 2) {
             infectivity.today[i] <- 0
             infectivity.today[i] <- 1-(1-infectivity.today[i])^num.sex.acts.per.timestep
           }

           else if (viral.load.today[i] == 2) {
             infectivity.today[i] <- min.chronic.infectivity.unadj*late.mult
                                  ## set as 0.00497
             infectivity.today[i] <- 1-(1-infectivity.today[i])^num.sex.acts.per.timestep
           } else if (viral.load.today[i] > 2) {
             infectivity.today[i] <- min.chronic.infectivity.unadj*2.89^
                                     (viral.load.today[i]-2)*
                                       late.mult
             infectivity.today[i] <- 1-(1-infectivity.today[i])^num.sex.acts.per.timestep
           }

           if (verbose){
             cat("Late-Infected are ", i,
                 "with viral load", viral.load.today[i],                 
                 "with infectivity", infectivity.today[i],
                 "\n")
           }

         }
           
         }
       }
     }
     ## 10Jun13: Adjust infectivity of pregnant women
     ## browser()
     curr.pregnant <- which(curr.pregnancy.status == 1)
     if (length(curr.pregnant) > 0){
       infectivity.today[curr.pregnant] <-
       infectivity.today[curr.pregnant]*preg.mult
   }
     if (verbose){
       cat("Currently pregnant women are ", curr.pregnant, "\n",
           "Adjusted infectivities of pregnant women: ",
           infectivity.today[curr.pregnant], 
           "\n")
           }
     
     ## Update Infectivity
        nw%v%"infectivity.today" <- infectivity.today


     
     ## Return Network Object
        return(nw)

   }


  
fac.fn <- function(num){
  if  (num == 1) {
    return (num)
  }
  else {
    return(num*fac.fn(num-1))
  }
}

arith.series <- function(num){
  if  (num<=0) {
    return (num)
  }
  else {
    return(num+arith.series(num-1))
  }
}

