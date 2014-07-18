###########################################################
### Biological Functions
###########################################################

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

### Function for computing decline in CD4 count

 compute.cd4.count <- function(nw, verbose,
                               cd4.at.infection.male,
                               cd4.at.infection.female,
                               per.day.untreated.cd4.decline,
                               cd4.recovery.time,
                               per.day.cd4.recovery,
                               ...
                               ){

   ## Top Matter
      inf.status <- nw%v%"inf.status"
      art.status <- nw%v%"art.status"
      time.since.infection <- nw%v%"time.since.infection" 
      time.since.art.initiation <- nw%v%"time.since.art.initiation" 
      cd4.count.today <- nw%v%"cd4.count.today"

      if (verbose){
        cat("ART status of infectives: ", art.status[which(inf.status==1)], "\n")
        cat("Cross-Tabulation of Infection and ART Status: ",
            xtabs(~factor(inf.status, exclude=NULL)+ ## for everyone
                  factor(art.status, exclude=NULL)),
            "\n")
      }

   ## Male and Female IDs    
      male.id.curr <- nwmodes(nw, 1)
      female.id.curr <- nwmodes(nw, 2)

      male.cd4.today <- (nw%v%"cd4.count.today")[male.id.curr]
      female.cd4.today <- (nw%v%"cd4.count.today")[female.id.curr]

      ## For help with debugging
      if (verbose) {
        cd4.count.prev <- cd4.count.today
      }

   ## Men
      for (i in length(male.id.curr)) {

        if (inf.status[i] == 1){

          if (!is.na(art.status[i]) && art.status[i] == 0) { # untreated individuals

            cd4.count.today[i] <-
              cd4.at.infection.male - # this is a constant value
                time.since.infection[i]*per.day.untreated.cd4.decline

             cat ("Declining men: ", i, "\n")

          } else if (!is.na(art.status[i]) && art.status[i] == 1) { # individuals on art
 ##           if ()){
            if ( (!is.na(time.since.art.initiation[i])) &&
                  ## cd4 recovery in men on art
                (time.since.art.initiation[i] <= cd4.recovery.time)  && 
                 (cd4.count.today[i] <= cd4.at.infection.male)
                ) { ## cd4 recovery only for first 3 years,
                ## or until  cd4 reaches healthy level
                cd4.count.today[i] <- cd4.count.today[i] + per.day.cd4.recovery
                ## recovery of 15 cells/month

   ##           }
                 cat ("Recovering men: ", i, "\n")
            }
          }
        }
      }

      for (i in (min(female.id.curr)):(max(female.id.curr)) ) {

        if (inf.status[i] == 1){

          if (!is.na(art.status[i]) && art.status[i] == 0) { # untreated individuals

            cd4.count.today[i] <-
              cd4.at.infection.female - # this is a constant value
                time.since.infection[i]*per.day.untreated.cd4.decline

            if (verbose){
              cat ("Declining women: ", i, "\n")
            }
              
          } else if (!is.na(art.status[i]) && art.status[i] == 1) { # individuals on art
            if ( (!is.na(time.since.art.initiation[i])) &&
                  ## cd4 recovery in men on art
                (time.since.art.initiation[i] <= cd4.recovery.time)  && 
                 (cd4.count.today[i] <= cd4.at.infection.female)
                ) { ## cd4 recovery only for first 3 years,
                ## or until  cd4 reaches healthy level
                cd4.count.today[i] <- cd4.count.today[i] + per.day.cd4.recovery
                ## recovery of 15 cells/month

                if (verbose) {
                  cat ("Recovering women: ", i, "\n")
                }
              }
          }
        }
      }

      
  ## For Debugging
     if (verbose){

       recoveries <- which (cd4.count.prev < cd4.count.today)
       declines <- which (cd4.count.prev > cd4.count.today)

       cat ("ART Status of Declines: ", art.status[declines], "\n")
       cat ("ART Status of Recoveries: ", art.status[recoveries], "\n")

       cat ("Infection Status of Declines: ", inf.status[declines], "\n")
       cat ("Infection Status of Recoveries: ", inf.status[recoveries], "\n")

       cat ("Magnitude of Declines: ", cd4.count.today[declines] -
            cd4.count.prev[declines], "\n")
       cat ("Magnitude of Recoveries: ", cd4.count.today[recoveries] -
            cd4.count.prev[recoveries], "\n")

       cat ("Absolute CD4 counts of Declines: ", cd4.count.today[declines], "\n")
       cat ("Absolute CD4 counts of Recoveries: ", cd4.count.today[recoveries], "\n")

     }
       
  ## Update cd4 count for everyone in network
  nw%v%"cd4.count.today" <- cd4.count.today

  return(nw)
  
}

## Compute Untreated Viral Load

  compute.viral.load <-
  function (nw, verbose, 
            time.infection.to.peak.viremia,
            peak.viral.load,
            time.infection.to.viral.set.point,
            set.point.viral.load,
            time.infection.to.late.stage,
            late.stage.viral.load,
            dur.inf,
            time.to.full.supp,
            undetectable.vl
            ) {


  ## Top Matter  
     inf.status <- nw%v%"inf.status"
     art.status <- nw%v%"art.status"
     time.since.infection <- nw%v%"time.since.infection" 
     time.since.art.initiation <- nw%v%"time.since.art.initiation" 
     viral.load.today <- nw%v%"viral.load.today"
     
  ## Male and Female IDs    
     male.id.curr <- nwmodes(nw, 1)
     female.id.curr <- nwmodes(nw, 2)

     male.viral.load.today <- (nw%v%"cd4.count.today")[male.id.curr]
     female.viral.load.today <- (nw%v%"cd4.count.today")[female.id.curr]

     for (i in 1:length(time.since.infection)){

       if (!is.na(art.status[i]) && art.status[i] == 0) { # untreated individuals --
                                 # men and women are not considered separately 

           if (time.since.infection[i] <= time.infection.to.peak.viral.load){

             viral.load.today[i] <- peak.viral.load*time.since.infection[i]/
               time.infection.to.peak.viral.load ## assumes viral load 0 at infection


             if (verbose){
               cat("Untreated actors going to peak viremia are: ", i, "\n")
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
               cat("Untreated actors going to set-point viremia are: ", i, "\n")
             }
             
           } else if ( (time.since.infection[i] %in%
                        (time.infection.to.viral.set.point+1):
                        (time.infection.to.late.stage))
                      ) {
             viral.load.today[i] <- set.point.viral.load

             if (verbose){
               cat("Untreated actors at set-point viremia are: ", i, "\n")
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
                                      # untreated individuals --
                                      # men and women are not considered separately

           if (viral.load.today[i] >= undetectable.vl){

             if ( !is.na(time.since.art.initiation[i]) &&
                 (time.since.art.initiation[i] < time.to.full.supp)){
               viral.load.current <- viral.load.today[i]
               viral.load.today[i] <- (viral.load.today[i] -
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



###########################################################
### Epidemiological Functions
###########################################################

### Assign infectivity based on viral load

    assign.infectivity <-
  function(nw, verbose,
           ## function describes extrapolated 
           ## infectivity at lowest viral load of log 2
           ## Hughes et al. give minimum infectivity at viral load of log 3.
           ## infection stages coded numerically
           min.chronic.infectivity,
           acute.mult,
           late.mult,
           ...) {

  ## Top Matter
     inf.status <- nw%v%"inf.status"
     art.status <- nw%v%"art.status"
     viral.load.today <- nw%v%"viral.load.today"
     stage.of.infection <- nw%v%"stage.of.infection"
     time.since.infection <- nw%v%"time.since.infection"
     infectivity.today <- nw%v%"infectivity.today"

     
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
             }
           
           else if (viral.load.today[i] == 2) {
             infectivity.today[i] <- min.chronic.infectivity

           } else if (viral.load.today[i] > 2) {
             infectivity.today[i] <- min.chronic.infectivity*2.89^
             (viral.load.today[i] - 2)
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
             }  else if (viral.load.today[i] == 2) {

               infectivity.today[i] <- min.chronic.infectivity*acute.mult
                                  ## set as 0.00497
           } else if (viral.load.today[i] > 2) {
             infectivity.today[i] <- min.chronic.infectivity*2.89^
             (viral.load.today[i] - 2)*acute.mult 

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
           }

           else if (viral.load.today[i] == 2) {
             infectivity.today[i] <- min.chronic.infectivity*late.mult
                                  ## set as 0.00497
           } else if (viral.load.today[i] > 2) {
             infectivity.today[i] <- min.chronic.infectivity*2.89^
                                     (viral.load.today[i]-2)*
                                       late.mult
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
     ## Update Infectivity
        nw%v%"infectivity.today" <- infectivity.today

     ## Return Network Object
        return(nw)

   }


  
