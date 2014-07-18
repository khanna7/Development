###########################################################
### Biological Functions
###########################################################

## 10Jul2013: Separated CD4 count function into its own
## file

###########################################################

### Function for computing decline in CD4 count
 compute.cd4.count <- function(nw, verbose,
                               cd4.at.infection.male,
                               cd4.at.infection.female,
                               per.day.untreated.cd4.decline,
                               cd4.recovery.time,
                               per.day.cd4.recovery,
                               size.of.timestep,
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
              ## cd4.at.infection.male - # this is a constant value
              ##   time.since.infection[i]*per.day.untreated.cd4.decline
              cd4.at.infection.male - # this is a constant value
                (time.since.infection[i]*per.day.untreated.cd4.decline*size.of.timestep)

             cat ("Declining men: ", i, "\n")

          } else if (!is.na(art.status[i]) && art.status[i] == 1) { # individuals on art
 ##           if ()){
            if ( (!is.na(time.since.art.initiation[i])) &&
                  ## cd4 recovery in men on art
                (time.since.art.initiation[i] <= cd4.recovery.time)  && 
                 (cd4.count.today[i] <= cd4.at.infection.male)
                ) { ## cd4 recovery only for first 3 years,
                ## or until  cd4 reaches healthy level
                ## cd4.count.today[i] <- cd4.count.today[i] + per.day.cd4.recovery
                              ## recovery of 15 cells/month
                cd4.count.today[i] <- cd4.count.today[i] + (per.day.cd4.recovery*size.of.timestep)


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
              ## cd4.at.infection.female - # this is a constant value
              ##   time.since.infection[i]*per.day.untreated.cd4.decline
              cd4.at.infection.female - # this is a constant value
                (time.since.infection[i]*per.day.untreated.cd4.decline*size.of.timestep)
               ## 28 May 2013

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
                ## cd4.count.today[i] <- cd4.count.today[i] + per.day.cd4.recovery
                ## recovery of 15 cells/month
                   cd4.count.today[i] <- cd4.count.today[i] + (per.day.cd4.recovery*size.of.timestep)
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


 ## Rewrite CD4.count -- to simplify computations so today's cd4 is being computed
 ## only with respect to yesterday's CD4.

 compute.cd4.count.rewrite <- function(nw, verbose,
                                       cd4.at.infection.male,
                                       cd4.at.infection.female,
                                       per.day.untreated.cd4.decline,
                                       cd4.recovery.time,
                                       per.day.cd4.recovery,
                                       size.of.timestep,
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

            cd4.current <- cd4.count.today[i]
            cd4.count.today[i] <- cd4.current - 
                (per.day.untreated.cd4.decline*size.of.timestep)

             cat ("Declining men: ", i, "\n")

          } else if (!is.na(art.status[i]) && art.status[i] == 1) { # individuals on art
 ##           if ()){
            if ( (!is.na(time.since.art.initiation[i])) &&
                  ## cd4 recovery in men on art
                (time.since.art.initiation[i] <= cd4.recovery.time)  && 
                 (cd4.count.today[i] <= cd4.at.infection.male)
                ) { ## cd4 recovery only for first 3 years,
                ## or until  cd4 reaches healthy level
                ## cd4.count.today[i] <- cd4.count.today[i] + per.day.cd4.recovery
                              ## recovery of 15 cells/month
              cd4.current <- cd4.count.today[i] 
              cd4.count.today[i] <- cd4.current + (per.day.cd4.recovery*size.of.timestep)

                 cat ("Recovering men: ", i, "\n")
            }
          }
        }
      }

      for (i in (min(female.id.curr)):(max(female.id.curr)) ) {

        if (inf.status[i] == 1){

          if (!is.na(art.status[i]) && art.status[i] == 0) { # untreated individuals

            cd4.current <- cd4.count.today[i]
            cd4.count.today[i] <- cd4.current -
              (per.day.untreated.cd4.decline*size.of.timestep)

            if (verbose){
              cat ("Declining women: ", i, "\n")
            }
              
          } else if (!is.na(art.status[i]) && art.status[i] == 1) { # individuals on art
            if ( (!is.na(time.since.art.initiation[i])) &&
                  ## cd4 recovery in wommen on art
                (time.since.art.initiation[i] <= cd4.recovery.time)  && 
                 (cd4.count.today[i] <= cd4.at.infection.female)
                ) {
                   cd4.current <- cd4.count.today[i]
                   cd4.count.today[i] <- cd4.current + (per.day.cd4.recovery*size.of.timestep)
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

 ## 11June13: Rewrite to include short courese tx during Option A
 compute.cd4.count.rewrite.art2 <- function(nw, verbose,
                                       cd4.at.infection.male,
                                       cd4.at.infection.female,
                                       per.day.untreated.cd4.decline,
                                       cd4.recovery.time,
                                       per.day.cd4.recovery,
                                       optA.sc.art.cd4.perstep.rec, #11Jun13
                                       size.of.timestep,
                                       ...
                                       ){

   ##11Jun13: Make changes to CD4 computation  
   
   ## Top Matter
      inf.status <- nw%v%"inf.status"
      art.status <- nw%v%"art.status"
      art.type <- nw%v%"art.type"
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

            cd4.current <- cd4.count.today[i]
            cd4.count.today[i] <- cd4.current - 
                (per.day.untreated.cd4.decline*size.of.timestep)

             cat ("Declining men: ", i, "\n")

          } else if (!is.na(art.status[i]) && art.status[i] == 1
                     && art.type[i] == 1) {
                                        # individuals on "regular" art, 

            if ( (!is.na(time.since.art.initiation[i])) &&
                  ## cd4 recovery in men on art
                (time.since.art.initiation[i] <= cd4.recovery.time)  && 
                 (cd4.count.today[i] <= cd4.at.infection.male)
                ) { ## cd4 recovery only for first 3 years,
                ## or until  cd4 reaches healthy level
                ## cd4.count.today[i] <- cd4.count.today[i] + per.day.cd4.recovery
                              ## recovery of 15 cells/month
              cd4.current <- cd4.count.today[i] 
              cd4.count.today[i] <- cd4.current +
                (per.day.cd4.recovery*size.of.timestep)

                 cat ("Recovering men: ", i, "\n")
            }
          } ## men don't receive scART as per Option A,
            ## so no modification necessary here
        }
      }

      for (i in (min(female.id.curr)):(max(female.id.curr)) ) {

        if (inf.status[i] == 1){

          if (!is.na(art.status[i]) && art.status[i] == 0) {
                                        # untreated individuals

            cd4.current <- cd4.count.today[i]
            cd4.count.today[i] <- cd4.current -
              (per.day.untreated.cd4.decline*size.of.timestep)

            if (verbose){
              cat ("Declining women: ", i, "\n")
            }
              
          } else if (!is.na(art.status[i]) && art.status[i] == 1 &&
                     art.type[i] == 1) {
                                        # individuals on regular art
            if ( (!is.na(time.since.art.initiation[i])) &&
                ## cd4 recovery in wommen on art
                (time.since.art.initiation[i] <= cd4.recovery.time)  && 
                 (cd4.count.today[i] <= cd4.at.infection.female)
                ) {
              
                   cd4.current <- cd4.count.today[i]
                   cd4.count.today[i] <- cd4.current +
                     (per.day.cd4.recovery*size.of.timestep)
                   
                if (verbose) {
                  cat ("Recovering women: ", i, "\n")
                }
              }
          } else if (!is.na(art.status[i]) && art.status[i] == 1 &&
                     art.type[i] == 2) {

            cd4.current <- cd4.count.today[i]
            cd4.count.today[i] <- cd4.current + optA.sc.art.cd4.perstep.rec

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

 ## 11June13: Rewrite to include short courese tx during Option A
 compute.cd4.count.sexage.acct <- function(nw, verbose,
                                       cd4.at.infection.male,
                                       cd4.at.infection.female,
                                       per.day.untreated.cd4.decline,
                                       cd4.recovery.time,
                                       per.day.cd4.recovery,
                                       optA.sc.art.cd4.perstep.rec, #11Jun13
                                       size.of.timestep,
                                       ...
                                       ){

   ##6Jul13: Account for sex and age changes
   
   ## Top Matter
      inf.status <- nw%v%"inf.status"
      art.status <- nw%v%"art.status"
      art.type <- nw%v%"art.type"
      time.since.infection <- nw%v%"time.since.infection" 
      time.since.art.initiation <- nw%v%"time.since.art.initiation" 
      cd4.count.today <- nw%v%"cd4.count.today"
      age <- nw%v%"age"

   ## 7Jul13: Adjust parameter n below (no. of years since infection)
   ## by size of timestep.
   ## n=time.since.infection/365*size.of.timestep
   ## 6Jul13 other needed parameters
      ## B1 (ref) + B2 (african) + B3 (female) + 
      ## (B4 (ref) +B5 (african) +B6 (age)) -- repeated once for each year )^2
      ## mathematical expression therefore is:
      ## B1 (ref) + B2 (african) + B3 (female) + n^2(B4 (ref) +B5 (african) +B6 (age))^2

      b1.ref <- 23.53
      b2.african <- -0.76
      b3.female <- 1.11
      b4.cd4.ref <- -1.49
      b5.african <- 0.34
      b6.age.15to29 <- 0
      b6.age.30to39 <- -0.1
      b6.age.40to49 <- -0.34
      b6.age.50ormore <- -0.63


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
      for (i in 1:length(male.id.curr)) {

        if (inf.status[i] == 1){

          if (is.na(art.status[i]) || art.status[i] == 0) { # untreated individuals

            ## cd4.current <- cd4.count.today[i]
            ## cd4.count.today[i] <- cd4.current - 
            ##     (per.day.untreated.cd4.decline*size.of.timestep)

      ## B1 (ref) + B2 (african) + B3 (female) + n^2(B4 (ref) +B5 (african) +B6 (age))^2

            if (age[i] %/% 10 < 3){ ## need quotient operator
                cd4.count.today[i] <- (b1.ref+b2.african+
                                    (time.since.infection[i]/365*size.of.timestep)*
                                    (b4.cd4.ref+b5.african+b6.age.15to29))^2
                } else if (age[i] %/% 10 == 3){
                          cd4.count.today[i] <- (b1.ref+b2.african+
                                    (time.since.infection[i]/365*size.of.timestep)*
                                    (b4.cd4.ref+b5.african+b6.age.30to39))^2
                } else if (age[i] %/% 10 == 4){
                          cd4.count.today[i] <- (b1.ref+b2.african+
                                    (time.since.infection[i]/365*size.of.timestep)*
                                    (b4.cd4.ref+b5.african+b6.age.40to49))^2
                } else if (age[i] %/% 10 >= 5){
                          cd4.count.today[i] <- (b1.ref+b2.african+
                                    (time.since.infection[i]/365*size.of.timestep)*
                                    (b4.cd4.ref+b5.african+b6.age.50ormore))^2
                } 

               if (verbose){
             cat ("Declining men: ", i, "\n")
           }
             
          } else if (!is.na(art.status[i]) && art.status[i] == 1##  && 
                     ## art.type[i] == 1
                     ) {
                                        # individuals on "regular" art, 

            if ( (!is.na(time.since.art.initiation[i])) &&
                  ## cd4 recovery in men on art
                (time.since.art.initiation[i] <= cd4.recovery.time)  && 
                 (cd4.count.today[i] <= cd4.at.infection.male)
                ) { ## cd4 recovery only for first 3 years,
                ## or until  cd4 reaches healthy level
                ## cd4.count.today[i] <- cd4.count.today[i] + per.day.cd4.recovery
                              ## recovery of 15 cells/month
              cd4.current <- cd4.count.today[i] 
              cd4.count.today[i] <- cd4.current +
                (per.day.cd4.recovery*size.of.timestep)

                 cat ("Recovering men: ", i, "\n")
            }
          } ## men don't receive scART as per Option A,
            ## so no modification necessary here
        }
      }

      for (i in (min(female.id.curr)):(max(female.id.curr)) ) {

        if (inf.status[i] == 1){

          if (is.na(art.status[i]) || art.status[i] == 0) {
                                        # untreated individuals

            ## cd4.current <- cd4.count.today[i]
            ## cd4.count.today[i] <- cd4.current -
            ##   (per.day.untreated.cd4.decline*size.of.timestep)
            if (age[i] %/% 10 < 3){ ## need quotient operator
                cd4.count.today[i] <- (b1.ref+b2.african+b3.female+
                                    (time.since.infection[i]/365*size.of.timestep)*
                                    (b4.cd4.ref+b5.african+b6.age.15to29))^2
                } else if (age[i] %/% 10 == 3){
                          cd4.count.today[i] <- (b1.ref+b2.african+b3.female+
                                    (time.since.infection[i]/365*size.of.timestep)*
                                    (b4.cd4.ref+b5.african+b6.age.30to39))^2
                } else if (age[i] %/% 10 == 4){
                          cd4.count.today[i] <- (b1.ref+b2.african+b3.female+
                                    (time.since.infection[i]/365*size.of.timestep)*
                                    (b4.cd4.ref+b5.african+b6.age.40to49))^2
                } else if (age[i] %/% 10 >= 5){
                          cd4.count.today[i] <- (b1.ref+b2.african+b3.female+
                                    (time.since.infection[i]/365*size.of.timestep)*
                                    (b4.cd4.ref+b5.african+b6.age.50ormore))^2
                } 

            if (verbose){
              cat ("Declining women: ", i, "\n")
            }
              
          } else if (!is.na(art.status[i]) && art.status[i] == 1 &&
                      art.type[i] == 1
                     ) {
                                        # individuals on regular art
            if ( (!is.na(time.since.art.initiation[i])) &&
                ## cd4 recovery in wommen on art
                (time.since.art.initiation[i] <= cd4.recovery.time)  && 
                 (cd4.count.today[i] <= cd4.at.infection.female)
                ) {
              
                   cd4.current <- cd4.count.today[i]
                   cd4.count.today[i] <- cd4.current +
                     (per.day.cd4.recovery*size.of.timestep)
                   
                if (verbose) {
                  cat ("Recovering women: ", i, "\n")
                }
              }
          } else if (!is.na(art.status[i]) && art.status[i] == 1 &&
                     art.type[i] == 2) {

            cd4.current <- cd4.count.today[i]
            cd4.count.today[i] <- cd4.current + optA.sc.art.cd4.perstep.rec

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

