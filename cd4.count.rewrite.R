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

            cd4.count.current <- cd4.count.today[i]
            cd4.count.today[i] <- cd4.count.current - 
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

            cd4.count.today.current <- cd4.count.today[i]
            cd4.count.today[i] <- (per.day.untreated.cd4.decline*size.of.timestep)

            if (verbose){
              cat ("Declining women: ", i, "\n")
            }
              
          } else if (!is.na(art.status[i]) && art.status[i] == 1) { # individuals on art
            if ( (!is.na(time.since.art.initiation[i])) &&
                  ## cd4 recovery in wommen on art
                (time.since.art.initiation[i] <= cd4.recovery.time)  && 
                 (cd4.count.today[i] <= cd4.at.infection.female)
                ) {
                   cd4.count.current <- cd4.count.today[i]
                   cd4.count.today[i] <- cd4.count.current + (per.day.cd4.recovery*size.of.timestep)
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
