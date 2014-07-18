    ## Model biological parameters

    ## 4 June 2013: Try test-based development, removing the networks from
    ## being inserted as
    ## an argument inside of main functions

    ## 29 May 2013: implement new version of computing viral load.

    ## 29 May 2013: Call new version of "compute.cd4.count"

    ## 28 May 2013: Adjust for size of timestep.

    ## 7 April 2013: Trying to fix updating of ART status for women going on treatment

    ## 4 April 2013: List the various attributes that are needed here.
    ## Baseline Scenario: cd4.count.today,
    ##                    time.of.art.initiation, time.since.art.initiation,
    ##                    time.of.pregnancy, time.since.pregnancy


    ## Compute CD4 count: inf.status, art.status, time.since.infection,
    ##                    time.of.art.initiation, time.since.art.initiation,
    ##                    cd4.count.today,
    ##                    

    ## Compute Viral Load: inf.status, art.status, time.since.infection,
    ##                     time.since.art.initiation, viral.load.today
    ##                    

    ## 1 April 2013: Model biological parameters

    ## 28 March 2013: Currently all new entires are HIV negative. 
    ## Should change as per proportion of HIV positives in the population.

    ## 27 March 2013: Make death rate age specific.
    ## "d1" has working version with a general death rate (and constant pregnancy).

    ## 27 March 2013: Initial version completed.
    ## Make death rate age-specific
    ## Make birth rate comparable to population of 15 year olds,

     update.biological.parameters.tdd <-
  function(male.id.today, ## first of parameters is dataset from network
           female.id.today, 
           inf.status,
           art.status,
           time.since.infection,
           time.since.art.initiation,
           cd4.count.today,
           viral.load.today,
           male.cd4.today,
           female.cd4.today,
           pregnant.today,
           verbose=TRUE, # verbose
           scenario = "baseline",
           dur.inf=dur.inf, # non-network parameters needed for simulation
           eligibile.cd4=eligible.cd4,
           baseline.cd4.at.art.initiation.men,
           baseline.cd4.at.art.initiation.women,
           baseline.time.art.initiation.preg.women,
           cd4.at.infection.male,
           cd4.at.infection.female,
           per.day.untreated.cd4.decline,
           cd4.recovery.time,
           per.day.cd4.recovery,
           acute.length, # classify infection state
           chronic.length, # classify infection state
           late.length, # classify infection state
           time.infection.to.peak.viremia,
           size.of.timestep,
           ...
           ){
       
         if (scenario == "baseline"){

           untreated <- which(art.status == 0) 
           untreated.male <- intersect(untreated, male.id.today) # for men
           untreated.female <- intersect(untreated, female.id.today) # for women
           cd4.below.eligible <- which(cd4.count.today <= eligible.cd4)
                                                    

           cd4.below.init.realistic.formen <- 
             which(cd4.count.today <=
                   baseline.cd4.at.art.initiation.men)
           cd4.below.init.realistic.forwomen <- 
             which(cd4.count.today <=
                   baseline.cd4.at.art.initiation.women)
           
           male.below.cd4.init.realistic <-
             intersect(cd4.below.init.realistic.formen,
                       male.id.today)
           
           female.below.cd4.init.realistic <-
             intersect(cd4.below.init.realistic.forwomen,
                       female.id.today)
           
           untreated.below.cd4.init.realistic <-
             intersect(untreated,
                       female.below.cd4.init.realistic)
           
            untreated.male.below.realistic.cd4 <-
               intersect(untreated.male,
                         male.below.cd4.init.realistic)
           cat("Number of untreated males below realistic CD4 initiation ",
               "value are: ",
               length(untreated.male.below.realistic.cd4), "\n")
           
           untreated.female.below.realistic.cd4 <-
               intersect(untreated.female,
                         female.below.cd4.init.realistic)
           
           cat("Number of untreated females below realistic CD4 initiation ",
               "value are: ",
               length(untreated.female.below.realistic.cd4), "\n")

           
             untreated.art.eligible.male <- intersect(untreated.male,
                                                      cd4.below.eligible)
             untreated.art.eligible.female <- intersect(untreated.female,
                                                      cd4.below.eligible)
             
         }
           
           if (verbose) {
     
             cat("Number of untreated men below realistic starting CD4 count =
                 Number of untreated men going on treatment = ",
                 length(untreated.male.below.realistic.cd4), "\n")
             ## all these men will get treatement

             cat("Number of untreated women below realistic starting CD4 count =
                  Number of untreated women going on treatment = ",
                 length(untreated.female.below.realistic.cd4), "\n")
             ## all these women will get treatement
             
             cat("Number of treatment-eligible men = ", 
                 length(untreated.art.eligible.male), "\n")

             cat("Number of treatment-eligible women = ", 
                 length(untreated.art.eligible.female), "\n")

         }


           art.status[untreated.male.below.realistic.cd4] <- 1
           art.status[untreated.female.below.realistic.cd4] <- 1
           time.of.art.initiation[untreated.male.below.realistic.cd4] <- time
           time.of.art.initiation[untreated.female.below.realistic.cd4] <- time
           time.since.art.initiation[untreated.male.below.realistic.cd4] <- 0
           time.since.art.initiation[untreated.female.below.realistic.cd4] <- 0
           
           ## for pregnant women (revisit later)
         ##   pregnant.women.threshold <- which(nw%v%"time.since.pregnancy" >=
         ##                                     baseline.time.art.initiation.preg.women)

         ##   untreated.pregnant.women.eligible <- intersect(pregnant.women.threshold,
         ##                                                  untreated)

         ##   set.vertex.attribute(nw, "art.status", 1,
         ##                        untreated.pregnant.women.eligible)
         ##   set.vertex.attribute(nw, "time.of.art.initiation", time,
         ##                        untreated.pregnant.women.eligible)
         ##   set.vertex.attribute(nw, "time.since.art.initiation", 0,
         ##                        untreated.pregnant.women.eligible)
         ## }

         ## UPDATE STAGE OF INFECTION
         ## based on time since infection
           stage.of.infection <- unlist(lapply(time.since.infection,
                                               classify.stage.numeric))
            ## for (i in 1:length(time.since.infection)){
            ##   if (time.since.infection[i] %in% acute.length){
            ##     stage.of.infection[i] <- 0
            ##   } else if (time.since.infection[i] %in% chronic.length){
            ##     stage.of.infection[i] <- 1
            ##   } else if (time.since.infection[i] %in% late.length){
            ##     stage.of.infection[i] <- 2
            ##   }
            ## }

           ## in theory the above lapply within an unist should work,
              ## but I got an error that says:
              ## ``Error: cannot allocate vector of size 75.5 Mb''
            ## browser()
            nw %v% "stage.of.infection" <- stage.of.infection      

         ## UPDATE CD4 COUNTS 
         ## based on time since infection
         ## first in untreated
         ## ( make sure "art.status" attribute has values 
         ##   0 for HIV positive not on treatment
         ##   1 for those on treatment
         ##   NA for not infected )
         
             ## nw <-
             ##   compute.cd4.count.rewrite(nw, verbose=FALSE,
             ##                             cd4.at.infection.male=cd4.at.infection.male,
             ##                             cd4.at.infection.female=
             ##                             cd4.at.infection.female,
             ##                             per.day.untreated.cd4.decline=
             ##                             per.day.untreated.cd4.decline,
             ##                             cd4.recovery.time=cd4.recovery.time,
             ##                             per.day.cd4.recovery=per.day.cd4.recovery,
             ##                             size.of.timestep=size.of.timestep, #28May13
             ##                                 ...
             ##                                 )


         ## update viral load
         ## based on time since infection
         ## first in untreated

             ## nw <- compute.viral.load(nw, verbose=FALSE,
             ##                          time.infection.to.peak.viremia=
             ##                          time.infection.to.peak.viremia,
             ##                          peak.viral.load=peak.viral.load,
             ##                          time.infection.to.viral.set.point=
             ##                          time.infection.to.viral.set.point,
             ##                          set.point.viral.load=
             ##                          set.point.viral.load,
             ##                          time.infection.to.late.stage=
             ##                          time.infection.to.late.stage,
             ##                          late.stage.viral.load=
             ##                          late.stage.viral.load,
             ##                          dur.inf=dur.inf,
             ##                          time.to.full.supp=time.to.full.supp,
             ##                          undetectable.vl=undetectable.vl,
             ##                          size.of.timestep=size.of.timestep, #28May13
             ##                          ...
             ##                          )

          ## nw <- compute.viral.load.midpt(nw, verbose=FALSE,
          ##                             time.infection.to.peak.viremia=
          ##                             time.infection.to.peak.viremia,
          ##                             peak.viral.load=peak.viral.load,
          ##                             time.infection.to.viral.set.point=
          ##                             time.infection.to.viral.set.point,
          ##                             set.point.viral.load=
          ##                             set.point.viral.load,
          ##                             time.infection.to.late.stage=
          ##                             time.infection.to.late.stage,
          ##                             late.stage.viral.load=
          ##                             late.stage.viral.load,
          ##                             dur.inf=dur.inf,
          ##                             time.to.full.supp=time.to.full.supp,
          ##                             undetectable.vl=undetectable.vl,
          ##                             size.of.timestep=size.of.timestep, #28May13
          ##                             ...
          ##                             )



        out <- cbind(art.status,
                     time.of.art.initiation,
                     time.since.art.initiation,
                     stage.of.infection)



          return(out)

        }

