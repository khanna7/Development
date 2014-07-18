    ## Model biological parameters

    ## 12 June 2013: Add attribute for slope of viral load trajectory
    ## for art types 1, 2 and 3,
    ## and populate this attribute at the initiation of ART.

    ## 11 June 2013: Make modifications in baseline scenario, to
    ## update ART status for women receivied sc ART post-delivery --
    ## done in assign.pregnancy_d2.R

    ## Can write separate function(s) for
    ## increase in viral load and decrease in CD4 count
    ## after cessation of sc ART (Option A).


    ## 11 June 2013: Make changes to the viral load computations, to include
    ## short course ART treatment under option A.

    ## 6 June 2013: Add treatment for pregnant women.
    ## In the baseline model, pregnant women initiate treatment at: 
    ## a. first gestational visit after initiation of pregnancy
    ## a.1 if their CD4 count < 350, they keep treatment for life.
    ## a.2 if their CD4 count > 350, they take scART for duration of pregnancy.

    ## Also add new attribute, "art.type" -- 1 for ART,
    ## 2 for short-course ART (scART) for pregnant.women


    ## 5 June 2013: Add treatment for pregnant women. File
    ## "update.biological.parameters_d4.R" contains an attempt at a
    ## testing-driven-development (TDD) program that did not work. 

    ## 29 May 2013: implement new version of computing viral load.

    ## 29 May 2013: Call new version of "compute.cd4.count"

    ## 28 May 2013: Adjust for size of timestep.

    ## 7 April 2013: Trying to fix updating of ART status for
    ## women going on treatment

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

     update.biological.parameters <-
  function(nw, verbose=TRUE,
           scenario = "baseline",
           dur.inf=dur.inf,
           eligibile.cd4=eligible.cd4,
           baseline.cd4.at.art.initiation.men,
           baseline.cd4.at.art.initiation.women,
           ## baseline.time.art.initiation.preg.women,
           baseline.f.ges.visit,
           optA.thres,
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
           optA.vl.reduction,
           full.term,
           ...
           ){
       
       ## FIRST DETERMINE TREATMENT ELIGIBILITY
       ## in baseline: not on treatment already
       ## eligible at CD4 count ~350 cells/mm3
          male.id.curr <- nwmodes(nw, 1)
          female.id.curr <- nwmodes(nw, 2)

          male.cd4.today <- (nw%v%"cd4.count.today")[male.id.curr]
          female.cd4.today <- (nw%v%"cd4.count.today")[female.id.curr]

          curr.pregnant <- which(nw%v%"pregnant" == 1)
          ## pregnant women -- treatment eligible at ~14 weeks
          ## after onset of pregnancy, though treatment is initiated
          ## around ~23rd week.

          ## 12Jun2013: Add attribute for viral load trajectory
          vl.art.traj.slope <- nw%v%"vl.art.traj.slope"
          time.since.art.initiation <- nw%v%"time.since.art.initiation"

          
         if (scenario == "baseline"){

           untreated <- which(nw %v% "art.status" == 0) 

           untreated.male <- intersect(untreated, male.id.curr) # for men
           untreated.female <- intersect(untreated, female.id.curr) # for women

           male.below.cd4.init.realistic <-
             which(nw%v%"cd4.count.today" <=
                   baseline.cd4.at.art.initiation.men)

           female.below.cd4.init.realistic <-
             which(nw%v%"cd4.count.today" <=
                   baseline.cd4.at.art.initiation.women)

           untreated.below.cd4.init.realistic <-
             intersect(untreated,
                       female.below.cd4.init.realistic)
           
           cd4.below.eligible <- which(nw%v%"cd4.count.today" <=
                                                    eligible.cd4)
           
           ## baseline.cd4.art.initiation is the real CD4 count at which art
           ## begins
           untreated.male.below.realistic.cd4 <-
               intersect(untreated.male,
                         male.below.cd4.init.realistic)

           untreated.female.below.realistic.cd4 <-
             intersect(untreated.female,
                       female.below.cd4.init.realistic)

           untreated.bl.initiators <- c(untreated.male.below.realistic.cd4,
                                        untreated.female.below.realistic.cd4
                                        )

           cat("Number of untreated males below realistic CD4
                initiation value are: ",
                length(untreated.male.below.realistic.cd4), "\n")
           
           
           cat("Number of untreated females below realistic CD4
                initiation value are: ",
               length(untreated.female.below.realistic.cd4), "\n")

           
             untreated.art.eligible.male <- intersect(untreated.male,
                                                      cd4.below.eligible)
             untreated.art.eligible.female <- intersect(untreated.female,
                                                      cd4.below.eligible)

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


           ## Compute viral load change slopes
           vl.art.traj.slope[untreated.bl.initiators] <- 
             (undetectable.vl - vl.art.traj.slope[untreated.bl.initiators])/
               time.to.full.supp
             

           if (verbose) {
     
             cat("Slope of viral load trajectory in 
                  men and women receiving regular ART: ",
                 vl.art.traj.slope[untreated.bl.initiators], "\n")
                 ## all these men will get treatement
           }

           
           set.vertex.attribute(nw, "art.status", 1,
                                untreated.male.below.realistic.cd4)
           set.vertex.attribute(nw, "art.status", 1,
                                untreated.female.below.realistic.cd4)
           set.vertex.attribute(nw, "time.of.art.initiation", time,
                                untreated.male.below.realistic.cd4)
           set.vertex.attribute(nw, "time.of.art.initiation", time,
                                untreated.female.below.realistic.cd4)
           set.vertex.attribute(nw, "time.since.art.initiation", 0,
                                untreated.male.below.realistic.cd4)
           set.vertex.attribute(nw, "time.since.art.initiation", 0,
                                untreated.female.below.realistic.cd4)
           ## 12Jun2013
           set.vertex.attribute(nw, "vl.art.traj.slope",
                                vl.art.traj.slope[untreated.bl.initiators],
                                untreated.bl.initiators)

           ## 6 June 2013
           ## for pregnant women
              ## list all pregnant women
              ## check their treatment status
              ## if not on treatment {
              ##       if (time.since.curr.pregnancy > time to first visit){
              ##           art is initiated
              ##           }
              ##        if (cd4 count < 350) {full ART}
              ##        if (cd4 count > 350) {sc ART} 
              ##        if (sc ART) {treatment stops after termination of preg}
              ##  } else if ## 11 Jun13
              ## if not on treatment type 2 {
           

            ##browser()
           
            curr.pregnancy.status <- nw%v%"curr.pregnancy.status"
            time.since.curr.pregnancy <- nw%v%"time.since.curr.pregnancy"
            art.status <- nw%v%"art.status"
            cd4.count.today <- nw%v%"cd4.count.today"
           
            curr.pregnant <- which(curr.pregnancy.status == 1)
            not.on.art <- which(art.status == 0)
            preg.init.eligible <- which(time.since.curr.pregnancy >
                                     baseline.f.ges.visit) # new argument
            ind.below.cd4.optA.thres <- which(cd4.count.today < optA.thres)
                                      # new argument
            ind.above.cd4.optA.thres <- which(cd4.count.today >= optA.thres)


            preg.not.on.art <- intersect(curr.pregnant, not.on.art)
            preg.initiators <- intersect(preg.init.eligible,
                                         preg.not.on.art)
            preg.init.reg.art <- intersect(preg.initiators,
                                           ind.below.cd4.optA.thres)
            preg.init.sc.art <- intersect(preg.initiators,
                                          ind.above.cd4.optA.thres)

            ## 12Jun13: Add viral load trajectory attribute
            vl.art.traj.slope[preg.init.reg.art] <-
              (undetectable.vl - vl.art.traj.slope[preg.init.reg.art])/
               time.to.full.supp ## for pregnant women on regular ART

            vl.art.traj.slope[preg.init.sc.art] <-
              - optA.vl.reduction/
                (full.term -
                 time.since.curr.pregnancy[preg.init.sc.art])
           
            set.vertex.attribute(nw, "art.status", 1,
                                 preg.initiators)
            set.vertex.attribute(nw, "time.of.art.initiation", time,
                                 preg.initiators)
            set.vertex.attribute(nw, "time.since.art.initiation", 0,
                                 preg.initiators)

            set.vertex.attribute(nw, "art.type", 1, 
                                preg.init.reg.art)
            set.vertex.attribute(nw, "art.type", 2, 
                                preg.init.sc.art)
            set.vertex.attribute(nw, "vl.art.traj.slope",
                                 vl.art.traj.slope[preg.init.reg.art], 
                                 preg.init.reg.art)
            set.vertex.attribute(nw, "vl.art.traj.slope",
                                 vl.art.traj.slope[preg.init.sc.art], 
                                 preg.init.sc.art)


           if (verbose){

              cat("Pregnant women initiating ART are",
                  preg.initiators, "\n")

              cat("Pregnant women initiating regular ART are",
                  preg.init.reg.art, "\n")

              cat("Pregnant women initiating short-course ART are",
                  preg.init.sc.art, "\n")

              cat("Slope for change in viral load of pregnant women
                   receiving regular ART are",
                  vl.art.traj.slope[preg.init.reg.art], "\n") #12Jun13

              cat("Slope for change in viral load of pregnant women
                   receiving sc ART are",
                  vl.art.traj.slope[preg.init.sc.art], "\n"
                  ) #12Jun13
           
            }

         }
         ## UPDATE STAGE OF INFECTION
         ## based on time since infection

            infected <- which(nw %v% "inf.status" == 1)
            untreated <- which(nw %v% "art.status" == 0)
            time.since.infection <- nw %v% "time.since.infection"
            ## stage.of.infection <- rep(NA, length(time.since.infection))

            ## stage.of.infection <- unlist(lapply(time.since.infection,
            ##                                     classify.stage))
            ## this line gives a memory allocation error

            ## stage.of.infection <- unlist(lapply(time.since.infection,
            ##                                     classify.stage.numeric))
            ## this line gives a memory allocation error, too

            for (i in 1:length(time.since.infection)){
              if (time.since.infection[i] %in% acute.length){
                stage.of.infection[i] <- 0
              } else if (time.since.infection[i] %in% chronic.length){
                stage.of.infection[i] <- 1
              } else if (time.since.infection[i] %in% late.length){
                stage.of.infection[i] <- 2
              }
            } ## in theory the above lapply within an unist should work,
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
         
             nw <- compute.cd4.count.rewrite(nw, verbose=FALSE,
                                             cd4.at.infection.male=
                                             cd4.at.infection.male,
                                             cd4.at.infection.female=
                                             cd4.at.infection.female,
                                             per.day.untreated.cd4.decline=
                                             per.day.untreated.cd4.decline,
                                             cd4.recovery.time=cd4.recovery.time,
                                             per.day.cd4.recovery=per.day.cd4.recovery,
                                             size.of.timestep=size.of.timestep, #28May13
                                             ...
                                             )


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

            nw <- compute.vl.mp.art2(nw, verbose, 
                                     time.infection.to.peak.viremia,
                                     peak.viral.load,
                                     time.infection.to.viral.set.point,
                                     set.point.viral.load,
                                     time.infection.to.late.stage,
                                     late.stage.viral.load,
                                     dur.inf,
                                     time.to.full.supp,
                                     undetectable.vl,
                                     size.of.timestep, # 28May13
                                     optA.sc.art.cd4.perstep.rec,
                                        #11Jun13: CD4 recovery w scART
                                     optA.sc.art.vl.perstep.dec,
                                        #11Jun13: vl decline w scART
                                     ...
                                     )



          return(nw)

        }

