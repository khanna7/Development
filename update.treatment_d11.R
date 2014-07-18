## Update treatment

  ## Change name of "vl.at.art.init" to "vl.at.art.initiation"
  ## and name of "cd4.at.art.init" to "cd4.at.art.initiation"

  ## 19,20 Feb 2014: Record
  ## a. attribute "vl.at.art.init" for viral load at art initiation
  ## b. (20Feb14): attribute "cd4.at.art.init" for CD4 at art initiation

  ## 30 Jan 2014: Trying to resolve bugs in Option B

  ## 14 jan 2014: Readded "vl.art.traj.slope" lines from
  ## 1 Aug 2013, for some reason they were wmissing in this version - "d9_w*R"

  ## 20 Dec 2013: Write code for Option B.
  ## a. Option B: Initiate triple ART for all infected pregnant women (accounting for coverage),
  ## Let's call this ART type 4, it is identical to ART type 1, except it will be terminated
  ## 12 months after cessation of pregnancy. 
  ## b. ART cessation 12 months after delivery
  ## c. Make sure at all relevant places for Type 1 ART and Type 4 ART have same effect. 

  ## 2 Dec 2013: Correct viral load trajectory for individuals women stopping
  ## scART when they reach their pre-ART level

  ## 1 Nov 2013: Found bug in initiation of treatment -- even not ART covered
  ## men and women were getting an  "art.type" of 1 -- fixed that here, i think.

  ## 30 Oct 2013: Change name of "preg.art.covered" to "preg.covered"

  ## 28 Oct 2013: Remove all arguments inside function calls

  ## 14 Oct 2013: PMTCT B+ intervention

  ## 27 Aug 2013: Comment out "dur.inf" from update.treatment
  ## next iteration, since this is not a global
  ## constant anymore -- ALREADY COMMENTED OUT!!!

   ## 22 Aug 2013: Add age-based duration of infection at time of infection
   ## The old "dur.inf" (homogeneous duration of infection) did not appear here,
   ## except as an argument. That argument has been removed.

   ## 16 Aug 2013: At equilibrium, number of treated males appears to be too high.
   ## To debug, add output for number of 

   ## 14 Aug 2013: Found a mistake in the ID's whose ART status was getting
   ## updated to 1. I had
   ## set.vertex.attribute(nw, "art.status", 1,
   ##                      untreated.male.below.realistic.cd4)
   ## set.vertex.attribute(nw, "art.status", 1,
   ##                      untreated.female.below.realistic.cd4)
   ## so all men and women below CD4 initiation levels were accessing ART.
   ## This should be changed to those who are classified as initiatiors -- 
   ## "bl.initiators." I had coded the classification of "bl.initiators" in d4,
   ## but not changed who is getting ART in "set.vertex.attribute" call.

   ## 1 Aug 2013: Need to fix error that appears in "assign.infectivity" function.
   ## Appears to occur because "vl.art.traj.slope" attribute stays at NA, when treatment
   ## is initiated

    ## 30 July 2013: Move assignment of infection stage to "assign.infectivity"
    ## Also move associated arguments "acute.length", "chronic.length" and "late.length"

    ## 12 July 2013: Take ART coverage into account while assigning treatment.

    ## 12Jul2103:  Compputation for viral load trajectory should have
          ## vl.art.today[untreated.bl.initiators]
           ## in numerator -- where vl.art.today is the viral load at ART initiation.
           ## vl.art.traj.slope[untreated.bl.initiators] <- 
           ##   abs((undetectable.vl - viral.load.today[untreated.bl.initiators])/
           ##       time.to.full.supp)
    ## Made this correction -- initially had
    ## vl.art.traj.slope[untreated.bl.initiators] there. 

    ## 11Jul2013: Attribute here named "pregnant" -- should be named
    ## "curr.pregnancy.status." -- this was actually okay.

    ## 11 Jul 2013: Added "undetectable.vl" and "time.to.full.supp" as arguments. 
    ## Currently stage of infection is assigned here. Maybe make it its own function.
    ## Not necessary to pass full network object here.

    ## 11 Jul 2013: Removed the "per.day.untreated.cd4.decline" argument from
    ## "update.treatment" function -- since CD4 counts in infected people are now
    ## updated using a model that accouts for age and sex (gender).

    ## 10Jul2013: Use "compute.cd4.count.sexageacct" instead of
    ##"compute.cd4.count.rewrite."

    ## 7Jul2013: Changed name of "time.infection.to.peak.viremia"
    ## to "time.infection.to.peak.viral.load."

    ## 6Jul13: Add art.type for non-pregnant women ART initiators -- 
    ## code 1 for regular ART

    ## 5Jul2013: Made modifications for including all four treatment states -- 
    ## 0 (untreated); 1 (regular ART), 2 (ongoing scART for pregnant women);
    ## 3 (stopped scART for pregnant women) --
    ## in the viral load computations

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

     update.treatment <-
  function(nw, verbose,
           scenario,
           eligibile.cd4,
           baseline.cd4.at.art.initiation.men,
           baseline.cd4.at.art.initiation.women,
           baseline.f.ges.visit,
           optA.thres,
           cd4.at.infection.male,
           cd4.at.infection.female,
           cd4.recovery.time,
           per.day.cd4.recovery,
           time.infection.to.peak.viral.load,
           size.of.timestep, #28May13
           optA.vl.reduction, ##5Jul13
           full.term,
           undetectable.vl,
           time.to.full.supp,
           ...
           ){
       
       ## FIRST DETERMINE TREATMENT ELIGIBILITY
       ## in baseline: not on treatment already
       ## eligible at CD4 count ~350 cells/mm3
          ## browser()
          male.id.curr <- nwmodes(nw, 1)
          female.id.curr <- nwmodes(nw, 2)

          male.cd4.today <- (nw%v%"cd4.count.today")[male.id.curr]
          female.cd4.today <- (nw%v%"cd4.count.today")[female.id.curr]

          ##curr.pregnant <- which(nw%v%"pregnant" == 1)
          ## curr.pregnancy.status <- nw%v%"curr.pregnancy.status"
          ## pregnant women -- treatment eligible at ~14 weeks
          ## after onset of pregnancy, though treatment is initiated
          ## around ~23rd week.

          ## 12Jun2013: Add attribute for viral load trajectory
          vl.art.traj.slope <- nw%v%"vl.art.traj.slope"
          time.since.art.initiation <- nw%v%"time.since.art.initiation"
          ## 12Jul2013: Add attribute for viral load
          viral.load.today <- nw%v%"viral.load.today"

          ## if (time >= 1142) {browser()}
          
         if (scenario == "baseline" || scenario == "pmtct.b+" || scenario == "option.b"){#14Oct13,
                                                                                         #20Dec13
           ## browser()

           
           
           untreated <- which(nw %v% "art.status" == 0) 
           untreated.male <- intersect(untreated, male.id.curr) # for men
           untreated.female <- intersect(untreated, female.id.curr) # for women

           male.below.cd4.init.realistic <-
             which(nw%v%"cd4.count.today" <=
                   baseline.cd4.at.art.initiation.men)
           ## which people are below the ART initiation level for men

           female.below.cd4.init.realistic <-
             which(nw%v%"cd4.count.today" <=
                   baseline.cd4.at.art.initiation.women)
           ## which people are below the ART initiation level for women
           
           ##12Jul13: Extract coverage information
           ## browser()
           art.covered <- which(nw%v%"art.covered" ==1)
           art.covered.male <- intersect(art.covered, male.id.curr)
           art.covered.female <- intersect(art.covered, female.id.curr)

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

           ##############################################################
           ## 12Jul13: Very important addition -- 
           ## Change below to include coverage
           ## untreated.bl.initiators <- c(untreated.male.below.realistic.cd4,
           ##                              untreated.female.below.realistic.cd4
           ##                              )

           untreated.bl.initiator.male <- intersect(untreated.male.below.realistic.cd4,
                                                    art.covered.male)
           
           untreated.bl.initiator.female <- intersect(untreated.female.below.realistic.cd4,
                                                      art.covered.female)

           untreated.bl.initiators <- c(untreated.bl.initiator.male,
                                        untreated.bl.initiator.female)

           ##############################################################

           if (verbose){
           cat("Number of untreated males below realistic CD4
                initiation value are: ",
                length(untreated.male.below.realistic.cd4), "\n")
           
           
           cat("Number of untreated females below realistic CD4
                initiation value are: ",
               length(untreated.female.below.realistic.cd4), "\n")

           cat("Number of untreated males who are covered",
               length(untreated.bl.initiator.male)) #16Aug13

           cat("Number of untreated females who are covered",
               length(untreated.bl.initiator.male)) #16Aug13
         }

           
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


           ###################################################
           ## 1 Aug 2013: To fix error of NA appearing in "vl.art.traj.slope" for
           ## initiators
           #################################################


           ## 30 Jan 2014: Comment out lines below and modify
           ## vl.art.traj.slope[untreated.bl.initiators] <- 
           ##   abs((undetectable.vl - viral.load.today[untreated.bl.initiators])/
           ##       time.to.full.supp)
           ## vl.art.traj.slope <- 
           ##   abs((undetectable.vl - viral.load.today)/time.to.full.supp)
           ##14Jan14: these lines here are critical. for some reason, were missing from version d9.

           vl.art.traj.slope[untreated.bl.initiators] <- #30Jan 2014
              abs((undetectable.vl - viral.load.today[untreated.bl.initiators])/
                  time.to.full.supp)
            


           if (verbose) {
     
             cat("Slope of viral load trajectory in 
                  men and women receiving regular ART for first time: ",
                 vl.art.traj.slope[untreated.bl.initiators], "\n")
                 ## all these men will get treatement
           }

           ###########################################################

           ## 13Aug13: Fixing bug in ART status assignment 

           ## set.vertex.attribute(nw, "art.status", 1,
           ##                      untreated.male.below.realistic.cd4)
           ## set.vertex.attribute(nw, "art.status", 1,
           ##                      untreated.female.below.realistic.cd4)
           set.vertex.attribute(nw, "art.status", 1,
                                untreated.bl.initiators)
           set.vertex.attribute(nw, "vl.at.art.initiation",
                                viral.load.today[untreated.bl.initiators],
                                untreated.bl.initiators) #19Feb14: Record viral load
                                                         # at ART initiation
           set.vertex.attribute(nw, "cd4.at.art.initiation", #20Feb14
                                cd4.count.today[untreated.bl.initiators],
                                untreated.bl.initiators)
           ###########################################################
           
           set.vertex.attribute(nw, "art.type", 1,
                                untreated.bl.initiator.male) # corrected 1Nov13
           set.vertex.attribute(nw, "art.type", 1,
                                untreated.bl.initiator.female) 
           set.vertex.attribute(nw, "time.of.art.initiation", time,
                                untreated.bl.initiator.male)
           set.vertex.attribute(nw, "time.of.art.initiation", time,
                                untreated.bl.initiator.female)
           set.vertex.attribute(nw, "time.since.art.initiation", 0,
                                untreated.bl.initiator.male)
           set.vertex.attribute(nw, "time.since.art.initiation", 0,
                                untreated.bl.initiator.female)

         

           ###############################################################
           ## 30 Jan 2014: Comment out and add lines below
           ## set.vertex.attribute(nw, "vl.art.traj.slope",
           ##                      vl.art.traj.slope
           ##                      )

           set.vertex.attribute(nw, "vl.art.traj.slope",
                                vl.art.traj.slope[untreated.bl.initiators],
                                v=untreated.bl.initiators
                                )

         } ## 14Oct13: closing parenthesis on scenario "baseline" OR "b+pmtct"
           ###############################################################


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
            ##12Jul13: Extract coverage information for PMTCT
            #preg.art.covered <- nw%v%"preg.art.covered"
            preg.covered <- nw%v%"preg.covered" #30Oct13: change name to "preg.covered" 
           
            curr.pregnant <- which(curr.pregnancy.status == 1)
            not.on.art <- which(art.status == 0)
            ##################################################
            ##12Jul13: Add lines below to account for coverage
            # covered <- which(preg.art.covered == 1) #12Jul13: to acct. for coverage
              covered <- which(preg.covered == 1) #30Oct13: change name to "preg.covered" 
            ##################################################              
            preg.init.eligible <- which(time.since.curr.pregnancy >
                                     baseline.f.ges.visit) # new argument
            ##################################################
            ##12Jul13: Add lines below to account for coverage
            preg.eligible.and.covered <- intersect(preg.init.eligible,
                                                   covered)
            ##################################################
            ## browser()
          
            if (scenario == "baseline"){ ##14Oct13: Baseline intervention for
                                          ## pregnant women
            ## browser() 
            ind.below.cd4.optA.thres <- which(cd4.count.today < optA.thres)
                                      # new argument
            ind.above.cd4.optA.thres <- which(cd4.count.today >= optA.thres)


            preg.not.on.art <- intersect(curr.pregnant, not.on.art)
            ##################################################
            ##12Jul13: Add lines below to account for coverage
            ## preg.initiators <- intersect(preg.init.eligible,
            ##                              preg.not.on.art)
            preg.initiators <- intersect(preg.eligible.and.covered,
                                         preg.not.on.art)
            ##################################################
            preg.init.reg.art <- intersect(preg.initiators,
                                           ind.below.cd4.optA.thres)
            preg.init.sc.art <- intersect(preg.initiators,
                                          ind.above.cd4.optA.thres)

            ############################################################################
            ## 30 Jan 2014: Comment out and add lines below

            ## 12Jun13: Add viral load trajectory attribute
            ## vl.art.traj.slope[preg.init.reg.art] <-
            ##   (undetectable.vl - viral.load.today[preg.init.reg.art])/
            ##    time.to.full.supp ## for pregnant women on regular ART
            vl.art.traj.slope[preg.init.reg.art] <-
              abs(undetectable.vl - viral.load.today[preg.init.reg.art])/
                time.to.full.supp ## for pregnant women on regular ART

            
            vl.art.traj.slope[preg.init.sc.art] <-
               optA.vl.reduction/ # remember all entries in vl.art.traj.slope
                (full.term -      # are positive, addn/subtn done approp. in vlcomputation
                 time.since.curr.pregnancy[preg.init.sc.art])
            ############################################################################
           
            set.vertex.attribute(nw, "art.status", 1,
                                 preg.initiators)
            set.vertex.attribute(nw, "time.of.art.initiation", time,
                                 preg.initiators)
            set.vertex.attribute(nw, "time.since.art.initiation", 0,
                                 preg.initiators)
            set.vertex.attribute(nw, "vl.at.art.initiation",
                                 viral.load.today[preg.initiators],
                                 preg.initiators) #19Feb14
            set.vertex.attribute(nw, "cd4.at.art.initiation", #20Feb14
                                 cd4.count.today[preg.initiators],
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

         } ## 14 Oct13: closing parentheses on scenario baseline
           ## for PMTCT

            
            if (scenario == "pmtct.b+"){ ##14Oct13: B+ intervention for
                                          ## pregnant women

            preg.not.on.art <- intersect(curr.pregnant, not.on.art)
            preg.initiators <- intersect(preg.eligible.and.covered,
                                         preg.not.on.art)

            preg.init.reg.art <- preg.initiators

            ## 30 Jan 2014: Comment out and add lines below
            ## 12Jun13: Add viral load trajectory attribute
            ## vl.art.traj.slope[preg.init.reg.art] <-
            ##   (undetectable.vl - viral.load.today[preg.init.reg.art])/
            ##    time.to.full.supp ## for pregnant women on regular ART
            vl.art.traj.slope[preg.init.reg.art] <-
              abs(undetectable.vl - viral.load.today[preg.init.reg.art])/
                time.to.full.supp ## for pregnant women on regular ART

            
          
            set.vertex.attribute(nw, "art.status", 1,
                                 preg.initiators)
            set.vertex.attribute(nw, "time.of.art.initiation", time,
                                 preg.initiators)
            set.vertex.attribute(nw, "time.since.art.initiation", 0,
                                 preg.initiators)
            set.vertex.attribute(nw, "vl.at.art.initiation",
                                 viral.load.today[preg.initiators],
                                 preg.initiators) #19Feb14
            set.vertex.attribute(nw, "cd4.at.art.initiation", #20Feb14
                                 cd4.count.today[preg.initiators],
                                 preg.initiators)

            set.vertex.attribute(nw, "art.type", 1, 
                                preg.initiators)
            set.vertex.attribute(nw, "vl.art.traj.slope",
                                 vl.art.traj.slope[preg.initiators], 
                                 preg.initiators)
            
           if (verbose){

              cat("Pregnant women initiating ART are",
                  preg.initiators, "\n")

              cat("Pregnant women initiating regular ART are",
                  preg.initiators, "\n")

              cat("Slope for change in viral load of pregnant women
                   receiving regular ART are",
                  vl.art.traj.slope[preg.initiators], "\n") #12Jun13
           
            }

         }

          if (scenario == "option.b"){ ##20Dec13: PMTCT Option B for
                                       ##pregnant women

            preg.not.on.art <- intersect(curr.pregnant, not.on.art)
            preg.initiators <- intersect(preg.eligible.and.covered,
                                         preg.not.on.art)

            preg.init.reg.art <- preg.initiators

            ## 30 Jan 2014: Comment out lines below and modify
            ## 12Jun13: Add viral load trajectory attribute
            ## vl.art.traj.slope[preg.init.reg.art] <-
            ##   (undetectable.vl - viral.load.today[preg.init.reg.art])/
            ##    time.to.full.supp ## for pregnant women on regular ART
            vl.art.traj.slope[preg.init.reg.art] <-
              abs(undetectable.vl - viral.load.today[preg.init.reg.art])/
                time.to.full.supp ## for pregnant women on regular ART

          
            set.vertex.attribute(nw, "art.status", 1,
                                 preg.initiators)
            set.vertex.attribute(nw, "time.of.art.initiation", time,
                                 preg.initiators)
            set.vertex.attribute(nw, "time.since.art.initiation", 0,
                                 preg.initiators)
            set.vertex.attribute(nw, "vl.at.art.initiation",
                                 viral.load.today[preg.initiators],
                                 preg.initiators) #19Feb14
            set.vertex.attribute(nw, "cd4.at.art.initiation",
                                 cd4.count.today[preg.initiators],
                                 preg.initiators) #20Feb14

            ## browser()
            ## set.vertex.attribute(nw, "art.type", 1, 
            ##                     preg.initiators)
            set.vertex.attribute(nw, "art.type", 4,
                                 preg.initiators) #20Dec13: Reg ART on a/c of PMTCT Opt B
            set.vertex.attribute(nw, "vl.art.traj.slope",
                                 vl.art.traj.slope[preg.initiators], 
                                 preg.initiators)
            
           if (verbose){

              cat("Pregnant women initiating ART are",
                  preg.initiators, "\n")

              cat("Pregnant women initiating regular ART are",
                  preg.initiators, "\n")

              cat("Slope for change in viral load of pregnant women
                   receiving regular ART are",
                  vl.art.traj.slope[preg.initiators], "\n") #12Jun13
           
            }

         } ## 14 Oct13: closing parentheses on scenario B+
           ## for PMTCT


          
         ## UPDATE STAGE OF INFECTION
         ## based on time since infection

            ## 20Dec13: lines below are not needed. 
            ## infected <- which(nw %v% "inf.status" == 1)
            ## untreated <- which(nw %v% "art.status" == 0)
            ## time.since.infection <- nw %v% "time.since.infection"

  return(nw)          

        }
