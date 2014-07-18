    ## Model vital dynamics 

    ## 14 Jan 2014: Add "size.of.timestep" argument to
    ## "assign.mortality.male" and "assign.mortality.female" functions

    ## 3 Dec 2013:
    ## a. Differentiate recruit prevalence for men and women
    ## b. Set "num.of.pregnancies" to 0 for new arrivals.

    ## 2 Dec 2013: Record time of birth and death 

    ## 11 Nov 2013: Change entry age to 18.

    ## 7 Nov 2013:
    ## a. Getting error: Error in if (viral.load.today[i] < 2) { :
    ##  missing value where TRUE/FALSE needed -- has to be fixed via
    ## "vl.art.traj.slope" -- see note dated 1 Aug 2013.
    ## To fix, we need arguments "undetectable.vl" and "time.to.full.supp"
    ## We assume all new entrants are infected, untreated, and have viral
    ## load 0 on day of entry. We need to assign them a numerical 
    ## "vl.art.traj.slope" based on these assumptions -- which is what
    ## we do here. 

    ## 6 Nov 2013: Have 5% of rectuits be infected at time of entry. Time of
    ## infection = time of entry, and ART status is 0.
    ## For this, add new argument, rectuit.inf

    ## 28 Oct 2013: Remove values for all arguments inside the function

    ## 30 Sep 2013:
    ## a. Add assignment of age categories to everyone in the population
    ## b. Add age category 0 to all new entrants


    ## 29 Aug 2013: Change "female" to 1 and "male" to 0 to reduce size of "nw object"

    ## 26 Aug 2013: I never built in exits at age 55!!! Do that NOW!!! --
    ## This was NOT
    ## the problem. The problem was that with infectivies receving a mortality based on
    ## combination of age and sex, exits were not happening at age 55. That is now fixed.

    ## 23 Aug 2013: Record at every timestep
    ## a. number of births 
    ## b. number of male births
    ## c. number of female births
    ## d. total number of deaths #not needed
    ## e. total number of male deaths # not needed
    ## f. number of male deaths due to AIDS 
    ## g. number of female deaths due to AIDS
    ## h. number of male deaths due to natural mortality
    ## i. number of female deaths due to natural mortality

    ## 22 Aug 2013: Add age-based duration of infection at time of infection

    ## 13 Aug 2013: Add ART-coverage indicator as NA here. Assign value of 0 or 1
    ## at point of infection.

    ## 8 Aug 2013: Limit lifespan of infection for only those who are not
    ## on ART. (i.e. ART status is 0, and this time-of-infection-specific mortality
    ## will only apply to individuals who don't have either regular or short-course
    ## ART. 

    ## 25 Jul 2013: Update "time.since.art.cessation"

    ## 12 Jul 2013: Add scenario, baseline.art.coverate.rate and
    ## baseline.preg.coverage.rate arguments to function, and
    ## assign respective attributes for these parameters.

    ## 9 Jul 2013: Add prop.f as an argument to "update.vital.dynamics to
    ## make sure sex for new arrivals is assigned in proportion to sex distribution
    ## at the beginning

    ## 8Jul2013: Add "art.status" argument to "assign.mortality.male" and
    ## "assign.mortality.female."

    ## 6 July 2013: rewrite mortality functions to include:
       ## a. background mortality (already present)
       ## b. mortality when duration of infection is reachd (already present)
       ## c. age specific mortality for HIV-negatives (currently implemented for everyone)
       ## d. mortality based on CD4 count for HIV-positives

    ## 7 June 2013: Adjust probability of death before limit of "dur.inf"
    ## is reached, ideally based on CD4 count, but another way might be to adjust
    ## this probability between HIV-infecteds and non-infecteds.

    ## 5 June 2013: Currently new male nodes are all nodes with ID less than N,
    ## and all female nodes are those with IDs greater than N.
    ## This needs to be fixed, so that sex is assigned by whether a node
    ## appears as an "actor" in the bipartite network, or an "event."
    ## See code below.
    ## Changed name of "pregnancy.status" attribute to "curr.pregnancy.status"
   
    ## 28 May 2013: readapt for size of time.step

    ## 17 May 2013: Moved "give feedback" step to the main sim loop.

    ## 6 April 2013: Add sex, CD4 counts and viral load
    ##               for new people entering the population

    ## 5 April 2013: Fixed model for deaths. Toggles-error was related to this.
    ## Have to be careful with cross-sectional network and full network
    ## Debug deaths due to AIDS

    ## 4 April 2013: Change Name of function to update vital dynamics

    ## 28 March 2013: Currently all new entires are HIV negative. 
    ## Should change as per proportion of HIV positives in the population.

    ## 27 March 2013: Make death rate age specific.
    ## "d1" has working version with a general death rate (and constant pregnancy).

    ## 27 March 2013: Initial version completed.
    ## Make death rate age-specific
    ## Make birth rate comparable to population of 15 year olds,

     update.vital.dynamics <-
  function(nw, verbose,
           max.survival, #26Aug2013
           # dur.inf=dur.inf, #total lifespan for infected individuals -- rmvd on 22Aug13
           asmr.male.male,
           asmr.female.female,
           phi,
           size.of.timestep,
           prop.f, # 9Jul13
           circum.rate, #12Jul13
           baseline.art.coverage.rate, #6Nov13: these are needed
           baseline.preg.coverage.rate,
           #recruit.inf.prop,
           recruit.inf.prop.male, #3Dec13
           recruit.inf.prop.female, #3Dec13
           undetectable.vl, #7Nov13
           time.to.full.supp, #7Nov13
           given.dur.inf.by.age, #7Nov13
           ...
           ){

    ## Update temporal attributes
       ## age
       age <- nw%v%"age"
       ## 7Jun 13: age here is in year-units, not 14 day timesteps, as
       ## everything else
       age <- age+(size.of.timestep/365) ## update everyone's age by 14 days
       nw%v%"age" <- age
       ## time since infection
       time.since.infection <- nw%v%"time.since.infection"  
       time.since.infection <- time.since.infection + 1
       nw%v%"time.since.infection" <- time.since.infection
       ## time since art initiation
       time.since.art.initiation <- nw%v%"time.since.art.initiation"
       time.since.art.initiation <- time.since.art.initiation+1
       nw%v%"time.since.art.initiation" <- time.since.art.initiation

       nw%v%"time.since.art.cessation" <- (nw%v%"time.since.art.cessation")+1
       # 25Jul13: Update 'time.since.art.cessation' 

       time.since.curr.pregnancy <- nw%v%"time.since.curr.pregnancy"
       nw%v%"time.since.curr.pregnancy" <- (time.since.curr.pregnancy)+1
       time.since.last.pregnancy <- nw%v%"time.since.last.pregnancy"
       nw%v%"time.since.last.pregnancy" <- (time.since.last.pregnancy)+1       

       ## 30Sep13: 
        age.cat <- assign.age.cat(age, cutoffs=c(25,35,45,55))
        nw%v%"age.cat" <- age.cat



       
    ## deaths from AIDS
    ##    inf.time <- nw %v% 'inf.time'
    ##    dying.of.aids <- which(time-inf.time==dur.inf &
    ##                           is.active(nw,v=1:network.size(nw), at=time))

       time.since.infection <- nw%v%"time.since.infection"
       art.status <- nw%v%"art.status" # 8 Aug 2013: need to limit duration of 
       ## infection for those who are not on any ART. 
       dur.inf.by.age <- nw%v%"dur.inf.by.age" #22Aug13: age-based duration of infection
       

       #######################################################################
       
       ## 26Aug2013: First death by age of maximum survial = "max.survival"
       
       dying.of.age <- which(floor(age) == max.survival &
                             is.active(nw, v=1:network.size(nw), at=time))#26Aug13:VERYIMP
                                        # 26Aug13: floor(age) to round below to
                                        # max survival (55 years)
                                        # using >= will show the total
                                        # number of deaths due to age 

       if(length(dying.of.age)>0) {
         nw <- deactivate.vertices(nw, onset=time, terminus=Inf, v=dying.of.age)
         dying.of.age.edges <- get.edgeIDs.active(nw, dying.of.age[1], at=time)
         ## In theory an sapply with an unlist
         ## could handle this; in practice, there are issues.
         for (i in dying.of.age[-1]) {
           dying.of.age.edges <- c(dying.of.age.edges,
                                    get.edgeIDs.active(nw, i, at=time))
         }

         if (length(dying.of.age.edges)>0) {
           nw <- deactivate.edges(nw,onset=time, terminus=Inf, e=dying.of.age.edges)
         }

       }

       if (verbose) cat("Deaths due to age", length(dying.of.age), "\n")

     # Activation of network that is left
       node.active <- is.active(nw, v=1:network.size(nw), at=time)
                                        # update the list of still-alive nodes
       active.nodes <- which(node.active)

       popsize.temp <- sum(node.active)

       if(popsize.temp==0) break

       
       #######################################################################





       #######################################################################
       
       ## 22Aug2013: Change below to make deaths due to AIDS individualized to 
       ## life expectancy by age at time of infection
       
       ## 8 Aug 2013: comment below and add "no-ART" condition for
       ## time-since-infection-based death.
       ## dying.of.aids <- which(time.since.infection == dur.inf &
       ##                        is.active(nw, v=1:network.size(nw), at=time))

       ## dying.of.aids <- which(time.since.infection == dur.inf &
       ##                        art.status != 1 &
       ##                         is.active(nw, v=1:network.size(nw), at=time))

       dying.of.aids <- which(time.since.infection == dur.inf.by.age &
                                        # 22Aug13:compare #with aboves
                                        # made sure all "dur.inf.by.age" values are 
                                        # integers -- by rounding
                              art.status != 1 & 
                              is.active(nw, v=1:network.size(nw), at=time))

       if (verbose) cat("AIDS deaths", length(dying.of.aids), "\n")

       if(length(dying.of.aids)>0) {
         nw <- set.vertex.attribute(nw, "time.of.death",
                                    time, v=dying.of.aids)
                                        #2Dec13: time of death due to AIDS
         
         nw <- deactivate.vertices(nw, onset=time, terminus=Inf, v=dying.of.aids)
         dying.of.aids.edges <- get.edgeIDs.active(nw, dying.of.aids[1], at=time)

         ## In theory an sapply with an unlist
         ## could handle this; in practice, there are issues.
         for (i in dying.of.aids[-1]) {
           dying.of.aids.edges <- c(dying.of.aids.edges,
                                    get.edgeIDs.active(nw, i, at=time))
         }

         if (length(dying.of.aids.edges)>0) {
           nw <- deactivate.edges(nw,onset=time, terminus=Inf, e=dying.of.aids.edges)
         }

       }

     # 26 Aug 2013: Update active nodes
       node.active <- is.active(nw, v=1:network.size(nw), at=time)
                                        # update the list of still-alive nodes
       active.nodes <- which(node.active)

       popsize.temp <- sum(node.active)

       if(popsize.temp==0) break

       #######################################################################
       
       

       ##########################################
       ### ASK: add for age-specific mortality
       ##########################################
        ## m.curr <- which(get.vertex.attribute(nw,'sex')==0) ##ASK
        ## f.curr <- which(get.vertex.attribute(nw,'sex')==1) ##ASK

       ##  age.m.curr <- nw%v%"age"[which(get.vertex.attribute(nw,'sex')==0)
       ##  age.f.curr <- nw%v%"age"[which(get.vertex.attribute(nw,'sex')==1)]
           nw.curr.wo.dead.nodes <- network.extract(nw, at=time)       
           male.id.curr <- nwmodes(nw.curr.wo.dead.nodes, 1)
           female.id.curr <- nwmodes(nw.curr.wo.dead.nodes, 2)

           male.curr.age <- get.vertex.attribute(nw.curr.wo.dead.nodes,
                                                 "age")[male.id.curr]
           female.curr.age <- get.vertex.attribute(nw.curr.wo.dead.nodes,
                                                   "age")[female.id.curr]
          ## 6Jul13: Infection status-based mortalities
           male.inf.status <- get.vertex.attribute(nw.curr.wo.dead.nodes,
                                                   "inf.status")[male.id.curr]
           female.inf.status <- get.vertex.attribute(nw.curr.wo.dead.nodes,
                                                     "inf.status")[female.id.curr]
           male.cd4.today <- get.vertex.attribute(nw.curr.wo.dead.nodes,
                                                   "cd4.count.today")[male.id.curr]
           female.cd4.today <- get.vertex.attribute(nw.curr.wo.dead.nodes,
                                                     "cd4.count.today")[female.id.curr]
           ## 6Jul13: Add info on art.status here
           male.art.status <- get.vertex.attribute(nw.curr.wo.dead.nodes,
                                                   "art.status")[male.id.curr]
           female.art.status <- get.vertex.attribute(nw.curr.wo.dead.nodes,
                                                     "art.status")[female.id.curr]
       
       ## write function assigning non-aids probability for men and women
       ## based on age in separate file.
       
       ## call that function here.

           ## 6Jul13: infection-status based
           ## male.mu.non.aids <- assign.asmr.male(male.curr.age,
           ##                                      asmr.male=asmr.male)
           ## female.mu.non.aids <- assign.asmr.female(female.curr.age,
           ##                                           asmr.female=asmr.female)
           ## browser()
           male.mu.non.aids <- assign.mortality.male(male.curr.age=
                                                     male.curr.age,
                                                     asmr.male=
                                                     asmr.male,
                                                     male.inf.status=
                                                     male.inf.status,
                                                     male.cd4.today=
                                                     male.cd4.today,
                                                     male.art.status=
                                                     male.art.status,
                                                     size.of.timestep=
                                                     size.of.timestep#14Jan14
                                                     )

           female.mu.non.aids <- assign.mortality.female(female.curr.age=
                                                         female.curr.age,
                                                         asmr.female=
                                                         asmr.female,
                                                         female.inf.status=
                                                         female.inf.status,
                                                         female.cd4.today=
                                                         female.cd4.today,
                                                         female.art.status=
                                                         female.art.status,
                                                         size.of.timestep=
                                                         size.of.timestep#14Jan14
                                                         )
       

           mu.non.aids <- c(male.mu.non.aids, female.mu.non.aids)
           ## mu.non.aids <- 1/40

          if(verbose){ ## ASK
            cat("Number of men at timestep ",  time, 
               "(before non-AIDS birth-death process) is ",
                length(male.id.curr), "\n") ## ASK
            cat("Number of women at timestep", time, 
                "(before non-AIDS birth-death process) is",
                length(female.id.curr), "\n") ## ASK
          }


       ##########################################
       
       dying.natural.index <- which(rbinom(popsize.temp, 1, mu.non.aids) == 1)

       dying.natural <- active.nodes[dying.natural.index]

       if (verbose) { ## ASK
         cat("Non-AIDS deaths (Men): ", length(which(dying.natural.index <=
                                                   max(male.id.curr))), "\n")
         cat("Non-AIDS deaths (Women): ", length(which(dying.natural.index >
                                                   max(male.id.curr))), "\n")

         cat("Total non-AIDS deaths: ", length(dying.natural), "\n")

       } # 6Jul2013: These are non-AIDS deaths now, not non-HIV deaths


       if (length(dying.natural)>0){

              nw <- set.vertex.attribute(nw, "time.of.death",
                                         time, v=dying.natural)
                                        #2Dec13: record time of death

              nw <- deactivate.vertices(nw,onset=time, terminus=Inf,
                                        v=dying.natural)


       	      dying.natural.edges <- get.edgeIDs.active(nw, dying.natural[1],
                                                        at=time)
              # In theory an sapply with an unlist could handle this;
              # in practice, there are issues.

              for (i in dying.natural[-1]) dying.natural.edges <-
                c(dying.natural.edges, get.edgeIDs.active(nw,i,at=time))

              if (length(dying.natural.edges)>0)
                nw <- deactivate.edges(nw,onset=time, terminus=Inf,
                                       e=dying.natural.edges)
	    }
	    		
     # 26 Aug 2013: Update active nodes info
       node.active <- is.active(nw, v=1:network.size(nw), at=time)
                                        # update the list of still-alive nodes
       active.nodes <- which(node.active)

       popsize.temp <- sum(node.active)

       if(popsize.temp==0) break

       ##########################################


     ### births
	    
      node.active <- is.active(nw, v=1:network.size(nw), at=time)
                                        # update the list of still-alive nodes
      active.nodes <- which(node.active)
      popsize.temp <- sum(node.active)

      if(popsize.temp==0) break

      ##popsize.f.curr <- sum(active.nodes<get.network.attribute(nw,'bipartite'))
        popsize.m.curr <- sum(active.nodes<get.network.attribute(nw,'bipartite')) ##ASK
        popsize.f.curr <- popsize.temp - popsize.m.curr ##ASK
        
        nintros <- rpois(1, popsize.f.curr*phi)
        ## 7Jun13:
        ## maybe adjust phi based on data on proportion of 15-year olds in pop

                                 
        nintros.female <- rbinom(1, nintros, prop.f)
       ## ASK: these should fix the sex-dist
       ## to that specified initially
       ## since that's when prop.m and prop.f
       ## are specified.
        nintros.male <- nintros - nintros.female		

        if (verbose){ 
          cat("Number of Intros is ", nintros, "\n")                         
          cat("Number of Male Intros is ", nintros.male, "\n")
          cat("Number of Female Intros is ", nintros.female , "\n")
        }
       
      ## ASK: Comment this out, because in mine men are the first gender
      ## in the bipartite network
       
      ## if (nintros.feml>0) {
      ##   for (zzz in 1:nintros.feml) nw <- add.vertices(nw, 1, last.mode=F)
      ## } # This loop approach is temp to get around bug in network

       if (nintros.male>0) { ## ASK, changed to account for males being the first ID
            for (zzz in 1:nintros.male) nw <- add.vertices(nw, 1, last.mode=FALSE)
          } # This loop approach is temp to get around bug in network

       ## nw <- add.vertices(nw,nintros.feml,last.mode=F)
        				# this is the line we want to use when
                                        # bug is fixed
                                        # BE AWARE: ALL MALE VERTEX IDS CHANGE
                                        # WHEN NEW FEMALES ARE ADDED

       nw <- add.vertices(nw, nintros.female, last.mode=TRUE) ## ASK
                            ## question we need a loop above, but not here.
                            ## Why is that?
      

                                        # shortcut, since new nodes don't
                                        # have status set yet

       ########################################################################
       ## ASK: Lines below commented out because we don't
       ## have an attribute called status
       ## important for assigning status to new entries
       ## nodes.to.activate <- which(is.na(nw%v%'status'))
       ## if (verbose) cat("Births",length(nodes.to.activate),"\n")
       ## if (length(nodes.to.activate)>0) {
       ##   nw <- activate.vertices(nw,onset=time, terminus=Inf, v=nodes.to.activate)
       ##   nw <- set.vertex.attribute(nw,'status', 0, nodes.to.activate)
       ## }

       ## browser()
       
       nodes.to.activate <- which(is.na(nw%v%'inf.status'))
       
       if (verbose) cat("Number with Infection Status NA (nodes to activate) ",
                        length(nodes.to.activate), "\n") ## changed output 

       if (length(nodes.to.activate)>0) { ## if new nodes enter the network


           length.new.nodes <- length(nodes.to.activate)
           new.male.nodes <- nodes.to.activate[which(nodes.to.activate <=
                                                    (nw%n%'bipartite'))]
           new.female.nodes <- nodes.to.activate[which(nodes.to.activate >
                                                      (nw%n%'bipartite'))]
          
         ## update relevant attributes here 
           nw <- activate.vertices(nw, onset=time,
                                   terminus=Inf, v=nodes.to.activate)

           nw <- set.vertex.attribute(nw, "time.of.birth", time, nodes.to.activate)
                                 ## 2Dec13: record time of birth
           
           nw <- set.vertex.attribute(nw, "age", 18, nodes.to.activate)
               ## 11 November 2013: revise initial age at entry to 18 

           nw <- set.vertex.attribute(nw, "age.cat", 1, nodes.to.activate)
               ## 30Sep13: set initial age category to 1,
               ## since all new ages are 18 

           nw <- set.vertex.attribute(nw, "num.of.pregnancies",
                                      0, nodes.to.activate)#3Dec13
           
           nw <- set.vertex.attribute(nw, "sex", 0,
                                      new.male.nodes
                                      )
                                    ## All nodes with ID below 5000 will be male

           nw <- set.vertex.attribute(nw, "sex", 1,
                                      new.female.nodes
                                      )
                                    ## All nodes with ID above 5000 will be female

           nw <- set.vertex.attribute(nw, "cd4.count.today", 518,
                                      new.male.nodes
                                      )
                                    ## All new male nodes will have CD4 518

           nw <- set.vertex.attribute(nw, "cd4.count.today", 570,
                                      new.female.nodes
                                      )
                                    ## All new female nodes will have CD4 570
         
           nw <- set.vertex.attribute(nw, "viral.load.today", 0,
                                      nodes.to.activate)

           nw <- set.vertex.attribute(nw, "curr.pregnancy.status", NA,
                                      new.male.nodes)

           nw <- set.vertex.attribute(nw, "curr.pregnancy.status", 0,
                                      new.female.nodes)

           nw <- set.vertex.attribute(nw, "time.since.curr.pregnancy", NA,
                                      nodes.to.activate)

           nw <- set.vertex.attribute(nw, "time.since.last.pregnancy", NA,
                                      nodes.to.activate)

          ## add circumcision status for men
           circum.status.new.males <- rbinom(length(new.male.nodes), 1,
                                             circum.rate
                                             )
           nw <- set.vertex.attribute(nw, "circum.status", circum.status.new.males,
                                      new.male.nodes)
           nw <- set.vertex.attribute(nw, "circum.status", NA,
                                      new.female.nodes)
           
           nw <- set.vertex.attribute(nw, "curr.pregnancy.status", 0,
                                      new.female.nodes)

           ## 6 Nov 2013: Infection and ART status for recruits

           ## recruit.inf.status             <- rbinom(length(nodes.to.activate), 1,
           ##                                         recruit.inf.prop)
           recruit.inf.status.male        <- rbinom(length(new.male.nodes), 1,
                                                    recruit.inf.prop.male) #3Dec13
           recruit.inf.status.female      <- rbinom(length(new.female.nodes), 1,
                                                      recruit.inf.prop.female)#3Dec13
           recruit.inf.status             <- c(recruit.inf.status.male,
                                               recruit.inf.status.female)#3Dec13
           recruit.infected               <- which(recruit.inf.status == 1)
           recruit.infected.ID            <- nodes.to.activate[recruit.infected] #8Nov13
           recruit.infected.art.indicator <- rbinom(length(recruit.infected),
                                                    1, baseline.art.coverage.rate)
           recruit.infected.art.covered   <- which(recruit.infected.art.indicator == 1)
           fem.recruit.infected           <- intersect(new.female.nodes,
                                                       nodes.to.activate[recruit.infected]
                                                       )
           fem.recruit.infected.preg.indicator <- rbinom(length(fem.recruit.infected),
                                                         1, baseline.preg.coverage.rate)
           fem.recruit.infected.preg.covered <- which(fem.recruit.infected.preg.indicator
                                                      == 1)
 
           nw <- set.vertex.attribute(nw,'inf.status', 0, nodes.to.activate)

           nw <- set.vertex.attribute(nw, "art.covered", NA,
                                      nodes.to.activate)

           nw <- set.vertex.attribute(nw, "preg.covered", NA,
                                       nodes.to.activate)

           ## browser()
           if (length(recruit.infected) > 0){
             
             vl.art.traj.slope <- 
             abs((undetectable.vl -  0)/time.to.full.supp)
                                        #7Nov13: needed attribute for
                                        # infecteds at recruitment
                                        # all entrants have VL 0 at entry

             ## browser()
             
             nw <- set.vertex.attribute(nw,'inf.status',
                                        1, recruit.infected.ID)
             nw <- set.vertex.attribute(nw,'art.covered',
                                        0, recruit.infected.ID)
             nw <- set.vertex.attribute(nw,'preg.covered',
                                        0, recruit.infected.ID)
             nw <- set.vertex.attribute(nw,'viral.load.today',
                                        0, recruit.infected.ID)
             nw <- set.vertex.attribute(nw,'art.status',
                                        0, recruit.infected.ID)
             nw <- set.vertex.attribute(nw,'time.of.infection',
                                        time, recruit.infected.ID)
             nw <- set.vertex.attribute(nw,'time.since.infection',
                                        0, recruit.infected.ID)
             nw <- set.vertex.attribute(nw,'infector.ID',
                                        NA, recruit.infected)

             nw <- set.vertex.attribute(nw, "vl.art.traj.slope",
                                        vl.art.traj.slope,
                                        recruit.infected.ID)
                                        #7Nov13

             nw <- set.vertex.attribute(nw, "dur.inf.by.age",
                                        given.dur.inf.by.age[1],
                                        recruit.infected.ID)
                                        #7Nov13
           }

           if (length(recruit.infected.art.covered) > 0){
             nw <- set.vertex.attribute(nw, 'art.covered',
                                        1,
                                        recruit.infected.ID[recruit.infected.art.covered])
           }

           if (length(fem.recruit.infected.preg.covered) > 0){
             nw <- set.vertex.attribute(nw, 'preg.covered',
                                        1,
                                        fem.recruit.infected[fem.recruit.infected.preg.covered])
           }
           
         ########################################################
           }
           
        ########################################################################

       
    ## Compile results 
	    
       node.active <- is.active(nw, v=1:network.size(nw), at=time)
       nw.curr.wo.dead.nodes <- network.extract(nw, at=time)
       
       if (verbose) {

         alive.male <- nwmodes(nw.curr.wo.dead.nodes, 1)
         alive.female <- nwmodes(nw.curr.wo.dead.nodes, 2)       

         cat("Number Alive (Men): ", length(alive.male),
             "Number Alive (Women): ", length(alive.female),
             "Number Alive (Total): ", network.size(nw.curr.wo.dead.nodes),
             "\n"
             )

       }

       
       popsize[time]  <- network.size(nw.curr.wo.dead.nodes) 
       ## popsize.f[time]    <- nw.curr.wo.dead.nodes%n%'bipartite' ## ASK: changed to match
       ## popsize.m[time]    <- popsize[time] - popsize.f[time] ## ASK:  gender to bipartite
       popsize.m[time]    <- nw.curr.wo.dead.nodes%n%'bipartite' ## ASK: changed to match
       popsize.f[time]    <- popsize[time] - popsize.m[time] ## ASK:  gender to bipartite


       ### ASK: prev information below commented out.
       ## We do not have "status" attribute

       ## prev.i[time]     <- mean((nw.curr.wo.dead.nodes%v%'status'),na.rm=T)
       ## prev.i.f[time]   <- mean((nw.curr.wo.dead.nodes%v%'status')[1:popsize.f[time]],
       ##                          na.rm=T)
       ## prev.i.m[time]   <- mean((nw.curr.wo.dead.nodes%v%'status')[(popsize.f[time]+1):
       ##                                                             popsize[time]],na.rm=T)

       prev.i[time]     <- mean((nw.curr.wo.dead.nodes%v%'inf.status'),na.rm=T)
       prev.i.f[time]   <- mean((nw.curr.wo.dead.nodes%v%'inf.status')
                                [1:popsize.f[time]], na.rm=T)
       prev.i.m[time]   <- mean((nw.curr.wo.dead.nodes%v%'inf.status')
                                [(popsize.f[time]+1): popsize[time]],na.rm=T)
                                
       
       ## num.deaths.aids[time] <- length(dying.of.aids)
       num.deaths.natural[time] <- length(dying.natural)
       ## num.births[time]   		<- length(nodes.to.activate)
	 
    ## Update edges coef, give feedback
        theta.form[1] <-  theta.form[1] + log(popsize[time-1]) - log(popsize[time])
       ## cat("Finished timestep ",time," of ",timesteps,".\n",sep="")

       ####################################################
       ## 23 Aug 2013: Machinery to record demographic data
       ## 26 Aug 2013: Also add deaths due to age here
       ####################################################

       dem.data <- paste(date, ".demography", ".csv", sep="")

       deaths.natural.male <- length(which(dying.natural.index <=
                                    max(male.id.curr)))
       deaths.natural.female <- length(which(dying.natural.index >
                                      max(male.id.curr)))

       deaths.ofaids.male <- length(which(dying.of.aids <=
                                          max(male.id.curr)))
       deaths.ofaids.female <- length(which(dying.of.aids >
                                             max(male.id.curr)))

       deaths.ofage.male <- length(which(dying.of.age <= #26Aug13: dying of age
                                          max(male.id.curr)))
       deaths.ofage.female <- length(which(dying.of.age > #26Aug13: dying of age
                                             max(male.id.curr)))

       
       write.table(cbind(time,
                         nintros, #births
                         nintros.male, #male births
                         nintros.female, #female births
                         deaths.ofage.male, #26Aug13: death of age (male)
                         deaths.ofage.female, #26Aug13: death of age (female)
                         deaths.natural.male, #non-AIDS deaths (natural)
                         deaths.natural.female,
                         deaths.ofaids.male,
                         deaths.ofaids.female
                         ),
                   file=dem.data, ## to note total number of new infections
                   append=TRUE,
                   col.names=FALSE,
                   row.names=FALSE
                   )
       
       return(nw)
     }
