    ## Model vital dynamics 

    ## 28 May 2013: readapt for size of time.stepo

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
  function(nw, verbose=TRUE,
           dur.inf=dur.inf, #total lifespan for infected individuals
           asmr.male=asmr.male,
           asmr.female=asmr.female,
           phi=phi,
           size.of.timestep=size.of.timestep,
           ...
           ## Add arguments for age-specific mortality rate
           ## birth rate (population of 15 year olds)
           ## 
           ){

    ## Update temporal attributes
       ## age
       age <- nw%v%"age"
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
       
    ## deaths from AIDS
    ##    inf.time <- nw %v% 'inf.time'
    ##    dying.of.aids <- which(time-inf.time==dur.inf &
    ##                           is.active(nw,v=1:network.size(nw), at=time))

       time.since.infection <- nw%v%"time.since.infection"
       dying.of.aids <- which(time.since.infection == dur.inf &
                              is.active(nw, v=1:network.size(nw), at=time))

       
       if (verbose) cat("AIDS deaths", length(dying.of.aids), "\n")

       if(length(dying.of.aids)>0) {
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

     # other deaths
       node.active <- is.active(nw, v=1:network.size(nw), at=time)
                                        # update the list of still-alive nodes
       active.nodes <- which(node.active)

       popsize.temp <- sum(node.active)

       if(popsize.temp==0) break


       ##########################################
       ### ASK: add for age-specific mortality
       ##########################################
        ## m.curr <- which(get.vertex.attribute(nw,'sex')=="male") ##ASK
        ## f.curr <- which(get.vertex.attribute(nw,'sex')=="female") ##ASK

       ##  age.m.curr <- nw%v%"age"[which(get.vertex.attribute(nw,'sex')=="male")
       ##  age.f.curr <- nw%v%"age"[which(get.vertex.attribute(nw,'sex')=="female")]
           nw.curr.wo.dead.nodes <- network.extract(nw, at=time)       
           male.id.curr <- nwmodes(nw.curr.wo.dead.nodes, 1)
           female.id.curr <- nwmodes(nw.curr.wo.dead.nodes, 2)

           male.curr.age <- get.vertex.attribute(nw.curr.wo.dead.nodes,
                                                 "age")[male.id.curr]
           female.curr.age <- get.vertex.attribute(nw.curr.wo.dead.nodes,
                                                   "age")[female.id.curr]
        
           
       ## ## write function assigning non-aids probability for men and women                            based on age in separate file.
       ## ## call that function here.
           male.mu.non.aids <- assign.asmr.male(male.curr.age, asmr.male=asmr.male)
           female.mu.non.aids <- assign.asmr.female(female.curr.age,
                                                     asmr.female=asmr.female)


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
         cat("Non-HIV deaths (Men): ", length(which(dying.natural.index <=
                                                   max(male.id.curr))), "\n")
         cat("Non-HIV deaths (Women): ", length(which(dying.natural.index >
                                                   max(male.id.curr))), "\n")

         cat("Total non-HIV deaths: ", length(dying.natural), "\n")

       }


       if (length(dying.natural)>0){

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
	    		
    # births
	    
      node.active <- is.active(nw, v=1:network.size(nw), at=time)
                                        # update the list of still-alive nodes
      active.nodes <- which(node.active)
      popsize.temp <- sum(node.active)

      if(popsize.temp==0) break

      ##popsize.f.curr <- sum(active.nodes<get.network.attribute(nw,'bipartite'))
        popsize.m.curr <- sum(active.nodes<get.network.attribute(nw,'bipartite')) ##ASK
        popsize.f.curr <- popsize.temp - popsize.m.curr ##ASK
        
        nintros <- rpois(1, popsize.f.curr*phi)

                                 
        nintros.female <- rbinom(1, nintros, prop.f) ## ASK: these should fix the sex-dist
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

       nodes.to.activate <- which(is.na(nw%v%'inf.status'))
       
       if (verbose) cat("Number with Infection Status NA (nodes to activate) ",
                        length(nodes.to.activate), "\n") ## changed output 

       if (length(nodes.to.activate)>0) {
               ## update relevant attributes here 
         nw <- activate.vertices(nw, onset=time, terminus=Inf, v=nodes.to.activate)
         nw <- set.vertex.attribute(nw,'inf.status', 0, nodes.to.activate)
               ## all initial infected states are 0, will
               ## modify to proportion of children who are infected initially
         nw <- set.vertex.attribute(nw, "age", 15, nodes.to.activate)
               ## set initial age as 15 for 
         


         nw <- set.vertex.attribute(nw, "sex", "male", 
                                    nodes.to.activate[which(nodes.to.activate < N)])
                                    ## All nodes with ID below 5000 will be male

         nw <- set.vertex.attribute(nw, "sex", "female",
                                    nodes.to.activate[which(nodes.to.activate > N)])
                                    ## All nodes with ID above 5000 will be female

         nw <- set.vertex.attribute(nw, "cd4.count.today", 518,
                                    nodes.to.activate[which(nodes.to.activate < N)])
                                    ## All new male nodes will have CD4 518

         nw <- set.vertex.attribute(nw, "cd4.count.today", 570,
                                    nodes.to.activate[which(nodes.to.activate > N)])
                                    ## All new female nodes will have CD4 570
         
         nw <- set.vertex.attribute(nw, "viral.load.today", 0,
                                    nodes.to.activate)
               ## set initial age as 15 for 
         nw <- set.vertex.attribute(nw, "pregnancy.status", 0,
                                    nodes.to.activate)

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

       return(nw)
     }
