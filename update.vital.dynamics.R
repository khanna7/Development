    ## Model vital dynamics 

    ## 4 April 2013: Change name of function to update vital dynamics

    ## 27 March 2013: Initial version completed.
    ## Make death rate age-specific
    ## Make birth rate comparable to population of 15 year olds,

     update.vital.dynamics <-
  function(nw,
                                       verbose=TRUE
                                       ## Add arguments for age-specific mortality rate
                                ## birth rate (population of 15 year olds)
                                ## 
                                ){

       
     # deaths from AIDS
       inf.time <- nw %v% 'inf.time'
       dying.of.aids <- which(time-inf.time==dur.inf &
                              is.active(nw,v=1:network.size(nw), at=time))

       if (verbose) cat("AIDS deaths", length(dying.of.aids), "\n")

       if(length(dying.of.aids)>0) {
         nw <- deactivate.vertices(nw, onset=time, terminus=Inf, v=dying.of.aids)
         dying.of.aids.edges <- get.edgeIDs.active(nw, dying.of.aids[1], at=time)

                                        # In theory an sapply with an unlist
                                        # could handle this; in practice, there are issues.
         for (i in dying.of.aids[-1]) {
           dying.of.aids.edges <- c(dying.of.aids.edges,
                                    get.edgeIDs.active(nw, i, at=time))
         }

         if (length(dying.of.aids.edges)>0) {
           nw <- deactivate.edges(nw,onset=time, terminus=Inf, e=dying.of.aids.edges)
         }

       }

     # other deaths
       node.active <- is.active(nw,v=1:network.size(nw),at=time)
                                        # update the list of still-alive nodes
       active.nodes <- which(node.active)

       popsize.temp <- sum(node.active)

       if(popsize.temp==0) break
	
       dying.natural.index <- which(rbinom(popsize.temp,1,mu.non.aids)==1)

       dying.natural <- active.nodes[dying.natural.index]

       if (verbose) cat("Other deaths", length(dying.natural), "\n")

       if (length(dying.natural)>0){

              nw <- deactivate.vertices(nw,onset=time, terminus=Inf, v=dying.natural)
       	      dying.natural.edges <- get.edgeIDs.active(nw,dying.natural[1],at=time)
              # In theory an sapply with an unlist could handle this;
              # in practice, there are issues.

              for (i in dying.natural[-1]) dying.natural.edges <-
                c(dying.natural.edges, get.edgeIDs.active(nw,i,at=time))

              if (length(dying.natural.edges)>0)
                nw <- deactivate.edges(nw,onset=time, terminus=Inf, e=dying.natural.edges)
	    }
	    		
    # births
	    
      node.active <- is.active(nw,v=1:network.size(nw),at=time)
                                        # update the list of still-alive nodes
      active.nodes <- which(node.active)
      popsize.temp <- sum(node.active)

      if(popsize.temp==0) break

      ##popsize.f.curr <- sum(active.nodes<get.network.attribute(nw,'bipartite'))
        popsize.m.curr <- sum(active.nodes<get.network.attribute(nw,'bipartite')) ##ASK
        popsize.f.curr <- popsize.temp - popsize.m.curr ##ASK
        
      nintros <- rpois(1, popsize.f.curr*phi)

      nintros.feml <- rbinom(1, nintros, prop.f) ## ASK: these should fix the sex-dist
                                                 ## to that specified initially
                                                 ## since that's when prop.m and prop.f
                                                 ## are specified.
       nintros.male <- nintros - nintros.feml		

      ## ASK: Comment this out, because in mine men are the first gender
      ## in the bipartite network
       
      ## if (nintros.feml>0) {
      ##   for (zzz in 1:nintros.feml) nw <- add.vertices(nw, 1, last.mode=F)
      ## } # This loop approach is temp to get around bug in network

       if (nintros.male>0) { ## ASK, changed to account for males being the first ID
            for (zzz in 1:nintros.male) nw <- add.vertices(nw, 1, last.mode=F)
          } # This loop approach is temp to get around bug in network

       ## nw <- add.vertices(nw,nintros.feml,last.mode=F)
					# this is the line we want to use when
                                        # bug is fixed
                                        # BE AWARE: ALL MALE VERTEX IDS CHANGE
                                        # WHEN NEW FEMALES ARE ADDED

       ## nw <- add.vertices(nw,nintros.male,last.mode=T) ## ASK: changed below
       nw <- add.vertices(nw,nintros.male,last.mode=T) ## ASK
      
       nodes.to.activate <- which(is.na(nw%v%'status'))
                                        # shortcut, since new nodes don't
                                        # have status set yet
       if (verbose) cat("Births",length(nodes.to.activate),"\n")
       if (length(nodes.to.activate)>0) {
         nw <- activate.vertices(nw,onset=time, terminus=Inf, v=nodes.to.activate)
         nw <- set.vertex.attribute(nw,'status', 0, nodes.to.activate)
       }

    ## Compile results 
	    
       node.active <- is.active(nw, v=1:network.size(nw), at=time)
       nw.curr.wo.dead.nodes <- network.extract(nw, at=time)
		
       popsize[time]  <- network.size(nw.curr.wo.dead.nodes) 
       ## popsize.f[time]    <- nw.curr.wo.dead.nodes%n%'bipartite' ## ASK: changed to match
       ## popsize.m[time]    <- popsize[time] - popsize.f[time] ## ASK:  gender to bipartite
       popsize.m[time]    <- nw.curr.wo.dead.nodes%n%'bipartite' ## ASK: changed to match
       popsize.f[time]    <- popsize[time] - popsize.m[time] ## ASK:  gender to bipartite

       prev.i[time]     <- mean((nw.curr.wo.dead.nodes%v%'status'),na.rm=T)
       prev.i.f[time]   <- mean((nw.curr.wo.dead.nodes%v%'status')[1:popsize.f[time]],
                                na.rm=T)
       prev.i.m[time]   <- mean((nw.curr.wo.dead.nodes%v%'status')[(popsize.f[time]+1):
                                                                   popsize[time]],na.rm=T)

       num.deaths.aids[time] <- length(dying.of.aids)
       num.deaths.natural[time] <- length(dying.natural)
       num.births[time]   		<- length(nodes.to.activate)
	 
    ## Update edges coef, give feedback

       theta.form[1] <-  theta.form[1] + log(popsize[time-1]) - log(popsize[time])
       cat("Finished timestep ",time," of ",timesteps,".\n",sep="")
    
     }
