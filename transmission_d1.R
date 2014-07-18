#####################################################
### Model transmission  
#####################################################

  ### Extract partnership network
    
     nw.el <- as.edgelist(network.extract(nw,
                                          at = time,
                                          retain.all.vertices = T))

     status.el <- matrix((nw %v% "inf.status")[nw.el], ncol = 2)
     inf.time <- nw %v% "inf.time"
     time.since.infection <- nw %v% "time.since.infection" # ASK

     circum.status <- nw %v% "circum.status" # ASK
     pregnancy.status <- nw %v% "pregnancy.status" # ASK
     art.status <- nw %v% "art.status" # ASK


  ### Transmission from male to female

     discordant.mpos <- status.el[ ,1]==1 & status.el[ ,2]==0
     transmittable.m <- nw.el[discordant.mpos, 1]
     infectible.m <- nw.el[discordant.mpos, 2]
     ##transmittable.m.inf.time.bp <- time-inf.time[transmittable.m]
     transmittable.m.time.since.infection <- time.since.infection # ASK

     ##transmittable.m.beta <-  beta.per.month[transmittable.m.time.since.infection]
       
                              # check units for transmission probability vector

     ## Remember circumcision status has no impact
     ## when male is infected partner

     ## transmissions.m <- rbinom(length(transmittable.f.beta), 1,
     ##                           transmittable.f.beta)

     transmissions.m <- rbinom(length(transmittable.f.beta), 1,
                               transmittable.f.beta)

     newinf.m <- infectible.m[transmissions.m == 1]
     nw <- set.vertex.attribute(nw,'inf.status', 1, v=newinf.m)
     nw <- set.vertex.attribute(nw,'inf.time', time, v=newinf.m)
     nw <- set.vertex.attribute(nw,'time.since.infection', 0, v=newinf.m)

    # transmission from female to male
    	discordant.mpos <- status.el[,2]==1 & status.el[,1]==0
		transmittable.m <- nw.el[discordant.mpos,2]
		infectible.f <- nw.el[discordant.mpos,1]
		transmittable.m.inf.time.bp <- time-inf.time[transmittable.m]
		transmittable.m.beta <-  beta.per.month[transmittable.m.inf.time.bp]
		transmissions.m <- rbinom(length(transmittable.m.beta),1,transmittable.m.beta)
		newinf.f <- infectible.f[transmissions.m==1]
		nw <- set.vertex.attribute(nw,'status',1,v=newinf.f)
		nw <- set.vertex.attribute(nw,'inf.time',time,v=newinf.f)
	    
	# calculate incidence
		
  	inci[time] <- length(newinf.f) + length(newinf.m)
  	inci.f[time] <- length(newinf.f)
  	inci.m[time] <- length(newinf.m)
  	if (verbose) cat("Transmissions",inci[time],"\n")
