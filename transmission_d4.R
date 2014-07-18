## 7 June 2013: For new infectives, add "art.status" attribute, in addition to
## 'inf.status', 'inf.time', and 'time.since.infection' attributes.
## Also found error in updating attributes of newly infected males.
## The vertex argument said "v=newinf.f", instead of "newinf.f".

## 24 May 2013: This code assumes daily sex. Need to assign frequency of unprotected intercourse

## 20 May 2013: Differentiate updating in the "full network" and the
## cross-sectional network. (This plan put on hold, may not be necessary due to
## "retain.all.vertices" argument in "network.extract" function.

## Make space to record results in csv file.

## 9 April 2013: One idea:
## Set up a list of infectivities thats
## correspond to the relative serostatuses of the two partners
## For concordant partners, this infectivity will be 0.
## For discordant partners, this infectivity will be = to
## infectivity of infected partner
## If male partner is susceptible and circumcised, infectivity reduces.
## If female partner is pregnant, infectivity increases (regardless of infection status).

## 3 April 2013: Rewrite as a function
## Make modifications in infectivity on account of
## circumcision (for susceptible men) or pregnancy here

#####################################################
### Model transmission  
#####################################################

  ### Extract partnership network

      transmission <-
  function(nw,
           preg.mult,
           circum.mult,
           verbose,
           ...
           ) {

       nw.el <- as.edgelist(network.extract(nw,
                                            at = time,
                                            retain.all.vertices = T))
       ## 3Jun13: only pulling out active network

       status.el <- matrix((nw %v% "inf.status")[nw.el], ncol = 2)
       inf.time <- nw %v% "inf.time"
       time.since.infection <- nw %v% "time.since.infection" # ASK

       inf.status <- nw %v% "inf.status"
       circum.status <- nw %v% "circum.status" # ASK
       pregnancy.status <- nw %v% "pregnancy.status" # ASK
       art.status <- nw %v% "art.status" # ASK
       infectivity.today <- nw %v% "infectivity.today" # ASK

       ## Transmission from male to female

       discordant.mpos <- status.el[ ,1]==1 & status.el[ ,2]==0
       transmittable.m <- nw.el[discordant.mpos, 1]
       infectible.f <- nw.el[discordant.mpos, 2]
       ##transmittable.m.inf.time.bp <- time-inf.time[transmittable.m]
       ##transmittable.m.time.since.infection <- time.since.infection # ASK

       ##transmittable.m.beta <-  beta.per.month[transmittable.m.time.since.infection]
                                  # check units for transmission probability vector
 
       ## Remember circumcision status has no impact
       ## when male is infected partner

       ## Modify infectivity on account of pregnant female partner
       ## if (pregnancy.status[infectible.f] == 1){
       ##   infectivity.today[transmissible.m] <- 
       ##     infectivity.today[transmissible.m] * preg.mult
       ## }
       preg.fem <- which(pregnancy.status == 1)
       preg.wom.alt.infectivity <- which(infectible.f %in% preg.fem)
       
       transmissions.m <- rbinom(length(transmittable.m), 1,
                               infectivity.today[transmittable.m])

       newinf.f <- infectible.f[transmissions.m == 1]
       nw <- set.vertex.attribute(nw,'inf.status', 1, v=newinf.f)
       nw <- set.vertex.attribute(nw,'inf.time', time, v=newinf.f)
       nw <- set.vertex.attribute(nw,'time.since.infection',
                                  0, v=newinf.f)
       ## 7Jun13: Add attributes for "art.status"
       nw <- set.vertex.attribute(nw,'art.status',
                                  0, v=newinf.f)

       ## Transmission from female to male

       discordant.fpos <- status.el[, 2] == 1 & status.el[, 1] == 0
       transmittable.f <- nw.el[discordant.fpos, 2]
       infectible.m    <- nw.el[discordant.fpos, 1]
       ## transmittable.m.inf.time.bp <- time-inf.time[transmittable.m]


       ## Modify infectivity on account of pregnant female partner

       ## if (pregnancy.status[transmittable.f] == 1){ ## REWRITE
       ##   infectivity.today[transmittable.f] <- 
       ##     infectivity.today[infectible.m] * preg.mult
       ## }

       ## Modify infectivity on account of circumcised, uninfected male partner
       ## if (circum.status[infectible.m] == 1){ ## REWRITE
       ##   infectivity.today[transmissible.f] <- 
       ##     infectivity.today[transmissible.f] * circum.mult
       ## }

       ## transmittable.m.beta <-  beta.per.month[transmittable.m.inf.time.bp]
       ## transmissions.m <- rbinom(length(transmittable.m.beta), 1, transmittable.m.beta)

       transmissions.f <- rbinom(length(transmittable.f), 1,
                                 infectivity.today[transmittable.f])


       newinf.m <- infectible.m[transmissions.f == 1]
       nw <- set.vertex.attribute(nw, 'inf.status', 1, v=newinf.m) 
       nw <- set.vertex.attribute(nw, 'inf.time', time, v=newinf.m)
       nw <- set.vertex.attribute(nw, 'time.since.infection', 0,
                                  v=newinf.m)
       nw <- set.vertex.attribute(nw, 'art.status', 0, v=newinf.m)
       # corrected vertices on 7Jun13
       
       # calculate incidence
		
       inci[time] <- length(newinf.f) + length(newinf.m)
       inci.f[time] <- length(newinf.f)
       inci.m[time] <- length(newinf.m)

       if (verbose) cat("Transmissions", inci[time],"\n")

       ## write.table(cbind(time, total.new.infections),
       ##              file=incidence_data, ## to note total number of new infections
       ##              append=TRUE,
       ##              col.names=FALSE,
       ##              row.names=FALSE
       ##              )
       
       incidence_data <- paste("trial", ".csv", sep="")
       
       write.table(cbind(time, inci[time]),
                   file=incidence_data, ## to note total number of new infections
                   append=TRUE,
                   col.names=FALSE,
                   row.names=FALSE
                   )
       
       return(nw)
       
     }
