## 13 Aug 2013 --
## a. Add coverage indicator here at time of infection.
## Add "scenario", "baseline.art.coverage.rate" and "baseline.art.preg.rate"
## arguments here. 

## b. Record more things: num. of pregnant women, num. of infected pregnant
## women, number of infected pregnant women on ART, total number on regular ART.

## 11 Aug 2013 -- set up structure to record prevalence and incidence at every time step

## 30 Jul2013: Set up structure to record infector IDs -- "infector.ID"

## 30Jul13: Caught mistake in reduced susceptibility of men due to
## circumcision -- should have been "infectible.m"
## instead i had "infectible.f"

## 10Jul13: Add "art.type" attribute for newly infected.

## 10 June 2013: Implement modification in transmissibility
## on account of circumcision, when susceptible partner is male, 
## and pregnancy, when susceptible partner is female. 

## 7 June 2013: Also modify infectivity due to pregnancy and circumcision.

## one way to do this may be:

## The problem occurs when there are different infectivities across
## different partnerships for the same individual. For example if
## if woman W is in partnerships with circumcised man C1 and uncircumcised
## man C2, her infectivities across the two partnerships will be different.

## Therefore, infectivity cannot simply be an individual-level attribute.

## So to appropriately account for differential infectivities,
## we list all the discordant partnerships where the male is susceptible

## m1-f1, m2-f2, ..., mn-fn.

## We know the infectivity (unadjused for circumcision and pregnancy) for each infected woman above.
##We define this infectivity as the base infectivity for each partnership. We then adjust these infectivities by the circumcision status of the uninfected male partner.
## Next, we adjust partnership-level infectivities according to
## the pregnancy status of the female partner.

## We consider transmission as a bernoulli event across each partnership.
## The probability of transmission in each of these partnerships is then
## partnership specific.

## Finally, in partnerships where transmission is recorded,
## we will change the infection status of the men to infected.

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
  function(nw, verbose,
           preg.mult,
           circum.mult,
           scenario, # 13Aug13: Add these arguments
           baseline.art.coverage.rate,
           baseline.preg.coverage.rate,
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
       curr.pregnancy.status <- nw %v% "curr.pregnancy.status" # ASK
       art.status <- nw %v% "art.status" # ASK
       infectivity.today <- nw %v% "infectivity.today" # ASK

       ## Transmission from male to female
       ## browser()
       
       ## discordant.mpos <- status.el[ ,1]==1 & status.el[ ,2]==0
       discordant.mpos <- intersect(which(status.el[, 1] == 1),
                                    which(status.el[, 2] == 0))
       ## i thought steve's commented "discordant.mpos" had confusing 
       ## output -- instead of giving a list of
       ## rownumbers, it gave a list with True and false -- but that is okay.

       transmittable.m <- nw.el[discordant.mpos, 1]
       infectible.f <- nw.el[discordant.mpos, 2]

       ## 10 June 2013: Incorporate effect of susceptible ``infectible''
       ## pregnant women -- infectivities across these partnerships will be
       ## greater
       
       ## browser()
       
       curr.pregnant <- which(curr.pregnancy.status == 1)

       ## The 3 commented lines below can probably go

       ## preg.infectible.f.id <- which(curr.pregnant %in% infectible.f)
       ## preg.infectible.f <- curr.pregnant[preg.infectible.f.id]
       ## preg.infectible.f.in.rel <- intersect(preg.infectible.f, nw.el[,2])

       infectible.preg <- which(infectible.f %in% curr.pregnant)
                    ## which susceptible women are pregnant
                    ## initially vector called "b"

       infectivity.transmittable.m <- infectivity.today[transmittable.m]
                    ## what are the infectivities of their male partners?
       
       if (length(infectible.preg) > 0){
       infectivity.transmittable.m[infectible.preg] <-
         infectivity.transmittable.m[infectible.preg]*preg.mult
       }           ## for men with susceptible male partners, modify infectivity

       transmit.prob.infectible.f <- runif(length(infectible.f))
                   ## probabilities for transmission in male-positive partnerships
       
       ## transmissions.m <- rbinom(length(transmittable.m), 1,
       ##                           infectivity.today[transmittable.m])

       transmissions.mtof.id <- which(infectivity.transmittable.m >=
                                      transmit.prob.infectible.f)
       ## browser()
       newinf.f <- infectible.f[transmissions.mtof.id]
       ## 30 Jul 2013: Record infector IDs
       newinf.f.infectorID <- transmittable.m[transmissions.mtof.id]
       
       ## newinf.f <- infectible.f[transmissions.m == 1]
       nw <- set.vertex.attribute(nw,'inf.status', 1, v=newinf.f)
       nw <- set.vertex.attribute(nw,'inf.time', time, v=newinf.f)
       nw <- set.vertex.attribute(nw,'time.since.infection',
                                  0, v=newinf.f)
       ## 7Jun13: Add attributes for "art.status"
       nw <- set.vertex.attribute(nw,'art.status',
                                  0, v=newinf.f)
       nw <- set.vertex.attribute(nw,'art.type',
                                  NA, v=newinf.f) #10Jul13

       nw <- set.vertex.attribute(nw,'infector.ID',
                                  newinf.f.infectorID, v=newinf.f) #30Jul13

       ## Transmission from female to male
       
       ## discordant.fpos <- status.el[, 2] == 1 & status.el[, 1] == 0
       discordant.fpos <- intersect(which(status.el[, 2] == 1),
                                    which(status.el[, 1] == 0)
                                    )
       ## i thought steve's commented "discordant.fpos" had confusing 
       ## output -- instead of giving a list of
       ## rownumbers, it gave a list with True and false -- but that is okay.
       transmittable.f <- nw.el[discordant.fpos, 2]
       infectible.m    <- nw.el[discordant.fpos, 1]

       ## Modify infectivity on account of circumcised susceptible male partner
       ## browser()
       circumcised <- which(circum.status == 1)
       infectible.circumcised <- which(infectible.m %in% circumcised)
                    ## 30Jul13: Caught mistake here -- should be "infectible.m"
                    ## instead i had "infectible.f"
                    ## which susceptible men are circumcised
       infectivity.transmittable.f <- infectivity.today[transmittable.f]
                    ## what are the infectivities of their female partners?
       if (length(infectible.circumcised > 0)){
       infectivity.transmittable.f[infectible.circumcised] <-
         infectivity.transmittable.f[infectible.circumcised]*circum.mult
       }           ## for men with susceptible male partners, modify infectivity

       transmit.prob.infectible.m <- runif(length(infectible.m))
                   ## Unif(0,1) random numbers
                   ## for transmission in female-positive partnerships
       
       ## transmissions.m <- rbinom(length(transmittable.m), 1,
       ##                           infectivity.today[transmittable.m])

       transmissions.ftom.id <- which(infectivity.transmittable.f >=
                                      transmit.prob.infectible.m)
       newinf.m <- infectible.m[transmissions.ftom.id]
       ## 30Jul13: Record infector IDs
       newinf.m.infectorID <- transmittable.f[transmissions.ftom.id]

       nw <- set.vertex.attribute(nw, 'inf.status', 1, v=newinf.m) 
       nw <- set.vertex.attribute(nw, 'inf.time', time, v=newinf.m)
       nw <- set.vertex.attribute(nw, 'time.since.infection', 0,
                                  v=newinf.m)
       nw <- set.vertex.attribute(nw, 'art.status', 0, v=newinf.m)
       nw <- set.vertex.attribute(nw, 'art.type', NA, v=newinf.m) #10Jul13

       nw <- set.vertex.attribute(nw, 'infector.ID', newinf.m.infectorID,
                                  v=newinf.m) #30Jul13
       # corrected vertices on 7Jun13
       
       # calculate incidence
		
       inci[time] <- length(newinf.f) + length(newinf.m)
       inci.f[time] <- length(newinf.f)
       inci.m[time] <- length(newinf.m)


       ########################################################
       ### 13Aug13: Add code to assign ART coverage indicator
       ########################################################

       newinf <- c(newinf.m, newinf.f)
       
       if (scenario == "baseline"){

         art.covered <- rbinom(length(newinf), 1,
                               baseline.art.coverage.rate)

         preg.covered <- rbinom(length(newinf.f), 1,
                                   baseline.preg.coverage.rate)
         
         nw <- set.vertex.attribute(nw, "art.covered", art.covered,
                                    v=newinf)
         
         ## nw <- set.vertex.attribute(nw, "preg.covered", NA,
         ##                            new.male.nodes)
         
         nw <- set.vertex.attribute(nw, "preg.covered", preg.covered,
                                    v=newinf.f)

       }


       ########################################################

       ## browser()
       
       if (verbose) cat("Transmissions", inci[time],"\n")

       ## 11 Aug 2013: added lines below to record information at every        

       ## write.table(cbind(time, total.new.infections),
       ##              file=incidence_data, ## to note total number of new infections
       ##              append=TRUE,
       ##              col.names=FALSE,
       ##              row.names=FALSE
       ##              )
       
       ## incidence_data <- paste("trial", ".csv", sep="")
       
       ## write.table(cbind(time, inci[time]),
       ##             file=incidence_data, ## to note total number of new infections
       ##             append=TRUE,
       ##             col.names=FALSE,
       ##             row.names=FALSE
       ##             )


       ########################################################
       ### 13Aug13: Recording information (started on 11Aug13)
       
       male.id <- nwmodes(nw, 1)
       female.id <- nwmodes(nw, 2)

       n.male <- length(male.id)       
       n.female <- length(female.id)
       
       n.inf <- length(which(nw%v%"inf.status" == 1))
       n.inf.on.art <- length(intersect(which(nw%v%"inf.status" == 1),
                                        which(nw%v%"art.status" == 1))
                              )
                                        
       prevalence <- length(which(nw%v%"inf.status" == 1))/network.size(nw)
       prev.m <- length(intersect(which(nw%v%"inf.status" == 1),
                                  male.id))/length(male.id)
       prev.f <- length(intersect(which(nw%v%"inf.status" == 1),
                                  female.id))/length(female.id)

       n.preg <- length(which(nw%v%"curr.pregnancy.status" == 1))
       n.preg.inf <- length(intersect(which(nw%v%"curr.pregnancy.status" == 1),
                                         which(nw%v%"inf.status" == 1)))
       n.preg.on.art <- length(intersect(which(nw%v%"curr.pregnancy.status" == 1),
                                         which(nw%v%"art.status" == 1)))


       data <- paste(date, ".prev.inc.data", ".csv", sep="")

       ## browser()
       write.table(cbind(time,
                         network.size(nw),
                         network.edgecount(nw),
                         n.male, n.female,
                         inci[time],
                         n.inf,
                         prevalence,
                         prev.m, prev.f,
                         ##n.inf,
                         n.inf.on.art,
                         n.preg,
                         n.preg.inf,
                         n.preg.on.art),
                   file=data, ## to note total number of new infections
                   append=TRUE,
                   col.names=FALSE,
                   row.names=FALSE
                   )

       
       return(nw)
       
     }
