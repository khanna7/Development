## Sep 2014: Spelling error LHS said cd4.count.toda in
## cd4.count.today <- nw%v%"cd4.count.today"

## 14 Feb 2014: I was missing condidtional 
## || scenario == "option.b" -- for ART coverage probabilities.
## Therefore, coverage status was not being assigned for Option B.
## We don't need that conditional at all. Take it out.

## 8 Nov 2013: Change name of data "dur.inf.by.age" to "given.dur.inf.by.age"
## to avoid confusion with the attribute which appears later

## 8 Nov 13: Record "time.of.infection"

## 30 Oct 2013: Record exposure characteristics
   ## n0%v%"infector.sex"
   ## n0%v%"infector.age"
   ## n0%v%"infector.inf.status"
   ## n0%v%"infector.cd4.count"
   ## n0%v%"infector.viral.load"
   ## n0%v%"infector.preg.status"
   ## n0%v%"infector.circum.status"
   ## n0%v%"infector.time.since.infection"
   ## n0%v%"infector.art.status"
   ## n0%v%"infector.art.type"

## Remove "inf.time" -- we are already recording "time.of.infection"

## 28 Oct 2013:
## New things to be added throughout the code
## (From list below,
## e alreaded implemented on 23 Oct 2013.
## Implement d -- for everyone, and then for pregnant women)

## a. number of pregnancies
## b. CD4 count when ART is supposed to be initiated for pregnant women
## c. ART status at termination of pregnancy
## d. In summary data, record number of eligible people at every time step.
## e. Differentiate pregnancy multiplier for susceptible women -- 1.7
## implemented in version "transmission_d10.R"

## 23 Oct 2013:

## a. Created from version "transmission_d9.R."

## b. The pregnancy multiplier should be "preg.susc.mult" which is slightly
## different than multiplier for pregnant women who are infected. The latter is
## 2.54 and the former is 1.7.

## 15 Oct 2013: Add * || scenario == "pmtct.b+" * to scenario statement

## ## 23 Aug 2013: Also record "age.at.infection" and "time.of.infection" (I thought
## ## the latter was already being recorded)

## ## 22 Aug 2013: Add age-based life expectancy at time of infection

## ## 15 Aug 2013:
## ## a. Extract networks at current time step to record information.
## ## That seems to be necessary to get accurate counts of individuals with various
## ## attributes at any time step.

## ## b. Add output of number of total number of actors (alive and dead)
## ## and total number of edges (alive and dead) at the end.

## ## 13 Aug 2013 --
## ## a. Add coverage indicator here at time of infection.
## ## Add "scenario", "baseline.art.coverage.rate" and "baseline.art.preg.rate"
## ## arguments here. 

## ## b. Record more things: num. of pregnant women, num. of infected pregnant
## ## women, number of infected pregnant women on ART, total number on regular ART.

## ## 11 Aug 2013 -- set up structure to record prevalence and incidence at every time step

## ## 30 Jul2013: Set up structure to record infector IDs -- "infector.ID"

## ## 30Jul13: Caught mistake in reduced susceptibility of men due to
## ## circumcision -- should have been "infectible.m"
## ## instead i had "infectible.f"

## ## 10Jul13: Add "art.type" attribute for newly infected.

## ## 10 June 2013: Implement modification in transmissibility
## ## on account of circumcision, when susceptible partner is male, 
## ## and pregnancy, when susceptible partner is female. 

## ## 7 June 2013: Also modify infectivity due to pregnancy and circumcision.

## ## one way to do this may be:

## ## The problem occurs when there are different infectivities across
## ## different partnerships for the same individual. For example if
## ## if woman W is in partnerships with circumcised man C1 and uncircumcised
## ## man C2, her infectivities across the two partnerships will be different.

## ## Therefore, infectivity cannot simply be an individual-level attribute.

## ## So to appropriately account for differential infectivities,
## ## we list all the discordant partnerships where the male is susceptible

## ## m1-f1, m2-f2, ..., mn-fn.

## ## We know the infectivity (unadjused for circumcision and pregnancy) for each infected woman above.
## ##We define this infectivity as the base infectivity for each partnership. We then adjust these infectivities by the circumcision status of the uninfected male partner.
## ## Next, we adjust partnership-level infectivities according to
## ## the pregnancy status of the female partner.

## ## We consider transmission as a bernoulli event across each partnership.
## ## The probability of transmission in each of these partnerships is then
## ## partnership specific.

## ## Finally, in partnerships where transmission is recorded,
## ## we will change the infection status of the men to infected.

## ## 7 June 2013: For new infectives, add "art.status" attribute, in addition to
## ## 'inf.status', 'inf.time', and 'time.since.infection' attributes.
## ## Also found error in updating attributes of newly infected males.
## ## The vertex argument said "v=newinf.f", instead of "newinf.f".

## ## 24 May 2013: This code assumes daily sex. Need to assign frequency of unprotected intercourse

## ## 20 May 2013: Differentiate updating in the "full network" and the
## ## cross-sectional network. (This plan put on hold, may not be necessary due to
## ## "retain.all.vertices" argument in "network.extract" function.

## ## Make space to record results in csv file.

## ## 9 April 2013: One idea:
## ## Set up a list of infectivities thats
## ## correspond to the relative serostatuses of the two partners
## ## For concordant partners, this infectivity will be 0.
## ## For discordant partners, this infectivity will be = to
## ## infectivity of infected partner
## ## If male partner is susceptible and circumcised, infectivity reduces.
## ## If female partner is pregnant, infectivity increases (regardless of infection status).

## ## 3 April 2013: Rewrite as a function
## ## Make modifications in infectivity on account of
## ## circumcision (for susceptible men) or pregnancy here

## #####################################################
## ### Model transmission  
## #####################################################

##   ### Extract partnership network

      transmission <-
  function(nw, verbose,
           ## preg.mult, ## 23Oct13: Make this specific to pregnant women who are susc.
           preg.susc.mult,
           circum.mult,
           scenario, # 13Aug13: Add these arguments
           baseline.art.coverage.rate,
           baseline.preg.coverage.rate,
           given.dur.inf.by.age, #22Aug13: Vector of duraton of infection by age-groups
                        # ages 15-24, 25-34, 25-44, 45 and above
           eligible.cd4, #28Oct13: (d)
           baseline.f.ges.visit, #28Oct13: (d) -- for pregnant women
           ...
           ) {

       nw.el <- as.edgelist(network.extract(nw,
                                            at = time,
                                            retain.all.vertices = T)) #8Oct13



       ## 3Jun13: only pulling out active network

       status.el <- matrix((nw %v% "inf.status")[nw.el], ncol = 2)
       inf.time <- nw %v% "inf.time"
       time.since.infection <- nw %v% "time.since.infection" # ASK

       inf.status <- nw %v% "inf.status"
       circum.status <- nw %v% "circum.status" # ASK
       curr.pregnancy.status <- nw %v% "curr.pregnancy.status" # ASK
       art.status <- nw %v% "art.status" # ASK
       infectivity.today <- nw %v% "infectivity.today" # ASK
       age <- nw%v%"age" #22Aug2013
       sex <- nw%v%"sex" ## 30 Oct 2013 -- more attribute inf recorded
       cd4.count.today <- nw%v%"cd4.count.today" ##Spelling error in cd4.count.today <-  ## for understanding detail about infectors
       viral.load.today <- nw%v%"viral.load.today"
       preg.status <- nw%v%"curr.pregnancy.status"
       circum.status <- nw%v%"circum.status"
       time.since.infection <- nw%v%"time.since.infection"
       art.status <- nw%v%"art.status"
       art.type <- nw%v%"art.type"

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
         infectivity.transmittable.m[infectible.preg]*preg.susc.mult
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
       ##nw <- set.vertex.attribute(nw,'inf.time', time, v=newinf.f)
       nw <- set.vertex.attribute(nw,'time.of.infection',
                                  time, v=newinf.f) #8Nov13
       nw <- set.vertex.attribute(nw,'time.since.infection',
                                  0, v=newinf.f)
       ## 7Jun13: Add attributes for "art.status"
       nw <- set.vertex.attribute(nw,'art.status',
                                  0, v=newinf.f)
       nw <- set.vertex.attribute(nw,'art.type',
                                  NA, v=newinf.f) #10Jul13

       
       ## Record infector characteristics for male to female transmissions
       ## 30 Oct 2013

       nw <- set.vertex.attribute(nw, 'infector.ID',
                                  newinf.f.infectorID, v=newinf.f) #30Jul13--always recorded
       nw <- set.vertex.attribute(nw, 'infector.sex',
                                  sex[newinf.f.infectorID],
                                  v=newinf.f) 
       nw <- set.vertex.attribute(nw, 'infector.age',
                                  age[newinf.f.infectorID],
                                  v=newinf.f) 
       nw <- set.vertex.attribute(nw, 'infector.cd4.count.today',
                                  cd4.count.today[newinf.f.infectorID],
                                  v=newinf.f) 
       nw <- set.vertex.attribute(nw, 'infector.viral.load.today',
                                  viral.load.today[newinf.f.infectorID],
                                  v=newinf.f) 
       nw <- set.vertex.attribute(nw, 'infector.preg.status',
                                  preg.status[newinf.f.infectorID],
                                  v=newinf.f) 
       nw <- set.vertex.attribute(nw, 'infector.circum.status',
                                  circum.status[newinf.f.infectorID],
                                  v=newinf.f) 
       nw <- set.vertex.attribute(nw, 'infector.art.status',
                                  art.status[newinf.f.infectorID],
                                  v=newinf.f) 
       nw <- set.vertex.attribute(nw, 'infector.art.type',
                                  art.type[newinf.f.infectorID],
                                  v=newinf.f) 

       #################################################################
       #################################################################
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

       nw <- set.vertex.attribute(nw, 'inf.status', 1, v=newinf.m)# corre. vertices on 7Jun13
       ##nw <- set.vertex.attribute(nw, 'inf.time', time, v=newinf.m)
       nw <- set.vertex.attribute(nw, 'time.of.infection',
                                  time, v=newinf.m) #8Nov13
       nw <- set.vertex.attribute(nw, 'time.since.infection', 0,
                                  v=newinf.m)
       nw <- set.vertex.attribute(nw, 'art.status', 0, v=newinf.m)
       nw <- set.vertex.attribute(nw, 'art.type', NA, v=newinf.m) #10Jul13



       ## Record infector characteristics for female to male transmissions
       ## 30 Oct 2013
       
       nw <- set.vertex.attribute(nw, 'infector.ID', newinf.m.infectorID,
                                  v=newinf.m) #30Jul13 -- always recorded
       
       nw <- set.vertex.attribute(nw, 'infector.sex',
                                  sex[newinf.m.infectorID],
                                  v=newinf.m) 
       nw <- set.vertex.attribute(nw, 'infector.age',
                                  age[newinf.m.infectorID],
                                  v=newinf.m) 
       nw <- set.vertex.attribute(nw, 'infector.cd4.count.today',
                                  cd4.count.today[newinf.m.infectorID],
                                  v=newinf.m) 
       nw <- set.vertex.attribute(nw, 'infector.viral.load.today',
                                  viral.load.today[newinf.m.infectorID],
                                  v=newinf.m) 
       nw <- set.vertex.attribute(nw, 'infector.preg.status',
                                  preg.status[newinf.m.infectorID],
                                  v=newinf.m) 
       nw <- set.vertex.attribute(nw, 'infector.circum.status',
                                  circum.status[newinf.m.infectorID],
                                  v=newinf.m) 
       nw <- set.vertex.attribute(nw, 'infector.art.status',
                                  art.status[newinf.m.infectorID],
                                  v=newinf.m) 
       nw <- set.vertex.attribute(nw, 'infector.art.type',
                                  art.type[newinf.m.infectorID],
                                  v=newinf.m) 

       #################################################################
       #################################################################

       
       # calculate incidence
		
       inci[time] <- length(newinf.f) + length(newinf.m)
       inci.f[time] <- length(newinf.f)
       inci.m[time] <- length(newinf.m)


       ########################################################
       ### 13Aug13: Add code to assign ART coverage indicator
       ########################################################

       newinf <- c(newinf.m, newinf.f)

       ## if (scenario == "baseline" || scenario=="pmtct.b+"){14Feb14: don't need this conditional
                                                             ## or inf the modified form below.
       
       ## if (scenario == "baseline" || scenario=="pmtct.b+" || scenario=="option.b"){
         ## 14Feb14: Adding Option B to conditional above is critical -- but conditional
         ## itself is not needed because coverage probabilities are assigned
         ## regardles of scenario. 

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

       ## } ## 14Feb14: Take out conditional

       ######################################################
       ## 23 Aug 2013: Also add attribute for age at time of infection
       ## 22 Aug 2013: Add age-based duration of infection
       ######################################################
       
       duration.of.inf <- rep(NA, length(newinf))
       ##browser()
       if (length(newinf) > 0){
         for (i in 1:length(newinf)){
           
           if (age[newinf[i]] <= 24){
             duration.of.inf[i] <- given.dur.inf.by.age[1]
           } else if (age[newinf[i]] > 24 && age[newinf[i]] <= 34){
             duration.of.inf[i] <- given.dur.inf.by.age[2]
           } else if (age[newinf[i]] > 34 && age[newinf[i]] <= 44){
             duration.of.inf[i] <- given.dur.inf.by.age[3]
           } else if (age[newinf[i]] > 44){
             duration.of.inf[i] <- given.dur.inf.by.age[4]
           }

         }

       }

       set.vertex.attribute(nw, "dur.inf.by.age", duration.of.inf,
                            v=newinf)

       set.vertex.attribute(nw, "age.at.infection", age[newinf],
                            v=newinf) # 23Aug13:age at time of infection

       set.vertex.attribute(nw, "time.of.infection", time,
                            v=newinf) # 23Aug13: time of infection
           
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
      ### 15Aug13:
      ### a. Extract object "net" at a given timestep to
      ### record size of populations of individuals with a given attribute at
      ### particular time step

      ### b. Add output of total number of actors, and total number of edges.
       
      ### 13Aug13: Recording information (started on 11Aug13)
      ########################################################
       net <- network.extract(nw, at=time) # 15Aug13: extract network at given time

       male.id <- nwmodes(net, 1) # 15Aug13: Convert "nw" HERE AND BELOW to net
       female.id <- nwmodes(net, 2)

       n.male <- length(male.id)       
       n.female <- length(female.id)
       
       n.inf <- length(which(net%v%"inf.status" == 1))
       n.inf.on.art <- length(intersect(which(net%v%"inf.status" == 1),
                                        which(net%v%"art.status" == 1))
                              )
                                        
       prevalence <- length(which(net%v%"inf.status" == 1))/network.size(net)
       prev.m <- length(intersect(which(net%v%"inf.status" == 1),
                                  male.id))/length(male.id)
       prev.f <- length(intersect(which(net%v%"inf.status" == 1),
                                  female.id))/length(female.id)

       n.preg <- length(which(net%v%"curr.pregnancy.status" == 1))
       n.preg.inf <- length(intersect(which(net%v%"curr.pregnancy.status" == 1),
                                         which(net%v%"inf.status" == 1)))
       n.preg.on.art <- length(intersect(which(net%v%"curr.pregnancy.status" == 1),
                                         which(net%v%"art.status" == 1)))



       ## browser()
       not.on.art.today <- which(net%v%"art.status" == 0) #28Oct13
       eligible.today <- which(net%v%"cd4.count.today" <= eligible.cd4)#28Oct13
       eligible.today.not.on.art <- intersect(eligible.today, not.on.art.today)#28Oct13
       num.eligible.today.not.on.art <- length(eligible.today.not.on.art)#28Oct13

       preg <- which(net%v%"curr.pregnancy.status" == 1) #28Oct13
       inf <- which(net%v%"inf.status" == 1) #28Oct13

       past.f.ges.visit <- which(net%v%"time.since.curr.pregnancy" > baseline.f.ges.visit)
       ## 28Oct13: in idealized implementation, baseline.f.ges.visit is set =
        ##                                      idealized.f.ges.visit

       preg.inf <- intersect(preg, inf) #28Oct13
       preg.inf.not.on.art.today <- intersect(preg.inf, not.on.art.today) #28Oct13
       preg.inf.not.on.art.today.eligible <- intersect(preg.inf.not.on.art.today,
                                                   past.f.ges.visit) #28Oct13

       num.preg.inf.not.on.art.today.eligible <- length(preg.inf.not.on.art.today.eligible)
                                        #28Oct13
       

       data <- paste(date, ".prev.inc.data", ".csv", sep="")

       

       ## browser()
       write.table(cbind(time,
                         network.size(net),
                         network.edgecount(net),
                         n.male, n.female,
                         inci[time],
                         n.inf,
                         prevalence,
                         prev.m, prev.f,
                         ##n.inf,
                         n.inf.on.art,
                         n.preg,
                         n.preg.inf,
                         n.preg.on.art,
                         num.eligible.today.not.on.art, #28Oct13
                         num.preg.inf.not.on.art.today.eligible, #28Oct13
                         network.size(nw),
                         network.edgecount(nw)), # total number of edges
                   file=data, ## to note total number of new infections
                   append=TRUE,
                   col.names=FALSE,
                   row.names=FALSE
                   )

       
       return(nw)
       
     }
