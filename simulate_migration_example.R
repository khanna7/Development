#####################################################
### Progression of versions
#####################################################

## Goal: put all pieces together for one simulation file
## control file -- just modify the mig_step --
## all output is sent to an external csv file

## 24 May 2012: Mig-Step 12

## 20 May 2012: Modified mig-step to 3 to have multiple runs at the
## same value

## 20 April 2012: Implemented change so only one cumulative network
## object is saved, and cross-sectional network objects are obtained from that

## 18 April 2012: investigate saving

## 2 March 2012: organized for final runs

## 30 Jan 2012: After discussion with Steve:
## a. reduced late stage to 9 months (40 wks) from 2 years.
## consequently, initial distribution of times of infection
## had to be changed to uniform between (0, 12+500+40)
## b. added functionality to store networks in real-time.
## c. output incidence data 

## 27 Jan 2012: simulate using new term in estimation to account
## for desired partnership structure

## 22 Jan 2012: Need to run simulations longer than 3000
## steps to get results

## 20 Jan 2012: Re-run simulations with 'time.infected' distributed
## between 0 and 616

## 17 Jan 2012: Fixed 'time.infected' attribute, was
## not getting updated initially. This change slows
## down infection transmission because infected people
## enter the long phase of chronic infection during which
## infection passes forward very slowly. Changed to 50
## infected women in urban and rural regions eachb to have
## enough sero-discordant ties at the start.

## 13 Jan 2012: After discuossion with Steve,
## time.infected.fatal has been re-introduced.
## An infected individual dies 616 weeks after infection.
### do runs at intervals of increasing 3 weekly intervals
### 1 -- 30 weeks

## 3 Jan 2012: After conversation with Steve decided to not
## have a time.infected.fatal at all, instead for late-stage
## folks, there will be a reduced life expectancy.

## "inf.prob.death" re-labeled as "late.prob.death"

## 2 Jan 2012: added inf.prob.death to model reduced life-expectancy
## due to disease. had not coded this before. 

## 1 Jan 2012: pop.rate.birth=625*8/(45*52) seems to work
## to produce a constant population in the absence of disease.
## experiment to see what happens particularly to total population
## size when we have disease in the population:
## infected: 1 rural + 1 urban female

## transmission_d6a.R contains the unvectorized transmission code
## which works

#####################################################

#####################################################
### Top-Matter
#####################################################
rm(list=ls())
library(ergm)
load(file="est_dur100_fixinterval_newterm_meandeg12.RData")

source("../MSM.common.functions.R")
source("../networkDynamic.access.R")
source("../networkDynamic.simulation.R")
source("../networkDynamic.cross.section.R")
source("../networkDynamic.zzz.R")

source("../vital_dynamics_d7.R") # accounts for new k-star term
##source("../migration_wrapper_d1.R")
source("../transmission_6h_detmigrations.R") #deterministic migrations
source("../update_network_d5.R")
#####################################################

#####################################################
### Migration-Step (Most Important Variable)
#####################################################
mig_step <- 3
runno <- 1
#####################################################''

#####################################################
### Network-Object
#####################################################
net <- migration.fit.network$network
#####################################################''
 
#####################################################
### Demography
#####################################################

pop.rate.of.birth <- 625*8/(45*52) 
birth.type.prob <- c(1/8,1/4, 1/8, # type male: R, M, U
                     1/4,1/4) # type female: R, M, U
time.infected.fatal <- 12+500+40 # 31 Jan '12: after discussion with steve
std.prob.death <- 1/(45*52) # 45-year life expectancy
late.prob.death <- 1/(2*52) # 2 Jan 2012: had not added before 
                                        # though this is not used anymore because we have a time.infected.fatal
## for migration and disease transmission

#####################################################
### Biology
#####################################################

chronic.prob=0.0007
acute.prob=chronic.prob*26
late.prob=chronic.prob*7 # multiplied by 2.5 coital acts per week in
                                        # transmission file
acute.chronic.transition=12 # check this from ode write-up
chronic.late.transition=500 # should be 500+12
late.stage.begin=acute.chronic.transition+chronic.late.transition
                                        # no need to have
                                        # this separately. should be the
                                        #same as chronic.late.transition

#####################################################
### Other Simulation-Related Parameters
#####################################################
burnin.sim.network <- 25e3

ntimesteps <- 5e3
net <- network.crosssection(net, max(net%v%"active")-1)
popsize <- network.size(net)

net %v% "vertex.names" <- 1:popsize
net <- activate.vertices(net, onset=0, terminus=1,
                           v=1:popsize)
net <- activate.edges(net, onset=0, terminus=1,
                      e=1:network.edgecount(net))
activenet <- network.crosssection(net, 0)

#####################################################

#####################################################
### Organize Output
#####################################################
condition <- "meandeg12_dur100_"

filename <- paste(condition, "mig-step",
                 mig_step, "runno", runno, ".csv", sep="") 

saveobject <- paste(condition, "mig_step_",
                    "runno", runno, 
                    mig_step,".RData", sep="")

real_time_cumnet <- paste("real_time_cumnets", saveobject,
                          sep=""
                          )

cum.nets <- list() # object to store cumulative network
#####################################################


#####################################################
### Time-Loop
#####################################################

for (timestep in 1:ntimesteps){

   set.seed(Sys.time()) # RNG
  
   curr.time <- timestep
   prev.time <- curr.time-1

   net <- mig.vital.dynamics(net=net,
                            curr.time=curr.time,
                            pop.rate.of.birth=pop.rate.of.birth,
                            birth.type.prob=birth.type.prob,
                            time.infected.fatal=time.infected.fatal,
                            std.prob.death=std.prob.death,
                            late.prob.death=late.prob.death,
                            late.stage.begin=late.stage.begin,
                            filename=filename
                            )


   
  cat("total number of nodes is ", network.size(net),",\n",
      "total number of edges is ", network.edgecount(net), ",\n", 
      "at time-step " , curr.time,  ".\n")

  activenet <- network.crosssection(net, curr.time)
  
  net <- update.network(net=net,
                        curr.time=curr.time,
                        theta.network=theta.network,
                        burnin.sim.network=burnin.sim.network, # put in run file
                        diss.network=diss.network,
                        gamma.network=gamma.network,
                        formula.network.w.offset=formula.network.w.offset,
                        constraints.network=constraints.network,
                        activenet=activenet,
                        dyninterval=dyninterval
  ##                  ## steve also has steady.model.change
  ##                  ## but that is not relevant here
                        )

   ## save cumulative network object
   cum.nets <- net
   save(cum.nets, file=real_time_cumnet)
   
  active.net.updated <- network.crosssection(net, curr.time)
  cat("Number of alive edges is ", length(active.net.updated$mel), ".\n")

  net <- transmission(net=net,
                      mig_step=mig_step,
                      acute.prob=acute.prob,
                      chronic.prob=chronic.prob,
                      late.prob=late.prob,
                      acute.chronic.transition=acute.chronic.transition,
                      chronic.late.transition=chronic.late.transition,
                      curr.time=curr.time,
                      filename=filename
                      )

      cat("Total Number of Infected, after transmissions at time-step",
       curr.time,
       "is",
       length(which(net%v%"inf.status"==1)),
      ".\n", "\n")


}
#####################################################

#####################################################
### Final Object
#####################################################
save.image(saveobject)
#####################################################
