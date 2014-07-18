###########################################################
### Biological Functions
###########################################################

## 15 Jan 2014: Put in Fielding mortality rates.

## 14 Jan 2014:
## a. We need to partition mortality of ART-naitve by CD4 counts.
## b. also need to adjust mortality of infectives by size of timestep.

## 13 Jan 2014: comment out the stopifnot statements --
## stopifnot(all(asmr.male.out <= 1)) and stopifnot(all(asmr.female.out <= 1)) --
## in "assign.mortality.male" and "assign.mortality.female"
## functions respectively. These stopifnot statements do not caused problems
## in long burnins, or simulations for baseline or PMTCT B+. The first time
## noticed them at all was in Option B simulations for South Africa

## 29 Sep 2013: Add function for age categories

## 2 Sep 2013: Add intersect3 function

## 30 AUg 13: Add cleanMem function

## 30Jul13: Added Lawn's estimate for post-ART individuals with really low
## CD4 counts (<=50 cells/mm3)

## 30 Jul 2013: Moved "assign.infectivity" to its own file.

## 11 Jul 2013: Remove all the CD4 and viral load computations from here.
## Those have separate files now, and removing this will ensure that
## we don't have two versions of the same function. For version histories
## of these functions, see "common.functions_d7.R."

## 8Jul2013: Add "art.status" argument to "assign.mortality.male" and
## "assign.mortality.female."

## 7Jul2013: Changed name of "time.infection.to.peak.viremia"
## to "time.infection.to.peak.viral.load."

## 6 July 2013: Rewrite "assign.asmr.male" and "assign.asmr.female" functions
## to implement age-based mortality for HIV-negatives and CD4 based mortality
## for infecteds

## 5 July 2013: Implemented viral load with with 4 trajectories:
## treatment-naive, regular ART, sc ART for pregnant women (ongoing),
## sc ART for pregnant women (post-cessation).

## 11 June 13: Describe effect on scART for pregnant women
## Can write separate function(s) for
## increase in viral load and decrease in CD4 count
## after cessation of sc ART (Option A).

## 10 June 2013: Need to describe impact of ART treatment type "2" --
## Option A for pregnant women with CD4 < 350,
## for its impact on CD4 count and viral load.
## The treatment status of such women can be described as "3" when they are off
## treatment. (art.status 1 is regular treatment,
## 2 is Option A before delivery for CD4<350,
## 3 is when Option A treatment is terminated.)

## 10 June 2013: Account for change in infectivity of HIV-positive
## women on account of pregnancy. This is a nodal level attribute of pregnancy.

## 7 June 2013: min.chronic.infectivity.unadj: "unadj" refers to
## number of sex acts
## and HSV prevalence estimates are variable, we are currently in the high end,
## but to reduce overall transmissions, reducing HSC prevalence might
## be possible too.

## 4 June 2013: Try test-based development, removing the networks from being inserted as
## an argument inside of main functions

## 29 May: Rewrite "compute.viral load" so that at the end of each time step,
## the viral load is the average of the viral loads at the two endpoints,
## rather than the value of the right endpoint.

## 29 May 2013: Rewrite "compute.cd4.counts" function

## 28 May 2013: Adjust cd4 count and viral load for size of timestep.

## 27 May 2013: Adjust "infectivity.today" to account for size of timestep.
## and add "num.sex.acts.per.timestep" argument

## 24 May 2013: To fix "assign.infectivity" function.

### Classify infection  state

classify.stage <- function(x){
       if (x %in% acute.length){
         stage.of.infection <- "Acute"
       } else  if (x %in% chronic.length){
         stage.of.infection <- "Chronic"
       } else  if (x %in% late.length){
         stage.of.infection <- "Late"
       }
       return(stage.of.infection)
     }

classify.stage.numeric <- function(x){
  if (x %in% acute.length){
    stage.of.infection <- 0
  } else  if (x %in% chronic.length){
    stage.of.infection <- 1
  } else  if (x %in% late.length){
    stage.of.infection <- 2
  }
  return(stage.of.infection)
}

classify.stage.numeric.rewrite <- function(time.since.infection){
  ## less preferable, since it 
  ## takes the entire vector "time.since.infection" as input --
  ## but should be usable with apply, too

  stage.of.infection <- rep(NA, length(time.since.infection))

  for (i in 1:length(time.since.infection)){

    if (time.since.infection[i] %in% acute.length){
      stage.of.infection[i] <- 0

    } else if (time.since.infection[i] %in% chronic.length){
      stage.of.infection[i] <- 1

    } else if (time.since.infection[i] %in% late.length){
      stage.of.infection[i] <- 2

    }

  }
  return(stage.of.infection)
}



###########################################################
### Network Related Functions
###########################################################

    nwmodes <- function(nw, mode) {
      if (mode == 1) out <- 1:(nw %n% 'bipartite')
      if (mode == 2) out <- ((nw %n% 'bipartite')+1):(nw %n% 'n')
      return(out)
    }


###########################################################
### Demographic Functions
###########################################################

    assign.mortality.male <- function(male.curr.age, asmr.male,
                                      male.inf.status,
                                      male.cd4.today,
                                      male.art.status,
                                      size.of.timestep #14Jan14
                                      ){

      ## 6Jul13: implement age-based mortality for negatives,
      ## and cd4 based mortality for positives
      
      ## male.curr.age is a vector of all men in the population 
      ## asmr.male is a vector of non-AIDS related mortality in the population
      ## asmr.male is a vector of 8 individuals

      
      asmr.male.out <- male.curr.age

      for (i in 1:length(male.curr.age)){

        if (male.inf.status[i] == 0){
          if (male.curr.age[i] >= 15 && male.curr.age[i] < 20){
            asmr.male.out[i] <- asmr.male[1] #14Jan14: these are already adjusted
                                             #for"size.of.timestep"
          } else if (male.curr.age[i] >= 20 && male.curr.age[i] < 25){
            asmr.male.out[i] <- asmr.male[2]
          } else if (male.curr.age[i] >= 25 && male.curr.age[i] < 30){
            asmr.male.out[i] <- asmr.male[3]
          } else if (male.curr.age[i] >= 30 && male.curr.age[i] < 35){
            asmr.male.out[i] <- asmr.male[4]
          } else if (male.curr.age[i] >= 35 && male.curr.age[i] < 40){
            asmr.male.out[i] <- asmr.male[5] 
          } else if (male.curr.age[i] >= 40 && male.curr.age[i] < 45){
            asmr.male.out[i] <- asmr.male[6] 
          } else if (male.curr.age[i] >= 45 && male.curr.age[i] < 50){
            asmr.male.out[i] <- asmr.male[7] 
          } else if (male.curr.age[i] >= 50 && male.curr.age[i] < 55){
            asmr.male.out[i] <- asmr.male[8]
          }  else if (male.curr.age[i] >= 55){
            asmr.male.out[i] <- 1
          }
        } else if (male.inf.status[i] == 1){
             if (is.na(male.art.status[i]) ||
                 male.art.status[i] == 0){
               ## asmr.male.out[i] <- 38.6/(100*365)

               if (male.cd4.today[i] %/% 50 == 0){ #quotient operator is needed
                 asmr.male.out[i] <- 43.2*size.of.timestep/(100*365) #14Jan14: Fielding, adjust for size.of.timestep
               } else if (male.cd4.today[i] %/% 50 == 1){
                 asmr.male.out[i] <- 11.4*size.of.timestep/(100*365) 
               } else if (male.cd4.today[i] %/% 100 == 1){
                 asmr.male.out[i] <- 11.4*size.of.timestep/(100*365) 
               } else if ((male.cd4.today[i] >= 200) && (male.cd4.today[i] < 350)){
                 asmr.male.out[i] <- 3.3*size.of.timestep/(100*365)
               } else if ((male.cd4.today[i] >= 350)){
                 asmr.male.out[i] <- 0.6*size.of.timestep/(100*365)
               }
             } else if (!is.na(male.art.status[i]) &&
                        male.art.status[i] > 0){
               if (male.cd4.today[i] %/% 50 == 0){ #quotient operator is needed
                 asmr.male.out[i] <- 38.6*size.of.timestep/(100*365) #30Jul13: Lawn's, adjust for size.of.timestep
               } else if (male.cd4.today[i] %/% 50 == 1){
                 asmr.male.out[i] <- 12.8*size.of.timestep/(100*365) #30Jul13: Lawn's, adjust for size.of.timestep
               } else if (male.cd4.today[i] %/% 100 == 1 ){
                 asmr.male.out[i] <- 5.4*size.of.timestep/(100*365)
               } else if (male.cd4.today[i] %/% 100 == 2 ){
                 asmr.male.out[i] <- 2.7*size.of.timestep/(100*365)
               } else if (male.cd4.today[i] %/% 100 > 2 ){
                 asmr.male.out[i] <- 2.0*size.of.timestep/(100*365)
               }
             }
           }
      }
      ## stopifnot(all(asmr.male.out <= 1)) #13Jan14
      return(asmr.male.out)
    }

      assign.mortality.female <- function(female.curr.age, asmr.female,
                                          female.inf.status,
                                          female.cd4.today,
                                          female.art.status,
                                          size.of.timestep){

        ## 6Jul13: implement age-based mortality for negatives,
        ## and cd4 based mortality for positives
        
        ## female.curr.age is a vector of all men in the population 
        ## asmr.female is a vector of non-AIDS related mortality in the population
        ## asmr.female is a vector of 8 individuals
      
        asmr.female.out <- female.curr.age
        
        for (i in 1:length(female.curr.age)){
          if (female.inf.status[i] == 0){
            if (female.curr.age[i] >= 15 && female.curr.age[i] < 20){
              asmr.female.out[i] <- asmr.female[1] #14Jan14: these are already adjusted
                                                   #for"size.of.timestep"
            } else if (female.curr.age[i] >= 20 && female.curr.age[i] < 25){
              asmr.female.out[i] <- asmr.female[2]
            } else if (female.curr.age[i] >= 25 && female.curr.age[i] < 30){
              asmr.female.out[i] <- asmr.female[3]
            } else if (female.curr.age[i] >= 30 && female.curr.age[i] < 35){
              asmr.female.out[i] <- asmr.female[4]
            } else if (female.curr.age[i] >= 35 && female.curr.age[i] < 40){
              asmr.female.out[i] <- asmr.female[5] 
            } else if (female.curr.age[i] >= 40 && female.curr.age[i] < 45){
              asmr.female.out[i] <- asmr.female[6] 
            } else if (female.curr.age[i] >= 45 && female.curr.age[i] < 50){
              asmr.female.out[i] <- asmr.female[7] 
            } else if (female.curr.age[i] >= 50 && female.curr.age[i] < 55){
              asmr.female.out[i] <- asmr.female[8] 
            } else if (female.curr.age[i] >= 55){
              asmr.female.out[i] <- 1
            }
          } else if (female.inf.status[i] == 1){
             if (is.na(female.art.status[i]) || female.art.status[i] == 0){

               ## asmr.female.out[i] <- 38.6/(100*365)
               if (female.cd4.today[i] %/% 50 == 0){ #quotient operator is needed
                 asmr.female.out[i] <- 43.2*size.of.timestep/(100*365) #14Jan14: Fielding, adjust for size.of.timestep
               } else if (female.cd4.today[i] %/% 50 == 1){
                 asmr.female.out[i] <- 11.4*size.of.timestep/(100*365) 
               } else if (female.cd4.today[i] %/% 100 == 1){
                 asmr.female.out[i] <- 11.4*size.of.timestep/(100*365) 
               } else if ((female.cd4.today[i] >= 200) && (female.cd4.today[i] < 350)){
                 asmr.female.out[i] <- 3.3*size.of.timestep/(100*365)
               } else if ((female.cd4.today[i] >= 350)){
                 asmr.female.out[i] <- 0.6*size.of.timestep/(100*365)
               }
             } else if (!is.na(female.art.status[i]) && female.art.status[i] > 0){
               if (female.cd4.today[i] %/% 50 == 1){ ## need quotient operator
                 asmr.female.out[i] <- 38.6*size.of.timestep/(100*365) #Lawn, adjust for size.of.timestep
               } else if (female.cd4.today[i] %/% 50 == 1){ ## need quotient operator
                 asmr.female.out[i] <- 12.8*size.of.timestep/(100*365)
               } else if (female.cd4.today[i] %/% 100 == 1 ){
                 asmr.female.out[i] <- 5.4*size.of.timestep/(100*365)
               } else if (female.cd4.today[i] %/% 100 == 2 ){
                 asmr.female.out[i] <- 2.7*size.of.timestep/(100*365)
               } else if (female.cd4.today[i] %/% 100 > 2 ){
                 asmr.female.out[i] <- 2.0*size.of.timestep/(100*365)
               }
             }
           }
        }
        ## cat(which(asmr.female.out > 1))
        ## cat(asmr.female.out[asmr.female.out > 1])
        ## stopifnot(all(asmr.female.out <= 1))#13Jan14
        return(asmr.female.out)
      }

fac.fn <- function(num){
  if  (num == 1) {
    return (num)
  }
  else {
    return(num*fac.fn(num-1))
  }
}

arith.series <- function(num){
  if  (num<=0) {
    return (num)
  }
  else {
    return(num+arith.series(num-1))
  }
}

cleanMem <- function(n=10) { for (i in 1:n) gc() } #30 Aug 2013

intersect3 <- function(x, y, z){
  out <- intersect(x, intersect(y,z))
  return(out)
}

## 29 Sep 2013: Add "age.cat" function

assign.age.cat <- function(age, cutoffs = c(cutoff1, cutoff2, cutoff3, cutoff4)){

  age.cat <- rep(NA, length(age))

  for (i in 1:length(age)){
  if (age[i] <= cutoffs[1]){
       age.cat[i] <- 1
     } else if (age[i] > cutoffs[1] & age[i] <= cutoffs[2]){
       age.cat[i] <- 2
     } else if (age[i] > cutoffs[2] & age[i] <= cutoffs[3]){
       age.cat[i] <- 3
     } else if (age[i] > cutoffs[3] & age[i] <= cutoffs[4]){
       age.cat[i] <- 4
     }
}
return(age.cat)
}
