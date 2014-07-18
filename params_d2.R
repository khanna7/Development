## 16 Nov 2013: need to make change to burnin and intervention simulation,
## make sure "assign.infectivity" function has argument "preg.mult" and
## "transmission" function has argument "preg.susc.mult."

## 8 Nov 2013: Change name of data "dur.inf.by.age" to "given.dur.inf.by.age"
## to avoid confusion with the attribute which appears later

## 7 Nov 2013: Change min.age to 18.

## 28 Oct 2013: Add idealized intervention parameters here, so they
## don't have to be called separately. 

## 23 Oct 2013:
## a. add the pregnancy parameters: "full.term", "optA.vl.reduction" and
## "sc.art.postcess.rest.bl" here
## b. Add "preg.susc.mult" to differentiate multiplier for susceptible women

## 1 Oct 2013: add partnership duration restricted to 15 to 55.

## 26 Sep 2013: Increase number of births per 1000 women by
## 15%

## 3 Sep 2013: Revise fertility rates (per 1000) based on Sarah's
## email dated 3 Sep 2013.

## 2 Sep 2013: Add attributes for stratifying pregnancy by age
## and infection status.

## 26 Aug 2013: Max Survival for everyone -- add that NOW!!!!

## 22 Aug 2013:
## a. Add age-based expected life expectancy at time of infection
## b. Realistic CD4 initiation 131 (100 for South Africa)

## 20 Aug 2013: Change initial prevalence to 10%.
## 15 Aug 2013: I think min.chronic.infectivity is off by an order of magnitude.

### File to maintain comprehensive list of parameters

  ## Basic Population Set UP
  num.male <- 2500
  num.female <- 2500 
  N <- num.male+num.female

  ## BEHAVIOR
  ## Network Related Statistics
  ## Degree Distribution, Partnership Duration
  ## and Number of Partnerships
  male.deg.dist <- c(34.8, 52.3, 9.8, 3.1)/100 ## Phase 2 data
  female.deg.dist <- c(42.0, 55.7, 2.3, 0)/100 ## Phase 2 data

  size.of.timestep <- 14 ## each time step is 14 days
  duration <- 4303/size.of.timestep
  duration.15to55 <- 4139/size.of.timestep #1Oct2013 
  diagnostics <- T

  female.deg.counts <- female.deg.dist*num.female
  male.deg.counts <- male.deg.dist*num.male
  female.deg.tot <- (0*female.deg.counts[1] + 1*female.deg.counts[2] +
                     2*female.deg.counts[3] + 3*female.deg.counts[4])
  male.deg.tot <- (0*male.deg.counts[1] + 1*male.deg.counts[2] +
                   2*male.deg.counts[3] + 3*male.deg.counts[4])

  ## to match the degree totals for men and women, reduce the number of
  ## isolates in the women, and increase the number of women with 1 partner
  female.deg.dist.matched <- c(22.0, 75.7, 2.3, 0)/100
  female.deg.counts.matched <- female.deg.dist.matched*num.female
  female.deg.tot.matched <- (0*female.deg.counts.matched[1] +
                             1*female.deg.counts.matched[2] +
                             2*female.deg.counts.matched[3] +
                             3*female.deg.counts.matched[4])




  ## DEMOGRAPHIC ATTRIBUTES 
 
      max.survival <- 55 # 26 Aug 2013 -- max age (in years)
      ## Sex, age, circumcision status, 
      ## pregnancy status, 
      ## other pregnany related parameters


      ## Age (in accordance with proportions from census data)
      ##min.age <- 15
      min.age <- 18
      max.age <- 54
      age.distribution <- c(0.25468095,
                            0.20238352,
                            0.155598745,
                            0.119586275,
                            0.095660855,
                            0.07367249,
                            0.058677315,
                            0.04014263)

      age.classes <- c(#seq(15, 19, by=1),
                       seq(18, 19, by=1), #7Nov13
                       seq(20, 24, by=1),
                       seq(25, 29, by=1),
                       seq(30, 34, by=1),
                       seq(35, 39, by=1),
                       seq(40, 44, by=1),
                       seq(45, 49, by=1),
                       seq(50, 54, by=1)
                       ) # create a vector of all ages of interest

      ## circumcision status
      circum.rate <- 96/(851+96) ## No 851; Yes 96; Refusal 1; NA 0,
                               ## see file "data_report.pdf"



  ## num.births.per1k.byage =
  ## round(c(83.2206172368,
  ##         154.3021435787,
  ##         167.5508974253,
  ##         122.3312442802,
  ##         76.2926740922,
  ##         29.3483793413,
  ##         9.424565988), 1) #2Sep13: CHECK!!!!!!!!!


      num.births.per1k.byage = round(c(
        174.967,
        345.9442,
        320.1459,
        266.055,	
        184.28,	
        79.91913,
        36.50134),
        1) # for 1 decimal place

      ## prop.stillbirth = 0.10 #2Sep13
         prop.stillbirth = 24.8/1000 #2Sep13
         inf.preg.red = 0.53 #2Sep13

      num.births.per1k.byage.15pcinc <- num.births.per1k.byage+
  (0.15*num.births.per1k.byage)
  
  ## BIOLOGICAL ATTRIBUTES 

      ## Infection Status 
      ## init.hiv.prev <- 0.06 # set to about 6% for Uganda
      ## init.hiv.prev <- 0.10 # set to about 6% for Uganda
      ## init.hiv.prev <- 0.15 # 22Aug2013
         init.hiv.prev <- 0.10 # 23 Aug 2013
         init.hiv.prev.6 <- 0.06 # 23 Aug 2013
         init.hiv.prev.15 <- 0.15
         init.hiv.prev.10 <- 0.10 # 3Sep13

      ## Time Since Infection 
      duration.of.infection <- 3300 ## in days, modify later

      ## infectivity for infected individuals
      # min.chronic.infectivity <- 0.00497/2.89
        #min.chronic.infectivity.unadj <- 0.00497/2.89
                                        # changed to include infection at log 2

     min.chronic.infectivity.unadj <- 0.000497/2.89 # 15 Aug 2013: was off by order. of
                                        # magnitude
  ## Time of Infection 
      acute.length <- 1:floor(121/size.of.timestep) ## in daily time units
      chronic.length <- ceiling(121/size.of.timestep):floor(1877/size.of.timestep)
      late.length <- ceiling(1877/size.of.timestep):floor(3300/size.of.timestep)

      ## CD4 Counts
             ## Set to 518 for men and 570 for women
             ## For positives, this will change as we step through time loop.
             ## Relevant parameters
      cd4.at.infection.male <- 518 #cells/mm3
      cd4.at.infection.female <- 570 #cells/mm3
      untreated.cd4.daily.decline <- 0.14 # (for men and women)
      untreated.cd4.perstep.decline <- untreated.cd4.daily.decline*size.of.timestep
      untreated.cd4.time.to.350.men <- 3.3*365/size.of.timestep # changed due to timestep
      untreated.cd4.time.to.350.men <- 4.2*365/size.of.timestep # changed due to timestep

      ## Viral Load Today
      ## List viral load parameters, adjusted for size of timestep
      time.infection.to.peak.viremia <-   floor(14/size.of.timestep)
      time.infection.to.peak.viral.load <- time.infection.to.peak.viremia
      peak.viral.load <- 6.17
      time.infection.to.viral.set.point <- floor(121/size.of.timestep)
      set.point.viral.load <- 4.2
      time.infection.to.late.stage <- floor(1877/size.of.timestep)
      dur.inf <- floor(3300/size.of.timestep)
      late.stage.viral.load <- 5.05 ## (max?)
      time.infection.to.peak.viral.load
      time.to.full.supp <- 4*30/size.of.timestep ## 4 months
      undetectable.vl <- log(50, base=10)

      ## dur.inf.by.age <- round(c(12.8*365/size.of.timestep,
      ##                           10.6*365/size.of.timestep,
      ##                           7.5*365/size.of.timestep,
      ##                           5.6*365/size.of.timestep))

      given.dur.inf.by.age <- round(c(12.8*365/size.of.timestep,
                                      10.6*365/size.of.timestep,
                                      7.5*365/size.of.timestep,
                                      5.6*365/size.of.timestep))

  ## ART 
      baseline.art.coverage.rate <- 0.40 # coverage
      baseline.preg.art.coverage.rate <- baseline.preg.coverage.rate <- 0.43 #14Oct13
                                        # coverage # CHECK THIS

      idealized.art.coverage.rate <- 0.90 # 28Oct13
      idealized.preg.coverage.rate <- 0.90 # 28Oct13

      cd4.recovery.time <- 3*365/size.of.timestep ## CD4 recovery for 3 years
      per.day.cd4.recovery <- 15/30 ## rate of 15 cells/month
      eligible.cd4 <- 350
      baseline.cd4.at.art.initiation.men <- 131 # 22Aug13: customized for UG
      baseline.cd4.at.art.initiation.women <- 131 # 22Aug13: customized for UG
      bl.min.art.init.timestep.male <- (cd4.at.infection.male -
                                        baseline.cd4.at.art.initiation.men)/
                                        untreated.cd4.perstep.decline
      bl.min.art.init.timestep.female <- (cd4.at.infection.female -
                                          baseline.cd4.at.art.initiation.women)/
                                          untreated.cd4.perstep.decline


      idealized.cd4.at.art.initiation.men <- 350
      idealized.cd4.at.art.initiation.women <- 350


  ## Option A
      optA.sc.art.vl.perstep.dec <- 1.1/((40-23)*7)*size.of.timestep
                ## decline is 1.1 log over 17 weeks(from first visit to delivery)
                ## Per day decline, therefore, is 1.1/((40-23)*7)
                ## Per time step decline, therefore, is given by expr. above
      optA.sc.art.cd4.perstep.rec <- 50/((40-23)*7)*size.of.timestep
                ## recovery is 50 cells/mm3 over 17 weeks
                ## (from first visit to delivery)
                ## same logic as above applies

       optA.thres=350

  ## Demographic Parameters

      ## Mortality
      ## 7Jul13: Adjusted to realistic values
      asmr.perperson.perday <- c(6.87671232876712E-006,
                                 1.31232876712329E-005,
                                 1.93424657534247E-005,
                                 2.66027397260274E-005,
                                 3.7013698630137E-005,
                                 4.59452054794521E-005,
                                 5.29315068493151E-005,
                                 5.68493150684932E-005
                                 )

      asmr.perperson.pertimestep <- asmr.perperson.perday*size.of.timestep
      asmr.male <- asmr.perperson.pertimestep
      asmr.female <- asmr.perperson.pertimestep

      ## Births
      phi <- 0.001*5 ## mean parameter for poisson process
      phi <- (phi/5)*3 #29Aug13, 2Sep13
      phi.std <- 0.001*1
      phi.std2 <- 0.001*2
      phi.zero <- 0 #22 Aug 2013
      phi.std5 <- 0.001*5
      phi.std4 <- 0.001*4  

      ## Pregnancy
         full.term=40/14*7
         min.preg.interval=15*30/14
         optA.vl.reduction=1.1
         sc.art.postcess.ret.bl=6*30/14 ## return in 6 months = 180/14 timesteps
         baseline.f.ges.visit=23*7/14
         idealized.f.ges.visit=14*7/14

  ## Transmission Parameters
  ## Frequency of Sex
     num.sex.acts.per.timestep <- 2.4*size.of.timestep/7
     acute.mult <- 4.98
     late.mult <- 3.49
     preg.mult <- 2.5 ## check
     circum.mult <- 0.60 ## check
     preg.susc.mult <- 1.7
