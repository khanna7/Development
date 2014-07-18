## 10 Jul 2014: Coverage rate 40%

## 7 Jul 2014: Vary PMTCT coverage: 20%, 50%, 60%, 70% 80%
## and compute how many would be virally suppressed in
## those cases

## 1 Jul 14: Update ART coverage for eligibility at 500
## to 51%

## 26 June 2014: Add parameters for ART eligibility at 500 :-
## Coverage: 45% (estimate with 350 eligibility was 45.05% -- stick with that)
## Mean CD4 at initiation: 168

## 28 April 2014: Add "OptB.thres" argument --
## see "notes.txt" with this date

## 23 April 2014:
## a. hemodilution start ART for women at CD4 of 450

## 14 March 2014: Add UG fertility rates for counterfactual
## scenario

## 21 Feb 2014:
## a. Set sc.art.postcess.ret.bl= sc.art.postcess.ret.bl.chung
## to avoid confusion later.
## b. Add optb.vl.postcess.ret.bl=2*30/14 ##21Feb14
## c. Differentiate
## optA.sc.art.vl.perstep.dec and optA.sc.art.cd4.perstep.rec with
## optA.sc.art.vl.perstep.dec.idealized and optA.sc.art.cd4.perstep.rec.idealized
## (these idealized "dec" and "ret" params to not have to be used -- see
## email from Sarah dated 21 Feb 2014)
## Also change 23 weeks to 22 weeks in calculations above

## 12 Feb 2014: Add 1 month return post-ART cessation

## 30 Jan 2014: Increase duration to reduce overall
## incidence in South Africa

## 15 Jan 2014: Put in Hollingsworth's multipliers

## 22 Dec 2013:
## Add parameter for cessation of Option B ART:
## optB.cess.time = round(12*30/14)

## 17 Dec 2013:
## Add parameters "idealized.cd4.at.art.initiation.men" and 
## "idealized.cd4.at.art.initiation.women", both = 350 here.

## 9 Dec 2013:
## Add growth rates of 3.3, 3.5 and 3.75 to see if those will control mean
## degree and consequent rise in prevalence

## 4 Dec 2013:
## a. Differentiate prevalence at recruitment for men and women -- 1.9% for men,
## 4.9% for women; earlier, I was using an average recruit prevalence of 3% for everyone

## b. Decided on 75% adherence for PMTCT under idealized intervention, consistent
## with adherence for realistics PMTCT intervention. For regular ART, adherence estimate
## under realistic and idealized interventions are also the same. The realistic
## coverage rate for PMTCT already accounts for adherence. 

## 3 Dec 2013:
## Differentiate prevalence at recruitment for men and women -- 1% for men,
## 5% for women; earlier, I was using an average recruit prevalence of 3% for everyone

## 1 Dec 2013: Also adjust "idealized.art.coverage.rate" for adherence using
## multiplicative model -- similar to adjustment for
## "baseline.art.coverage.rate"

## 25 Nov 2013:
## Adjust "baseline.art.coverage.rate" for adherence. We will do this by using
## a multiplicative model for coverage and adherence. See email dated 22 Nov 2013
## from Sarah    

## 16 Nov 2013:
## a. "assign.infectivity" function has argument "preg.mult" and
##    "transmission" function has argument "preg.susc.mult."
## b. baseline pregnancy coverage rate is 0.67.
## c. Comment out artificially increasing degree 1 for women and reducing
##    degree 1 for men. 
## d. degree distributions for men and women based on ages restricted between 18-55
##    (see email dated 25 Sep 2013)
## e. mean partnership duration
## f. age distribution

## 12 Nov 2013: Entry age should be 18

## 17 Sep 2013: Adapt to South African data

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
  ## and Number ofs Partnerships

## Male (age restricted) ##16Nov13
##           0           1           2           3           4           5
## 0.141089109 0.660891089 0.113861386 0.049504950 0.009900990 0.007425743
##           6          19          77          99        <NA>
##   0.002475248 0.002475248 0.007425743 0.004950495 0.000000000

## Female (age restricted) ##16Nov13
##           0           1           2           3          77          99
## 0.242574257 0.725247525 0.014851485 0.002475248 0.012376238 0.002475248
##        <NA>
##   0.000000000

  male.deg.dist <- c(14.1, 66.1, 11.4, 4.9)/100 ## 16Nov13: Phase 2 data
  female.deg.dist <- c(24.2, 72.5, 1.5, 0.2)/100 ## 16Nov13: Phase 2 data

  size.of.timestep <- 14 ## each time step is 14 days
  duration <- 2221/size.of.timestep #16Nov13: See email dated Sep 25, 2013. Phase 2 data
  duration.100 <- (2221+100)/size.of.timestep #30Jan14
  duration.200 <- (2221+200)/size.of.timestep #30Jan14
  duration.300 <- (2221+300)/size.of.timestep #30Jan14
  duration.500 <- (2221+500)/size.of.timestep #30Jan14
  duration.700 <- (2221+700)/size.of.timestep #30Jan14
  duration.1000 <- (2221+1000)/size.of.timestep #30Jan14

  diagnostics <- T


  ##################################################################
  ### Phase 2 data
  ##################################################################

  female.deg.counts <- female.deg.dist*num.female
  male.deg.counts <- male.deg.dist*num.male

  female.deg.tot <- (0*female.deg.counts[1] + 1*female.deg.counts[2] +
                     2*female.deg.counts[3] + 3*female.deg.counts[4])
  male.deg.tot <- (0*male.deg.counts[1] + 1*male.deg.counts[2] +
                   2*male.deg.counts[3] + 3*male.deg.counts[4])

  #n.edges <- female.deg.tot
  n.edges <- male.deg.tot
  #n.edges <- (male.deg.tot+female.deg.tot)/2
  male.deg.edgecount <- n.edges*male.deg.dist
  female.deg.edgecount <- n.edges*female.deg.dist

  sum(male.deg.edgecount[1:2])
  sum(female.deg.edgecount[1:2])
  ## to match the degree totals for men and women, reduce the number of
  ## isolates in the women, and increase the number of women with 1 partner
  ## female.deg.dist.matched <- c(10, 88, 2, 0)/100 #revise if model doesnt converge
  ## female.deg.counts.matched <- female.deg.dist.matched*num.female
  ## female.deg.tot.matched <- c(0*female.deg.counts.matched[1] +
  ##                            1*female.deg.counts.matched[2] +
  ##                            2*female.deg.counts.matched[3] +
  ##                            3*female.deg.counts.matched[4])



  ##################################################################

  ##################################################################
  ### CS data
  ##################################################################

  male.deg.dist.cs <- c(36.5, 55.5, 6.1, 1.8)/100 #community survey
  female.deg.dist.cs <- c(42.1, 57.3, 0.5, 0)/100 #community survey
  ##female.deg.dist.cs <- male.deg.dist.cs
  ##female.deg.dist.cs <- c(32.1, 67.3, 0.5, 0)/100 #red.singles & inc. ones
  ##female.deg.dist.cs <- c(37.1, 62.3, 0.5, 0)/100 #red.singles & inc. ones

  female.deg.counts.cs <- female.deg.dist.cs*num.female
  male.deg.counts.cs <- male.deg.dist.cs*num.male

  female.deg.tot.cs <- (0*female.deg.counts.cs[1] + 1*female.deg.counts.cs[2] +
                        2*female.deg.counts.cs[3] + 3*female.deg.counts.cs[4])
  male.deg.tot.cs <- (0*male.deg.counts.cs[1] + 1*male.deg.counts.cs[2] +
                      2*male.deg.counts.cs[3] + 3*male.deg.counts.cs[4])


  n.edges.cs <- male.deg.tot.cs
  #n.edges.cs <- 2500
  male.deg.edgecount.cs <- n.edges.cs*male.deg.dist.cs
  female.deg.edgecount.cs <- n.edges.cs*female.deg.dist.cs

  sum(male.deg.edgecount.cs[1:2])
  sum(female.deg.edgecount.cs[1:2])
  ##################################################################

  ## DEMOGRAPHIC ATTRIBUTES 
 
      max.survival <- 55 # 26 Aug 2013 -- max age (in years)
      ## Sex, age, circumcision status, 
      ## pregnancy status, 
      ## other pregnany related parameters


      ## Age (in accordance with proportions from census data)
      #min.age <- 15
      min.age <- 18
      max.age <- 54
      ##age.distribution <- c(0.18, 0.19, 0.17, 0.12, 0.10, 0.08, 0.08, 0.07)
      age.distribution <- c(0.07, 0.20, 0.19, 0.15, 0.11, 0.09, 0.08, 0.07) #16Nov13

      ## age.classes <- c(seq(15, 19, by=1), #12Nov13: Commented out, 
      ##                  seq(20, 24, by=1), # age at entry should be 18
      ##                  seq(25, 29, by=1),
      ##                  seq(30, 34, by=1),
      ##                  seq(35, 39, by=1),
      ##                  seq(40, 44, by=1),
      ##                  seq(45, 49, by=1),
      ##                  seq(50, 54, by=1)
      ##                  ) # create a vector of all ages of interest

      age.classes <- c(seq(18, 19, by=1), #12Nov13: Commented out, 
                       seq(20, 24, by=1), # age at entry should be 18
                       seq(25, 29, by=1),
                       seq(30, 34, by=1),
                       seq(35, 39, by=1),
                       seq(40, 44, by=1),
                       seq(45, 49, by=1),
                       seq(50, 54, by=1)
                       ) # create a vector of all ages of interest

      ## circumcision status
      circum.rate <- 116/(364+116) ## No 851; Yes 96; Refusal 1; NA 0,
                                   ## see file "data_report.pdf"

      num.births.per1k.byage = round(c(
        c(80.6,
          139,
          141.8,
          105.6,
          67.4,
          27.1,
          8.8)),
        1) # for 1 decimal place

         prop.stillbirth = 20.4/1000 #2Sep13
         inf.preg.red = 0.53 #2Sep13

      num.births.per1k.byage.15pcinc <- num.births.per1k.byage+
  (0.15*num.births.per1k.byage)

  ####################################################
  ## 16Mar14: South Africa fertility rates for
  ## counter factual analysis
  ####################################################

      num.births.per1k.byage.ug = round(c(
        174.967,
        345.9442,
        320.1459,
        266.055,	
        184.28,	
        79.91913,
        36.50134),
        1) # for 1 decimal place

      ## prop.stillbirth = 0.10 #2Sep13
      prop.stillbirth.ug = 24.8/1000 #2Sep13
      num.births.per1k.byage.15pcinc.ug <- num.births.per1k.byage.ug+
  (0.15*num.births.per1k.byage.ug)

####################################################

  ## BIOLOGICAL ATTRIBUTES 

      ## Infection Status 
         init.hiv.prev <- 0.25 # 23 Aug 2013
         recruit.inf.prop.male <- 0.01 #3Dec13
         recruit.inf.prop.female <- 0.05 #3Dec13

      ## Time Since Infection 
         duration.of.infection <- 3300 ## in days, modify later

      ## infectivity for infected individuals
        #min.chronic.infectivity <- 0.00497/2.89
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
      time.infection.to.peak.viremia <- time.infection.to.peak.viral.load <-
  floor(14/size.of.timestep)
      peak.viral.load <- 6.17
      time.infection.to.viral.set.point <- floor(121/size.of.timestep)
      set.point.viral.load <- 4.2
      time.infection.to.late.stage <- floor(1877/size.of.timestep)
      dur.inf <- floor(3300/size.of.timestep)
      late.stage.viral.load <- 5.05 ## (max?)
      time.infection.to.peak.viral.load
      time.to.full.supp <- 4*30/size.of.timestep ## 4 months
      undetectable.vl <- log(50, base=10)

    given.dur.inf.by.age <- round(c(12.8*365/size.of.timestep,
                                10.6*365/size.of.timestep,
                                7.5*365/size.of.timestep,
                                5.6*365/size.of.timestep))


  ## ART 
      ## baseline.art.coverage.rate <-  0.40# coverage
      baseline.art.coverage.rate <-  0.53 #25Nov13
      art.adherence.rate <- 0.854 #25Nov13
      baseline.art.coverage.rate <- baseline.art.coverage.rate*art.adherence.rate #25Nov13
      baseline.art.coverage.rate.artelig500 <- 0.51 #1Jul14

      baseline.preg.art.coverage.rate <- baseline.preg.coverage.rate <- 0.67 # 16Nov13

      idealized.art.coverage.rate <- 0.90 # 28Oct13
      idealized.art.coverage.rate <- idealized.art.coverage.rate*art.adherence.rate
                                        # 1 Dec 2013
      idealized.preg.coverage.rate <- 0.90 # 28Oct13
      preg.adherence.rate          <- 0.75 #4Dec13
      idealized.preg.coverage.rate <- idealized.preg.coverage.rate*preg.adherence.rate
                                        #4Dec13

      idealized.preg.coverage.rate.50 <- 0.50 #7Jul14
      idealized.preg.coverage.rate.60 <- 0.60 #7Jul14
      idealized.preg.coverage.rate.70 <- 0.70 #7Jul14
      idealized.preg.coverage.rate.80 <- 0.80 #7Jul14
      idealized.preg.coverage.rate.20 <- 0.20 #7Jul14
      idealized.preg.coverage.rate.40 <- 0.40 #10Jul14

      cd4.recovery.time <- 3*365/size.of.timestep ## CD4 recovery for 3 years
      per.day.cd4.recovery <- 15/30 ## rate of 15 cells/month
      eligible.cd4 <- 350
      baseline.cd4.at.art.initiation.men <-  100# 22Aug13: customized for UG
      baseline.cd4.at.art.initiation.women <- 100 # 22Aug13: customized for UG

      baseline.cd4.at.art.initiation.men.artelig500 <-  168 #26Jun14 
      baseline.cd4.at.art.initiation.women.artelig500 <- 168 #26Jun14 

      idealized.cd4.at.art.initiation.men <-  350 #17Dec13
      idealized.cd4.at.art.initiation.women <-  350 #17Dec13

      cd4.at.art.initiation.women.450 <- 450 #23Apr2014


      bl.min.art.init.timestep.male <- (cd4.at.infection.male -
                                        baseline.cd4.at.art.initiation.men)/
                                        untreated.cd4.perstep.decline
      bl.min.art.init.timestep.female <- (cd4.at.infection.female -
                                          baseline.cd4.at.art.initiation.women)/
                                          untreated.cd4.perstep.decline


  ## Option A
      optA.sc.art.vl.perstep.dec <- 1.1/((40-22)*7)*size.of.timestep
                ## decline is 1.1 log over 17 weeks(from first visit to delivery)
                ## Per day decline, therefore, is 1.1/((40-23)*7)
                ## Per time step decline, therefore, is given by expr. above
      optA.sc.art.cd4.perstep.rec <- 50/((40-22)*7)*size.of.timestep
                ## recovery is 50 cells/mm3 over 17 weeks
                ## (from first visit to delivery)
                ## same logic as above applies
      optA.sc.art.vl.perstep.dec.idealized <- 1.1/((40-14)*7)*size.of.timestep #21Feb14
      optA.sc.art.cd4.perstep.rec.idealized <- 50/((40-14)*7)*size.of.timestep #21Feb14

      optA.thres=350
      optB.thres=350#28Apr14

  ## Option B
     optB.cess.time = round(12*30/14)

  ## Demographic Parameters

      ## Mortality
      ## 7Jul13: Adjusted to realistic values
      asmr.male.perperson.perday <- c(5.88767E-06,
                                      1.52822E-05,
                                      2.93342E-05,
                                      5.74548E-05,
                                      7.06466E-05,
                                      7.82712E-05,
                                      7.48904E-05,
                                      8.14575E-05,
                                      0.000413225
                                      )

       asmr.female.perperson.perday <- c(4.87945E-06,
                                         2.89014E-05,
                                         5.46219E-05,
                                         6.36603E-05,
                                         5.60767E-05,
                                         4.36411E-05,
                                         3.95808E-05,
                                         4.22959E-05,
                                         0.000333658)


      #asmr.perperson.pertimestep <- asmr.perperson.perday*size.of.timestep
      asmr.male <- asmr.male.perperson.perday*size.of.timestep
      asmr.female <- asmr.female.perperson.perday*size.of.timestep

      ## Births
      phi.std5 <- 0.001*5 ## mean parameter for poisson process
      phi.std4 <- 0.001*4 ## mean parameter for poisson process
      phi.std3 <- 0.001*3 ## mean parameter for poisson process
      phi.std2 <- 0.001*2 ## mean parameter for poisson process
      phi.std  <- 0.001    ## mean parameter for poisson process
      phi.std35 <- 0.001*3.5
      phi.std45 <- 0.001*4.5
      phi.std37 <- 0.001*3.7
      phi.std33 <- 0.001*3.3

      ## Pregnancy
         full.term=40/14*7
         min.preg.interval=15*30/14
         optA.vl.reduction=1.1
         ## sc.art.postcess.ret.bl=6*30/14 ## return in 6 months = 180/14 timesteps
         baseline.f.ges.visit=23*7/14
         idealized.f.ges.visit=14*7/14
         sc.art.postcess.ret.bl.chung=1*30/14 ## return in 1 month = 30/14 timesteps
         sc.art.postcess.ret.bl <- sc.art.postcess.ret.bl.chung
         optB.vl.postcess.ret.bl=2*30/14 ##21Feb14
  ## Transmission Parameters
  ## Frequency of Sex
     num.sex.acts.per.timestep <- 2.4*size.of.timestep/7
     acute.mult <- 4.98
     acute.mult.holling <- 4.98 ##15Jan14
     late.mult <- 3.49
     late.mult.holling <- 7 ##15Jan14
     preg.mult <- 2.5 ## check
     circum.mult <- 0.60 ## check
     preg.susc.mult <- 1.7
