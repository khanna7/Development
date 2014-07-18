## 30 Sep 2013: Analyze data with 29 Sep 2013 with correction factor

## 27 Sep 2013: pregnancy results with rates "bumped up"

## 25 Sep 2013: Correction factor in "assign.pregnancy_5d2.R."

## 23 Sep 2013: Check age distribution of women at end to see
## if pregnancy summaries make sense

## 17 Sep 2013: Plot results from models b and c (defined below) -- with
## loop fixed. These data are prefixed "16Sep2013"

## 13 Sep 2013: Plot results from different pregnancy tests that are on.

## Three different models being tested:

## a. Pregnancy criteria that include being in a relationship, and using the "correction factor" (for proportion of women who meet criteria with respect to all women).
## b. Immaculate conception pregnancy with the correction.
## c. Immaculate conception without the correction.

## In model (a), probability of pregnancies is artificially restricted to not exceed 1.
## In models (b) and (c), I have no such restriction -- since I think the probability criteria should not be violated. If it is we should be able to see that by Monday morning.

rm(list=ls())
####################################################################
## Prevalence and population size data
####################################################################

modela.run2 <- read.csv("13Sep13_10pc_3phi_recnumpregtest_testbinompregob_1300steps_run2.prev.inc.data.csv", header=FALSE, sep=" ")
modela.run3 <- read.csv("13Sep13_10pc_3phi_recnumpregtest_testbinompregob_1300steps_run3.prev.inc.data.csv", header=FALSE, sep=" ")

modelb.run1 <- read.csv("13Sep13_10pc_3phi_immconc_withcorrn_recnumpregtest_testbinompregob_1300steps_run1.prev.inc.data.csv", header=FALSE, sep=" ")
modelb.run2 <- read.csv("13Sep13_10pc_3phi_immconc_withcorrn_recnumpregtest_testbinompregob_1300steps_run2.prev.inc.data.csv", header=FALSE, sep=" ")

modelb.16sep.run1 <- read.csv("16Sep13_10pc_3phi_immconc_withcorrn_recnumpregtest_testbinompregob_1300steps_run1.prev.inc.data.csv", header=FALSE, sep=" ")
modelb.16sep.run2 <- read.csv("16Sep13_10pc_3phi_immconc_withcorrn_recnumpregtest_testbinompregob_1300steps_run2.prev.inc.data.csv", header=FALSE, sep=" ") #17Sep2013

modelb.5d2.run1 <- read.csv("22Sep13_10pc_3phi_immconc_wcorrnd5d2_recnumpregtest_testbinompregob_1300steps_run1.prev.inc.data.csv", header=FALSE, sep=" ") #25Sep2013
modelb.5d2.run2 <- read.csv("22Sep13_10pc_3phi_immconc_wcorrnd5d2_recnumpregtest_testbinompregob_1300steps_run2.prev.inc.data.csv", header=FALSE, sep=" ") #25Sep2013

modelb.5d3.run1 <- read.csv("25Sep13_10pc_3phi_cross-net_1300steps_run1.prev.inc.data.csv", header=FALSE, sep=" ") #25Sep2013
modelb.5d3.run2 <- read.csv("25Sep13_10pc_3phi_cross-net_1300steps_run2.prev.inc.data.csv", header=FALSE, sep=" ") #25Sep2013

modelc.run1 <- read.csv("13Sep13_10pc_3phi_immconc_wocorrn_recnumpregtest_testbinompregob_1300steps_run1.prev.inc.data.csv", header=FALSE, sep=" ")
modelc.run2 <- read.csv("13Sep13_10pc_3phi_immconc_wocorrn_recnumpregtest_testbinompregob_1300steps_run2.prev.inc.data.csv", header=FALSE, sep=" ")

modelc.16sep.run1 <- read.csv("16Sep13_10pc_3phi_immconc_wocorrn_recnumpregtest_testbinompregob_1300steps_run1.prev.inc.data.csv", header=FALSE, sep=" ")
modelc.16sep.run2 <- read.csv("16Sep13_10pc_3phi_immconc_wocorrn_recnumpregtest_testbinompregob_1300steps_run2.prev.inc.data.csv", header=FALSE, sep=" ") #16Sep2013

modelc.26sep.incfert.run1 <- read.csv("26Sep13_10pc_3phi_wocorrn_inc_fert_rate_1300steps_run1.prev.inc.data.csv", header=FALSE, sep=" ") #27Sep13
modelc.26sep.incfert.run2 <- read.csv("26Sep13_10pc_3phi_wocorrn_inc_fert_rate_1300steps_run2.prev.inc.data.csv", header=FALSE, sep=" ") #27Sep13

prev.modela.run2 <- modela.run2[,8]
prev.modela.run3 <- modela.run3[,8]

prev.modelb.run1 <- modelb.run1[,8]
prev.modelb.run2 <- modelb.run2[,8]

prev.modelb.16sep.run1 <- modelb.16sep.run1[,8]
prev.modelb.16sep.run2 <- modelb.16sep.run2[,8]

prev.modelb.5d3.run1 <- modelb.16sep.run1[,8]
prev.modelb.5d3.run2 <- modelb.16sep.run2[,8]

prev.modelc.run1 <- modelc.run1[,8]
prev.modelc.run2 <- modelc.run2[,8]

prev.modelc.16sep.run1 <- modelc.16sep.run1[,8]
prev.modelc.16sep.run2 <- modelc.16sep.run2[,8]

prev.modelc.26sep.incfert.run1 <- modelc.26sep.incfert.run1[,8] #27Sep13
prev.modelc.26sep.incfert.run2 <- modelc.26sep.incfert.run2[,8] #27Sep13

## Plot prevalence
plot(modela.run2[,1], prev.modela.run2, type="l", col="blue", ylim=c(0, 0.2),
     xlab="Time", ylab="Prevalence") # prevalence plot
lines(modela.run3[,1], prev.modela.run3, col="blue") 

## lines(modelb.run1[,1], prev.modelb.run1, col="red")
## lines(modelb.run2[,1], prev.modelb.run2, col="red") 

lines(modelb.16sep.run1[,1], prev.modelb.16sep.run1, col="red")
lines(modelb.16sep.run2[,1], prev.modelb.16sep.run2, col="red") 

## lines(modelc.run1[,1], prev.modelc.run1, col="green")
## lines(modelc.run2[,1], prev.modelc.run2, col="green") 

## lines(modelc.16sep.run1[,1], prev.modelc.16sep.run1, col="green")
## lines(modelc.16sep.run2[,1], prev.modelc.16sep.run2, col="green") 

lines(modelc.26sep.incfert.run1[,1], prev.modelc.26sep.incfert.run1, col="green")
lines(modelc.26sep.incfert.run2[,1], prev.modelc.26sep.incfert.run2, col="green")

legend("topleft", c("Model a", "Model b",
                    "Model c"), col = c("blue", "red", "green"),
       lty=1,
       title = "")

####################################################################

####################################################################
## Pregnancy data
####################################################################

modela.preg.run2 <- read.csv("13Sep13_10pc_3phi_recnumpregtest_testbinompregob_1300steps_run2.preg.csv", header=FALSE, sep=" ")
modela.preg.run3 <- read.csv("13Sep13_10pc_3phi_recnumpregtest_testbinompregob_1300steps_run3.preg.csv", header=FALSE, sep=" ")

modelb.preg.run1 <- read.csv("13Sep13_10pc_3phi_immconc_withcorrn_recnumpregtest_testbinompregob_1300steps_run1.preg.csv", header=FALSE, sep=" ")
modelb.preg.run2 <- read.csv("13Sep13_10pc_3phi_immconc_withcorrn_recnumpregtest_testbinompregob_1300steps_run2.preg.csv", header=FALSE, sep=" ")

modelb.preg.16sep.run1 <- read.csv("16Sep13_10pc_3phi_immconc_withcorrn_recnumpregtest_testbinompregob_1300steps_run1.preg.csv", header=FALSE, sep=" ")
modelb.preg.16sep.run2 <- read.csv("16Sep13_10pc_3phi_immconc_withcorrn_recnumpregtest_testbinompregob_1300steps_run2.preg.csv", header=FALSE, sep=" ")

modelb.preg.5d2.run1 <- read.csv("22Sep13_10pc_3phi_immconc_wcorrnd5d2_recnumpregtest_testbinompregob_1300steps_run1.preg.csv", header=FALSE, sep=" ") #25Sep13
modelb.preg.5d2.run2 <- read.csv("22Sep13_10pc_3phi_immconc_wcorrnd5d2_recnumpregtest_testbinompregob_1300steps_run2.preg.csv", header=FALSE, sep=" ") #25Sep13

modelb.preg.5d3.run1 <- read.csv("25Sep13_10pc_3phi_cross-net_1300steps_run1.preg.csv", header=FALSE, sep=" ") #25Sep13
modelb.preg.5d3.run2 <- read.csv("25Sep13_10pc_3phi_cross-net_1300steps_run2.preg.csv", header=FALSE, sep=" ") #25Sep13

modelc.preg.run1 <- read.csv("13Sep13_10pc_3phi_immconc_wocorrn_recnumpregtest_testbinompregob_1300steps_run1.preg.csv", header=FALSE, sep=" ")
modelc.preg.run2 <- read.csv("13Sep13_10pc_3phi_immconc_wocorrn_recnumpregtest_testbinompregob_1300steps_run2.preg.csv", header=FALSE, sep=" ")

modelc.preg.16sep.run1 <- read.csv("16Sep13_10pc_3phi_immconc_wocorrn_recnumpregtest_testbinompregob_1300steps_run1.preg.csv", header=FALSE, sep=" ")
modelc.preg.16sep.run2 <- read.csv("16Sep13_10pc_3phi_immconc_wocorrn_recnumpregtest_testbinompregob_1300steps_run2.preg.csv", header=FALSE, sep=" ")

modelc.preg.26sep.incfert.run1 <- read.csv("26Sep13_10pc_3phi_wocorrn_inc_fert_rate_1300steps_run1.preg.csv", header=FALSE, sep=" ") #27Sep13
modelc.preg.26sep.incfert.run2 <- read.csv("26Sep13_10pc_3phi_wocorrn_inc_fert_rate_1300steps_run2.preg.csv", header=FALSE, sep=" ") #27Sep13

npreg.modela.run2 <- modela.preg.run2[,2]
npreg.modela.run3 <- modela.preg.run3[,2]

npreg.modelb.run1 <- modelb.preg.run1[,2]
npreg.modelb.run2 <- modelb.preg.run2[,2]

npreg.modelb.16sep.run1 <- modelb.preg.16sep.run1[,2]
npreg.modelb.16sep.run2 <- modelb.preg.16sep.run2[,2]

npreg.modelb.5d2.run1 <- modelb.preg.5d2.run1[,2]
npreg.modelb.5d2.run2 <- modelb.preg.5d2.run2[,2]

npreg.modelb.5d3.run1 <- modelb.preg.5d3.run1[,2]
npreg.modelb.5d3.run2 <- modelb.preg.5d3.run2[,2]

npreg.modelc.run1 <- modelc.preg.run1[,2]
npreg.modelc.run2 <- modelc.preg.run2[,2]

npreg.modelc.16sep.run1 <- modelc.preg.16sep.run1[,2]
npreg.modelc.16sep.run2 <- modelc.preg.16sep.run2[,2]

npreg.modelc.preg.26sep.incfert.run1 <- modelc.preg.26sep.incfert.run1[,2]#27Sep13
npreg.modelc.preg.26sep.incfert.run2 <- modelc.preg.26sep.incfert.run2[,2]#27Sep13

## Plot NUMBER of pregnancies
plot(modela.preg.run2[,1], npreg.modela.run2, type="l", col="blue", #ylim=c(0, 0.2),
     xlab="Time", ylab="Num of pregnancies")  
lines(modela.preg.run3[,1], npreg.modela.run3, col="blue") 

## lines(modelb.preg.run1[,1], npreg.modelb.run1, col="red")
## lines(modelb.preg.run2[,1], npreg.modelb.run2, col="red") 
lines(modelb.preg.16sep.run1[,1], npreg.modelb.16sep.run1, col="red")
lines(modelb.preg.16sep.run2[,1], npreg.modelb.16sep.run2, col="red") 

## lines(modelc.preg.run1[,1], npreg.modelc.run1, col="green")
## lines(modelc.preg.run2[,1], npreg.modelc.run2, col="green") 
lines(modelc.preg.16sep.run1[,1], npreg.modelc.16sep.run1, col="green")
lines(modelc.preg.16sep.run2[,1], npreg.modelc.16sep.run2, col="green") 


legend("topleft", c("Model a", "Model b",
                    "Model c"), col = c("blue", "red", "green"),
       lty=1,
       title = "")

summary(c(npreg.modela.run2, npreg.modela.run3))
summary(c(npreg.modelb.run1, npreg.modelb.run1))
summary(c(npreg.modelc.run1, npreg.modelc.run1))

summary(c(npreg.modelb.16sep.run1, npreg.modelb.16sep.run2)) #17Sep2013
summary(c(npreg.modelc.16sep.run1, npreg.modelc.16sep.run2)) #17Sep2013

summary(c(npreg.modelb.5d2.run1, npreg.modelb.5d2.run2)) #25Sep13
summary(c(npreg.modelc.preg.26sep.incfert.run1,
          npreg.modelc.preg.26sep.incfert.run2)) #25Sep13

####################################################################

####################################################################
## Scale by number of women to compute incidence rates
####################################################################

nfem.modela.run2 <- modela.run2[,5]
nfem.modela.run3 <- modela.run3[,5]

nfem.modelb.run1 <- modelb.run1[,5]
nfem.modelb.run2 <- modelb.run2[,5]

nfem.modelb.16sep.run1 <- modelb.16sep.run1[,5]
nfem.modelb.16sep.run2 <- modelb.16sep.run2[,5]

nfem.modelb.5d2.run1 <- modelb.5d2.run1[,5] #25Sep13
nfem.modelb.5d2.run2 <- modelb.5d2.run2[,5] #25Sep13

nfem.modelb.5d3.run1 <- modelb.5d3.run1[,5] #25Sep13
nfem.modelb.5d3.run2 <- modelb.5d3.run2[,5] #25Sep13

nfem.modelc.run1 <- modelc.run1[,5]
nfem.modelc.run2 <- modelc.run2[,5]

nfem.modelc.16sep.run1 <- modelc.16sep.run1[,5]
nfem.modelc.16sep.run2 <- modelc.16sep.run2[,5]

nfem.modelc.26sep.incfert.run1 <- modelc.26sep.incfert.run1[,5]#27Sep13
nfem.modelc.26sep.incfert.run2 <- modelc.26sep.incfert.run2[,5]#27Sep13

pregrate.modela.run2 <- npreg.modela.run2/nfem.modela.run2
pregrate.modela.run3 <- npreg.modela.run3/nfem.modela.run3

pregrate.modelb.run1 <- npreg.modelb.run1/nfem.modelb.run1
pregrate.modelb.run2 <- npreg.modelb.run2/nfem.modelb.run2

pregrate.modelb.16sep.run1 <- npreg.modelb.16sep.run1/nfem.modelb.16sep.run1
pregrate.modelb.16sep.run2 <- npreg.modelb.16sep.run2/nfem.modelb.16sep.run2

pregrate.modelb.5d2.run1 <- npreg.modelb.5d2.run1/nfem.modelb.5d2.run1
pregrate.modelb.5d2.run2 <- npreg.modelb.5d2.run2/nfem.modelb.5d2.run2

pregrate.modelb.5d3.run1 <- npreg.modelb.5d3.run1/nfem.modelb.5d3.run1
pregrate.modelb.5d3.run2 <- npreg.modelb.5d3.run2/nfem.modelb.5d3.run2

pregrate.modelc.run1 <- npreg.modelc.run1/nfem.modelc.run1
pregrate.modelc.run2 <- npreg.modelc.run2/nfem.modelc.run2

pregrate.modelc.16sep.run1 <- npreg.modelc.16sep.run1/nfem.modelc.16sep.run1
pregrate.modelc.16sep.run2 <- npreg.modelc.16sep.run2/nfem.modelc.16sep.run2

pregrate.modelc.26sep.incfert.run1 <-
npreg.modelc.preg.26sep.incfert.run1/nfem.modelc.26sep.incfert.run1
pregrate.modelc.26sep.incfert.run2 <-
npreg.modelc.preg.26sep.incfert.run2/nfem.modelc.26sep.incfert.run2


summary(c(pregrate.modela.run2, pregrate.modela.run3))
summary(c(pregrate.modelb.run1, pregrate.modelb.run2))
summary(c(pregrate.modelc.run1, pregrate.modelc.run2))

summary(c(pregrate.modelb.16sep.run1, pregrate.modelb.16sep.run2))
summary(c(pregrate.modelc.16sep.run1, pregrate.modelc.16sep.run2))

summary(c(pregrate.modelb.5d2.run1, pregrate.modelb.5d2.run2)) #25Sep13
summary(c(pregrate.modelb.5d3.run1, pregrate.modelb.5d3.run2)) #25Sep13

summary(c(pregrate.modelc.26sep.incfert.run1,
          pregrate.modelc.26sep.incfert.run2
          ))

summary(c(pregrate.modelb.16sep.run1))
summary(c(pregrate.modelb.16sep.run2))
summary(c(pregrate.modelc.16sep.run1))
summary(c(pregrate.modelc.16sep.run2))

ign.init <- 1:500 #to ignore few entries at home
summary(c(pregrate.modelb.16sep.run1[-c(ign.init)],
          pregrate.modelb.16sep.run2[-c(ign.init)]))
summary(c(pregrate.modelc.16sep.run1[-c(ign.init)],
          pregrate.modelc.16sep.run2[-c(ign.init)]))

summary(c(pregrate.modela.run2[1000:length(pregrate.modela.run2)],
          pregrate.modela.run3[1000:length(pregrate.modela.run2)]))

plot(modela.preg.run2[,1], pregrate.modela.run2, type="l", col="blue", #ylim=c(0, 0.2),
     xlab="Time", ylab="Pregnancy Rate")  
lines(modela.preg.run3[,1], pregrate.modela.run3, col="blue") 

## lines(modelb.preg.run1[,1], pregrate.modelb.run1, col="red")
## lines(modelb.preg.run2[,1], pregrate.modelb.run2, col="red") 
lines(modelb.preg.16sep.run1[,1], pregrate.modelb.16sep.run1, col="red")
lines(modelb.preg.16sep.run2[,1], pregrate.modelb.16sep.run2, col="red") 

## lines(modelc.preg.run1[,1], pregrate.modelc.run1, col="green")
## lines(modelc.preg.run2[,1], pregrate.modelc.run2, col="green")
## lines(modelc.preg.16sep.run1[,1], pregrate.modelc.16sep.run1, col="green")
## lines(modelc.preg.16sep.run2[,1], pregrate.modelc.16sep.run2, col="green")
lines(modelc.preg.26sep.incfert.run1[,1],
      pregrate.modelc.26sep.incfert.run1, col="green")
lines(modelc.preg.26sep.incfert.run2[,1],
      pregrate.modelc.26sep.incfert.run2, col="green")
## 27Sep13:
## "modelc.preg.26sep.incfert.run1" AND "modelc.preg.26sep.incfert.run2"
## have fertility rates artifically increased  and are therefore 
## different from the models a and b above, which have fertility rates as
## specified in the data.

####################################################################

####################################################################
## 23 Sep 2013: Check age distribution of women at end to see
## if pregnancy summaries make sense
####################################################################
library(ergm)
library(tergm)
library(networkDynamic)
source("common.functions_d8.R")

## run 1
load("simulation.16Sep13_10pc_3phi_immconc_wocorrn_recnumpregtest_testbinompregob_1300steps_run1.RData")

net.1300.wocorrn.run1 <- network.extract(nw, at=time)
net.1300.wocorrn.run1

fem.1300.wocorrn.run1 <- nwmodes(net.1300.wocorrn.run1, 2)
net.1300.wocorrn.run1%v%"age"
(get.vertex.attribute(net.1300.wocorrn.run1, "age"))[nw%n%'bipartite']

fem.age.1300.run1 <- ((get.vertex.attribute(net.1300.wocorrn.run1, "age"))
                      [(net.1300.wocorrn.run1%n%'bipartite'+1):
                       (net.1300.wocorrn.run1%n%'n')])

summary(fem.age.1300.run1)

num.fem.ageclass.1300.run1 <-   c(length(which(fem.age.1300.run1 <= 19)),
                                length(which(fem.age.1300.run1 > 19 &
                                             fem.age.1300.run1 <= 24)),
                                length(which(fem.age.1300.run1 > 24 &
                                             fem.age.1300.run1 <= 29)),
                                length(which(fem.age.1300.run1 > 29 &
                                             fem.age.1300.run1 <= 34)),
                                length(which(fem.age.1300.run1 > 34 &
                                             fem.age.1300.run1 <= 39)),
                                length(which(fem.age.1300.run1 > 39 &
                                             fem.age.1300.run1 <= 44)),
                                length(which(fem.age.1300.run1 > 44 &
                                             fem.age.1300.run1 <= 49)),
                                length(which(fem.age.1300.run1 > 50 &
                                             fem.age.1300.run1 <= 54))
                                  )

length(which(fem.age.1300.run1 > 54))/length(fem.age.1300.run1)

prop.fem.ageclass.1300.run1 <- num.fem.ageclass.1300.run1/length(fem.age.1300.run1)
prop.fem.ageclass.1300.run1

prop.fem.ageclass.1300.run1.ignorelastage <-
  num.fem.ageclass.1300.run1/length(which(fem.age.1300.run1 <= 49))

prop.fem.ageclass.1300.run1
prop.fem.ageclass.1300.run1.ignorelastage

data.fertility.by.ageclass <-
  c(174.967,
    345.9442,
    320.1459,
    266.055,
    184.28,
    79.91913,
    36.50134,
    0
    )

sum(data.fertility.by.ageclass*prop.fem.ageclass.1300.run1) 
## this calculation assumes everyone is uninfected
sum(data.fertility.by.ageclass*prop.fem.ageclass.1300.run1.ignorelastage) 
## this calculation assumes everyone is uninfected

## adjust for infection
## net.1300.wocorrn.run1%v%"age"
fem.inf.status.1300.run1 <- ((get.vertex.attribute(net.1300.wocorrn.run1, "inf.status"))
                             [(net.1300.wocorrn.run1%n%'bipartite'+1):
                              (net.1300.wocorrn.run1%n%'n')])

fem.inf.summary <- table(fem.inf.status.1300.run1)/sum(table(fem.inf.status.1300.run1))

data.fertility.by.ageclass.infadjusted <- data.fertility.by.ageclass*fem.inf.summary[1]+
  data.fertility.by.ageclass*0.53*fem.inf.summary[2]

sum(data.fertility.by.ageclass.infadjusted*prop.fem.ageclass.1300.run1)
sum(data.fertility.by.ageclass.infadjusted*prop.fem.ageclass.1300.run1.ignorelastage) 

## run 2
load("simulation.16Sep13_10pc_3phi_immconc_wocorrn_recnumpregtest_testbinompregob_1300steps_run2.RData")

net.1300.wocorrn.run2 <- network.extract(nw, at=time)
net.1300.wocorrn.run2

fem.1300.wocorrn.run2 <- nwmodes(net.1300.wocorrn.run2, 2)
net.1300.wocorrn.run2%v%"age"
(get.vertex.attribute(net.1300.wocorrn.run2, "age"))[nw%n%'bipartite']

fem.age.1300.run2 <- ((get.vertex.attribute(net.1300.wocorrn.run2, "age"))
                      [(net.1300.wocorrn.run2%n%'bipartite'+1):
                       (net.1300.wocorrn.run2%n%'n')])

summary(fem.age.1300.run2)

num.fem.ageclass.1300.run2 <-   c(length(which(fem.age.1300.run2 <= 19)),
                                length(which(fem.age.1300.run2 > 19 &
                                             fem.age.1300.run2 <= 24)),
                                length(which(fem.age.1300.run2 > 24 &
                                             fem.age.1300.run2 <= 29)),
                                length(which(fem.age.1300.run2 > 29 &
                                             fem.age.1300.run2 <= 34)),
                                length(which(fem.age.1300.run2 > 34 &
                                             fem.age.1300.run2 <= 39)),
                                length(which(fem.age.1300.run2 > 39 &
                                             fem.age.1300.run2 <= 44)),
                                length(which(fem.age.1300.run2 > 44 &
                                             fem.age.1300.run2 <= 49)),
                                length(which(fem.age.1300.run2 > 50 &
                                             fem.age.1300.run2 <= 54))
                                  )

length(which(fem.age.1300.run2 > 54))/length(fem.age.1300.run2)

prop.fem.ageclass.1300.run2 <- num.fem.ageclass.1300.run2/length(fem.age.1300.run2)
prop.fem.ageclass.1300.run2

prop.fem.ageclass.1300.run2.ignorelastage <-
  num.fem.ageclass.1300.run2/length(which(fem.age.1300.run2 <= 49))
prop.fem.ageclass.1300.run2.ignorelastage

sum(data.fertility.by.ageclass*prop.fem.ageclass.1300.run2)
sum(data.fertility.by.ageclass*prop.fem.ageclass.1300.run2.ignorelastage)

## this calculation assumes everyone is uninfected

## adjust for infection
## net.1300.wocorrn.run1%v%"age"
fem.inf.status.1300.run2 <- ((get.vertex.attribute(net.1300.wocorrn.run2, "inf.status"))
                             [(net.1300.wocorrn.run2%n%'bipartite'+1):
                              (net.1300.wocorrn.run2%n%'n')])

fem.inf.summary <- table(fem.inf.status.1300.run2)/sum(table(fem.inf.status.1300.run2))

data.fertility.by.ageclass.infadjusted <- data.fertility.by.ageclass*fem.inf.summary[1]+
  data.fertility.by.ageclass*0.53*fem.inf.summary[2]

sum(data.fertility.by.ageclass.infadjusted*prop.fem.ageclass.1300.run2)
sum(data.fertility.by.ageclass.infadjusted*prop.fem.ageclass.1300.run2.ignorelastage)

##############################################################################

##############################################################################
### Model b from 29 Sep 2013
##############################################################################

modelb.29sep.5d2.run1 <- read.csv("29Sep13_10pc_3phi_1300steps_run1.prev.inc.data.csv", header=FALSE, sep=" ")
modelb.29sep.5d2.run2 <- read.csv("29Sep13_10pc_3phi_1300steps_run2.prev.inc.data.csv", header=FALSE, sep=" ") #17Sep2013

modelb.preg.29sep.5d2.run1 <- read.csv("29Sep13_10pc_3phi_1300steps_run1.preg.csv", header=FALSE, sep=" ")
modelb.preg.29sep.5d2.run2 <- read.csv("29Sep13_10pc_3phi_1300steps_run2.preg.csv", header=FALSE, sep=" ")

prev.modelb.29sep.5d2.run1 <- modelb.29sep.5d2.run1[,8]
prev.modelb.29sep.5d2.run2 <- modelb.29sep.5d2.run2[,8]

npreg.modelb.29sep.5d2.run1 <- modelb.preg.29sep.5d2.run1[,2]
npreg.modelb.29sep.5d2.run2 <- modelb.preg.29sep.5d2.run2[,2]

nfem.modelb.29sep.5d2.run1 <- modelb.29sep.5d2.run1[,5]
nfem.modelb.29sep.5d2.run2 <- modelb.29sep.5d2.run2[,5]

pregrate.modelb.29sep.5d2.run1 <- npreg.modelb.29sep.5d2.run1/nfem.modelb.29sep.5d2.run1
pregrate.modelb.29sep.5d2.run2 <- npreg.modelb.29sep.5d2.run2/nfem.modelb.29sep.5d2.run2

summary(c(pregrate.modelb.29sep.5d2.run1, pregrate.modelb.29sep.5d2.run2))*26
##############################################################################
