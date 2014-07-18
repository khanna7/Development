## 8 Oct 2013: Incidence counts over interventions 

## 1 Oct 2013: Exposure results for Dobromir

## 4 Sep 2013: analyze prevalence/demography data for WIP

rm(list=ls())

###################################################
### Incidence Analysis
###################################################

## Baseline

bl.run1 <- read.csv("7Oct13_blinterv_diagstrzero_age15to55_collapsenet_del_lgobj_260steps_run1.prev.inc.data.csv",
                  header=FALSE, sep=" ")
bl.run2 <- read.csv("7Oct13_blinterv_diagstrzero_age15to55_collapsenet_del_lgobj_260steps_run2.prev.inc.data.csv",
                  header=FALSE, sep=" ")
bl.run3 <- read.csv("7Oct13_blinterv_diagstrzero_age15to55_collapsenet_del_lgobj_260steps_run3.prev.inc.data.csv",
                  header=FALSE, sep=" ")
bl.run4 <- read.csv("7Oct13_blinterv_diagstrzero_age15to55_collapsenet_del_lgobj_260steps_run4.prev.inc.data.csv",
                  header=FALSE, sep=" ")
bl.run5 <- read.csv("7Oct13_blinterv_diagstrzero_age15to55_collapsenet_del_lgobj_260steps_run5.prev.inc.data.csv",
                  header=FALSE, sep=" ")

## run 6 only has one point

n.newinf.bl.run1 <- bl.run1[,6]
n.newinf.bl.run2 <- bl.run2[,6]
n.newinf.bl.run3 <- bl.run3[,6]
n.newinf.bl.run4 <- bl.run4[,6]
n.newinf.bl.run5 <- bl.run5[,6]

n.newinf.bl.run1.4yrs <- sum(n.newinf.bl.run1[1:104])
n.newinf.bl.run2.4yrs <- sum(n.newinf.bl.run2[1:104])
n.newinf.bl.run3.4yrs <- sum(n.newinf.bl.run3[1:104])
n.newinf.bl.run4.4yrs <- sum(n.newinf.bl.run4[1:104])
n.newinf.bl.run5.4yrs <- sum(n.newinf.bl.run5[1:104])

n.newinf.bl <- c(n.newinf.bl.run1.4yrs, n.newinf.bl.run2.4yrs,
                 n.newinf.bl.run3.4yrs, n.newinf.bl.run4.4yrs,
                 n.newinf.bl.run5.4yrs)
 
summary(n.newinf.bl)

popsize.bl.run1 <- bl.run1[,2]
popsize.bl.run2 <- bl.run2[,2]
popsize.bl.run3 <- bl.run3[,2]
popsize.bl.run4 <- bl.run4[,2]
popsize.bl.run5 <- bl.run5[,2]

inci.rate.bl.run1 <- n.newinf.bl.run1/popsize.bl.run1
inci.rate.bl.run2 <- n.newinf.bl.run2/popsize.bl.run2
inci.rate.bl.run3 <- n.newinf.bl.run3/popsize.bl.run3
inci.rate.bl.run4 <- n.newinf.bl.run4/popsize.bl.run4
inci.rate.bl.run5 <- n.newinf.bl.run5/popsize.bl.run5

inci.rate.bl.run1.4yrs <- inci.rate.bl.run1[1:104]
inci.rate.bl.run2.4yrs <- inci.rate.bl.run2[1:104]
inci.rate.bl.run3.4yrs <- inci.rate.bl.run3[1:104]
inci.rate.bl.run4.4yrs <- inci.rate.bl.run4[1:104]
inci.rate.bl.run5.4yrs <- inci.rate.bl.run5[1:104]

inci.rate.bl <- c(inci.rate.bl.run1.4yrs, inci.rate.bl.run2.4yrs,
                 inci.rate.bl.run3.4yrs, inci.rate.bl.run4.4yrs,
                 inci.rate.bl.run5.4yrs)

summary(inci.rate.bl)
summary(inci.rate.bl)*26

## Idealized

idinterv.run1 <- read.csv("7Oct13_idealizedinterv_diagstrzero_age15to55_collapsenet_del_lgobj_260steps_run1.prev.inc.data.csv",
                  header=FALSE, sep=" ")
idinterv.run2 <- read.csv("7Oct13_idealizedinterv_diagstrzero_age15to55_collapsenet_del_lgobj_260steps_run2.prev.inc.data.csv",
                  header=FALSE, sep=" ")
idinterv.run3 <- read.csv("7Oct13_idealizedinterv_diagstrzero_age15to55_collapsenet_del_lgobj_260steps_run3.prev.inc.data.csv",
                  header=FALSE, sep=" ")
idinterv.run4 <- read.csv("7Oct13_idealizedinterv_diagstrzero_age15to55_collapsenet_del_lgobj_260steps_run4.prev.inc.data.csv",
                  header=FALSE, sep=" ")
idinterv.run5 <- read.csv("7Oct13_idealizedinterv_diagstrzero_age15to55_collapsenet_del_lgobj_260steps_run5.prev.inc.data.csv",
                  header=FALSE, sep=" ") 


n.newinf.idinterv.run1 <- idinterv.run1[,6]
n.newinf.idinterv.run2 <- idinterv.run2[,6]
n.newinf.idinterv.run3 <- idinterv.run3[,6]
n.newinf.idinterv.run4 <- idinterv.run4[,6]
n.newinf.idinterv.run5 <- idinterv.run5[,6]

n.newinf.idinterv.run1.4yrs <- sum(n.newinf.idinterv.run1[1:104])
n.newinf.idinterv.run2.4yrs <- sum(n.newinf.idinterv.run2[1:104])
n.newinf.idinterv.run3.4yrs <- sum(n.newinf.idinterv.run3[1:104])
n.newinf.idinterv.run4.4yrs <- sum(n.newinf.idinterv.run4[1:104])
n.newinf.idinterv.run5.4yrs <- sum(n.newinf.idinterv.run5[1:104])

n.newinf.idinterv <- c(n.newinf.idinterv.run1.4yrs, n.newinf.idinterv.run2.4yrs,
                       n.newinf.idinterv.run3.4yrs, n.newinf.idinterv.run4.4yrs,
                       n.newinf.idinterv.run5.4yrs)
 
summary(n.newinf.idinterv)

popsize.idinterv.run1 <- idinterv.run1[,2]
popsize.idinterv.run2 <- idinterv.run2[,2]
popsize.idinterv.run3 <- idinterv.run3[,2]
popsize.idinterv.run4 <- idinterv.run4[,2]
popsize.idinterv.run5 <- idinterv.run5[,2]

inci.rate.idinterv.run1 <- n.newinf.idinterv.run1/popsize.idinterv.run1
inci.rate.idinterv.run2 <- n.newinf.idinterv.run2/popsize.idinterv.run2
inci.rate.idinterv.run3 <- n.newinf.idinterv.run3/popsize.idinterv.run3
inci.rate.idinterv.run4 <- n.newinf.idinterv.run4/popsize.idinterv.run4
inci.rate.idinterv.run5 <- n.newinf.idinterv.run5/popsize.idinterv.run5

inci.rate.idinterv.run1.4yrs <- inci.rate.idinterv.run1[1:104]
inci.rate.idinterv.run2.4yrs <- inci.rate.idinterv.run2[1:104]
inci.rate.idinterv.run3.4yrs <- inci.rate.idinterv.run3[1:104]
inci.rate.idinterv.run4.4yrs <- inci.rate.idinterv.run4[1:104]
inci.rate.idinterv.run5.4yrs <- inci.rate.idinterv.run5[1:104]

inci.rate.idinterv <- c(inci.rate.idinterv.run1.4yrs, inci.rate.idinterv.run2.4yrs,
                 inci.rate.idinterv.run3.4yrs, inci.rate.idinterv.run4.4yrs,
                 inci.rate.idinterv.run5.4yrs)

summary(inci.rate.idinterv)
summary(inci.rate.idinterv)*26
