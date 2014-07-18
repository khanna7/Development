###########################################################
### 23 Aug 13: Investigate recent runs of setting where
## life expectancy is decided by age at time of infection.
###########################################################

rm(list=ls())
load("simulation.22Aug13_dur_inf_by_age_1300steps_run2.RData")
    # run 1 did not complete

xtabs(~nw%v%"age" + nw%v%"dur.inf.by.age")

xtabs(~nw%v%"time.since.infection" + nw%v%"dur.inf.by.age")


table(nw%v%"dur.inf.by.age", useNA="always")
table(nw%v%"time.of.infection", useNA="always")



## check if new estimation objects have non-integer duration of infection by age.
load("estimation_uganda_d8.RData")
table(n0%v%"dur.inf.by.age", useNA="always") #no non integer values
table(n0%v%"time.of.infection", useNA="always")
table(n0%v%"time.since.infection", useNA="always")
table(n0%v%"age.at.infection", useNA="always")

inf.age.str <- xtabs(~factor(n0%v%"age.at.infection", exclude=NULL) +
                     factor(n0%v%"dur.inf.by.age", exclude=NULL)
                     )

inf.age.str.mat <- as.matrix(inf.age.str)
      
###########################################################

###########################################################
### 26 Aug 13: Investigate recent runs of setting where
### life expectancy is decided by age at time of infection
### and initial prevalence is either 6% or 10%.
###########################################################

rm(list=ls())

load("simulation.23Aug13_dur_inf_by_age_10pc_1300steps_run1.RData")

xtabs(~nw%v%"age" + nw%v%"dur.inf.by.age")
net.1300 <- network.extract(nw, at=1300)

summary(net.1300%v%"age")

xtabs(~ net.1300%v%"age" + net.1300%v%"inf.status" )
xtabs(~net.1300%v%"age" + net.1300%v%"dur.inf.by.age")

xtabs(~net.1300%v%"age" + net.1300%v%"dur.inf.by.age")
