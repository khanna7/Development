## 14 August 2013: Plot 13 August runs

## 12 August 2013: Prevalence data from CSV files.

rm(list=ls())

###################################################
### Analysis
###################################################

run1 <- read.csv("11Aug13_dur.inf-based-death-limited-to-noART_d2.prev.inc.data.csv",
                 header=FALSE, sep=" ")
run2 <- read.csv("11Aug13_comp_dur.inf-based-death-limited-to-noART_d2.prev.inc.data.csv",
                 header=FALSE, sep=" ")



timestep <- run1[,1]
prev.run1 <- run1[,5]
prev.run2 <- run2[,5]
popsize.run1 <- run1[,2]
popsize.run2 <- run2[,2]

##par(mfrow=c(2,1))

pdf(file="results_12aug13.pdf")

plot(timestep, prev.run1, type="l", col="blue", ylim=c(0, 0.2),
     xlab="Time", ylab="Prevalence") # prevalence plot
lines(timestep, prev.run2[1:length(prev.run1)], type="l", col="red")
legend("topleft", c("Run 1", "Run 2"), col = c("blue", "red"),
       lty=1,
       title = "")

plot(timestep, popsize.run1, type="l", col="blue", ylim=c(5000, 10000),
     xlab="Time", ylab="Population Size") # popsize plot
lines(timestep, popsize.run2[1:length(popsize.run1)], type="l", col="red")
legend("topleft", c("Run 1", "Run 2"), col = c("blue", "red"),
       lty=1,
       title = "")


dev.off()

###################################################
### Analysis of data simulated on 13 Aug 2013
###################################################

run1.13aug.tstep <- read.csv("13Aug13_sep_v31_dur_tstep_run1.prev.inc.data.csv",
                             header=FALSE, sep=" ")
run2.13aug.tstep <- read.csv("13Aug13_sep_v31_dur_tstep_run2.prev.inc.data.csv",
                             header=FALSE, sep=" ")

run1.13aug.tdays <- read.csv("13Aug13_sep_v31_run1.prev.inc.data.csv",
                             header=FALSE, sep=" ")
run2.13aug.tdays <- read.csv("13Aug13_sep_v31_run2.prev.inc.data.csv",
                             header=FALSE, sep=" ")

timestep <- run1.13aug.tstep[,1]

prev.tstep.run1 <- run1.13aug.tstep[,8]
prev.tstep.run2 <- run2.13aug.tstep[,8]
prev.tdays.run1 <- run1.13aug.tdays[,8]
prev.tdays.run2 <- run2.13aug.tdays[,8]

popsize.tstep.run1 <- run1.13aug.tstep[,2]
popsize.tstep.run2 <- run2.13aug.tstep[,2]
popsize.tdays.run1 <- run1.13aug.tdays[,2]
popsize.tdays.run2 <- run2.13aug.tdays[,2]

pdf(file="results_tstep_13aug13.pdf")
par(mfrow=c(2,1))

plot(timestep, prev.tstep.run1, type="l", col="blue", ylim=c(0, 0.2),
     xlab="Time Steps (14 = 1 day)", ylab="Prevalence",
     main="Prevalence (Duration: 300 timesteps)") # prevalence plot
lines(timestep, prev.tstep.run2[1:length(prev.tstep.run1)], type="l", col="red")
legend("topleft", c("Run 1", "Run 2"), col = c("blue", "red"),
       lty=1,
       title = "")

plot(timestep, popsize.tstep.run1, type="l", col="blue", ylim=c(5000, 10000),
     xlab="Time Steps (14 = 1 day)", ylab="Population Size",
     main="Prevalence (Duration: 300 timesteps)") # popsize plot
lines(timestep, popsize.tstep.run2[1:length(popsize.tstep.run1)], type="l", col="red")
legend("topleft", c("Run 1", "Run 2"), col = c("blue", "red"),
       lty=1,
       title = "")
dev.off()

par(mfrow=c(2,1))

pdf(file="results_tdays_13aug13.pdf")
plot(timestep, prev.tdays.run1, type="l", col="blue", ylim=c(0, 0.2),
     xlab="Days", ylab="Prevalence",
     main="Prevalence (Duration: 4300 days)") # prevalence plot
lines(timestep, prev.tdays.run2,#[1:length(prev.tdays.run1)],
      type="l", col="red")
legend("topleft", c("Run 1", "Run 2"), col = c("blue", "red"),
       lty=1,
       title = "")

plot(timestep, popsize.tdays.run1, type="l", col="blue", ylim=c(5000, 10000),
     xlab="Days", ylab="Population Size",
     main="Prevalence (Duration: 4300 days)") # popsize plot
lines(timestep, popsize.tdays.run2,#[1:length(popsize.tdays.run1)], type="l", col="red")
      type="l", col="red")
legend("topleft", c("Run 1", "Run 2"), col = c("blue", "red"),
       lty=1,
       title = "")

dev.off()

###################################################
### Analysis of data simulated on 14 Aug 2013
###################################################

run1.14aug.tstep <- read.csv("14Aug13_sep_v31_fix_art_status_run1.prev.inc.data.csv",
                             header=FALSE, sep=" ")
run2.14aug.tstep <- read.csv("14Aug13_sep_v31_fix_art_status_run2.prev.inc.data.csv",
                             header=FALSE, sep=" ")

timestep <- run1.14aug.tstep[,1]

prev.tstep.14aug.run1 <- run1.14aug.tstep[,8]
prev.tstep.14aug.run2 <- run2.14aug.tstep[,8]

popsize.tstep.14aug.run1 <- run1.14aug.tstep[,2]
popsize.tstep.14aug.run2 <- run2.14aug.tstep[,2]

pdf(file="results_tstep_14aug13.pdf")
par(mfrow=c(2,1))

plot(timestep, prev.tstep.14aug.run1, type="l", col="blue", ylim=c(0, 0.2),
     xlab="Time Steps (14 = 1 day)", ylab="Prevalence",
     main="Prevalence (Duration: 300 timesteps)") # prevalence plot
##lines(timestep, prev.tstep.14aug.run2,
      type="l", col="red")
legend("topleft", c("Run 1", "Run 2"), col = c("blue", "red"),
       lty=1,
       title = "")

plot(timestep, popsize.tstep.run1, type="l", col="blue", ylim=c(5000, 10000),
     xlab="Time Steps (14 = 1 day)", ylab="Population Size",
     main="Prevalence (Duration: 300 timesteps)") # popsize plot
##lines(timestep, popsize.tstep.run2[1:length(popsize.tstep.run1)], type="l", col="red")
legend("topleft", c("Run 1", "Run 2"), col = c("blue", "red"),
       lty=1,
       title = "")
dev.off()

par(mfrow=c(2,1))

pdf(file="results_tdays_13aug13.pdf")
plot(timestep, prev.tdays.run1, type="l", col="blue", ylim=c(0, 0.2),
     xlab="Days", ylab="Prevalence",
     main="Prevalence (Duration: 4300 days)") # prevalence plot
lines(timestep, prev.tdays.run2,#[1:length(prev.tdays.run1)],
      type="l", col="red")
legend("topleft", c("Run 1", "Run 2"), col = c("blue", "red"),
       lty=1,
       title = "")

plot(timestep, popsize.tdays.run1, type="l", col="blue", ylim=c(5000, 10000),
     xlab="Days", ylab="Population Size",
     main="Prevalence (Duration: 4300 days)") # popsize plot
lines(timestep, popsize.tdays.run2,#[1:length(popsize.tdays.run1)], type="l", col="red")
      type="l", col="red")
legend("topleft", c("Run 1", "Run 2"), col = c("blue", "red"),
       lty=1,
       title = "")

dev.off()
