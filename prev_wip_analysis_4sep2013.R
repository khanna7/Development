## 4 Sep 2013: analyze prevalence/demography data for WIP

rm(list=ls())

###################################################
### Analysis
###################################################

diagstr0.run1 <- read.csv(3"1Oct13_inc_fert_rate_wagemix_diagstrzero_1300steps_run1.prev.inc.data.csv",header=FALSE, sep=" ")
diagstr0.run2 <- read.csv("3Sep13_10pc_3phi_testbinompregob_1300steps_run2.prev.inc.data.csv",header=FALSE, sep=" ")

timestep <- run1[,1]
prev.run1 <- run1[,8]
prev.run2 <- run2[,8]
popsize.run1 <- run1[,2]
popsize.run2 <- run2[,2]

####################################################
### Plots
####################################################

### Prevalence
pdf(file="prev_results_3sep13.pdf") 
plot(timestep, prev.run1, type="l", col="blue", ylim=c(0, 0.2),
     xlab="Time", ylab="Prevalence") # prevalence plot
lines(run2[,1], prev.run2, type="l", col="red")
legend("topleft", c("Run 1", "Run 2"), col = c("blue", "red"),
       lty=1,
       title = "")
dev.off()

### Population size
pdf(file="popsize_results_3sep13.pdf") 
plot(timestep, popsize.run1, type="l", col="blue", ylim=c(0, 10e3),
     xlab="Time", ylab="Population Size") # Population Size
lines(run2[,1], popsize.run2, type="l", col="red")
legend("topleft", c("Run 1", "Run 2"), col = c("blue", "red"),
       lty=1,
       title = "")
dev.off()



####################################################
