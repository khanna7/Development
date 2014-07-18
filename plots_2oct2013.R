## 1 Oct 2013: Exposure results for Dobromir

## 4 Sep 2013: analyze prevalence/demography data for WIP

rm(list=ls())

###################################################
### Prevalence Analysis
###################################################

diagstr0.run1 <- read.csv("1Oct13_inc_fert_rate_wagemix_diagstrzero_1300steps_run1.prev.inc.data.csv",
                          header=FALSE, sep=" ")
diagstr0.run2 <- read.csv("1Oct13_inc_fert_rate_wagemix_diagstrzero_1300steps_run2.prev.inc.data.csv",
                 header=FALSE, sep=" ")


uplow.run1 <- read.csv("1Oct13_inc_fert_rate_wagemix_uptrlowtr_1300steps_run1.prev.inc.data.csv",
                          header=FALSE, sep=" ")
uplow.run2 <- read.csv("1Oct13_inc_fert_rate_wagemix_uptrlowtr_1300steps_run2.prev.inc.data.csv",
                          header=FALSE, sep=" ")



uplow.agerest.run1 <- read.csv("1Oct13_inc_fert_rate_wagemix_uptrlowtr_durn15to55_1300steps_run1.prev.inc.data.csv",
                          header=FALSE, sep=" ")

uplow.agerest.run2 <- read.csv("1Oct13_inc_fert_rate_wagemix_uptrlowtr_durn15to55_1300steps_run2.prev.inc.data.csv", header=FALSE, sep=" ")


timestep <- diagstr0.run1[,1]

prev.diagstr0.run1 <- diagstr0.run1[,8]
prev.diagstr0.run2 <- diagstr0.run2[,8]

prev.uplow.run1 <- uplow.run1[,8]
prev.uplow.run2 <- uplow.run2[,8]

prev.uplow.agerest.run1 <- uplow.agerest.run1[,8]
prev.uplow.agerest.run2 <- uplow.agerest.run2[,8]

inc.diagstr0.run1 <- diagstr0.run1[,6]
inc.diagstr0.run2 <- diagstr0.run2[,6]

popsize.diagstr0.run1 <- diagstr0.run1[,2]
popsize.diagstr0.run2 <- diagstr0.run2[,2]


####################################################
### Plots
####################################################

### Prevalence
#pdf(file="prev_wexposurecounts_1oct2013.pdf") 

pdf(file="prev_agemixing_4oct2013.pdf") 
plot(timestep, prev.diagstr0.run1, type="l", col="red", ylim=c(0, 0.5),
     xlab="Time", ylab="Prevalence") # prevalence plot
lines(diagstr0.run2[,1], prev.diagstr0.run2, type="l", col="red")

lines(uplow.run1[,1], prev.uplow.run1, col="blue", 
     ) # prevalence plot
lines(uplow.run2[,1], prev.uplow.run2, type="l", col="blue")

lines(uplow.agerest.run1[,1], prev.uplow.agerest.run1,
      col="green", 
     ) # prevalence plot
lines(uplow.agerest.run2[,1], prev.uplow.agerest.run2, type="l", col="green")



legend("topleft",
       c("Diagonals and Structural Zeros",
         "Upper and Lower Triangular Matrices", 
         "Upper and Lower Triangular Matrices w/ Restricted age durations" 
         ),
       col = c("red", "blue", "green"),
       lty=1,
       title = "")

dev.off()

### Population size
#pdf(file="popsize_wexposurecounts_1oct2013.pdf") 
plot(timestep, popsize.diagstr0.run1, type="l", col="blue", ylim=c(0, 10e3),
     xlab="Time", ylab="Population Size") # Population Size
lines(diagstr0.run2[,1], popsize.diagstr0.run2, type="l", col="red")
legend("topleft", c("Run 1", "Run 2"), col = c("blue", "red"),
       lty=1,
       title = "")
dev.off()



####################################################


####################################################
### Incidence Rate
####################################################

incirate.diagstr0.run1 <- diagstr0.run1[,6]/diagstr0.run1[,2]
incirate.diagstr0.run2 <- diagstr0.run2[,6]/diagstr0.run2[,2]

incirate.uplow.agerest.run1 <- uplow.agerest.run1[,6]/uplow.agerest.run1[,2]
incirate.uplow.agerest.run2 <- uplow.agerest.run2[,6]/uplow.agerest.run2[,2]

summary(incirate.diagstr0.run1)
summary(incirate.diagstr0.run2)

summary(incirate.uplow.agerest.run1)
summary(incirate.uplow.agerest.run2)

####################################################

###################################################
### Pregnancy Analysis
###################################################

preg.diagstr0.run1 <- read.csv("1Oct13_inc_fert_rate_wagemix_diagstrzero_1300steps_run1.preg.csv",
                          header=FALSE, sep=" ")
preg.diagstr0.run2 <- read.csv("1Oct13_inc_fert_rate_wagemix_diagstrzero_1300steps_run2.preg.csv",
                          header=FALSE, sep=" ")

preg.uplow.agerest.run1 <- read.csv("1Oct13_inc_fert_rate_wagemix_uptrlowtr_durn15to55_1300steps_run1.preg.csv",
                          header=FALSE, sep=" ")

preg.uplow.agerest.run2 <- read.csv("1Oct13_inc_fert_rate_wagemix_uptrlowtr_durn15to55_1300steps_run2.preg.csv", header=FALSE, sep=" ")



npreg.diagstr0.run1 <- preg.diagstr0.run1[,2]
npreg.diagstr0.run2 <- preg.diagstr0.run2[,2]
nfem.diagstr0.run1 <- diagstr0.run1[,5]
nfem.diagstr0.run2 <- diagstr0.run2[,5]


npreg.uplow.agerest.run1 <- preg.uplow.agerest.run1[,2]
npreg.uplow.agerest.run2 <- preg.uplow.agerest.run2[,2]
nfem.uplow.agerest.run1 <- uplow.agerest.run1[,5]
nfem.uplow.agerest.run2 <- uplow.agerest.run2[,5]

summary(npreg.diagstr0.run1) #mean number of pregnancies per timestep
summary(npreg.diagstr0.run2)
summary(npreg.uplow.agerest.run1)
summary(npreg.uplow.agerest.run2)

summary(npreg.diagstr0.run1/nfem.diagstr0.run1) #mean # preg. per timestep per woman
summary(npreg.diagstr0.run2/nfem.diagstr0.run2)
summary(npreg.uplow.agerest.run1/nfem.uplow.agerest.run1)
summary(npreg.uplow.agerest.run2/nfem.uplow.agerest.run2)


###################################################
### Plot proportion of infected on treatment
###################################################

ninf.diagstr0.run1 <- diagstr0.run1[,7]
ninf.diagstr0.run2 <- diagstr0.run2[,7]
nart.diagstr0.run1 <- diagstr0.run1[,11]
nart.diagstr0.run2 <- diagstr0.run2[,11]

ninf.uplow.run1 <- uplow.run1[,7]
ninf.uplow.run2 <- uplow.run2[,7]
nart.uplow.run1 <- uplow.run1[,11]
nart.uplow.run2 <- uplow.run2[,11]

ninf.uplow.agerest.run1 <- uplow.agerest.run1[,7]
ninf.uplow.agerest.run2 <- uplow.agerest.run2[,7]
nart.uplow.agerest.run1 <- uplow.agerest.run1[,11]
nart.uplow.agerest.run2 <- uplow.agerest.run2[,11]


pdf(file="prop_art_4oct2013_revised.pdf") 

plot(timestep, nart.diagstr0.run1/ninf.diagstr0.run1,
     type="l", col="red", ylim=c(0, 1),
     xlab="Time", ylab="Prevalence") # prevalence plot
lines(nart.diagstr0.run2/ninf.diagstr0.run2, col="red")

lines(nart.uplow.run1/ninf.uplow.run1, col="blue")
lines(nart.uplow.run2/ninf.uplow.run2, col="blue")

lines(nart.uplow.agerest.run1/ninf.uplow.agerest.run1, col="green")
lines(nart.uplow.agerest.run2/ninf.uplow.agerest.run2, col="green")

legend("topleft",
       c("Diagonals and Structural Zeros",
         "Upper and Lower Triangular Matrices", 
         "Upper and Lower Triangular Matrices w/ Restricted age durations" 
         ),
       col = c("red", "blue", "green"),
       lty=1,
       title = "")

dev.off()


summary(nart.diagstr0.run1/ninf.diagstr0.run1, nart.diagstr0.run2/ninf.diagstr0.run2)
summary(nart.uplow.run1/ninf.uplow.run1, nart.uplow.run2/ninf.uplow.run2)
summary(nart.uplow.agerest.run1/ninf.uplow.agerest.run1,
        nart.uplow.agerest.run2/ninf.uplow.agerest.run2)

###################################################
### Plot prevalences
###################################################

diagstr0.age15to55.4oct13.run1 <- read.csv("4Oct13_inc_fert_rate_wagemix_diagstrzero_age15to55_1300steps_run1.prev.inc.data.csv", header=FALSE, sep=" ")
diagstr0.age15to55.4oct13.run2 <- read.csv("4Oct13_inc_fert_rate_wagemix_diagstrzero_age15to55_1300steps_run2.prev.inc.data.csv", header=FALSE, sep=" ")
           
prev.diagstr0.age15to55.4oct13.run1 <- diagstr0.age15to55.4oct13.run1[,8]
prev.diagstr0.age15to55.4oct13.run2 <- diagstr0.age15to55.4oct13.run2[,8]

pdf(file="prev_diagstr0_age15to55_4oct13.pdf")
plot(prev.diagstr0.age15to55.4oct13.run1, type="l", col="red", ylim=c(0, 0.5),
     xlab="Time", ylab="Prevalence",
     main="Age Mixing: Diag + Str Zero (Duration restricted ages)") # prevalence plot
lines(prev.diagstr0.age15to55.4oct13.run2, type="l", col="red")
dev.off()
