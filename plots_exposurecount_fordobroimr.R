## 1 Oct 2013: Exposure results for Dobromir

## 4 Sep 2013: analyze prevalence/demography data for WIP

rm(list=ls())

###################################################
### Analysis
###################################################

run1 <- read.csv("22Sep13_0phi_exposure_count_run1.prev.inc.data.csv",
                          header=FALSE, sep=" ")
run2 <- read.csv("22Sep13_0phi_exposure_count_run2.prev.inc.data.csv",
                 header=FALSE, sep=" ")

timestep <- run1[,1]

prev.run1 <- run1[,8]
prev.run2 <- run2[,8]

inc.run1 <- run1[,6]
inc.run2 <- run2[,6]

popsize.run1 <- run1[,2]
popsize.run2 <- run2[,2]


####################################################
### Plots
####################################################

### Prevalence
pdf(file="prev_wexposurecounts_1oct2013.pdf") 

plot(timestep, prev.run1, type="l", col="blue", ylim=c(0, 0.5),
     xlab="Time", ylab="Prevalence") # prevalence plot
lines(run2[,1], prev.run2, type="l", col="red")



legend("topleft", c("Diagonals+Structural0", ),
       col = c("red", ),
       lty=1,
       title = "")
dev.off()

### Population size
pdf(file="popsize_wexposurecounts_1oct2013.pdf") 
plot(timestep, popsize.run1, type="l", col="blue", ylim=c(0, 10e3),
     xlab="Time", ylab="Population Size") # Population Size
lines(run2[,1], popsize.run2, type="l", col="red")
legend("topleft", c("Run 1", "Run 2"), col = c("blue", "red"),
       lty=1,
       title = "")
dev.off()



####################################################


####################################################
### Counts of exposure dat
####################################################

expos.data.run1 <- read.csv("22Sep13_0phi_exposure_count_run1.expos.data.csv",
                            header=FALSE, sep=" ")

expos.data.run2 <- read.csv("22Sep13_0phi_exposure_count_run2.expos.data.csv",
                            header=FALSE, sep=" ")


time.expos.data.run1 <- intersect(which(expos.data.run1[,1] > 600),
                                  which(expos.data.run1[,1] < 626)
                                  )

time.expos.data.run2 <- intersect(which(expos.data.run2[,1] > 600),
                                  which(expos.data.run2[,1] < 626)
                                  )


all.exposed.time.run1 <- expos.data.run1[time.expos.data.run1 ,2]
unique.exposed.time.run1 <- unique(all.exposed.time.run1)

all.exposed.time.run2 <- expos.data.run2[time.expos.data.run2 ,2]
unique.exposed.time.run2 <- unique(all.exposed.time.run2)

time.expos.data.run1

length(unique.exposed.time.run1)
length(unique.exposed.time.run2)

sum(inc.run1[c(601:626)])
sum(inc.run2[c(601:626)])

popsize.run1[c(601:626)]
popsize.run2[c(601:626)]
