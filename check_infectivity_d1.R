## 14 Aug 2013: Analyticcal calculations for infectivites of pregnant women.

library(tergm)
library(networkDynamic)
#############################################################
### 14 Aug 2013: Analytically 
#############################################################

min.ch.inf.unadj <- 0.000497/2.89
VL <- seq(0, 10, by=0.1)
AM <- 4.98
LM <- 3.49
n <- 2.4
PM <- 2.54

VL <- 5.185
ac.inf.today.1 <- min.ch.inf.unadj*2.89^(VL - 2)*AM
ac.inf.today.2 <- 1-(1-ac.inf.today.1)^n
ac.inf.today.3 <- ac.inf.today.2

plot(VL, ac.inf.today.3, type="l")
VL[which(ac.inf.today.3 > 1)]
summary(ac.inf.today.3)

la.inf.today.1 <- min.ch.inf.unadj*2.89^(VL - 2)*LM
la.inf.today.2 <- 1-(1-la.inf.today.1)^n
la.inf.today.3 <- la.inf.today.2*PM

plot(VL, la.inf.today.3, type="l")
VL[which(la.inf.today.3 > 1)]
summary(la.inf.today.3)


art.la.inf.today.1 <- min.ch.inf.unadj*2.89^(VL - 2)
art.la.inf.today.2 <- 1-(1-art.la.inf.today.1)^n
art.la.inf.today.3 <- art.la.inf.today.2*PM

plot(VL, art.la.inf.today.3, type="l")
VL[which(art.la.inf.today.3 > 1)]
summary(art.la.inf.today.3)


pdf(file="infectivies_pw.pdf")
plot(VL, ac.inf.today.3, type="l")
lines(VL, la.inf.today.3, type="l", col="blue")
lines(VL, art.la.inf.today.3, type="l", col="red")
legend("topleft", c("Acute", "Late", "Late, but on ART"),
       col = c("blue", "red"),
       lty=1,
       title = "")
dev.off()

#############################################################
### 15 Aug 2013: Through data simulated on 14 Aug 2013
#############################################################

rm(list=ls())
sim.data.14.8 <- load("simulation.14Aug13_sep_v31_fix_art_status_run1.RData")

summary(nw%v%"infectivity.today")
summary(nw%v%"viral.load.today")
summary(nw%v%"cd4.count.today")
table(nw%v%"art.status", useNA="always")
table(nw%v%"sex", useNA="always")
table((nw%v%"sex")[nw%v%"infector.ID"]) # this doesn't make sense because the
# the new males at any timestep get the ID's of the earliest women in the
# female vector. So 

nw.1 <- network.extract(nw, at=1)
table(nw.1%v%"inf.status", useNA="always")
summary(nw.1%v%"viral.load.today")
summary(nw.1%v%"infectivity.today")
table(nw.1%v%"art.status", useNA="always")
network.size(nw.1)
length(which(nw.1%v%"inf.status" == 1))
length(which(nw.1%v%"inf.status" == 1))/network.size(nw.1)


nw.10 <- network.extract(nw, at=10)
table(nw.10%v%"inf.status", useNA="always")
summary(nw.10%v%"viral.load.today")
summary(nw.10%v%"infectivity.today")
network.size(nw.10)
length(which(nw.10%v%"inf.status" == 1))
length(which(nw.10%v%"inf.status" == 1))/network.size(nw.10)

nw.50 <- network.extract(nw, at=50)
table(nw.50%v%"inf.status", useNA="always")
summary(nw.50%v%"viral.load.today")
summary(nw.50%v%"infectivity.today")
network.size(nw.50)
length(which(nw.50%v%"inf.status" == 1))/network.size(nw.50)

nw.200 <- network.extract(nw, at=200)
table(nw.200%v%"inf.status", useNA="always")
summary(nw.200%v%"viral.load.today")
network.size(nw.200)
length(which(nw.200%v%"inf.status" == 1))
length(which(nw.200%v%"inf.status" == 1))/network.size(nw.200)

nw.500 <- network.extract(nw, at=500)
table(nw.500%v%"inf.status", useNA="always")
table(nw.500%v%"art.status", useNA="always")
summary(nw.500%v%"viral.load.today")
network.size(nw.500)
length(which(nw.500%v%"inf.status" == 1))
length(which(nw.500%v%"art.status" == 1))
length(which(nw.500%v%"inf.status" == 1))/network.size(nw.500)

nw.1000 <- network.extract(nw, at=1000)
table(nw.1000%v%"inf.status", useNA="always")
table(nw.1000%v%"art.status", useNA="always")
summary(nw.1000%v%"viral.load.today")
network.size(nw.1000)
length(which(nw.1000%v%"inf.status" == 1))
length(which(nw.1000%v%"inf.status" == 1))/network.size(nw.1000)

nw.1300 <- network.extract(nw, at=1300)
table(nw.1300%v%"inf.status", useNA="always")
table(nw.1300%v%"art.status", useNA="always")
summary(nw.1300%v%"viral.load.today")
network.size(nw.1300)
length(which(nw.1300%v%"inf.status" == 1))
length(which(nw.1300%v%"inf.status" == 1))/network.size(nw.1300)

summary(n0%v%"infectivity.today")
summary(n0%v%"viral.load.today")
summary(n0%v%"cd4.count.today")
table(n0%v%"art.status", useNA="always")
table(n0%v%"sex", useNA="always")

