## 31 July 2013: Crude analysis of prevalence data

###################################################
### Analysis
###################################################

rm(list=ls())

library(tergm)
load(file="jul30_448timesteps.RData")

## Extract relevant networks

seq.of.networks <- seq(48, 448, by=10) #time steps 48 to 448 of interest
net.size <- rep(NA, length(seq.of.networks))
n.infected <- net.size

## extract cross sectional networks at time steps 48 to 448,
for (i in seq.of.networks){
  assign(paste("net.",i, sep=""), network.extract(nw, at=i))
}


## Record network size information
for (i in seq.of.networks){
  net.size[i] <- network.size(get(paste("net.",i, sep="")))
  ## get function is to treat the string "net.i" as a variable
}

## Record number of infecteds
for (i in seq.of.networks){
  n.infected[i] <- length(which((get(paste("net.",i, sep="")))%v%"inf.status" == 1))
  ## get function is to treat the string "net.i" as a variable
}


## net.size <- net.size[which(!is.na(net.size))]

(max(net.size, na.rm=TRUE) - min(net.size, na.rm=TRUE))/(448-48)
## Population grew by 16 every time step (i.e. 14 days) on average
## corresponds to a 16*24=384 new individuals per year.

## plot popsize
pdf(file="popsize_30jul2013.pdf")
plot(net.size, main="Cross-sectional population size",
     xlab="Week")
dev.off()

## plot prevalence
prev <- n.infected/net.size

pdf(file="prevalence_30jul2013.pdf")
plot(prev, main="Cross-sectional population size",
     xlab="Week")
dev.off()

###################################################

load(file="aug1_sim.RData")

seq.of.networks <- seq(1, 300, by=10) #time steps 48 to 448 of interest
net.size <- rep(NA, length(seq.of.networks))
n.infected <- net.size

## extract cross sectional networks at time steps 48 to 448,
for (i in seq.of.networks){
  assign(paste("net.",i, sep=""), network.extract(nw, at=i))
}


## Record network size information
for (i in seq.of.networks){
  net.size[i] <- network.size(get(paste("net.",i, sep="")))
  ## get function is to treat the string "net.i" as a variable
}

## Record number of infecteds
for (i in seq.of.networks){
  n.infected[i] <- length(which((get(paste("net.",i, sep="")))%v%"inf.status" == 1))
  ## get function is to treat the string "net.i" as a variable
}


## net.size <- net.size[which(!is.na(net.size))]

(max(net.size, na.rm=TRUE) - min(net.size, na.rm=TRUE))/(448-48)
## Population grew by 16 every time step (i.e. 14 days) on average
## corresponds to a 16*24=384 new individuals per year.

## plot popsize
pdf(file="popsize_30jul2013.pdf")
plot(net.size, main="Cross-sectional population size",
     xlab="Week")
dev.off()

## plot prevalence
prev <- n.infected/net.size

par(mfrow=c(2,1))
pdf(fil="initial_data.pdf")
plot(net.size, main="Cross-sectional population size",
     xlab="Week")

pdf(file="aug1_prev.pdf")
plot(prev, main="Preavalence",
     xlab="Week", ylim=c(0, 0.2)
     )
dev.off()

###################################################
### 5 August simulation
###################################################
load(file="simulation.5Aug13.RData")

seq.of.networks <- seq(1, 300, by=10) #time steps 48 to 448 of interest
net.size <- rep(NA, length(seq.of.networks))
n.infected <- net.size

## extract cross sectional networks at time steps 48 to 448,
for (i in seq.of.networks){
  assign(paste("net.",i, sep=""), network.extract(nw, at=i))
}

## Record network size information
for (i in seq.of.networks){
  net.size[i] <- network.size(get(paste("net.",i, sep="")))
  ## get function is to treat the string "net.i" as a variable
}

## Record number of infecteds
for (i in seq.of.networks){
  n.infected[i] <- length(which((get(paste("net.",i, sep="")))%v%"inf.status" == 1))
  ## get function is to treat the string "net.i" as a variable
}


## net.size <- net.size[which(!is.na(net.size))]

(max(net.size, na.rm=TRUE) - min(net.size, na.rm=TRUE))/(448-48)
## Population grew by 16 every time step (i.e. 14 days) on average
## corresponds to a 16*24=384 new individuals per year.

## plot popsize
pdf(file="popsize_30jul2013.pdf")
plot(net.size, main="Cross-sectional population size",
     xlab="Week")
dev.off()

## plot prevalence
prev <- n.infected/net.size

par(mfrow=c(2,1))
pdf(fil="initial_data.pdf")
plot(net.size, main="Cross-sectional population size",
     xlab="Week")

pdf(file="aug1_prev.pdf")
plot(prev, main="Preavalence",
     xlab="Week", ylim=c(0, 0.2)
     )
dev.off()


load(file="aug1_sim.RData")

seq.of.networks <- seq(1, 300, by=10) #time steps 48 to 448 of interest
net.size <- rep(NA, length(seq.of.networks))
n.infected <- net.size

## extract cross sectional networks at time steps 48 to 448,
for (i in seq.of.networks){
  assign(paste("net.",i, sep=""), network.extract(nw, at=i))
}


## Record network size information
for (i in seq.of.networks){
  net.size[i] <- network.size(get(paste("net.",i, sep="")))
  ## get function is to treat the string "net.i" as a variable
}

## Record number of infecteds
for (i in seq.of.networks){
  n.infected[i] <- length(which((get(paste("net.",i, sep="")))%v%"inf.status" == 1))
  ## get function is to treat the string "net.i" as a variable
}


## net.size <- net.size[which(!is.na(net.size))]

(max(net.size, na.rm=TRUE) - min(net.size, na.rm=TRUE))/(448-48)
## Population grew by 16 every time step (i.e. 14 days) on average
## corresponds to a 16*24=384 new individuals per year.

## plot popsize
pdf(file="popsize_30jul2013.pdf")
plot(net.size, main="Cross-sectional population size",
     xlab="Week")
dev.off()

## plot prevalence
prev <- n.infected/net.size

par(mfrow=c(2,1))
pdf(fil="initial_data.pdf")
plot(net.size, main="Cross-sectional population size",
     xlab="Week")

pdf(file="aug1_prev.pdf")
plot(prev, main="Preavalence",
     xlab="Week", ylim=c(0, 0.2)
     )
dev.off()

###################################################
### 5 August simulation
###################################################
load(file="simulation.5Aug13.RData")

seq.of.networks <- seq(1, 1000, by=10) #time steps 48 to 448 of interest
net.size <- rep(NA, length(seq.of.networks))
n.infected <- net.size

## extract cross sectional networks at time steps 48 to 448,
for (i in seq.of.networks){
  assign(paste("net.",i, sep=""), network.extract(nw, at=i))
}

## Record network size information
for (i in seq.of.networks){
  net.size[i] <- network.size(get(paste("net.",i, sep="")))
  ## get function is to treat the string "net.i" as a variable
}

## Record number of infecteds
for (i in seq.of.networks){
  n.infected[i] <- length(which((get(paste("net.",i, sep="")))%v%"inf.status" == 1))
  ## get function is to treat the string "net.i" as a variable
}


## net.size <- net.size[which(!is.na(net.size))]

(max(net.size, na.rm=TRUE) - min(net.size, na.rm=TRUE))/(448-48)
## Population grew by 16 every time step (i.e. 14 days) on average
## corresponds to a 16*24=384 new individuals per year.

## plot popsize
pdf(file="popsize_6aug2013.pdf")
plot(net.size, main="Cross-sectional population size",
     ylim=c(0, 7000),
     xlab="Week")
dev.off()

prev <- n.infected/net.size
pdf(file="prev_6aug2013.pdf")
plot(prev, main="Preavalence",
     xlab="Week", ylim=c(0, 0.2)
     )
dev.off()

###################################################
### 8 August simulation
### Limit duration-of-infection-based mortality
### to only those who don't have any ART
###################################################
load(file="simulation.8Aug13_dur.inf-based-death-limited-to-noART.RData")

seq.of.networks <- seq(1, 700, by=10) #time steps 48 to 448 of interest
net.size <- rep(NA, length(seq.of.networks))
n.infected <- net.size

## extract cross sectional networks at time steps 48 to 448,
for (i in seq.of.networks){
  assign(paste("net.",i, sep=""), network.extract(nw, at=i))
}

## Record network size information
for (i in seq.of.networks){
  net.size[i] <- network.size(get(paste("net.",i, sep="")))
  ## get function is to treat the string "net.i" as a variable
}

## Record number of infecteds
for (i in seq.of.networks){
  n.infected[i] <- length(which((get(paste("net.",i, sep="")))%v%"inf.status" == 1))
  ## get function is to treat the string "net.i" as a variable
}


## net.size <- net.size[which(!is.na(net.size))]

(max(net.size, na.rm=TRUE) - min(net.size, na.rm=TRUE))/(448-48)
## Population grew by 16 every time step (i.e. 14 days) on average
## corresponds to a 16*24=384 new individuals per year.

## plot popsize
pdf(file="popsize_8aug2013.pdf")
plot(net.size, main="Cross-sectional population size",
     ylim=c(0, 7000),
     xlab="Week")
dev.off()

prev <- n.infected/net.size
pdf(file="prev_8aug2013.pdf")
plot(prev, main="Preavalence",
     xlab="Week", ylim=c(0, 0.2)
     )
dev.off()

###################################################
### 9 August simulation -- 1300 timesteps
### Limit duration-of-infection-based mortality
### to only those who don't have any ART
###################################################
load(file="simulation.9Aug13_dur.inf-based-death-limited-to-noART.RData")

seq.of.networks <- seq(1, 1300, by=10) #time steps 48 to 448 of interest
net.size <- rep(NA, length(seq.of.networks))
n.infected <- net.size

## extract cross sectional networks at time steps 48 to 448,
for (i in seq.of.networks){
  assign(paste("net.",i, sep=""), network.extract(nw, at=i))
}

## Record network size information
for (i in seq.of.networks){
  net.size[i] <- network.size(get(paste("net.",i, sep="")))
  ## get function is to treat the string "net.i" as a variable
}

## Record number of infecteds
for (i in seq.of.networks){
  n.infected[i] <- length(which((get(paste("net.",i, sep="")))%v%"inf.status" == 1))
  ## get function is to treat the string "net.i" as a variable
}


## net.size <- net.size[which(!is.na(net.size))]

(max(net.size, na.rm=TRUE) - min(net.size, na.rm=TRUE))
## Population grew by 16 every time step (i.e. 14 days) on average
## corresponds to a 16*24=384 new individuals per year.

## plot popsize
net.size.noNA <- net.size[!is.na(net.size)] # NA's are networks whose size we did not rec.
pdf(file="popsize_9aug2013.pdf")
plot(seq.of.networks, net.size.noNA, main="Cross-sectional population size",
     ylim=c(0, 7000),
     xlab="Week",
     type="l")
dev.off()

prev <- n.infected/net.size
prev.noNA <- prev[!is.na(prev)]
pdf(file="prev_9aug2013.pdf")
plot(seq.of.networks, prev.noNA, main="Preavalence",
     xlab="Week", ylim=c(0, 0.2),
     type="l"
     )
dev.off()

###################################################
### 11 August simulation -- 1300 timesteps
### Limit duration-of-infection-based mortality
### to only those who don't have any ART
###################################################
seq.of.networks <- seq(1, 10, by=1) #time steps 48 to 448 of interest
net.size <- rep(NA, length(seq.of.networks))
n.infected <- net.size

## extract cross sectional networks at time steps 48 to 448,
for (i in seq.of.networks){
  assign(paste("net.",i, sep=""), network.extract(nw, at=i))
}

## Record network size information
for (i in seq.of.networks){
  net.size[i] <- network.size(get(paste("net.",i, sep="")))
  ## get function is to treat the string "net.i" as a variable
}

## Record number of infecteds
for (i in seq.of.networks){
  n.infected[i] <- length(which((get(paste("net.",i, sep="")))%v%"inf.status" == 1))
  ## get function is to treat the string "net.i" as a variable
}


## net.size <- net.size[which(!is.na(net.size))]

(max(net.size, na.rm=TRUE) - min(net.size, na.rm=TRUE))
## Population grew by 16 every time step (i.e. 14 days) on average
## corresponds to a 16*24=384 new individuals per year.

## plot popsize
net.size.noNA <- net.size[!is.na(net.size)] # NA's are networks whose size we did not rec.
pdf(file="popsize_9aug2013.pdf")
plot(seq.of.networks, net.size.noNA, main="Cross-sectional population size",
     ylim=c(0, 7000),
     xlab="Week",
     type="l")
dev.off()

prev <- n.infected/net.size
prev.noNA <- prev[!is.na(prev)]
pdf(file="prev_9aug2013.pdf")
plot(seq.of.networks, prev.noNA, main="Preavalence",
     xlab="Week", ylim=c(0, 0.2),
     type="l"
     )
dev.off()
