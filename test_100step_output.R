## 26Jul2013: Test simulation output

## Code to test 100 step output

sex <- nw%v%"sex"

inf.status <- nw%v%"inf.status"
art.status <- nw%v%"art.status"
art.type <- nw%v%"art.type"
curr.pregnancy.status <- nw%v%"curr.pregnancy.status"
viral.load.today <- nw%v%"viral.load.today"

time.since.infection <- nw%v%"time.since.infection"
time.of.infection <- nw%v%"time.of.infection"
time.since.art.initiation <- nw%v%"time.since.art.initiation"
time.of.art.initiation <- nw%v%"time.of.art.initiation"
time.since.art.cessation <- nw%v%"time.since.art.cessation"
time.since.curr.pregnancy  <- nw%v%"time.since.curr.pregnancy"
time.since.last.pregnancy  <- nw%v%"time.since.last.pregnancy"



high.vl <- which(viral.load.today >= 7)
summary(viral.load.today[high.vl])
## High viral loads summary statistics
  ##  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  ## 7.236   7.236   7.236   7.365   7.236   8.607 

## Check attributes of individuals with high viral load
inf.status[high.vl]
sex[high.vl] ## it is still only women who are achieving high viral load.
time.since.infection[high.vl]
curr.pregnancy.status[high.vl]
time.since.curr.pregnancy[high.vl]
time.since.last.pregnancy[high.vl]
time.since.art.cessation[high.vl]
length(which(duplicated(time.since.art.cessation[which(!is.na(time.since.art.cessation))])
             == TRUE))/
  length(duplicated(time.since.art.cessation[which(!is.na(time.since.art.cessation))]))

identical(time.since.last.pregnancy[which(nw%v%"sex"=="Female")],
          time.since.art.cessation[which(nw%v%"sex"=="Female")])
                                        # identical: "time.since last preg"
                                        # and "time.since.art.cessation"
          


## Cross-Tabs to check

   ## Infection and ART-status
   xtabs(~factor(art.status, exclude=NULL) +
         factor(inf.status, exclude=NULL))

   ## ART status and type
   xtabs(~factor(art.status, exclude=NULL) +
         factor(art.type, exclude=NULL))

   ## ART status and sex
   xtabs(~factor(art.status, exclude=NULL) +
         factor(sex, exclude=NULL))

  ## ART type and infection status       
   xtabs(~factor(art.type, exclude=NULL) +
         factor(inf.status, exclude=NULL))

  ## Time since infection and time of infection
   xtabs(~factor(time.since.infection, exclude=NULL) +
         factor(time.of.infection, exclude=NULL))

  ## Time since infection and time since ART initiation
   xtabs(~factor(time.since.infection, exclude=NULL) +
         factor(time.of.infection, exclude=NULL))


## Check viral load trajectories

   time.length <- 10
   viral.load.data <- as.list(1:time.length)
   min.time <- time-time.length

   for (time.pt in (time-time.length) : time){

     xn.net <- network.extract(nw, at=time.pt)
     data <- cbind(1:network.size(xn.net), net%v%"viral.load.today")
     viral.load.data[[(time.pt%% min.time)+1 ]] <- data # pt in list to be populated

   }


## Check how network size changes

i <- 90:100

nets <- list(i)


for (i in min.time:time){

  assign(paste("net.", i, sep=""),  network.extract(nw, at=i))

}

for (i in min.time:time){

  assign(paste("net.", i, sep=""),  network.collapse(nw, at=i))

}

