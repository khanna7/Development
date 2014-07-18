###########################################################
## 30 Jul 2013: Moved "assign.infectivity" to its own
## file from "common.functions_d8.R."
###########################################################

### Assign infectivity based on viral load

    assign.infectivity <-
  # 10Jun13: Account for effect of pregnancy
  function(nw, verbose,
           ## function describes extrapolated 
           ## infectivity at lowest viral load of log 2
           ## Hughes et al. give minimum infectivity at viral load of log 3.
           ## infection stages coded numerically
           min.chronic.infectivity.unadj, # changed in "common.functions_d3"
           num.sex.acts.per.timestep,
           acute.mult,
           late.mult,
           preg.mult, #10Jun13
           ...) {

  ## Top Matter
    ## browser()
     inf.status <- nw%v%"inf.status"
     ## art.status <- nw%v%"art.status"#30Jul13: Not needed
     viral.load.today <- nw%v%"viral.load.today"
     stage.of.infection <- nw%v%"stage.of.infection"
     time.since.infection <- nw%v%"time.since.infection"
     infectivity.today <- nw%v%"infectivity.today"
     curr.pregnancy.status <- nw%v%"curr.pregnancy.status"
     for (i in 1:network.size(nw)) {

       if (inf.status[i] == 1){
         ## 24 May '13: Add conditional for stage.of.infection not be ing NA
          if (!is.na(stage.of.infection[i])){
            ## browser()
            if (stage.of.infection[i] == 1) {
                                        # for chronically infected
             ## if (viral.load.today[i] == 3) {
             if (viral.load.today[i] < 2) {
                           ## browser()
               infectivity.today[i] <- 0
               infectivity.today[i] <- 1-(1-infectivity.today[i])^num.sex.acts.per.timestep
             }
           
           else if (viral.load.today[i] == 2) {
             infectivity.today[i] <- min.chronic.infectivity.unadj
             infectivity.today[i] <- 1-(1-infectivity.today[i])^num.sex.acts.per.timestep
           } else if (viral.load.today[i] > 2) {
             infectivity.today[i] <- min.chronic.infectivity.unadj*2.89^
             (viral.load.today[i] - 2)
             infectivity.today[i] <- 1-(1-infectivity.today[i])^num.sex.acts.per.timestep
           }

           if (verbose){
             cat("Chronically Infected are ", i,
                 "with viral load", viral.load.today[i],                 
                 "with infectivity", infectivity.today[i],
                 "\n")

           }
           } else if (stage.of.infection[i] == 0) {
                                        # for acutely infected
             ## browser()
             if (viral.load.today[i] < 2) {
               infectivity.today[i] <- 0
               infectivity.today[i] <- 1-(1-infectivity.today[i])^num.sex.acts.per.timestep
             }  else if (viral.load.today[i] == 2) {

               infectivity.today[i] <- min.chronic.infectivity.unadj*acute.mult
               infectivity.today[i] <- 1-(1-infectivity.today[i])^num.sex.acts.per.timestep
                                  ## set as 0.00497
           } else if (viral.load.today[i] > 2) {
             infectivity.today[i] <- min.chronic.infectivity.unadj*2.89^
             (viral.load.today[i] - 2)*acute.mult 
             infectivity.today[i] <- 1-(1-infectivity.today[i])^num.sex.acts.per.timestep
           }

           if (verbose){
             cat("Acutely Infected are ", i,
                 "with viral load", viral.load.today[i],
                 "with infectivity", infectivity.today[i],
                 "\n")
           }
         } else if (stage.of.infection[i] == 2){
                                        # for late infected
           if (viral.load.today[i] < 2) {
             infectivity.today[i] <- 0
             infectivity.today[i] <- 1-(1-infectivity.today[i])^num.sex.acts.per.timestep
           }

           else if (viral.load.today[i] == 2) {
             infectivity.today[i] <- min.chronic.infectivity.unadj*late.mult
                                  ## set as 0.00497
             infectivity.today[i] <- 1-(1-infectivity.today[i])^num.sex.acts.per.timestep
           } else if (viral.load.today[i] > 2) {
             infectivity.today[i] <- min.chronic.infectivity.unadj*2.89^
                                     (viral.load.today[i]-2)*
                                       late.mult
             infectivity.today[i] <- 1-(1-infectivity.today[i])^num.sex.acts.per.timestep
           }

           if (verbose){
             cat("Late-Infected are ", i,
                 "with viral load", viral.load.today[i],                 
                 "with infectivity", infectivity.today[i],
                 "\n")
           }

         }
           
         }
       }
     }
     ## 10Jun13: Adjust infectivity of pregnant women
     ## browser()
     curr.pregnant <- which(curr.pregnancy.status == 1)
     if (length(curr.pregnant) > 0){
       infectivity.today[curr.pregnant] <-
       infectivity.today[curr.pregnant]*preg.mult
   }
     if (verbose){
       cat("Currently pregnant women are ", curr.pregnant, "\n",
           "Adjusted infectivities of pregnant women: ",
           infectivity.today[curr.pregnant], 
           "\n")
           }
     
     ## Update Infectivity
        nw%v%"infectivity.today" <- infectivity.today


     
     ## Return Network Object
        return(nw)

   }
