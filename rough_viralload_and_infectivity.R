  ### Model Biological Variables # ASK
   ## CD4 Count, Viral Load, Infectivity, Time Since Infection
      time.since.infection <- time.since.infection+1

      ## CD4 Count
         infected <- which(net%v%"inf.status" == 1)
         not.on.treatment <- which(net %v% "art.status" == 0)


     ## Assign Stage of Infection
        classify.infection(time.since.infection)

     ## Assign Infectivity for Chronic Infectives

        ## 3 log 	0.00028        0.000497
        ## 4 log	0.00082	       0.00143633
        ## 5 log	0.0024	       0.004150994
        ## 6 log	0.0068         0.011996372

        if (infection.stage == "chronic") {
          if (viral.load == 3) {
             infectivity <- min.chronic.infectivity ## set as 0.00497
            } else if (viral.load > 3) {
              infectivity <- min.chronic.infectivity*2.89^(viral.load-3)
            }
         }

     ## Assign Infectivity for Acute and Late Infectives
        ## Acute
         if (infection.stage == "acute") {
           if (viral.load == 3) {
             infectivity <- min.chronic.infectivity ## set as 0.00497
           } else if (viral.load > 3) {
             infectivity <- min.chronic.infectivity*2.89^(viral.load-3)*4.98
           }
         }
             
        ## Late
         if (infection.stage == "late"){
           if (viral.load == 3) {
             infectivity <- min.chronic.infectivity ## set as 0.00497
           } else if (viral.load > 3) {
             infectivity <- min.chronic.infectivity*2.89^(viral.load-3)*3.49
           }
         }

     ## Adjust if uninfected male is cicumcised
        ## Have to do in transmission file because adjustment for circumcision is x
        ## partnership-specific

     ## Adjust if female is pregnant
        ## Have to do in transmission file because adjustment for pregnancy
        ## partnership-specific


