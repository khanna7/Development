# Instructions to run baseline and intervention models

## Baseline Models  

   * Comments at top: History of changes  
   * Clear memory and load statnet packages  
   * Load estimation object  
   * Source files from the **engine**

   * Placeholder to generate output data: "date_country_birthrate_class_runno"  

   * Set up network model  
     * Fit, formula, population features  
     * Other Needed Parameters for timesteps
     * Set some values for time 1
     * Record initial network statistics  
     * Start with all vertices and edges active

   * Specify intervention scenario:  
      `"baseline"` for Option A (burnin is only Option A)   

   * Some `rm` statements follow to free memory, and garbage collect.
   
   * Time Loop  
     Simulate network object
     Update vital dynamics, update network
     Assign pregnancy (includes modules for cessation of PMTCT Option A and B)
     Update treatment
     Update CD4 counts
     Update Viral Load
     Assign infectivity
     Transmit infection
     Clear memory, and save in case of crash (not problems on MOSIX3 with 64-bit computing)
     
   * Save burnin object for intervention runs

## Intervention Models
