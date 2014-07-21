# Instructions to estimate models, <br/> and simulate burnin and interventions 

## Burnin Models  

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
   * Comments at top: History of changes
   * Clear memory and load statnet packages
   * Load burnin object
   * Source files from engine
  
   * Source parameters file
     *date* is written in format: "dateofsimulation_Country_intervention_coveragelevel_runx"
     where    
 
        "dateofsimulation" is the date (28 Feb in this example)
        "Country" is either Uganda (UG) or South Africa (ZA)  
        "intervention": "bl" for Option A  
                        "option.b" for Option B
                        "pmtct.b+" for Option B+
        "coveragelevel":"cp" for Current practice   
                        "bestcasepreg" for Best PMTCT
                        "bestcaseall" for Best ART and PMTCT
        
        

   * Extract netwowork using `network.collapse(nw, last_time_of_burnin)`
   
   * Time length of simulation: we simulate over ten years, or 260 timesteps

   * Scenario: "baseline" for Option A  
               "option.b" for Option B
               "pmtct.b+" for Option B+

   * Time-loop: as in burnin
   
     To simulate different coverage levels

| Coverage Level | Parameters |
| ------ | ----------- |
| Current ART and PMTCT   | `baseline.art.coverage.rate=baseline.art.coverage.rate` `baseline.preg.coverage.rate=baseline.preg.coverage.rate` |
| Best PMTCT |`baseline.art.coverage.rate=baseline.art.coverage.rate`  `baseline.preg.coverage.rate=idealized.preg.coverage.rate`|
| Best ART and PMTCT |`baseline.art.coverage.rate=idealized.art.coverage.rate`  `baseline.preg.coverage.rate=idealized.preg.coverage.rate`|
    
       Both `baseline.art.coverage.rate` and `baseline.preg.coverage.rate` arguments appear twice:
       once in the `update.vital.dynamics` function, and once in the `transmission` function.       
       

   * For simulating Option B, additionally, we need to replace file `update.treatment_d11.R` with `update.treatment_d12.R`in the **engine**. Version `d12` can also be used for simulating Options A and B+, but `d11` cannot be used for simulating Option B.

   * Save object "interv.date.RData" 
