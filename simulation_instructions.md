# Instructions to estimate models, <br/> and simulate burnin and interventions 

## Estimation Routines
   * Comments at top: history of changes
   * Clear memory and load statnet packages

   * Set up population and initialize network
     * Formation models     
       Uganda: 
``` html
                edges+b1degree(0:1)+b2(0:1)+    
                 # num of edges + 
                 # num of males (b1) and females (b2) with 0 and 1 degrees
                 b1starmix(k=1,    
                         attrname="age.cat",      
                         base=c(age.leave.out),    
                         diff=TRUE)+
                 #: age-mixing matrix: specified diagonal entries
                         b1factor("age.cat", base=c(1:2))+    
                         b2factor("age.cat", base=c(1:2))   
                 #: row sums for males in age categories 3 and 4`
                 #: row sums for females in age categories 3 and 4`
```
  South Africa: 
``` html
                edges+b1degree(1:2)+b2degree(1:2)+    
                # num of edges + 
                # num of males (b1) and females (b2) with 1 and 2 degrees
                 b1starmix(k=1,    
                         attrname="age.cat",      
                         base=c(age.leave.out),    
                         diff=TRUE)+
                 #: age-mixing matrix: specified diagonal entries
                         b1factor("age.cat", base=c(1:2))+    
                         b2factor("age.cat", base=c(1:2))   
                 #: row sums for males in age categories 3 and 4`
                 #: row sums for females in age categories 3 and 4`
```

Only difference between formation model specifications for Uganda and South Africa
are that for Uganda number of men and women with degrees 0 and 1 are specified,
and for South Africa number of men and women with degrees 1 and 2 are specified
    
   * Compute target statistics for network models from empirical data 
   * Specify constraints (none specified, for both countries)
   * Initialize network
   * Add attributes (for time 0)
     * Demographic: Sex, age, cirumcision status, age-specific fertility rate(ASFR), pregnancy status (as a function of ASFR),                     time since current pregnancy (set to 0 initially for all pregnant women), 
                    time of last pregnancy (set to 0 initially for all pregnant women), 
                    number of pregnancies (1 for initially pregnant women, 0 for everyone else), 
                    
     * Biological and treatment-related: HIV status, ART covered, PMTCT covered, time since infection (uniformly distributed between 0 and max. length of infection for untreated people), time of infection (0 - time since infection), age at infection (age - time since infection), duration of infection by age, CD4 count today (function of time since infection, age at infection, and sex),  ART status (set to 1 for those who are covered, and consistent with empirical estimate), time since ART initiation (calculted from CD4 count), ART type (combination ART initially for all on treatment), viral load (function of infection status, time since infection, ART status, and time since ART initiation)
     

   * Fit ergm model
      
   * Save estimation object
   
## Burnin Models  

   * Comments at top: history of changes  
   * Clear memory and load statnet packages  
   * Load estimation object  
   * Source files from the **engine**

   * Placeholder to generate output data: "date_country_birthrate_class_runno"  

   * Set up network model  
     * Fit, formula, population features  
     * Other needed parameters for timesteps
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
   * Comments at top: history of changes
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
