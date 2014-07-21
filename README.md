# Code for PMTCT models and data
Repository to model PMTCT.

## Statnet packages and versions
The following packages need to be installed:

   [tergm_3.1.1.tar.gz](http://cran.r-project.org/src/contrib/Archive/tergm/)    
   [ergm_3.1-0.tar.gz](http://cran.r-project.org/src/contrib/Archive/ergm/)    
   [network_1.9.0.tar.gz](http://cran.r-project.org/src/contrib/Archive/network/)  
   [networkDynamic_0.4.1.tar.gz](http://cran.r-project.org/src/contrib/Archive/networkDynamic/)    


## Main Directories

### /Development 
* Contains the **engine** 
  * `/common.functions_fieldingmortality_d9.R`
  * `/update.vital.dynamics_newinfected_entryage18_d13a.R`
  * `/assign.pregnancy_d8d1.R`
  * `/update.treatment_d11.R`
  * `/compute.cd4.count_d3.R`
  * `/compute.viral.load_d7_d3.R`
  * `/assign.infectivity_d4.R`
  * `/transmission_d10.R`

* Contains files with country-specific parameters  
  * Uganda: `/params_uganda_d3.R`
  * South Africa: `/params_za_d2.R`
  
#### /Development/Uganda_Runs
* Burnin file
* Output RData file: contains network data 
* Example of simulation

#### /Development/South_Africa_Development_Runs 
* Burnin file
* Output RData file: contains network data 
* Example of simulation

Instructions to run burnin and simulation models are [here.](https://github.com/khanna7/Development/blob/master/simulation_instructions.md)

#### /Development/Simulation_Graphics 
* Output: `plot_pdfs.zip`  
       Incidence: `hiv_incidence_plot.pdf`  
       Prevalence: `hiv_prevalence_plot.pdf`  
       Incidence and Viral Suppression vs Coverage: `coverage_scenarios_plot.pdf`    
       Per cent Virally Suppressed: `viral_suppression_plot.pdf` 

* Data: `data.zip`     
       Incidence: `hiv_prevalence.RData`  
       Prevalence: `hiv_incidence.RData`  
       Viral Suppression: `viral_suppression.RData`  
       Incidence and Viral Suppression vs Coverage: `coverage_scenarios_data.RData`  

* Code: `plotfiles.zip`     
       Incidence: `hiv_prevalence.RData`  
       Prevalence: `hiv_prevalence_plot.R`  
       Viral Suppression: `viral_suppression_plot.R`  
       Incidence and Viral Suppression vs Coverage: `coverage_scenarios_plot.R`



