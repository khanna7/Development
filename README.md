#PMTCT-models-data
Repository to model PMTCT.


## Main Directories:

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
  
### /Development/Uganda_Runs
* Files and data specific to Uganda 
* Contains csv files with epidemic and demographic data; `RData` files with network data 

###/Development/South_Africa_Development_Runs 
* Files and data specific to South Africa
* Contains csv files with epidemic and demographic data; RData files with network data

###/Development/Simulation_Graphics 
* Files to generate graphics for paper
* Contains `pdf` files of plots 
