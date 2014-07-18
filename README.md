#PMTCT-models-data
Repository to model PMTCT.


## Main Directories:

### /Development 
* Contains the **engine** 
  * `/common.functions_fieldingmortality_d9.R`
  * `/update.vital.dynamics_newinfected_entryage18_d13a.R`
  * `/assign.pregnancy_d8d1.R`
  * `/update.treatmehttps://github.com/khanna7/PMTCT-models-datant_d11.R`
  * `/compute.cd4.count_d3.R`
  * `/compute.viral.load_d7_d3.R`
  * `/assign.infectivity_d4.R`
  * `/transmission_d10.R`

* Contains files with country-specific parameters  
  * Uganda: `/params_uganda_d3.R`
  * South Africa: `/params_za_d2.R`
  
#### /Development/Uganda_Runs
* Files and data specific to Uganda 
* Contains `RData` file with network data 

#### /Development/South_Africa_Development_Runs 
* Files and data specific to South Africa
* Contains RData files with burnin data

#### /Development/Simulation_Graphics 
* Graphics: code, datasets, output

##### /Development/Simulation_Graphics/data
* Data for prevalence plots: 
* Contains `RData` file with network data 