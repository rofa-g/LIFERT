FOLDERS AND FILES

-main folder-

Project.Rproj: R project 

metadata.txt: information about the data and variables 

session_info.txt: RStudio version used and the session information of the R session


/data

Files and scripts used in the data wrangling from LISS, GGS, and other sources. 

liss_data_wrangling.R: LISS data wrangling

ggs_data_wrangling.R: GGS data wrangling 

hfd_data_wrangling.R: HFD data wrangling

Huwen__kerncijfers_22022024_161621.csv: CSV file for calculating mean age at marriage (instruction for how to download it are in the liss_data_wrangling.R script)

physiology_data.R: calculations of physiological inputs into the model

various .RDS files: files used as inputs in data wrangling and simulation


/data/GGS and data/LISS

These are the folders where the original data files were kept during the data wrangling, 
but as these data require registration they could not be included in the archive


/functions_and_processing

confidence_interval_function.R: Simulation for checking variation between simulation runs

contraception_spacing_abortion.R: Contraception, spacing, abortion inputs

create_lifecourse.R: Function producing input data.table used in the union and reproduction simulations

fertility_intentions_education.R: Fertility intentions and education inputs

na_zero_vectors.R: length, NA and zero vectors used in functions

names.R: String vectors used in functions

physiology_functions.R: Functions for physiology inputs

reproduction_simulation.R: Simulation function of reproductive lifecourses

summary_functions.R: Summary functions that summarise the simulation outputs by cohort or cohort & education

union_simulation.R: Simulation of partnership histories

unions.R: Union inputs 


/functions_and_processing/unchanged_functions

Functions that are not edited but need to be updated after parameter changes when running the 
'simulation_mult_par_change.R' script in the '/simulation/multiple_par_changes/' folder 


/plots

Plot outputs and scripts for producing plots

figures_and_tables.R: Script for producing most of the tables and figures in the paper

mermaid_flowcharts.qmd: Quarto document for producing the mermaid flowcharts in the paper

tables_ms_word_export.R: Script to export tables as editable MS Word (.docx) files



/plots/mermaid_flowcharts_files

files produced when rendering 'mermaid_flowcharts.qmd' Quarto script


/plots/logos

Various logo PNG files used when producing mermaid flowcharts


/simulation

simulation scripts and inputs used in simulation

simulation.R: the main simulation script (best used for single simulation runs)

.RDS files: model inputs


/simulation/exact_replication_data

folder containing an .RDS file with the data used in the paper


/simulation/multiple_par_change

simulation_mult_par_change.R: R script used for multiple parameter adjustments (instructions commented in script)


/simulation/multiple_par_change/RDS_files

RDS files with summaries of parameter adjustments done in the paper produced using the simulation_mult_par_change.R script





REPLICATING THE RESULTS IN THE PAPER WITH THE PROVIDED DATA FILES 

1. Open the R.project file in the main directory

2. Run the R script 'simulation.R' located in the 'simulation' sub-directory.
For exact replication of results, use the provided .RDS data file in the 
 'exact_replication_data' folder within the same sub-folder (uncomment line 129 of the 'simulation.R' script). 

3. Figures and tables used in the paper can be reproduced using the R script 'figures_and_tables.R'
   in the 'plots' sub-folder. For exact replication of results, use the provided .RDS data file in the
   'exact_replication_data' folder within the same sub-folder (default in script).

4. The mermaid flowcharts (figures 2, A1, and A2) can be reproduced using the quarto(.qmd) file in the 'plots' folder, but for each of the images 
   included from the 'logos' folder inside the 'plots' folder, the exact path where the main folder is located on the PC used has to be specified. 


RUNNING MULTIPLE PARAMETER CHANGES

1. Open the R.project file in the main directory

2. Locate the 'simulation_mult_par_change.R' script in the 'simulation/multiple_par_change' folder
   and open it. 

3. Instructions for how each parameter adjustment performed in the paper was done is commented in the script

4. The simulation summaries of each parameter change are found as separate .RDS files in 'simulation/multiple_par_change/RDS_files'


REPLICATING THE DATA WRANGLING

1. Access and download the following data: 

Data access to both GGS, LISS, and HFD data is free of charge, but subject to registration.


-GGS-

To access GGS data you need to register an account at:
https://www.ggp-i.org/form/accounts/register/

And fill in the necessary application forms once you have been granted access. 


DATA FILE

Generations and Gender Survey I Wave 1 (.dta)
https://www.ggp-i.org/


-LISS-

To access LISS data you need to sign the LISS panel Data Statement and follow the instructions at: 

https://statements.centerdata.nl/liss-panel-data-statement


DATA FILES

LISS Family and Household waves 1-15 (.dta)
https://www.dataarchive.lissdata.nl/study-units/view/10

LISS monthly background variables from March 2007 to June 2023
https://www.dataarchive.lissdata.nl/study-units/view/322


-HUMAN FERTILITY DATABASE (HFD)-

Register an account at:

https://www.humanfertility.org/Account/UserAgreement

You need a HFD account to use the HMDHFDplus package in R


2. Remove the existing .RDS data files in the 'simulation' subdirectory 
(do not delete the files if you intend to exactly replicate the paper results,
 see the instructions at the top of this document)

3. Open the R.project file in the main directory

4. Run the GGS data wrangling script located in the "data" sub-directory. 
Remember to enter your Human Fertility Database credentials in the location 
specified in the script. 

5. Run the LISS data wrangling script located in the 'data' sub-directory

6. Run the 'physiology_data.R' R script in the 'data' sub-directory

7. Run the 'simulation.R' script located in the 'simulation' sub-directory.

