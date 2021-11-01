# WSS
Weight scale and shift code for covid epidemic modelling.

The model takes CASES from https://coronavirus.data.gov.uk/ .  All predictions are based on a compartment model 
moving people from CASE through hospitalization, ICU, to recovery or death.  Compartments are broken down by age region.

R-numbers are calculated by differentiating the case data, after a little data processing to make this legit.  
They reliably agree with "official" estimates published from SPI-M, but are available 16 days earlier.  

Medium term projections are made by extending the CASE data with R-based prediction. 

Parameterization is taken by fits to death/ICU/hospital data over the entire pandemic.  Parameters are (slowly) varying with variant and vaccination.
Lockdowns etc are not included directly, because their effect is mainly in CASES.

Model description and application is here...

https://www.medrxiv.org/content/10.1101/2021.04.14.21255385v1.article-info

Previous version, all calculations were spreadsheet-based
https://www.medrxiv.org/content/10.1101/2021.01.21.21250264v1

Ongoing version for BMJ
https://docs.google.com/document/d/1t7GydEG2PLbwHOkLVBnbXEByf5_h2OkZ/edit

Analysis article

# Execution

The code is in pure R and developed in and best run through Rstudio.  There are no external input files, data is read at runtime from internet.   
Workflow:
1/. Run **Covid_trimmed.R.**  This sets up the global parameters for the UK, calculates all R. parameters, generates plots for interactive monitoring, and does the compartment simulation for England, including medium term predictions.  

1*/ Legacy code from the WSS paper calculating CFR variation with time may be called from medrxiv.R and age_pdfplot.R, but these must be called by editing the calls in the code to {medout<-MedrxivPaper()} and setting pdfpo TRUE.

2/  Run **ScottishData.R**. This reads data from the Scottish government, reformats it, and runs the compartment model.  The code ends with some monitoring plots

3/  Run **CC_write.R**. to obtain R-numbers, growth rates, England and Scotland compartment model data in CrystalCast format

4/ Run **Regional.R**. to read data and obtain compartment model data for 7 NHS England Regions in CrystalCast format.

