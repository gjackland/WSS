# WSS
Weight scale and shift code for covid epidemic modelling.
This code has been part of the SPI_M and JBC suite of nowcasts, medium term predictions and scenarios since autumn 2021.

The model takes CASES from https://coronavirus.data.gov.uk/ .  All predictions are based on a compartment model moving people from CASE through hospitalization, ICU, to recovery or death.  Compartments are broken down by age region.

R-numbers are calculated by differentiating the case data, after a little data processing to make this legit.  
They reliably agree with "official" estimates published from SPI-M, but are available 16 days earlier.  

Medium term projections are made by extending the CASE data with R-based prediction. 

Parameterization is taken by fits to death/ICU/hospital data over the entire pandemic.  Parameters are (slowly) varying with variant and vaccination.
Lockdowns etc are not included directly, because their effect is mainly in CASES.

Model description and application is here (preprint and paper)

* https://www.medrxiv.org/content/10.1101/2021.09.23.21256065v1
* https://royalsocietypublishing.org/doi/10.1098/rsta.2021.0301

Previous preprints using the model

* https://www.medrxiv.org/content/10.1101/2021.01.21.21250264v1
* https://www.medrxiv.org/content/10.1101/2021.04.14.21255385v1

Ongoing version for BMJ

* https://docs.google.com/document/d/1t7GydEG2PLbwHOkLVBnbXEByf5_h2OkZ/edit

Analysis article

# Execution

The code is in pure R and developed in and best run through Rstudio.  Data is read at runtime from internet.   If online data is unavailable, code will fail with error messages.   
Since summer 2022, the testing regime has become quite sketchy and ONS estimates of prevalence (more accurate but delayed) are used to normalise the published case numbers.  Similarly, Scotland stopped reporting death data to the UK site, so these are inferred from NRS.  Wales and Northern Ireland data are unusable since summer 2022.

There are many in-code adjustable parameters, mainly gathered together in getParams.R.  For ordinary running startdate and enddate can be set to cover the pandemics period where data are available.  "reporting_delay" menas the last few days are excluded from nowcasting because dtat is incomplete. 

Workflow:

1. Run **Workflow.R**  This sets up the global parameters for the UK, calculates all R. parameters, generates plots for interactive monitoring, and does the compartment simulation for England, including medium term predictions.  
2.  **Covid_trimmed.R**.  Reads the current data from , calculates R numbers and pandemic hindcasts and medium term predictions
3.  **ScottishData.R**. This reads data from the Scottish government, reformats it because it uses different age banding, and runs the compartment model.  The code ends with some monitoring plots
4.  **Regional.R**. to read data and obtain compartment model data for 7 NHS England Regions, and write outputs in CrystalCast format for uploading to weekly JBC ensemble estimates. Writing is done in CC_write.R
5. **Sanity check**. R-numbers can be sensitive to late posting on recent cases, and wrong R-numbers tip over into the medium term predictions.  Medium term predictions are very sensitive to the calculated parameters R_BestGuess$.   Before believing anything, always check these and if not sensible investigate the last few days of input case data for anomalies.  The parameter "enddate" can be increased to eliminate incomplete data.  If R is sensitive to enddate, there is a data problem.

6. Legacy code from the WSS paper calculating CFR variation with time may be called from `medrxiv.R` and `age_pdfplot.R`, but these must be called by editing the calls in the code to:
   ```R
   medout <- MedrxivPaper()
   ```
   and setting:
   ```R
   pdfpo <- TRUE
   ```

scenarios - where a range of initial values for R_BestGuess$ are set rather than calculated - can be run using the Scenarios.R code.

## Predictions for SPI-MO and JBC

WSS is part of the suite of codes contributing to UK and Scottish government real-time  modelling.

## Licensing

The code is licensed under GPL-3.0. The data used for making the calculations is made available under an [Open Government Licence](http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/).
