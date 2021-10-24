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

