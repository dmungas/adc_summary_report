  This program originally used R and Sweave to generate a pdf summary report. ADC_Summary.Rnw generates this report. This file should be at the root level of the ADC_Summary directory.  ADC_Summary.Rnw has not been updated since 2014.
  
  As of 10-17-14 a R Markdown version ADC_Summary.Rmd was been added that generates a html report with customized formatting based on the custom-markdown_4.css stylesheet.
  
  The following files must either be available to be read by ADC_Summary.Rnw/ADC_Summary.Rmd or extracted within the program using a connection to ucdlava on the mysql server:
  
  * adc_data_catalog.csv (ADC Data Catalog report in Lava)
  * enrollment.csv (CohortEnrollment report in Lava)
  * assesslist.csv (ADC Visit Summary (Assess List) report in Lava)
  * uds health history (UDS Health History (A5) in Lava)
  * uds subject demographics (UDS Subject Demographics (A1) in Lava)
  * muds demographic (Muds Demographic in Lava)
  
  
  Selecting between reading csv input files versus downloading files via MySQL
  connection is accomplished by commenting/uncommenting relevant lines in
  R code Chunk 1.
  
Subfolders are: 
  
    Input Files - required if .csv files are directly inputted
    SQL Files - required if sql queries for adc_data_catalog and enrollment are being submitted
    Reports - required, where pdf copy of report is written by ADC_Summary.Rnw

R must be installed, along with the following libraries: 
    
    RMySQL, knitr,Gmisc, Hmisc, doBy, plyr, dplyr,tidyr,lattice,pander,htmlTable
    As of 10/7/2014 RMySQL must be installed from source and configuration is tricky. See RMySQL_works.sh.
MacTeX also must be installed (Rnw version).
RStudio should be installed (Rmd version).

To run ADC_Summary.Rnw from shell command line:

  * cd to main directory for ADC_Summary report
  * At shell prompt type: sh adc_summary_report.sh
  
To run ADC_Summary.Rnw from R command line:

  * Set ADC_Summary as the working  directly
  * At R prompt type: Sweave('ADC_Summary.Rnw')
  * Followed by: library(tools)
  * And: texi2pdf("ADC_Summary.tex")

To run ADC_Summary.Rmd from shell command line (using knit_to_html.R script):

  * cd to main directory for ADC_Summary report
  * At shell prompt type: R CMD BATCH knit_to_html.R
  
Output does not currently render correctly from R command line. PDF of html file
can be generated in Mac OS by opening html file in browser and using using 
Print Save as PDF.

