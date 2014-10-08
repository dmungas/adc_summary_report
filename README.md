  This program uses R and Sweave to generate a pdf summary report. ADC_Summary.Rnw generates this report. This file should be at the root level of the ADC_Summary directory.  
  
  The following files must either be available to be read by ADC_Summary.Rnw or extracted within the program using a connection to ucdlava on the mysql server:
  
  * adc_data_catalog.csv
  * enrollment.csv
  * assesslist.csv
  
Subfolders are: 
  
    Input Files - required if .csv files are directly inputted
    SQL Files - required if sql queries for adc_data_catalog and enrollment are being submitted
    Reports - required, where pdf copy of report is written

R must be installed, along with the following libraries: 
    
    RMySQL, Gmisc, Hmisc, doBy, plyr, dplyr
    As of 10/7/2014 RMySQL must be installed from source and configuration is tricky. See RMySQL_works.sh.
MacTeX also must be installed.

To run from shell command line:

  * cd to main directory for ADC_Summary report
  * At shell prompt type: sh adc_summary_report.sh
  
To run from R command line:

  * Set ADC_Summary as the working  directly
  * At R prompt type: Sweave('ADC_Summary.Rnw')
  * Followed by: library(tools)
  * And: texi2pdf("ADC_Summary.tex")

