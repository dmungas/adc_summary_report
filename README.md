  This program uses R and Sweave to generate a pdf summary report. 
  
  The following files must either be inputted directly or extracted using a connection to the mysql server:
  
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

  * cd to main directory for ADC_Summary report.
  * At shell prompt type: sh adc_summary_report.sh

