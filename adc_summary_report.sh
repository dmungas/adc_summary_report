# cd to main directory for ADC_Summary report.
# At shell prompt type: sh adc_summary_report.sh
# 'SQL Files' subdirectory is required if sql queries for adc_data_catalog and enrollment are being submitted.
#     These files should be in 'SQL Files'
# 'Input Files' subdirectory is required if csv data adc_data_catalog, enrollment, and assesslist are being submitted.
#     These files should be in 'Input Files'
# 'Reports' subdirectory is required and is where pdf copy of report is written.

# R must be installed, along with the following libraries: 
#   RMySQL, Gmisc, Hmisc, doBy, plyr, dplyr
#   As of 10/7/2014 RMySql must be installed from source and configuration is tricky. See RMysql_works.sh.
# MacTeX also must be installed.


R CMD Sweave 'ADC_Summary.Rnw'