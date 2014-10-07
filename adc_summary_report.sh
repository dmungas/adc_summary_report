# cd to main directory for ADC_Summary report.
# At shell prompt type: sh adc_summary_report.sh
# R must be installed, along with the following libraries: 
#   RMySQL, Gmisc, Hmisc, doBy, plyr, dplyr
#   As of 10/7/2014 RMySql must be installed from source and configuration is tricky. See RMysql_works.sh.
# MacTeX also must be installed.


R CMD Sweave 'ADC_Summary.Rnw'