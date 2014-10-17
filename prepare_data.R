
library(RMySQL)

col_names <- c("blue","green","red","purple")

cur_yr <- as.numeric(format(as.Date(date(), format="%a %b %d %H:%M:%S %Y"), "%Y")) 

# 	dcat <- read.table("./Input Files/adc_data_catalog_10-07-14.csv", header=TRUE, sep=",", na.strings = "")
# 	enr <- read.table("./Input Files/enrollment_10-07-14.csv", header=TRUE, sep=",", na.strings = "")
#   asl1 <- read.table("./Input Files/assesslist_10-07-14.csv", header=TRUE, sep=",", na.strings = "")

#   ---------------------------------------- Input Data -----------------------------------------------
sqlPrep <- function(file){
  paste(scan(file, sep = "\n", what = "character"), collapse = "\n")
}

sql_enr <- sqlPrep("./SQL Files/Enrollment.sql")
sql_dcat <- sqlPrep("./SQL Files/ADCDataCatalog_10-06-14.sql")

# sql_enr <- "CALL enrollment"
# sql_dcat <- "CALL adc_data_catalog"
sql_asl <- "SELECT a.* FROM assesslist AS a"

lavaDb <- dbConnect(MySQL(), group = "ucdlava", client.flag = CLIENT_MULTI_STATEMENTS)
res_enr <- dbSendQuery(lavaDb, sql_enr)
enr <- fetch(res_enr, n = -1)
dbDisconnect(lavaDb)

lavaDb <- dbConnect(MySQL(), group = "ucdlava", client.flag = CLIENT_MULTI_STATEMENTS)
res_dcat <- dbSendQuery(lavaDb, sql_dcat)
dcat <- fetch(res_dcat, n = -1)
dbDisconnect(lavaDb)

lavaDb <- dbConnect(MySQL(), group = "ucdlava", client.flag = CLIENT_MULTI_STATEMENTS)
res_asl <- dbSendQuery(lavaDb, sql_asl)
asl1 <- fetch(res_asl, n = -1)
dbDisconnect(lavaDb)

# ----------------------------------------------------------------------------------------------------

enr$LatestDate <- as.Date(enr$LatestDate)
enr$EnrolledDate <- as.Date(enr$EnrolledDate)
enr$DroppedDate <- as.Date(enr$DroppedDate)
enr$LostDate <- as.Date(enr$LostDate)
enr$RefusedDate <- as.Date(enr$RefusedDate)
enr$DeceasedDate <- as.Date(enr$DeceasedDate)

if ("id" %in% colnames(dcat)){
  colnames(dcat) <- gsub("id","ID",colnames(dcat))
}

dcat$DtDeath <- as.Date(dcat$DtDeath)
dcat$DtLastEval <- as.Date(dcat$DtLastEval)

dcat$age_ia <- as.numeric((as.Date(dcat$DtIA) - as.Date(dcat$DOB))/365.25)
dcat$RecrSource <- ifelse(is.na(dcat$SENAS_ID),"Clinic",ifelse(dcat$SENAS_ID<100000,"Clinic","Community"))
dcat$Cohort <- ifelse(!is.na(dcat$C2),ifelse(!is.na(dcat$Hillblom),"C2/Hillblom","C2"),ifelse(!is.na(dcat$IVD),"IVD",ifelse(!is.na(dcat$Hillblom),"Hillblom",ifelse(!is.na(dcat$ADNI) & !dcat$ADNI=="REFERRED","ADNI",ifelse(!is.na(dcat$C1),"C1",NA)))))
dcat$synd_ia <- ifelse(dcat$SYNDRIA==2,"Normal",ifelse(dcat$SYNDRIA==3,"Normal",ifelse(dcat$SYNDRIA==6,"MCI",ifelse(dcat$SYNDRIA==5,"Demented",NA))))
dcat$synd_mra <- ifelse(dcat$SYNDRMRA==2,"Normal",ifelse(dcat$SYNDRMRA ==3,"Normal",ifelse(dcat$SYNDRMRA ==6,"MCI",ifelse(dcat$SYNDRMRA ==5,"Demented",NA))))
colnames(dcat)[2] <- "ID"
dcat$grp <- ifelse(dcat$ETHNICITY==1,NA,ifelse(dcat$ETHNICITY==2,NA,ifelse(dcat$ETHNICITY==3,NA,ifelse(dcat$ETHNICITY==4,"African American",ifelse(dcat$ETHNICITY==5,NA,ifelse(dcat$ETHNICITY==6,"Hispanic",ifelse(dcat$ETHNICITY==7,"White",NA)))))))
dcat$grp2 <- ifelse(dcat$ETHNICITY==1,"Other",ifelse(dcat$ETHNICITY==2,"Other",ifelse(dcat$ETHNICITY==3,"Other",ifelse(dcat$ETHNICITY==4,"African American",ifelse(dcat$ETHNICITY==5,"Other",ifelse(dcat$ETHNICITY==6,"Hispanic",ifelse(dcat$ETHNICITY==7,"White",ifelse(dcat$ETHNICITY==8,"Other",NA))))))))
dcat$center <- ifelse(dcat$ID>=50000,"East_Bay","Sacramento")

dcat$ClinDxMRA <- ifelse(dcat$ETIOLMRA %in% c(7,8,24),2,ifelse(dcat$ETIOLMRA %in% c(1,2),1,ifelse(dcat$ETIOLMRA %in% c(3,4),3,ifelse(dcat$ETIOLMRA==9,4,ifelse(dcat$ETIOLMRA==21,ifelse(dcat$MIX1MRA %in% c(1,2) & dcat$MIX2MRA %in% c(3,4),5,ifelse(dcat$MIX1MRA %in% c(3,4) & dcat$MIX2MRA %in% c(1,2),5,ifelse(dcat$MIX1MRA %in% c(7,8,24) & dcat$MIX2MRA %in% c(1,2),2,ifelse(dcat$MIX2MRA %in% c(7,8,24) & dcat$MIX1MRA %in% c(1,2),2,NA)))),NA)))))

dcat$ClinDxMRA2 <- ifelse(dcat$SYNDRMRA==6,6,ifelse(dcat$SYNDRMRA %in% c(2,3),7,ifelse(dcat$ETIOLMRA %in% c(7,8,24),2,ifelse(dcat$ETIOLMRA %in% c(1,2),1,ifelse(dcat$ETIOLMRA %in% c(3,4),3,ifelse(dcat$ETIOLMRA==9,4,ifelse(dcat$ETIOLMRA==21,ifelse(dcat$MIX1MRA %in% c(1,2) & dcat$MIX2MRA %in% c(3,4),5,ifelse(dcat$MIX1MRA %in% c(3,4) & dcat$MIX2MRA %in% c(1,2),5,ifelse(dcat$MIX1MRA %in% c(7,8,24) & dcat$MIX2MRA %in% c(1,2),2,ifelse(dcat$MIX2MRA %in% c(7,8,24) & dcat$MIX1MRA %in% c(1,2),2,NA)))),NA)))))))


dcat$PathDx <- ifelse(dcat$PATHDX1 %in% c("2","3","4","4a","4b","4c"),ifelse(is.na(dcat$PATHDX2) | dcat$PATHDX2 %in% c("-6","-7","-9") | dcat$PATHDX2 %in% c("2","3","4","4a","4b","4c"),1,ifelse(dcat$PATHDX2 %in% c("6","6a","6b","6c","6d"),6,ifelse(dcat$PATHDX2 %in% c("7","7a","7b","7c","7d","7e","7f"),5,9))),ifelse(dcat$PATHDX1 %in% c("6","6a","6b","6c","6d"),ifelse(is.na(dcat$PATHDX2) | dcat$PATHDX2 %in% c("-6","-7","-9") | dcat$PATHDX2 %in% c("6","6a","6b","6c","6d"),2,ifelse(dcat$PATHDX2 %in% c("2","3","4","4a","4b","4c"),6,ifelse(dcat$PATHDX2 %in% c("7","7a","7b","7c","7d","7e","7f"),7,2))),ifelse(dcat$PATHDX1 %in% c("7","7a","7b","7c","7d","7e","7f"),ifelse(is.na(dcat$PATHDX2) | dcat$PATHDX2 %in% c("-6","-7","-9") | dcat$PATHDX2 %in% c("7","7a","7b","7c","7d","7e","7f"),3,ifelse(dcat$PATHDX2 %in% c("2","3","4","4a","4b","4c"),5,ifelse(dcat$PATHDX2 %in% c("6","6a","6b","6c","6d"),7,3))),ifelse(dcat$PATHDX1 %in% c("8","9"),4,ifelse(dcat$PATHDX1 %in% c("1","1a","1b","1c"),ifelse(is.na(dcat$PATHDX2) | dcat$PATHDX2 %in% c("-6","-7","-9") | dcat$PATHDX2 %in% c("1","1a","1b","1c"),8,ifelse(dcat$PATHDX2 %in% c("2","3","4","4a","4b","4c"),9,10)),ifelse(dcat$PATHDX1 %in% c("10","10a","10b","10c") & dcat$PATHDX2 %in% c("8","9"),4,ifelse(!is.na(dcat$PATHDX1) & !dcat$PATHDX1 %in% c("-6","-7","-9"),10,NA)))))))

dcat$n_eval <- ifelse(dcat$NMMS <=9,dcat$NMMS, 10)
dcat$n_mri <- ifelse(dcat$n_mri <=3,dcat$n_mri, 4)
dcat$n_senas_2 <- ifelse(dcat$n_senas<=9,dcat$n_senas,10)
dcat$n_ivd_npsy_2 <- ifelse(dcat$n_ivd_npsy<=9,dcat$n_ivd_npsy,10)
dcat$n_serum <- ifelse(dcat$n_serum<=1,dcat$n_serum, 2)
dcat$n_plasma <- ifelse(dcat$n_plasma<=1,dcat$n_plasma, 2)
dcat$n_dna <- ifelse(dcat$n_dna<=1,dcat$n_dna, 2)



#	table(dcat$Cohort)
#	table(dcat$synd_ia)

dcat2 <- dcat[,c("ID", "DtLastEval", "GENDER", "ETHNICITY", "EDUC", "age_ia", "RecrSource", "Cohort", "center","synd_ia", "synd_mra","grp")]
ensur <- merge(enr[enr$ProjName=="ADCLC",],dcat2,by="ID")

#	table(ensur$grp)

ensur$MaxTime <- as.numeric((as.Date(ensur$DtLastEval) - ensur$EnrolledDate)/365.25)
ensur$timeDeath <- ifelse(ensur$LatestDesc=="DECEASED",as.numeric((ensur$LatestDate - ensur$EnrolledDate)/365.25),NA)
ensur$timeNLF <- ifelse(ensur$LatestDesc=="LOST" | ensur$LatestDesc=="DROPPED" | ensur$LatestDesc=="REFUSED",as.numeric((ensur$LatestDate - ensur$EnrolledDate)/365.25),NA)

ensur$time2NLF <- ifelse(is.na(ensur$timeNLF),ensur$MaxTime,ensur$timeNLF)
ensur$time2Death <- ifelse(is.na(ensur$timeDeath),ensur$MaxTime,ensur$timeDeath)
ensur$time7NLF <- ifelse(is.na(ensur$timeNLF),ifelse(ensur$MaxTime>7,7,ensur$MaxTime),ifelse(ensur$timeNLF>7,7,ensur$timeNLF))
ensur$time7Death <- ifelse(is.na(ensur$timeDeath),ifelse(ensur$MaxTime>7,7,ensur$MaxTime),ifelse(ensur$timeDeath>7,7,ensur$timeDeath))
ensur$CensorNLF <- ifelse(is.na(ensur$timeNLF),1,0)
ensur$CensorDeath <- ifelse(is.na(ensur$timeDeath),1,0)
ensur$Censor7NLF <- ifelse(is.na(ensur$timeNLF),1,ifelse(ensur$timeNLF>7,1,0))
ensur$Censor7Death <- ifelse(is.na(ensur$timeDeath),1,ifelse(ensur$timeDeath>7,1,0))

dcat$synd_ia <- 
  factor(dcat$synd_ia, 
         labels=c("Demented",
                  "MCI", 
                  "Normal"))
dcat$synd_mra <- 
  factor(dcat$synd_mra, 
         labels=c("Demented",
                  "MCI", 
                  "Normal"))
dcat$grp <- 
  factor(dcat$grp, 
         labels=c("African American",
                  "Hispanic", 
                  "White"))
dcat$grp2 <- 
  factor(dcat$grp2, 
         labels=c("African American",
                  "Hispanic",
                  "Other", 
                  "White"))
dcat$GENDER <- 
  factor(dcat$GENDER, 
         labels=c("Male",
                  "Female"))
dcat$center <- 
  factor(dcat$center, 
         labels=c("East_Bay",
                  "Sacramento"))
dcat$RecrSource <- 
  factor(dcat$RecrSource, 
         labels=c("Clinic",
                  "Community"))
dcat$Cohort <- 
  factor(dcat$Cohort, 
         labels=c("ADNI",
                  "C1",
                  "C2",
                  "C2/Hillblom",
                  "Hillblom",
                  "IVD"))
dcat$ClinDxMRA <- 
  factor(dcat$ClinDxMRA, 
         labels=c("AD",
                  "LBD",
                  "CVD",
                  "FTD",
                  "AD+CVD"))
dcat$ClinDxMRA2 <- 
  factor(dcat$ClinDxMRA2, 
         labels=c("AD",
                  "LBD",
                  "CVD",
                  "FTD",
                  "AD+CVD",
                  "MCI",
                  "Normal"))
dcat$PathDx <- 
  factor(dcat$PathDx, 
         labels=c("AD",
                  "LBD",
                  "CVD",
                  "FTD",
                  "AD+CVD",
                  "AD+LBD",
                  "LBD+CVD",
                  "Normal Brain",
                  "AD+Other",
                  "Other"))

dcat$ADCLC_Status <- 
  factor(dcat$ADCLC_Status, 
         labels=c("Active",
                  "Deceased",
                  "Dropped",
                  "Lost",
                  "Not Enrolled",
                  "Refused"))

dcat$n_eval  <- factor(dcat$n_eval, labels=c("1","2","3","4","5","6","7","8","9","10+"))
dcat$n_mri  <- factor(dcat$n_mri, labels=c("1","2","3","4+"))
dcat$n_mri_3 <- factor(dcat$n_mri_3)
dcat$n_pib <- factor(dcat$n_pib)
dcat$n_senas_2  <- factor(dcat$n_senas_2, labels=c("1","2","3","4","5","6","7","8","9","10+"))
dcat$n_ivd_npsy_2  <- factor(dcat$n_ivd_npsy_2, labels=c("1","2","3","4","5","6","7","8","9","10+"))
dcat$n_serum  <- factor(dcat$n_serum, labels=c("1","2+"))
dcat$n_plasma  <- factor(dcat$n_plasma, labels=c("1","2+"))
dcat$n_dna  <- factor(dcat$n_dna, labels=c("1","2+"))


# A function that takes the variable name,  group by variable name, and dataset name
# and then runs descriptive statistics for the variable by group
getT1Stat <- function(varname, byname, dfname, digits=1, html=TRUE, hrzl_prop=TRUE){
  getDescriptionStatsBy(dfname[, varname], 
                        dfname[, byname], 
                        add_total_col=TRUE,
                        show_all_values=TRUE, 
                        hrzl_prop=hrzl_prop,
                        statistics=FALSE, 
                        html=html, 
                        digits=digits)
}


# A function that merges results from getT1Stat into a matrix
# and creates the output data, rgroup & n.rgroup variables for latex()
descStatTable <- function(table_data){
  rgroup <- c()
  n.rgroup <- c()
  output_data <- NULL
  for (varlabel in names(table_data)){
    output_data <- rbind(output_data, 
                         table_data[[varlabel]])
    rgroup <- c(rgroup, 
                varlabel)
    n.rgroup <- c(n.rgroup, 
                  nrow(table_data[[varlabel]]))
  } 
  table_out <- list("output_data"=output_data,"rgroup"=rgroup,"n.rgroup"=n.rgroup) 
}

#  Create enrollment and status by year and ethnicity tables

library(doBy)
library(plyr)

#	enr <- read.table("enrollment_07-17-14.csv", header=TRUE, sep=",", na.strings = "")
#	dcat <- read.table("adc_data_catalog_07-10-14.csv", header=TRUE, sep=",", na.strings = "")
#	colnames(dcat)[2] <- "ID"

# dc=Data Table("adc_data_catalog_02-18-14.jmp");
# dt1=Data Table( "enrollment_02-10-14.jmp" ) << Select All Rows<<Sort(
# By( :ID),
# Order( Ascending)
# );

enr$LatestDate <- as.Date(enr$LatestDate)
enr$EnrolledDate <- as.Date(enr$EnrolledDate)
enr$DroppedDate <- as.Date(enr$DroppedDate)
enr$LostDate <- as.Date(enr$LostDate)
enr$RefusedDate <- as.Date(enr$RefusedDate)
enr$DeceasedDate <- as.Date(enr$DeceasedDate)

lc <- enr[enr$ProjName == "ADCLC" & !is.na(enr$EnrolledDate),c("ID","ProjName","EnrolledDate")]
lc$YearEnrLC <- as.numeric(format(lc$EnrolledDate, "%Y"))
enr <- merge(enr,lc[,c("ID","YearEnrLC")], by="ID")

enr1 <- enr
enr1$YearEnr <- as.numeric(format(enr1$EnrolledDate, "%Y"))
enr2 <- enr1[!is.na(enr$EnrolledDate),c("ID", "LatestDesc", "ProjName", "YearEnr", "YearEnrLC")]
enr2$EnrolledDesc <- "ENROLLED"

#	table(enr2$ProjName)

dc1 <- dcat[,c("ID","ETHNICITY")]
enr2 <- merge(enr2,dc1,by="ID")

# enr3 <- summaryBy(ID ~ YearEnr + ProjName + ETHNICITY, data = enr2, FUN=c(length))
# colnames(enr3)[4] <- "N"

# enr3$ETHNICITY <- factor(enr3$ETHNICITY)
# enr3$YearEnr <- factor(enr3$YearEnr)

# enr4 <- split(enr3, list(enr3$ProjName))

# for(i in 1:length(enr4)){
# if (i==1){
# enr5 <- as.data.frame(enr4[1])[,c(1,3,4)]
# colnames(enr5)[1:2] <- c("YearEnr","ETHNICITY")		
# } else {
# enrt <- as.data.frame(enr4[i])[,c(1,3,4)]
# colnames(enrt)[1:2] <- c("YearEnr","ETHNICITY")
# enr5 <- merge(enr5,enrt,by=c("YearEnr","ETHNICITY"),all=TRUE)		
# }
# }


enr3 <- ddply(enr2[!is.na(enr2$ETHNICITY),], .(YearEnrLC, ETHNICITY, ProjName), summarise,
              N = length(ID))

# enr3 <- ddply(enr2[!is.na(enr2$ETHNICITY) & enr2$YearEnr == enr2$YearEnrLC,], .(YearEnr, ETHNICITY, ProjName), summarise,
# N = length(ID))

#	Cohort enrollment by year and ethnicity

for (x in 1986:cur_yr){
  if(!x %in% enr3$YearEnrLC){
    rval <- data.frame(YearEnrLC = x, ETHNICITY = 9, ProjName = "ADCLC", N = 0)
    enr3 <- rbind(enr3,rval)
  }
}

enr4 <- ddply(enr3, .(YearEnrLC, ETHNICITY), summarise, 
              ADCLC.N = sum(ifelse(ProjName=="ADCLC",N,0)),
              C1.N = sum(ifelse(ProjName=="C1",N,0)),
              C2.N = sum(ifelse(ProjName=="C2",N,0)),
              Hillblom.N = sum(ifelse(ProjName=="Hillblom",N,0)),
              IVD.N = sum(ifelse(ProjName=="IVD",N,0)),
              ADNI.N = sum(ifelse(ProjName=="ADNI",N,0)))


#	Cohort 2 by year and ethnicity

enr5 <- ddply(enr4, .(YearEnrLC), summarise, 
              African_American.C2.N = sum(ifelse(ETHNICITY==4,C2.N,0)),
              Hispanic.C2.N = sum(ifelse(ETHNICITY==6, C2.N,0)),
              White.C2.N = sum(ifelse(ETHNICITY==7, C2.N,0)),
              Other.C2.N = sum(ifelse(ETHNICITY %in% c(1,2,3,5,8), C2.N,0))
)

enr5$Total <- enr5$African_American.C2.N + enr5$Hispanic.C2.N + enr5$White.C2.N + enr5$Other.C2.N 

#	ADCLC by year and ethnicity

enr6 <- ddply(enr4, .(YearEnrLC), summarise, 
              African_American.ADCLC.N = sum(ifelse(ETHNICITY==4,ADCLC.N,0)),
              Hispanic.ADCLC.N = sum(ifelse(ETHNICITY==6, ADCLC.N,0)),
              White.ADCLC.N = sum(ifelse(ETHNICITY==7, ADCLC.N,0)),
              Other.ADCLC.N = sum(ifelse(ETHNICITY %in% c(1,2,3,5,8), ADCLC.N,0))
)

enr6$Total <- enr6$African_American.ADCLC.N + enr6$Hispanic.ADCLC.N + enr6$White.ADCLC.N + enr6$Other.ADCLC.N

#	----------------------------------------------------------------------------------------------------
#	Enrollment Status
#	----------------------------------------------------------------------------------------------------


#	ADCLC by year and ethnicity


enrst <- enr[enr$ProjName=="ADCLC",]

for (yr in 2001:cur_yr){
  lbl <- paste("Status_",yr,sep="")
  dt <- paste(yr,"-12-31",sep="")
  enrst[,lbl] <- ifelse(is.na(enrst$EnrolledDate),"Not_Enrolled",ifelse(!is.na(enrst$RefusedDate) & enrst$RefusedDate <= dt & enrst$RefusedDate >= enrst$EnrolledDate,"Refused",ifelse(!is.na(enrst$DroppedDate) & enrst$DroppedDate <= dt & enrst$DroppedDate >= enrst$EnrolledDate,"Dropped",ifelse(!is.na(enrst$DeceasedDate) & enrst$DeceasedDate <= dt & enrst$DeceasedDate >= enrst$EnrolledDate,"Deceased",ifelse(!is.na(enrst$LostDate) & enrst$LostDate <= dt & enrst$LostDate >= enrst$EnrolledDate,"Lost",ifelse(enrst$EnrolledDate <= dt,"Active",NA))))))	
}

dcat1 <- dcat[,c("ID","ETHNICITY", "ADCLC_Status")]
enrst1 <- merge(enrst,dcat1,by="ID")
#	enrst1 <- enrst1[enrst1$ADCLC_Status=="Active",]
#	nrow(enrst1)
enrst1 <- enrst1[!is.na(enrst1$ETHNICITY),]

cnmst <- sapply(2001:cur_yr, function(x) paste("Status_",x,sep=""))
enrst2 <- data.frame(enrst1[,c("ID","ETHNICITY")], stack(enrst1[,cnmst]))
enrst2 <- enrst2[with(enrst2,order(ID)),]

# enrst2 <- data.frame(enrst1[,c("ID","ETHNICITY")], stack(enrst1[,c("Status_2001","Status_2002","Status_2003","Status_2004", "Status_2005","Status_2006","Status_2007","Status_2008","Status_2009","Status_2010","Status_2011","Status_2012", "Status_2013","Status_2014")]))
# enrst2 <- enrst2[with(enrst2,order(ID)),]

enrst2$year <- substr(enrst2 $ind,8,11)
enrst2 <- enrst2[!is.na(enrst2$values),]
enrst2 <- enrst2[enrst2$values=="Active",]


enrst3 <- ddply(enrst2, .(ETHNICITY, year), summarise,
                N = length(ID))


enrst4 <- ddply(enrst3, .(year), summarise, 
                African_American.ADCLC.N = sum(ifelse(ETHNICITY==4,N,0)),
                Hispanic.ADCLC.N = sum(ifelse(ETHNICITY==6, N,0)),
                White.ADCLC.N = sum(ifelse(ETHNICITY==7, N,0)),
                Other.ADCLC.N = sum(ifelse(ETHNICITY %in% c(1,2,3,5,8), N,0))
)

enrst4$Total <- enrst4$African_American.ADCLC.N + enrst4$Hispanic.ADCLC.N + enrst4$White.ADCLC.N + enrst4$Other.ADCLC.N	


#	C2 by year and ethnicity with transfers

enrstc2 <- enr[enr$ProjName=="C2",]

for (yr in 2001:cur_yr){
  lbl <- paste("Status_",yr,sep="")
  dt <- paste(yr,"-12-31",sep="")
  enrstc2[,lbl] <- ifelse(is.na(enrstc2$EnrolledDate),"Not_Enrolled",ifelse(!is.na(enrstc2$RefusedDate) & enrstc2$RefusedDate <= dt & enrstc2$RefusedDate >= enrstc2$EnrolledDate,"Refused",ifelse(!is.na(enrstc2$DroppedDate) & enrstc2$DroppedDate <= dt & enrstc2$DroppedDate >= enrstc2$EnrolledDate,"Dropped",ifelse(!is.na(enrstc2$DeceasedDate) & enrstc2$DeceasedDate <= dt & enrstc2$DeceasedDate >= enrstc2$EnrolledDate,"Deceased",ifelse(!is.na(enrstc2$LostDate) & enrstc2$LostDate <= dt & enrstc2$LostDate >= enrstc2$EnrolledDate,"Lost",ifelse(enrstc2$EnrolledDate <= dt,"Active",NA))))))	
}

dcat1 <- dcat[,c("ID","ETHNICITY", "ADCLC_Status")]
enrstc21 <- merge(enrstc2,dcat1,by="ID")
#	enrstc21 <- enrstc21[enrstc21$ADCLC_Status=="Active",]
#	nrow(enrstc21)
enrstc21 <- enrstc21[!is.na(enrstc21$ETHNICITY),]

#	enrstc21[,c("ID","ETHNICITY","Status_2001","Status_2002","Status_2003","Status_2004", "Status_2005","Status_2006","Status_2007","Status_2008","Status_2009","Status_2010","Status_2011","Status_2012", "Status_2013","Status_2014")]

enrstc22 <- data.frame(enrstc21[,c("ID","ETHNICITY")], stack(enrstc21[,cnmst]))
enrstc22 <- enrstc22[with(enrstc22,order(ID)),]

# enrstc22 <- data.frame(enrstc21[,c("ID","ETHNICITY")], stack(enrstc21[,c("Status_2001","Status_2002","Status_2003","Status_2004", "Status_2005","Status_2006","Status_2007","Status_2008","Status_2009","Status_2010","Status_2011","Status_2012", "Status_2013","Status_2014")]))
# enrstc22 <- enrstc22[with(enrstc22,order(ID)),]

enrstc22$year <- substr(enrstc22 $ind,8,11)
enrstc22 <- enrstc22[!is.na(enrstc22$values),]
enrstc22 <- enrstc22[enrstc22$values=="Active",]


enrstc23 <- ddply(enrstc22, .(ETHNICITY, year), summarise,
                  N = length(ID))


enrstc24 <- ddply(enrstc23, .(year), summarise, 
                  African_American.C2.N = sum(ifelse(ETHNICITY==4,N,0)),
                  Hispanic.C2.N = sum(ifelse(ETHNICITY==6, N,0)),
                  White.C2.N = sum(ifelse(ETHNICITY==7, N,0)),
                  Other.C2.N = sum(ifelse(ETHNICITY %in% c(1,2,3,5,8), N,0))
)

enrstc24$Total <- enrstc24$African_American.C2.N + enrstc24$Hispanic.C2.N + enrstc24$White.C2.N + enrstc24$Other.C2.N	


#	C2 by year and ethnicity without transfers

enrstc2t <- enr[enr$ProjName=="C2",]

enrstivd <- enr[enr$ProjName=="IVD" & !is.na(enr$EnrolledDate),c("ID","EnrolledDate")]
colnames(enrstivd) <- c("ID","IVD_Enr_Date")
enrstc1 <- enr[enr$ProjName=="C1" & !is.na(enr$EnrolledDate),c("ID","EnrolledDate")]
colnames(enrstc1) <- c("ID","C1_Enr_Date")
enrstadni <- enr[enr$ProjName=="ADNI" & !is.na(enr$EnrolledDate),c("ID","EnrolledDate")]
colnames(enrstadni) <- c("ID","ADNI_Enr_Date")

enrstc2t <- merge(enrstc2t,enrstivd,by="ID",all.x=TRUE)
enrstc2t <- merge(enrstc2t,enrstc1,by="ID",all.x=TRUE)
enrstc2t <- merge(enrstc2t,enrstadni,by="ID",all.x=TRUE)
enrstc2t <- enrstc2t[(is.na(enrstc2t$IVD_Enr_Date) | enrstc2t$IVD_Enr_Date > enrstc2t$EnrolledDate) & (is.na(enrstc2t$C1_Enr_Date) | enrstc2t$C1_Enr_Date > enrstc2t$EnrolledDate) & (is.na(enrstc2t$ADNI_Enr_Date) | enrstc2t$ADNI_Enr_Date > enrstc2t$EnrolledDate),]


for (yr in 2001:cur_yr){
  lbl <- paste("Status_",yr,sep="")
  dt <- paste(yr,"-12-31",sep="")
  enrstc2t[,lbl] <- ifelse(is.na(enrstc2t$EnrolledDate),"Not_Enrolled",ifelse(!is.na(enrstc2t$RefusedDate) & enrstc2t$RefusedDate <= dt & enrstc2t$RefusedDate >= enrstc2t$EnrolledDate,"Refused",ifelse(!is.na(enrstc2t$DroppedDate) & enrstc2t$DroppedDate <= dt & enrstc2t$DroppedDate >= enrstc2t$EnrolledDate,"Dropped",ifelse(!is.na(enrstc2t$DeceasedDate) & enrstc2t$DeceasedDate <= dt & enrstc2t$DeceasedDate >= enrstc2t$EnrolledDate,"Deceased",ifelse(!is.na(enrstc2t$LostDate) & enrstc2t$LostDate <= dt & enrstc2t$LostDate >= enrstc2t$EnrolledDate,"Lost",ifelse(enrstc2t$EnrolledDate <= dt,"Active",NA))))))	
}

dcat1 <- dcat[,c("ID","ETHNICITY", "ADCLC_Status")]
enrstc2t1 <- merge(enrstc2t,dcat1,by="ID")
#	enrstc2t1 <- enrstc2t1[enrstc2t1$ADCLC_Status=="Active",]
#	nrow(enrstc2t1)
enrstc2t1 <- enrstc2t1[!is.na(enrstc2t1$ETHNICITY),]

#	enrstc2t1[,c("ID","ETHNICITY","Status_2001","Status_2002","Status_2003","Status_2004", "Status_2005","Status_2006","Status_2007","Status_2008","Status_2009","Status_2010","Status_2011","Status_2012", "Status_2013","Status_2014")]

enrstc2t2 <- data.frame(enrstc2t1[,c("ID","ETHNICITY")], stack(enrstc2t1[,cnmst]), row.names=NULL)
enrstc2t2 <- enrstc2t2[with(enrstc2t2,order(ID)),]

# enrstc2t2 <- data.frame(enrstc2t1[,c("ID","ETHNICITY")], stack(enrstc2t1[,c("Status_2001","Status_2002","Status_2003","Status_2004", "Status_2005","Status_2006","Status_2007","Status_2008","Status_2009","Status_2010","Status_2011","Status_2012", "Status_2013","Status_2014")]))
# enrstc2t2 <- enrstc2t2[with(enrstc2t2,order(ID)),]

enrstc2t2$year <- substr(enrstc2t2 $ind,8,11)
enrstc2t2 <- enrstc2t2[!is.na(enrstc2t2$values),]
enrstc2t2 <- enrstc2t2[enrstc2t2$values=="Active",]

#	table(enrstc2t2$values)

enrstc2t3 <- ddply(enrstc2t2, .(ETHNICITY, year), summarise,
                   N = length(ID))


enrstc2t4 <- ddply(enrstc2t3, .(year), summarise, 
                   African_American.C2.N = sum(ifelse(ETHNICITY==4,N,0)),
                   Hispanic.C2.N = sum(ifelse(ETHNICITY==6, N,0)),
                   White.C2.N = sum(ifelse(ETHNICITY==7, N,0)),
                   Other.C2.N = sum(ifelse(ETHNICITY %in% c(1,2,3,5,8), N,0))
)

enrstc2t4$Total <- enrstc2t4$African_American.C2.N + enrstc2t4$Hispanic.C2.N + enrstc2t4$White.C2.N + enrstc2t4$Other.C2.N

#	Autopsy by year and ethnicity 

enrstaut <- enr[enr$ProjName=="Autopsy",]

enrstadclc <- enr[enr$ProjName=="ADCLC" & !is.na(enr$EnrolledDate),c("ID","EnrolledDate","RefusedDate","DeceasedDate","DroppedDate","LostDate")]
colnames(enrstadclc) <- c("ID","ADCLC_Enr_Date","ADCLC_Ref_Date","ADCLC_Dec_Date","ADCLC_Drop_Date","ADCLC_Lost_Date")

enrstaut <- merge(enrstaut,enrstadclc,by="ID")
enrstaut <- enrstaut[!is.na(enrstaut$ADCLC_Enr_Date),]

for (yr in 2001:cur_yr){
  lbl <- paste("Status_",yr,sep="")
  dt <- paste(yr,"-12-31",sep="")
  enrstaut[,lbl] <- ifelse(is.na(enrstaut$EnrolledDate),"Not_Enrolled",ifelse(!is.na(enrstaut$RefusedDate) & enrstaut$RefusedDate <= dt & enrstaut$RefusedDate >= enrstaut$EnrolledDate,"Refused",ifelse(!is.na(enrstaut$DroppedDate) & enrstaut$DroppedDate <= dt & enrstaut$DroppedDate >= enrstaut$EnrolledDate,"Dropped",ifelse(!is.na(enrstaut$DeceasedDate) & enrstaut$DeceasedDate <= dt & enrstaut$DeceasedDate >= enrstaut$EnrolledDate,"Deceased",ifelse(!is.na(enrstaut$LostDate) & enrstaut$LostDate <= dt & enrstaut$LostDate >= enrstaut$EnrolledDate,"Lost",ifelse(enrstaut$EnrolledDate <= dt,"Active",NA))))))	
}

for (yr in 2001:cur_yr){
  lbl <- paste("Status_LC_",yr,sep="")
  dt <- paste(yr,"-12-31",sep="")
  enrstaut[,lbl] <- ifelse(is.na(enrstaut$ADCLC_Enr_Date),"Not_Enrolled",ifelse(!is.na(enrstaut$ADCLC_Ref_Date) & enrstaut$ADCLC_Ref_Date <= dt & enrstaut$ADCLC_Ref_Date >= enrstaut$ADCLC_Enr_Date,"Refused",ifelse(!is.na(enrstaut$ADCLC_Drop_Date) & enrstaut$ADCLC_Drop_Date <= dt & enrstaut$ADCLC_Drop_Date >= enrstaut$ADCLC_Enr_Date,"Dropped",ifelse(!is.na(enrstaut$ADCLC_Dec_Date) & enrstaut$ADCLC_Dec_Date <= dt & enrstaut$ADCLC_Dec_Date >= enrstaut$ADCLC_Enr_Date,"Deceased",ifelse(!is.na(enrstaut$ADCLC_Lost_Date) & enrstaut$ADCLC_Lost_Date <= dt & enrstaut$ADCLC_Lost_Date >= enrstaut$ADCLC_Enr_Date,"Lost",ifelse(enrstaut$ADCLC_Enr_Date <= dt,"Active",NA))))))	
}

for (yr in 2001:cur_yr){
  lbl <- paste("Status_LC_",yr,sep="")
  dt <- paste(yr,"-12-31",sep="")
  enrstaut[,lbl] <- ifelse(is.na(enrstaut$ADCLC_Enr_Date),"Not_Enrolled",ifelse(!is.na(enrstaut$ADCLC_Ref_Date) & enrstaut$ADCLC_Ref_Date <= dt & enrstaut$ADCLC_Ref_Date >= enrstaut$ADCLC_Enr_Date,"Refused",ifelse(!is.na(enrstaut$ADCLC_Drop_Date) & enrstaut$ADCLC_Drop_Date <= dt & enrstaut$ADCLC_Drop_Date >= enrstaut$ADCLC_Enr_Date,"Dropped",ifelse(!is.na(enrstaut$ADCLC_Dec_Date) & enrstaut$ADCLC_Dec_Date <= dt & enrstaut$ADCLC_Dec_Date >= enrstaut$ADCLC_Enr_Date,"Deceased",ifelse(!is.na(enrstaut$ADCLC_Lost_Date) & enrstaut$ADCLC_Lost_Date <= dt & enrstaut$ADCLC_Lost_Date >= enrstaut$ADCLC_Enr_Date,"Lost",ifelse(enrstaut$ADCLC_Enr_Date <= dt,"Active",NA))))))	
}



#	Autopsy


dcat1 <- dcat[,c("ID","ETHNICITY", "ADCLC_Status")]
enrstaut1 <- merge(enrstaut,dcat1,by="ID")
#	enrstaut1 <- enrstaut1[enrstaut1$ADCLC_Status=="Active",]
#	nrow(enrstaut1)
enrstaut1 <- enrstaut1[!is.na(enrstaut1$ETHNICITY),]

#	enrstaut1[,c("ID","ETHNICITY","Status_2001","Status_2002","Status_2003","Status_2004", "Status_2005","Status_2006","Status_2007","Status_2008","Status_2009","Status_2010","Status_2011","Status_2012", "Status_2013","Status_2014")]

cnmsta <- sapply(2001:cur_yr, function(x) paste("Status_LC_",x,sep=""))
enrstaut2 <- data.frame(enrstaut1[,c("ID","ETHNICITY")], stack(enrstaut1[,cnmst]), stack(enrstaut1[,cnmsta]), row.names=NULL)

# enrstaut2 <- data.frame(enrstaut1[,c("ID","ETHNICITY")], stack(enrstaut1[,c("Status_2001","Status_2002","Status_2003","Status_2004", "Status_2005","Status_2006","Status_2007","Status_2008","Status_2009","Status_2010","Status_2011","Status_2012", "Status_2013","Status_2014")]), stack(enrstaut1[,c("Status_LC_2001","Status_LC_2002","Status_LC_2003","Status_LC_2004", "Status_LC_2005","Status_LC_2006","Status_LC_2007","Status_LC_2008","Status_LC_2009","Status_LC_2010","Status_LC_2011","Status_LC_2012", "Status_LC_2013","Status_LC_2014")]))

enrstaut2 <- enrstaut2[with(enrstaut2,order(ID)),]

enrstaut2$year <- substr(enrstaut2 $ind,8,11)
enrstaut2 <- enrstaut2[!is.na(enrstaut2$values) & !is.na(enrstaut2$values.1),]
enrstaut2 <- enrstaut2[enrstaut2$values.1=="Active",]

enrstlc2 <- enrstaut2

enrstaut2 <- enrstaut2[enrstaut2$values=="Active",]
#	table(enrstaut2$values)

enrstaut3 <- ddply(enrstaut2, .(ETHNICITY, year), summarise,
                   N = length(ID))


enrstaut4 <- ddply(enrstaut3, .(year), summarise, 
                   African_American.Aut.N = sum(ifelse(ETHNICITY==4,N,0)),
                   Hispanic.Aut.N = sum(ifelse(ETHNICITY==6, N,0)),
                   White.Aut.N = sum(ifelse(ETHNICITY==7, N,0)),
                   Other.Aut.N = sum(ifelse(ETHNICITY %in% c(1,2,3,5,8), N,0))
)

enrstaut4$Total_Aut <- enrstaut4$African_American.Aut.N + enrstaut4$Hispanic.Aut.N + enrstaut4$White.Aut.N + enrstaut4$Other.Aut.N	

#	ADCLC 


enrstlc3 <- ddply(enrstlc2, .(ETHNICITY, year), summarise,
                  N = length(ID))


enrstlc4 <- ddply(enrstlc3, .(year), summarise, 
                  African_American.ADCLC.N = sum(ifelse(ETHNICITY==4,N,0)),
                  Hispanic.ADCLC.N = sum(ifelse(ETHNICITY==6, N,0)),
                  White.ADCLC.N = sum(ifelse(ETHNICITY==7, N,0)),
                  Other.ADCLC.N = sum(ifelse(ETHNICITY %in% c(1,2,3,5,8), N,0))
)

enrstlc4$Total_LC <- enrstlc4$African_American.ADCLC.N + enrstlc4$Hispanic.ADCLC.N + enrstlc4$White.ADCLC.N + enrstlc4$Other.ADCLC.N	

enrstaut4 <- merge(enrstaut4,enrstlc4,by="year")

enrstaut4$Afr_Am_Pct_Aut <- (enrstaut4$African_American.Aut.N / enrstaut4$African_American.ADCLC.N) * 100
enrstaut4$Hisp_Pct_Aut <- (enrstaut4$Hispanic.Aut.N / enrstaut4$Hispanic.ADCLC.N) * 100
enrstaut4$White_Pct_Aut <- (enrstaut4$White.Aut.N / enrstaut4$White.ADCLC.N) * 100
enrstaut4$Total_Pct_Aut <- (enrstaut4$Total_Aut / enrstaut4$Total_LC) * 100


# ----------------------------------- Eval History -------------------------------------------------

library(dplyr)

colnames(asl1) <- gsub("idal","ID",colnames(asl1))
asl1$ID <- as.numeric(asl1$ID)
asl1$VDate <- as.Date(asl1$VDate)

asl1 <- asl1[!is.na(asl1$VisitType) & !asl1$VisitType == "",]

asl2 <- group_by(asl1, ID)
asl3 <- summarise(asl2,
                  min_assess_list_id = min(AssessListID))

asl1 <- merge(asl1,asl3,by="ID")
asl1$asn <- asl1$AssessListID - asl1$min_assess_list_id +1

asl4 <- asl1[,c("ID","asn","VDate","VisitType")]
colnames(asl4) <- gsub("VDate","vdt_next",colnames(asl4))
colnames(asl4) <- gsub("VisitType","vtyp_next",colnames(asl4))

asl4$asn <- asl4$asn-1


asl <- merge(asl1[,c("ID","AssessListID","asn","VDate","VisitType","LastOfficeEval")], asl4[,c("ID","asn","vdt_next","vtyp_next")], by=c("ID","asn"),all.x=TRUE)

rm(asl2,asl3,asl4)

vlist <- c("Home","Phone","Lost","Refused","No Longer Followed","Deceased","Other")

asl <- asl[!asl$VisitType %in% vlist,]

asl$time_betw <- as.numeric((asl$vdt_next - asl$VDate) / 365.25)

asl$year <- as.numeric(format(asl$vdt_next, "%Y"))
asl$month <- as.numeric(format(asl$vdt_next, "%m"))
asl$monthl <- format(asl$vdt_next, "%b")

# requires dcat from main program

dcat1 <- dcat[,c("ID","ETHNICITY", "Cohort", "center","ADCLC_Status","ADCLC_Enroll","DtLastEval")]

dcat1$time_last <- as.numeric(((as.Date(date(), format="%a %b %d %H:%M:%S %Y") - dcat1$DtLastEval)/365.25))
dcat1$year_last <- as.factor(format(dcat1$DtLastEval, "%Y"))


asl <- merge(asl,dcat1,by="ID")

asl1$year <- as.numeric(format(asl1$VDate, "%Y"))
asl1$month <- as.numeric(format(asl1$VDate, "%m"))
asl1$monthl <- format(asl1$VDate, "%b")

asl1 <- merge(asl1,dcat1[,c("ID","Cohort","ADCLC_Enroll","ADCLC_Status"),], by="ID")


# ------------------------------------ Enroll survival prelim --------------------------------------

eth <- ensur

eth$status <- ifelse(eth$CensorNLF==0,1,ifelse(eth$CensorNLF ==1,0,NA))
eth$status7 <- ifelse(eth$Censor7NLF==0,1,ifelse(eth$Censor7NLF ==1,0,NA))
eth$time7 <- ifelse(eth$time2NLF > 7, 7, eth$time2NLF)
eth$statusd <- ifelse(eth$CensorDeath==0,1,ifelse(eth$CensorDeath ==1,0,NA))
eth$status7d <- ifelse(eth$Censor7Death==0,1,ifelse(eth$Censor7Death ==1,0,NA))
eth$time7d <- ifelse(eth$time2Death > 7, 7, eth$time2Death)



ethc2 <- eth[eth$Cohort == "C2" | eth$Cohort == "C2/Hillblom" | eth$Cohort == "Hillblom",]
ethe <- eth[(eth$Cohort == "C2" | eth$Cohort == "C2/Hillblom" | eth$Cohort == "Hillblom") & eth$center=="East_Bay",]
eths <- eth[(eth$Cohort == "C2" | eth$Cohort == "C2/Hillblom" | eth$Cohort == "Hillblom") & eth$center=="Sacramento",]

leg1 = c("Demented","MCI","Normal")
leg2 = c("African American","Hispanic","White")


# --------------------------------- Path dx prelim ----------------------------------------------

dcat$ClinDxPres <- ifelse(!is.na(dcat$ClinDxMRA) | dcat$SYNDRMRA==2 | dcat$SYNDRMRA==3 | dcat$SYNDRMRA==6,1,2)
dcat$ClinDxPres <- 
  factor(dcat$ClinDxPres, 
         labels=c("All Clin Dx",
                  NA))

n_lbd_cvd <- nrow(dcat[!is.na(dcat$PathDx) & dcat$PathDx=="LBD+CVD",])
n_oth <- nrow(dcat[!is.na(dcat$PathDx) & dcat$PathDx=="Other",])

