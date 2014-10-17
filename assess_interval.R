library(dplyr)
library(ggplot2)


asl1 <- read.table("./Input Files/assesslist_09-12-14.csv", header=TRUE, sep=",", na.strings = "")

colnames(asl1) <- gsub("idal","ID",colnames(asl1))
asl1$ID <- as.numeric(asl1$ID)
asl1$VDate <- as.Date(asl1$VDate)

asl1 <- asl1[!is.na(asl1$VisitType),]

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

# requires dcat from main program

dcat1 <- dcat[,c("ID","ETHNICITY", "Cohort", "center","ADCLC_Status","ADCLC_Enroll","DtLastEval")]

dcat1$time_last <- as.numeric(((as.Date(date(), format="%a %b %d %H:%M:%S %Y") - dcat1$DtLastEval)/365.25))
dcat1$year_last <- as.factor(format(dcat1$DtLastEval, "%Y"))


asl <- merge(asl,dcat1,by="ID")

# asl$LastOfficeEval <- as.character(asl$LastOfficeEval)

asl5 <- asl[asl$ADCLC_Enroll == "Enrolled",]
# boxplot(asl5$time_betw ~ asl5$year, col="red",outcol="blue",whiskcol="blue",staplecol="blue",medcol="blue",boxcol="blue",main="ADCLC - Time Since Previous Evaluation")

asl6 <- group_by(asl5, year)
asl7 <- summarise(asl6,
                  mean_time_between = mean(time_betw))

asl5e <- asl[asl$ADCLC_Enroll == "Enrolled" & asl$center == "East_Bay",]
asl6e <- group_by(asl5e, year)
asl7e <- summarise(asl6e,
                  mean_time_between = mean(time_betw))

asl5s <- asl[asl$ADCLC_Enroll == "Enrolled" & asl$center == "Sacramento",]
asl6s <- group_by(asl5s, year)
asl7s <- summarise(asl6s,
                   mean_time_between = mean(time_betw))

plot(mean_time_between ~ year, data=asl7, type="n",pch=1, main="ADCLC - Average Time Between Office Evaluations by Year",xlim=c(2001,cur_yr), ylim=c(0.8,1.8))
lines(mean_time_between ~ year, data=asl7, type="l",col="red",lwd=2)
lines(mean_time_between ~ year, data=asl7e, type="l",col="blue",lwd=2)
lines(mean_time_between ~ year, data=asl7s, type="l",col="green",lwd=2)

legend("topleft",c("ADC Combined","East Bay","Sacramento"), lty=1, col=c("red","blue","green"),bty="n", lwd=2)

rm(asl5,asl6,asl5e,asl6e,asl5s,asl6s)



# Create time since last eval by year


enrdt <- enr[enr$ProjName=="ADCLC", c("ID","EnrolledDate","DroppedDate","LostDate","RefusedDate","DeceasedDate")]
# enrdt$ID <- as.numeric(enrdt$ID)
# enrdt <- enrdt[order(c"ID")),]
enrdt$EnrolledDate <- as.Date(enrdt$EnrolledDate)
enrdt$DroppedDate <- as.Date(enrdt$DroppedDate)
enrdt$LostDate <- as.Date(enrdt$LostDate)
enrdt$RefusedDate <- as.Date(enrdt$RefusedDate)
enrdt$DeceasedDate <- as.Date(enrdt$DeceasedDate)

vlist2 <- c("Lost","Refused","No Longer Followed","Deceased","Other")

tmlst  <- data.frame(ID=numeric(),
                          min_time_last=numeric(),
                          year=numeric(),
                          center=character(), 
                          stringsAsFactors=FALSE) 

for (yr in 2001:cur_yr){
  dt = as.Date(paste(yr,"-01-01",sep=""))
  
  asl00 <- asl1[asl1$VDate < dt & !asl1$VisitType %in% vlist2,c("ID","VDate","VisitType")]
  asl00 <- merge(asl00,enrdt,by="ID",all.x=TRUE)
  
  asl00 <- asl00[!is.na(asl00$EnrolledDate) & asl00$EnrolledDate <= asl00$VDate,]
  asl00 <- asl00[is.na(asl00$DroppedDate) | asl00$DroppedDate > dt,]
  asl00 <- asl00[is.na(asl00$LostDate) | asl00$LostDate > dt,]
  asl00 <- asl00[is.na(asl00$RefusedDate) | asl00$RefusedDate > dt,]
  asl00 <- asl00[is.na(asl00$DeceasedDate) | asl00$DeceasedDate > dt,]
  
  asl00$time_last <- as.numeric((dt - asl00$VDate) / 365.25)
  
  asl002 <- group_by(asl00, ID)
  asl003 <- summarise(asl002,
                      min_time_last = min(time_last))
  
  asl003$year <- yr
  asl003$center <- ifelse(asl003$ID < 50000,"Sacramento","East_Bay")
  
  tmlst <- rbind(tmlst,asl003)
  
}

tmlst1 <- group_by(tmlst, year)
tmlstc <- summarise(tmlst1,
                  mean_time_last = mean(min_time_last))

tmlst1e <- group_by(tmlst[tmlst$center == "East_Bay",], year)
tmlste <- summarise(tmlst1e,
                    mean_time_last = mean(min_time_last))

tmlst1s <- group_by(tmlst[tmlst$center == "Sacramento",], year)
tmlsts <- summarise(tmlst1s,
                    mean_time_last = mean(min_time_last))

plot(mean_time_last ~ year, data=tmlstc, type="n",pch=1, main="ADCLC - Average Time Since Last Evaluation by Year",xlim=c(2001,cur_yr), ylim=c(0.6,1.1))
lines(mean_time_last ~ year, data=tmlstc, type="l",col="red",lwd=2)
lines(mean_time_last ~ year, data=tmlste, type="l",col="blue",lwd=2)
lines(mean_time_last ~ year, data=tmlsts, type="l",col="green",lwd=2)

legend("topleft",c("ADC Combined","East Bay","Sacramento"), lty=1, col=c("red","blue","green"),bty="n", lwd=2)

rm(tmlst1,tmlst1e,tmlst1s)
##  ----------------------------------------------------------------------------------------------


asl1$year <- as.numeric(format(asl1$VDate, "%Y"))

asl1 <- merge(asl1,dcat1[,c("ID","Cohort","ADCLC_Enroll","ADCLC_Status"),], by="ID")

# Number of IA Visits per Year

noff1 <- group_by(asl1[asl1$VisitType == "Office" & asl1$FirstEval == "Yes" & asl1$ADCLC_Enroll == "Enrolled",], year)
noffc <- summarise(noff1,
                    n_office_vis = n())

noff1 <- group_by(asl1[asl1$VisitType == "Office" & asl1$ID >= 50000 & asl1$FirstEval == "Yes" & asl1$ADCLC_Enroll == "Enrolled",], year)
noffe <- summarise(noff1,
                   n_office_vis = n())

noff1 <- group_by(asl1[asl1$VisitType == "Office" & asl1$ID < 50000 & asl1$FirstEval == "Yes" & asl1$ADCLC_Enroll == "Enrolled",], year)
noffs <- summarise(noff1,
                   n_office_vis = n())

plot(n_office_vis ~ year, data=noffc, type="n",pch=1, main="Number of Longitudinal Cohort Initial Evaluations by Year",xlim=c(2001,cur_yr), ylim=c(0,200), xlab="Year", ylab="Number of Evaluations")
lines(n_office_vis ~ year, data=noffc, type="l",col="red",lwd=2)
lines(n_office_vis ~ year, data=noffe, type="l",col="blue",lwd=2)
lines(n_office_vis ~ year, data=noffs, type="l",col="green",lwd=2)

legend("topleft",c("ADC Combined","East Bay","Sacramento"), lty=1, col=c("red","blue","green"),bty="n", lwd=2)

rm(noff1)

# Number of RA Visits per Year

noff2 <- group_by(asl1[asl1$VisitType == "Office" & !"Yes" %in% asl1$Visitnum & asl1$ADCLC_Enroll == "Enrolled",], year)
noff2c <- summarise(noff2,
                   n_office_vis = n())

noff2 <- group_by(asl1[asl1$VisitType == "Office" & asl1$ID >= 50000 & !"Yes" %in% asl1$Visitnum & asl1$ADCLC_Enroll == "Enrolled",], year)
noff2e <- summarise(noff2,
                   n_office_vis = n())

noff2 <- group_by(asl1[asl1$VisitType == "Office" & asl1$ID < 50000 & !"Yes" %in% asl1$Visitnum &  asl1$ADCLC_Enroll == "Enrolled",], year)
noff2s <- summarise(noff2,
                   n_office_vis = n())

plot(n_office_vis ~ year, data=noff2c, type="n",pch=1, main="Number of Longitudinal Cohort Follow-up Evaluations by Year",xlim=c(2001,cur_yr), ylim=c(0,700), xlab="Year", ylab="Number of Evaluations")
lines(n_office_vis ~ year, data=noff2c, type="l",col="red",lwd=2)
lines(n_office_vis ~ year, data=noff2e, type="l",col="blue",lwd=2)
lines(n_office_vis ~ year, data=noff2s, type="l",col="green",lwd=2)

legend("topleft",c("ADC Combined","East Bay","Sacramento"), lty=1, col=c("red","blue","green"),bty="n", lwd=2)

rm(noff2)




