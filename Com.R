setwd("C:/Users/212713582/Desktop/SR-MIS/New sourc ivr log")

df1<- read.delim("Feb_22222.txt", header = TRUE, sep = ",")


df1$AOD <- as.POSIXct(df1$Dt.Acct.Opening.Date, format="%d-%B-%Y") 
df2 <- with(df1, df1[(AOD >= "0018-08-01"), ])

library(tidyverse)
library(dplyr)
df3 <- unique(df2)
write.csv(df3, file = "Feb_AO.csv", row.names=FALSE)

require(dplyr)
df3 <- anti_join(df2,df8)


D<- df8  %>% filter(!MobNo %in% m1$V1) 
E<- D  %>% filter(MobNo %in% m1$V1)


UAC_sql = sqldf("SELECT df8.AccNo, df8.MobNo FROM df8 where df8.MobNo NOT IN (SELECT m1.V1 from m1)")

library(sqldf)
df1 = df1[-1,]
library(RH2)
sqldf('select * from df1 where AOD > "01-08-0018"')

x6<- read.delim("Sep_2222.txt", header = TRUE, sep = ",", colClasses=c("C.Account.No"="character"))


df5<- anti_join(df4, M, by="MobNo")

E<- df5  %>% filter(MobNo %in% M$MobNo)
write.csv(df5, file = "AIF_rest.csv", row.names=FALSE)
write.table(df5, file = "AIF_rest.txt", sep = "\t", row.names = FALSE)
AIF <- read.csv('AIF With Mobile Nos. Till Aug 18.csv',stringsAsFactors = F,header=T, colClasses=c("ACC_MST_ACCT"="character"))
View(AIF)
E<- AIF  %>% filter(MAC_MST_MOBILE_PHONE %in% df5$MobNo)
View(E)
x1<- read.delim("Feb_22222.txt", header = TRUE, sep = ",", colClasses=c("C Account No"="character"))

 
library(tidyr)
df1<- df %>% drop_na(MobNo)
 
x1<- read.delim("Feb_22222.txt", header = TRUE, sep = ",", colClasses=c("C.Account.No"="character"))
x = rbind(x,x2)
x = distinct(x)


AIF = AIF %>% mutate(Active= ifelse(MobNo %in% M$MobNo,"Yes", ""))

x1<- read.delim("Feb_22222.txt", header = TRUE, sep = ",", colClasses=c("C.Account.No"="character"))
x1$AOD <- as.POSIXct(x1$Dt.Acct.Opening.Date, format="%d-%B-%Y") 
x1 <- with(x1, x1[(AOD >= "0018-08-01"), ])
View(x1)
x1 = x1[-(1:6), , drop = FALSE]
x2 <- x2[c("C.Account.No", "Dt.Acct.Opening.Date", "Mobile.No", "AOD")]

AIF = AIF %>% mutate(Active= ifelse(MobNo %in% M$MobNo | ACC_MST_ACCT %in% y$C.Account.No, "Yes", ""))

new_jan <- jan[rowSums(is.na(jan)) > 0,]
colSums(is.na(jan))

feb$SRD <- as.POSIXct(strptime(feb$SR.Created.Date, format = "%m/%d/%Y"))

G = inner_join(new_jan, janfeb, by = "acc")
unmHL <- read.csv('unmHL.csv', colClasses=c("USERID"="character"))

##Calculate Migration Logic
w.jan<- read.delim("webform-jan.txt", header = TRUE, sep = ",", colClasses=c("Account.No"="character"))
w.jan1<- subset(w.jan, select=c("Account.Number", "Category", "Level.1", "Level.2", "Channel"))
w.oct.jan <- w.oct1
w.oct.jan <- rbind(w.oct.jan,w.nov1)
w.oct.jan <- rbind(w.oct.jan,w.dec1)
w.oct.jan <- rbind(w.oct.jan,w.jan1)

UA.Jan <- read.csv('UA-Jan.csv',stringsAsFactors = F,header=T, colClasses=c("Jan"="character"))
colnames(UA.Jan)<- "Account.Number"
w.jan.nm.u<- UA.Jan  %>% filter(!Account.Number %in% w.jan1$Account.Number) 
w.jan.mg <- w.jan.nm.u %>% filter(Account.Number %in% w.oct.jan$Account.Number)
write.csv(w.jan.mg, file = "Webform-Jan-migrated.csv", row.names=FALSE)
write.csv(w.oct.jan, file = "Webform(Oct-Jan).csv", row.names=FALSE)


h.jun.m.u<- h.jun  %>% filter(Account.Number %in% UA.Jun$Account.Number) 


################ SMS Promotion###################################

# Mobile No. change SMS

MobNoChg<- read.delim("MobNoChg.txt", header = TRUE, sep = ",", colClasses=c("Account.No"="character"))

MobNoChg.Acc <- as.data.frame(MobNoChg$Account.Number)
colnames(MobNoChg.Acc) <- "AccNo"
MobNoChg.Acc <- unique(MobNoChg.Acc)

MobNoChg.SMS<- Fin.SMS  %>% filter(AccNo %in% MobNoChg.Acc$AccNo)

MobNoChg.unique <- MobNoChg.SMS[!duplicated(MobNoChg.SMS$AccNo), ]
write.table(MobNoChg.unique, file = "MobNoChg-SMS.txt", sep = "\t", row.names = FALSE)

#Email No. change SMS

EmailChg<- read.delim("EmailChg.txt", header = TRUE, sep = ",", colClasses=c("Account.No"="character"))
EmailChg.Acc <- as.data.frame(EmailChg$Account.Number)
colnames(EmailChg.Acc) <- "AccNo"
EmailChg.Acc <- unique(EmailChg.Acc)
EmailChg.SMS<- Fin.SMS  %>% filter(AccNo %in% EmailChg.Acc$AccNo)
EmailChg.unique <- EmailChg.SMS[!duplicated(EmailChg.SMS$AccNo), ]
write.table(EmailChg.unique, file = "EmailChg-SMS.txt", sep = "\t", row.names = FALSE)

#BT booking Enquiry

BTbook<- read.delim("BTbook.txt", header = TRUE, sep = ",", colClasses=c("Account.No"="character"))
BTbook.Acc <- as.data.frame(BTbook$Account.Number)
colnames(BTbook.Acc) <- "AccNo"
BTbook.Acc <- unique(BTbook.Acc)
BTbook.SMS<- Fin.SMS  %>% filter(AccNo %in% BTbook.Acc$AccNo)
BTbook.unique <- BTbook.SMS[!duplicated(BTbook.SMS$AccNo), ]
write.table(BTbook.unique, file = "BTBook-SMS.txt", sep = "\t", row.names = FALSE)


#Statement Details Enquiry

stmnt<- read.delim("Stmnt.txt", header = TRUE, sep = ",", colClasses=c("Account.No"="character"))
stmnt.Acc <- as.data.frame(stmnt$Account.Number)
colnames(stmnt.Acc) <- "AccNo"
stmnt.Acc <- unique(stmnt.Acc)
stmnt.SMS<- Fin.SMS  %>% filter(AccNo %in% stmnt.Acc$AccNo)
stmnt.unique <- stmnt.SMS[!duplicated(stmnt.SMS$AccNo), ]
write.table(stmnt.unique, file = "Statement-Details-SMS.txt", sep = "\t", row.names = FALSE)

#Transaction Status Enquiry
trans<- read.delim("Trans.txt", header = TRUE, sep = ",", colClasses=c("Account.No"="character"))
trans.Acc <- as.data.frame(trans$Account.Number)
colnames(trans.Acc) <- "AccNo"
trans.Acc <- unique(trans.Acc)
trans.SMS<- Fin.SMS  %>% filter(AccNo %in% trans.Acc$AccNo)
trans.unique <- trans.SMS[!duplicated(trans.SMS$AccNo), ]
write.table(trans.unique, file = "Transaction-History-SMS.txt", sep = "\t", row.names = FALSE)

#Payment Enquiry
payhist<- read.delim("payhist.txt", header = TRUE, sep = ",", colClasses=c("Account.No"="character"))
payhist.Acc <- as.data.frame(payhist$Account.Number)
colnames(payhist.Acc) <- "AccNo"
payhist.Acc <- unique(payhist.Acc)
payhist.SMS<- Fin.SMS  %>% filter(AccNo %in% payhist.Acc$AccNo)
payhist.unique <- payhist.SMS[!duplicated(payhist.SMS$AccNo), ]
write.table(payhist.unique, file = "Payment-History-SMS.txt", sep = "\t", row.names = FALSE)

#Outstanding Balance Enquiry
outbal<- read.delim("Outbal.txt", header = TRUE, sep = ",", colClasses=c("Account.No"="character"))
outbal.Acc <- as.data.frame(outbal$Account.Number)
colnames(outbal.Acc) <- "AccNo"
outbal.Acc <- unique(outbal.Acc)
outbal.SMS<- Fin.SMS  %>% filter(AccNo %in% outbal.Acc$AccNo)
outbal.unique <- outbal.SMS[!duplicated(outbal.SMS$AccNo), ]
write.table(outbal.unique, file = "Balance-Outstanding-SMS.txt", sep = "\t", row.names = FALSE)


#Credit Limit Enquiry
credlim<- read.delim("credlim.txt", header = TRUE, sep = ",", colClasses=c("Account.No"="character"))
credlim.Acc <- as.data.frame(credlim$Account.Number)
colnames(credlim.Acc) <- "AccNo"
credlim.Acc <- unique(credlim.Acc)
credlim.SMS<- Fin.SMS  %>% filter(AccNo %in% credlim.Acc$AccNo)
credlim.unique <- credlim.SMS[!duplicated(credlim.SMS$AccNo), ]
write.table(credlim.unique, file = "Credit-Limit-Outstanding-SMS.txt", sep = "\t", row.names = FALSE)

#PIN Generation Enquiry
pingen<- read.delim("pingen.txt", header = TRUE, sep = ",", colClasses=c("Account.No"="character"))
pingen.Acc <- as.data.frame(pingen$Account.Number)
colnames(pingen.Acc) <- "AccNo"
pingen.Acc <- unique(pingen.Acc)
pingen.SMS<- Fin.SMS  %>% filter(AccNo %in% pingen.Acc$AccNo)
pingen.unique <- pingen.SMS[!duplicated(pingen.SMS$AccNo), ]
write.table(pingen.unique, file = "PIN-Generate-SMS.txt", sep = "\t", row.names = FALSE)

