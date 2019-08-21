AIF <- read_csv("CAPRITM1482501.csv")

ACC = 0

UAC_P <- data.frame(ACC_MST_CUS_ORG = numeric(0), ACC_MST_ACCT= numeric(0), MAC_MST_CUS_CUST_NBR = numeric(0), MAC_MST_MOBILE_PHONE = numeric(0))

for(i in seq_len(nrow(UAC))){
  Acc = UAC$ACC[i]
  
  #subset
  #bar <- subset(df1, Date == "25/11/2018")
  AIF_s <- subset(AIF, ACC_MST_ACCT == Acc)
  
  #Append
  UAC_P <- rbind(UAC_P,AIF_s)
  #browser()
  
}

UAC_sql <- data.frame(ACC_MST_CUS_ORG = character(0), ACC_MST_ACCT= character(0), MAC_MST_CUS_CUST_NBR = character(0), MAC_MST_MOBILE_PHONE = character(0))
UAC_1 <- data.frame(ACC= numeric(0))
UAC_2 <- data.frame(ACC= numeric(0))

UAC_sql = sqldf("SELECT AIF.ACC_MST_CUS_ORG, AIF.ACC_MST_ACCT, AIF.MAC_MST_CUS_CUST_NBR, AIF.MAC_MST_MOBILE_PHONE FROM AIF INNER JOIN UAC ON AIF.ACC_MST_ACCT = UAC.ACC")

UAC_1 = sqldf("SELECT UAC.ACC FROM UAC INNER JOIN AIF ON UAC.ACC = AIF.ACC_MST_ACCT")
UAC_2 = sqldf("SELECT UAC.ACC FROM UAC INNER JOIN AIF ON UAC.ACC != AIF.ACC_MST_ACCT")
