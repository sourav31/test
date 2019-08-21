j1 <- read.csv('1-19.csv',stringsAsFactors = F,header=T, colClasses=c("USERID"="character"))
j2 <- read.csv('20-24.csv',stringsAsFactors = F,header=T, colClasses=c("USERID"="character"))
j3 <- read.csv('26-29.csv',stringsAsFactors = F,header=T, colClasses=c("USERID"="character"))
j4 <- read.csv('30.csv',stringsAsFactors = F,header=T, colClasses=c("USERID"="character"))

#Consolidated Master Sheet
MD <- j1
MD <- rbind(MD, j2)
MD <- rbind(MD, j3)
MD <- rbind(MD, j4)

#Calculate Total Queries
MD$CD <- as.POSIXct(strptime(MD$TIME, format = "%m/%d/%Y"))
tab.query = table(MD$CD)
total.queries <- as.data.frame(tab.query)

MD1 <- MD[,c(4,8)]
MD2 <- MD[,c(4,35)]
MD3 <- as.data.frame(unique(MD2$CD))
colnames(MD3)<- "CD"


#Calculate Unique Total Sessions
fs <- data.frame(TIME= character(0), TS= numeric(0))

for(i in seq_len(nrow(MD3))){
  TIME = MD3$CD[i]
  #subset
  #bar <- subset(df1, Date == "25/11/2018")
  MD2_s <- subset(MD2, CD == TIME)
  TS = length(unique(MD2_s$SESSIONID))
  #browser()
  #Append
  is = data.frame(TIME,TS )
  fs = rbind(fs,is)
  
}

#Select website channel
MD_p <- with(MD, MD[(CHANNEL != "mobileApp"), ])
MD_p <- with(MD_p, MD_p[(CHANNEL != "mobileapp"), ])

#Select Website Post login users
MD_web <- with(MD_p, MD_p[(USERID != "unknown"), ])

#Calculate Website post login Total Queries
tab.q = table(MD_web$CD)
total.web.queries <- as.data.frame(tab.q)

#Calculate Unique Total website postlogins
web.PL <- data.frame(TIME= character(0), TS= character(0))
fPL <- data.frame(TIME= character(0), TS= character(0))

#MD2 <- MD[,c(4,35)]
MD4 <- MD_web[,c(35,3)]

#MD3 <- as.data.frame(unique(MD2$CD))
MD5 <- as.data.frame(unique(MD4$CD))
colnames(MD5)<- "CD"

for(i in seq_len(nrow(MD5))){
  #browser()
  TIME.w = MD5$CD[i]
  #subset
  #bar <- subset(df1, Date == "25/11/2018")
  #MD4$USERID <- factor(MD4$USERID)
  #MD4[,sapply(MD4$USERID, is.character)]
  
  MD_web.s <- MD4 %>% filter(CD == TIME.w)
  
  #MD_web.s[,sapply(MD_web.s$USERID, is.character)]
  #MD_web.s <- subset(MD4,CD == TIME.w)
  #MD_web.s <- with(MD4, MD4[(CD == TIME.w), ])
  PL = length(unique(MD_web.s$USERID))
  #browser()
  #Append
  iPL = data.frame(TIME.w,PL)
  web.PL = rbind(web.PL,iPL)
  
}

#Calculate Website post login Total Queries
web.q = table(MD_web$INTENTS)
total.web.queries <- as.data.frame(web.q)


#Select Mobile App channel
#MD.app <- with(MD, MD[(CHANNEL == c("mobileApp", "mobileapp")), ])

MD.app1 <- with(MD, MD[(CHANNEL == "mobileApp"), ])
MD.app2 <- with(MD, MD[(CHANNEL == "mobileapp"), ])
MD.app1 <- rbind(MD.app1, MD.app2)


#Subset the actual Account No. strating with 000* and then compute total logins

app.acc <- MD.app1%>% filter(grepl("^000",MD.app1$USERID) == TRUE)

#Acc no check for 19digits from the above
app.acc <- app.acc%>% filter(nchar(app.acc$USERID) == 19)

#Acc no 4th position to be 4/5
app.acc1 <- app.acc%>% filter(substr(app.acc$USERID,4,4) == "4" | substr(app.acc$USERID,4,4) == "5")


##Calculate App Total Logins

app.PL <- data.frame(TIME= character(0), TS= character(0))
APL <- data.frame(TIME= character(0), TS= character(0))

#MD2 <- MD[,c(4,35)]
APP1 <- app.acc1[,c(35,3)]

#MD3 <- as.data.frame(unique(MD2$CD))
APP2 <- as.data.frame(unique(app.acc1$CD))
colnames(APP2)<- "CD"

for(i in seq_len(nrow(APP2))){
  #browser()
  TIME.a = APP2$CD[i]
  #subset
  App.s <- APP1 %>% filter(CD == TIME.a)
  
  A.PL = length(unique(App.s$USERID))
 
  #Append
  app.iPL = data.frame(TIME.a,A.PL)
  app.PL = rbind(app.PL,app.iPL)
  
}

##Calculate App Total Queries
app.q = table(app.acc1$CD)
total.app.queries <- as.data.frame(app.q)

#Select website Pre login channel
web.pre <- with(MD, MD[(CHANNEL != "mobileApp"), ])
web.pre <- with(web.pre, web.pre[(CHANNEL != "mobileapp"), ])
web.pre <- with(web.pre, web.pre[(USERID == "unknown"), ])


#Consolidate Total Queries by adding dumps of prelogin website, postlogin website and Mobile app
fin.MD <- web.pre
fin.MD <- rbind(fin.MD, MD_web)
fin.MD <- rbind(fin.MD, app.acc1)

#Queries and their repetititions dataframe with time
Int.count <- table(fin.MD$INTENTS)
Int.count <- as.data.frame(Int.count)
write.csv(Int.count, file = "Intents-count_f.csv", row.names=FALSE)

web.int.count <- table(MD_web$INTENTS)
web.int.count <- as.data.frame(web.int.count)
write.csv(web.int.count, file = "web-Intents-count_f.csv", row.names=FALSE)

app.int.count <- table(app.acc1$INTENTS)
app.int.count <- as.data.frame(app.int.count)
write.csv(app.int.count, file = "app-Intents-count_f.csv", row.names=FALSE)

length(fin.MD.june$QUESTION)

################# Live Chat Agent ##################

DA <- read.csv('DA.csv',stringsAsFactors = F,header=T, colClasses=c("USERID"="character"))

CD.LCA <- as.data.frame(unique(MD_web$CD))
colnames(CD.LCA)<- "CD"

f.lca <- data.frame(TIME= character(0), v.lca= numeric(0), o.lca= numeric(0))
f.user.lca <- data.frame(i.user.lca= character(0))
f.intent.lca <- data.frame(i.intent.lca = character(0))

for(i in seq_len(nrow(CD.LCA))){
  #browser()
  TIME.l = CD.LCA$CD[i]
  #subset
  web.s <- MD_web %>% filter(CD == TIME.l)
  lca.u <- web.s %>% filter(grepl('Live Chat Agent is Available', web.s$ANSWERID) == TRUE )
  UID.lca <- as.data.frame(unique(lca.u$USERID))
  colnames(UID.lca) <- "USERID"
  
  lca.s <- web.s %>% filter(grepl('chat with agent', web.s$ANSWERID) == TRUE )
  cl.lca <- lca.s %>% filter(!USERID %in% DA$USERID)
  view.lca <- length(cl.lca$ANSWERID)
  
  
  dump.lca <- lca.s %>% filter(USERID %in% UID.lca$USERID)
  #browser()
  opt.lca <- length(dump.lca$ANSWERID)
  user.lca <- as.data.frame(unique(dump.lca$USERID))
  intent.lca <- as.data.frame(dump.lca$INTENTS)
  
  #Append
  i.lca = data.frame(TIME.l,view.lca,opt.lca)
  f.lca = rbind(f.lca,i.lca)
  
  #Append userid
  f.user.lca = rbind(f.user.lca, user.lca)
  
  #Append intents
  f.intent.lca = rbind(f.intent.lca, intent.lca)
  
}

#Top 10 intents

colnames(f.intent.lca) <- "INTENTS"
lca.Int.count <- table(f.intent.lca$INTENTS)
lca.Int.count <- as.data.frame(lca.Int.count)

write.csv(lca.Int.count, file = "LCA-Intents-count.csv", row.names=FALSE)

##################### Live Chat Agent #########################################

#####Positive & Negative feedback
#Select App channel with accepted for positive feedback
app.jul.pos <- with(app.acc1, app.acc1[(ACCEPTED == "correct"), ])
app.jul.neg <- with(app.acc1, app.acc1[(ACCEPTED == "incorrect"), ])


tab.jul.pos = table(app.jul.pos$CD)
total.jul.pos <- as.data.frame(tab.jul.pos)

tab.jul.neg = table(app.jul.neg$CD)
total.jul.neg <- as.data.frame(tab.jul.neg)


########### Customer journey based on intent ############################


library(sunburstR)

#sequence_data1 <- read.csv('visit-sequences.csv',stringsAsFactors = F,header=F)
  
#View(sequence_data)
#sunburst(sequence_data,percent=FALSE,colors=c("#393E41","#D3D0CB","#E2C044","#587B7F","#9C0D38"))

uni.sess <- unique(fin.MD$SESSIONID)
uni.sess <- as.data.frame(uni.sess)

colnames(uni.sess)<- "US"

fsess <- data.frame(sess= character(0), str= character(0))

for(i in seq_len(nrow(uni.sess))){
  #browser()
  sess = uni.sess$US[i]
  #subset
  #bar <- subset(df1, Date == "25/11/2018")
  fin.MD.sess <- subset(fin.MD, SESSIONID == sess)
  fin.MD.s.o <- fin.MD.sess[order(fin.MD.sess$IDCHATANALYTICS),]
  #browser()
  str = paste(fin.MD.s.o$INTENTS, collapse = "-")
  

  #browser()
  #Append
  isess = data.frame(sess,str)
  fsess = rbind(fsess,isess)
  
}



for(i in seq_len(nrow(uni.sess))){
  #browser()
  sess = uni.sess$US[i]
  #subset
  #bar <- subset(df1, Date == "25/11/2018")
  fin.MD.sess <- subset(MD.w.f, SESSIONID == sess)
  fin.MD.s.o <- fin.MD.sess[order(fin.MD.sess$IDCHATANALYTICS),]
  #browser()
  str = paste(fin.MD.s.o$INTENTS, collapse = "-")
  
  
  #browser()
  #Append
  isess = data.frame(sess,str)
  fsess = rbind(fsess,isess)
  
}


for(i in seq_len(nrow(uni.sess1))){
  #browser()
  sess1 = uni.sess1$US[i]
  #subset
  #bar <- subset(df1, Date == "25/11/2018")
  fin.MD.sess1 <- subset(MD.a1, SESSIONID == sess1)
  fin.MD.s.o1 <- fin.MD.sess1[order(fin.MD.sess1$IDCHATANALYTICS),]
  #browser()
  str1 = paste(fin.MD.s.o1$INTENTS, collapse = "-")
  
  
  #browser()
  #Append
  isess1 = data.frame(sess1,str1)
  fsess1 = rbind(fsess1,isess1)
  
}


sess.count <- fsess$str1
sess.count <- fsess$str
sess.count <- as.data.frame(sess.count)


colnames(sess.count1) <- "SC"
tab.q1 = table(sess.count1$SC)
total.q1 <- as.data.frame(tab.q1)

sunburst(total.q1,percent=FALSE,colors=c("#393E41","#D3D0CB","#E2C044","#587B7F","#9C0D38"))

##############################################################################
#Repeat users Mobile App
app.jun <- read.csv('app.csv',stringsAsFactors = F,header=T, colClasses=c("USERID"="character"))
app.jun1 <- with(app.jun, app.jun[(CD != "2019-07-01"), ])




