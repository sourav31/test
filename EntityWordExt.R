library(dplyr)
library(readr)

# Read Entities
ent <- read_csv("Ent.csv")
ent1 <- read_csv("Ent1.csv")
ent2 <- read_csv("Ent2.csv")
ent3 <- read_csv("Ent3.csv")

# Read FAQ List
MD <- read_csv("FAQL.csv")
QL <- read_csv("QL.csv")

questn = ""

bar <- data.frame(AccNo= numeric(0), Date= character(0))
dat <- data.frame(i= numeric(0), unique_c= numeric(0),rep_c= numeric(0))

fQL <- data.frame(Entity= character(0), Entity1= character(0), Entity2= character(0), Entity3= character(0), Question= character(0))

for(i in seq_len(nrow(QL))){
  questn = QL$question[i]
  
  # Ent, Ent1, Ent2, Ent3 found
  
  if(questn contains )
  
  iQL = data.frame(Entity, Entity1, Entity2, Entity3, Question)
  fQL = rbind(fQL,iQL)
  
  # Ent, Ent2, Ent3 found
  
  iQL = data.frame(Entity, Entity1, Entity2, Entity3, Question)
  fQL = rbind(fQL,iQL)
  
  # Ent1, Ent2, Ent3 found
  
  iQL = data.frame(Entity, Entity1, Entity2, Entity3, Question)
  fQL = rbind(fQL,iQL)
  
  # Ent2, Ent3 found
  
  iQL = data.frame(Entity, Entity1, Entity2, Entity3, Question)
  fQL = rbind(fQL,iQL)
  
  # Else, NA
  iQL = data.frame(Entity, Entity1, Entity2, Entity3, Question)
  fQL = rbind(fQL,iQL)
  
  

}


ent.list <- as.list(as.data.frame(t(ent)))
sapply(ent.list, grepl, quest)
a = x[sapply(x, grepl, quest)]

x <- as.data.frame(t(ent)) 
x <- gdata::drop.levels(x)
a = x[sapply(x, grepl, quest)]


