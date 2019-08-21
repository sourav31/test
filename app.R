library(shiny)
require(shinydashboard)
library(ggplot2)
library(dplyr)
library(eeptools)
library(stringr)

#recommendation <- read.csv('recommendation.csv',stringsAsFactors = F,header=T)
#head(recommendation)

today <- read.csv('try.csv',stringsAsFactors = F,header=T)

#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Chatbot Daily Dashboard (Last updated on 29th Jan'19)", titleWidth = 550)  
#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
  width = 550,
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Visit-us", icon = icon("send",lib='glyphicon'), 
             href = "https://sbicard.com/ELA")
  )
)


frow1 <- fluidRow(
   valueBoxOutput("value1", width = 3)
  ,valueBoxOutput("value2", width = 3)
  ,valueBoxOutput("value3", width = 3)
  ,valueBoxOutput("value4", width = 3)
)

frow2 <- fluidRow( 
  box(
    title = "Post Login: Top 15 Intents"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("topIntentsPL", height = "410px")
  )
  ,box(
    title = "Post Login: Top 15 Queries"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("topQuesPL", height = "410px")
  ) 
)

# combine the two fluid rows to make the body
body <- dashboardBody(frow1, frow2)


#completing the ui part with dashboardPage
ui <- dashboardPage(title = 'This is my Page title', header, sidebar, body, skin='red')



server <- function(input, output) { 
  #some data manipulation to derive the values of KPI boxes
  total.queries <- length(today$QUESTION)
  total.disamb<- sum(today$DISAMBIGIOUS[today$DISAMBIGIOUS == "1"])
  succ.rate <- (total.queries-total.disamb)/total.queries
  succ.rate.per <- paste(round(succ.rate*100,digits=1),"%",sep="")
  
  post.login<- today %>% filter(str_detect(today$USERID, "000"))
  
  total.logins <- length(unique(post.login$USERID))
  total.post.queries <- length(post.login$QUESTION)
  
  
  #sales.account <- recommendation %>% group_by(Account) %>% summarise(value = sum(Revenue)) %>% filter(value==max(value))
  #prof.prod <- recommendation %>% group_by(Product) %>% summarise(value = sum(Revenue)) %>% filter(value==max(value))
  #creating the valueBoxOutput content
  output$value1 <- renderValueBox({
    valueBox(
      formatC(total.queries, format="d", big.mark=',')
      ,'Total Queries'
      ,icon = icon("search",lib='glyphicon')
      ,color = "purple")  
  })
  output$value2 <- renderValueBox({ 
    valueBox(
      formatC(succ.rate.per, format="d", big.mark=',')
      ,'Success Rate'
      ,icon = icon("ok",lib='glyphicon')
      ,color = "green")  
  })
  output$value3 <- renderValueBox({
    valueBox(
      formatC(total.logins, format="d", big.mark=',')
      ,'Total Post Login Users'
      ,icon = icon("user",lib='glyphicon')
      ,color = "aqua")   
  })
  output$value4 <- renderValueBox({
    valueBox(
      formatC(total.post.queries, format="d", big.mark=',')
      ,'Total Post Login Queries'
      ,icon = icon("search",lib='glyphicon')
      ,color = "yellow")   
  })
  
  #Top 10 Intents
  tab = table(post.login$INTENTS)
  tab_s <- sort(tab)
  top10 <- tail(names(tab_s), 15)
  top10_freq <- tail(tab_s, 15)
  pst <- data.frame(top10,top10_freq)
  pst$Var1 <- NULL
  
  #Top 10 Questions
  tab.ques = table(post.login$QUESTION)
  tab_s.ques <- sort(tab.ques)
  top10.ques <- tail(names(tab_s.ques), 15)
  top10_freq.ques <- tail(tab_s.ques, 15)
  pst.ques <- data.frame(top10.ques,top10_freq.ques)
  pst.ques$Var1 <- NULL
  
  #ggplot(pst, aes(x = reorder(top10, Freq), y = Freq)) + geom_bar(stat = "identity", fill = "#FF6666") + coord_flip() + theme_classic()
  
  
  #creating the plotOutput content
  output$topIntentsPL <- renderPlot({
    ggplot(pst, aes(x = reorder(top10, Freq), y = Freq)) + geom_bar(stat = "identity", fill = "#FF6666") + coord_flip() + theme_classic()
  })
  
  output$topQuesPL <- renderPlot({
    ggplot(pst.ques, aes(x = reorder(top10.ques, Freq), y = Freq)) + geom_bar(stat = "identity", fill = "#FF6666") + coord_flip() + theme_classic()
  })
  
}

shinyApp(ui, server)