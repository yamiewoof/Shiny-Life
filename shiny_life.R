library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(ggplot2)
require(DMwR)

se <- read.csv("https://drive.google.com/uc?export=download&id=1h5GKPHtblt0jHmtasvcxVJL5m04-ajBm")
se <- knnImputation(se)

se$Fatigue <- se$Fatigue %>% round() %>% as.factor()
se$Recommend <- se$Recommend %>% round() %>% as.factor()
se$GrowthHelp <- se$GrowthHelp %>% round() %>% as.factor()
se$CVHelp <- se$CVHelp %>% round() %>% as.factor()
edu <- sort(se$Education)
#############################實驗室##################################
#ggplot(se, aes(x=se$Industry, y=se$SalaryY)) + 
#  geom_boxplot(color = "black", size = 0.5) + 
# labs(x="Salary per year (10,000)", title="Industry Comparison") +
#theme(axis.text.y = element_text(angle=30, hjust=1)) + 
#coord_flip()
#####################################################################
ui <- (
  navbarPage(theme=shinytheme("readable"), h1("Personal Life-Value Estimation"),
             tabPanel(h4("Demission Rate & Job Satisfaction"),
                      h3("On my present job this is what I feel about..."),
                      selectInput("Q1", "1.	Being able to keep busy all the time",
                                  c("Satisfied" = 1, "Dissatisfied" = 0), width = '420px'),
                      selectInput("Q2", "2.	The chance to work alone on the job",
                                  c("Satisfied" = 1, "Dissatisfied" = 0), width = '420px'),
                      selectInput("Q3", "3.	The chance to do different things from time to time",
                                  c("Satisfied" = 1, "Dissatisfied" = 0), width = '420px'),
                      selectInput("Q4", "4.	The chance to be 'somebody' in the community",
                                  c("Satisfied" = 1, "Dissatisfied" = 0), width = '420px'),
                      selectInput("Q5", "5.	The way my boss handles his/her workers",
                                  c("Satisfied" = 1, "Dissatisfied" = 0), width = '420px'),
                      selectInput("Q6", "6.	The competence of my supervisor in making decisions",
                                  c("Satisfied" = 1, "Dissatisfied" = 0), width = '420px'),
                      selectInput("Q7", "7.	Being able to do things that don't go against my conscience",
                                  c("Satisfied" = 1, "Dissatisfied" = 0), width = '420px'),
                      selectInput("Q8", "8.	The way my job provides for steady employment",
                                  c("Satisfied" = 1, "Dissatisfied" = 0), width = '420px'),
                      selectInput("Q9", "9.	The chance to do things for other people",
                                  c("Satisfied" = 1, "Dissatisfied" = 0), width = '420px'),
                      selectInput("Q10", "10.	The chance to tell people what to do",
                                  c("Satisfied" = 1, "Dissatisfied" = 0), width = '420px'),
                      selectInput("Q11", "11.	The chance to do something that makes use of my abilities",
                                  c("Satisfied" = 1, "Dissatisfied" = 0), width = '420px'),
                      selectInput("Q12", "12.	The way company policies are put into practice",
                                  c("Satisfied" = 1, "Dissatisfied" = 0), width = '420px'),
                      selectInput("Q13", "13.	My pay and the amount of work I do",
                                  c("Satisfied" = 1, "Dissatisfied" = 0), width = '420px'),
                      selectInput("Q14", "14.	The chances for advancement on this job",
                                  c("Satisfied" = 1, "Dissatisfied" = 0), width = '420px'),
                      selectInput("Q15", "15.	The freedom to use my own judgment",
                                  c("Satisfied" = 1, "Dissatisfied" = 0), width = '420px'),
                      textOutput("r123"),
                      br(),
                      h5("#The questionnaire is citted from the 'Manual for the Minnesota Satisfaction Questionnaire. Weiss, D. J., Dawis, R. V., & England, G. W. (1967).'")
             ),
             
             tabPanel(h4("Job & Personal Life-Value"),
                      sidebarLayout(
                        sidebarPanel(
                          selectizeInput("aIndustry", "The industry you want to get in most:", se$Industry),
                          selectizeInput("aCompany", "The company you want to get in most:", se$Company),
                          numericInput("WorkYear", "Years you plan to work:",
                                       value = 1, min = 1, max = 46),
                          selectInput("Education", "Your education level:", edu),
                          selectInput("Year", "Your current working experience: [years]",
                                      c("0~1", "1~2", "2~3", "4~5", "5~7", "7~10", "10~15", "15~20", "20~30", "30+")),
                          selectInput("Hour", "The weekly working hours you can accept: [Hours/week]",
                                      c("30-", "30~40", "40~45", "45~50", "50~55", "55~60", "60~70", "70~80",
                                        "80~90", "90~100", "120+")),
                          sliderInput("Fatigue", "Your tolerance to fatigue at work:",
                                      min = 1, max = 8, value = 5),
                          sliderInput("Recommend", "How important do you think the recommendations of
                                      the current staff are:",
                                      min = 1, max = 5, value = 3),
                          sliderInput("GrowthHelp", "The job's importance to your future career growth",
                                      min = 1, max = 5, value = 3),
                          sliderInput("CVHelp","The job's importance to your resume:",
                                      min = 1, max = 5, value = 3)
                        ),
                        mainPanel(
                          tabsetPanel(
                            tabPanel(h4("Estimated Salary"),
                                     h1("Salary Comparison"),
                                     plotOutput("aCompare"),
                                     hr(),
                                     h2(htmlOutput("aTotalSalary")),
                                     h2(htmlOutput("aLifeIndex"))
                            ),
                            tabPanel(h4("Employee Salaries"),
                                     h3("Other Employees' Data"),
                                     tableOutput("aSalaryTable")
                            ),
                            tabPanel(h4("Distribution"),
                                     h1("Salary Distribution in Current Industry"),
                                     plotOutput("adensityind"), h4(htmlOutput("aPRInd")),
                                     br(), hr(), br(),
                                     h1("Salary Distribution in Current Education Level"),
                                     plotOutput("adensityedu"), h4(htmlOutput("aPREdu")),
                                     br()
                            ),
                            tabPanel(h4("Recommended Companies"),
                                     h4(htmlOutput("bestcomp")),
                                     h4(htmlOutput("bestsal"))
                            )
                          )
                        )
                        )
  ),
  
  tabPanel(h4("Comparision"),
           sidebarLayout(
             sidebarPanel(
               selectizeInput("Industry", "The first industry:", se$Industry),
               selectizeInput("Company", "The first company:", se$Company),
               hr(),
               selectizeInput("Industry2", "The second industry:", se$Industry),
               selectizeInput("Company2", "The second company:", se$Company),
               hr(),
               selectizeInput("Industry3", "The thrid industry:", se$Industry),
               selectizeInput("Company3", "The thrid company:", se$Company)
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel(h4("Estimated Salary"),
                          h1("Salary Comparison"),
                          plotOutput("Compare"),
                          hr(),
                          h1("The first company"),
                          h2(htmlOutput("TotalSalary")),
                          h1("The second company"),
                          h2(htmlOutput("TotalSalary2")),
                          h1("The third company"),
                          h2(htmlOutput("TotalSalary3"))
                 ),
                 tabPanel(h4("Employee Salaries"),
                          h3("The first company"),
                          tableOutput("SalaryTable"),
                          hr(),
                          h3("The second company"),
                          tableOutput("SalaryTable2"),
                          hr(),
                          h3("The third company"),
                          tableOutput("SalaryTable3")
                 ),
                 tabPanel(h4("Distribution"),
                          radioButtons("choice", "Choice:",
                                       c("First Company" = 1, "Second Company" = 2, "Third Company" = 3), 
                                       inline = TRUE),
                          h1("Salary Distribution in Current Industry"),
                          plotOutput("densityind"), h4(htmlOutput("PRInd")),
                          br(), hr(), br(),
                          h1("Salary Distribution in Current Education Level"),
                          plotOutput("densityedu"), h4(htmlOutput("PREdu")),
                          br()
                 )
               )
             ),
             position = c("left", "right")
           )
  ),
  
  tabPanel(h4("SalaryPlot"),
           fluidRow(column(width = 9, align = "center",
                           plotOutput("mpgPlot")
           ))
  ),
  
  tabPanel(h4("RawData"), tableOutput("data")
  )
             )
  )



server <- function(input, output, session){
  
  ###The 1st Tab
  Gamma <- reactive({
    Q1 <- input$Q1 %>% as.numeric()
    Q2 <- input$Q2 %>% as.numeric()
    Q3 <- input$Q3 %>% as.numeric()
    Q4 <- input$Q4 %>% as.numeric()
    Q5 <- input$Q5 %>% as.numeric()
    Q6 <- input$Q6 %>% as.numeric()
    Q7 <- input$Q7 %>% as.numeric()
    Q8 <- input$Q8 %>% as.numeric()
    Q9 <- input$Q9 %>% as.numeric()
    Q10 <- input$Q10 %>% as.numeric()
    Q11 <- input$Q11 %>% as.numeric()
    Q12 <- input$Q12 %>% as.numeric()
    Q13 <- input$Q13 %>% as.numeric()
    Q14 <- input$Q14 %>% as.numeric()
    Q15 <- input$Q15 %>% as.numeric()
    Qsum <- Q1+Q2+Q3+Q4+Q5+Q6+Q7+Q8+Q9+Q10+Q11+Q12+Q13+Q14+Q15
    r <- exp(Qsum)/(1+exp(Qsum))
  })
  output$r123 <- renderText({
    Gamma() %>% paste("Gamma = ",.)
  })
  
  ###The 2nd Tab
  #2-1 Input & BarChart & LifeIndex
  observeEvent(
    input$aIndustry,
    updateSelectizeInput(session, "aCompany",  "",
                         choices = se$Company [se$Industry == input$aIndustry])
  )
  
  Gdup <- reactive({
    req(input$aIndustry, input$aCompany, input$Education, input$Hour, input$Fatigue)
    sb <- se %>% lm(GrowthRate ~ Industry + Company + Education + Hour + Fatigue, .)
    sd <- data.frame(Industry = input$aIndustry, Company = input$aCompany, Education = input$Education,
                     Hour = input$Hour, Fatigue = as.factor(input$Fatigue))
    predG <- predict(sb, sd)
  })
  P1dup <- reactive({
    req(input$aIndustry, input$aCompany, input$Education, input$Year, input$Hour, input$Fatigue,
        input$Recommend, input$GrowthHelp, input$CVHelp)
    base <- se %>% lm(SalaryY ~ Industry + Company + Education + Year + Hour + Fatigue + Recommend
                      + GrowthHelp + CVHelp, .)
    i <- data.frame(Industry = input$aIndustry, Company = input$aCompany, Education = input$Education,
                    Year = input$Year, Hour = input$Hour, Fatigue = as.factor(input$Fatigue),
                    Recommend = as.factor(input$Recommend), GrowthHelp = as.factor(input$GrowthHelp),
                    CVHelp = as.factor(input$CVHelp))
    pred <- predict(base, i)
  })
  Sumdup <- reactive({
    req(input$WorkYear)
    g <- Gdup()
    i <- 2.73
    r <- (1+g/100)/(1+i/100)
    Pt <- Gamma()*P1dup()*(1-r^(input$WorkYear))/(1-r)*10000
    Pt <- Pt %>% ceiling()
  })
  
  output$aCompare <- renderPlot({
    req(Sumdup(),input$WorkYear)
    meanind <- 10*mean(subset(se$SalaryY, se$Industry == input$aIndustry))
    meanedu <- 10*mean(subset(se$SalaryY, se$Education == input$Education))
    meanoverall <- 10*mean(se$SalaryY)
    com <- c(Sumdup()/1000,meanind*(input$WorkYear),meanedu*(input$WorkYear),meanoverall*(input$WorkYear))
    name <- c("Estimated Salary", "Same Industry Avg.", "Same Education Avg.", "All Employees Avg.")
    barplot(com, main = "Comparison", las = 1, names.arg = name, col="yellow", ylab = "Total Salary(in thousands)")
  })
  
  LifeIndex <- reactive({
    req(input$aIndustry, input$Education, input$Fatigue, input$Recommend, input$GrowthHelp, input$CVHelp, input$WorkYear)
    meanind <- 10*mean(subset(se$SalaryY, se$Industry == input$aIndustry))
    sdind <- sd(subset(se$SalaryY, se$Industry == input$aIndustry))
    meanedu <- 10*mean(subset(se$SalaryY, se$Education == input$Education))
    sdedu <- sd(subset(se$SalaryY, se$Education == input$Education))
    meanoverall <- 10*mean(se$SalaryY)
    sdoverall <- sd(se$SalaryY)
    
    sum <- Sumdup()/(1000*input$WorkYear)
    
    zind <- (sum-meanind)/sdind
    zedu <- (sum-meanedu)/sdedu
    zoverall <- (sum-meanoverall)/sdoverall
    
    proind <- pnorm(zind)
    proedu <- pnorm(zedu)
    prooverall <- pnorm(zoverall)
    
    fatind <- input$Fatigue / 8
    fatrec <- input$Recommend / 5
    fatgro <- input$GrowthHelp / 5
    fatcv <- input$CVHelp / 5
    
    sumofall <- 10*(proind*0.125+proedu*0.125+prooverall*0.250+fatind*0.125+fatrec*0.125+fatgro*0.125+fatcv*0.125)
    finalindex <- round(sumofall,1)
  })
  
  output$aTotalSalary <- renderText({
    Sumdup <- Sumdup() %>% format(., big.mark=",", scientific=FALSE)
    paste("Estimated Total Salary:", "<font color=\"#FF0000\"><b>", Sumdup, "($ NTD)", "</b></font>")
  })
  output$aLifeIndex <- renderText({
    paste("Your Life-Value Index =", "<font color=\"#FF0000\"><b>", LifeIndex(), "/10", "</b></font>")
  })
  #2-2 Filter Data
  output$aSalaryTable <- renderTable({
    se %>% filter(input$aIndustry == se$Industry & input$aCompany == se$Company)
  })
  #2-3 Distribution & PR
  output$adensityind <- renderPlot({
    subset1 <- subset(se$SalaryY, se$Industry == input$aIndustry)
    abc <- density(subset1)
    point <- (Sumdup() / (input$WorkYear*10000))
    plot(abc,
         main = "Current Industry",
         xlab = "Salary(in millions)",
         ylab = "Density"
    )
    abline(v= point)
  })
  output$adensityedu <- renderPlot({
    subset1 <- subset(se$SalaryY, se$Education == input$Education)
    abc <- density(subset1)
    point <- (Sumdup() / (input$aWorkYear*10000))
    plot(abc,
         main = "Current Education Level",
         xlab = "Salary(in millions)",
         ylab = "Density"
    )
    abline(v= point)
  })
  
  IndPRdup <- reactive({
    req(input$aIndustry,input$WorkYear)
    est <- (Sumdup() / (input$WorkYear*10000))
    subset1 <- subset(se$SalaryY, se$Industry == input$aIndustry)
    highersubset <- subset(se$SalaryY, se$Industry == input$aIndustry & se$SalaryY>est)
    PR <- (1 - (length(highersubset) / length(subset1)))*100
    PR <- round(PR,1)
  })
  EduPRdup <- reactive({
    req(input$Education,input$WorkYear)
    est <- (Sumdup() / (input$WorkYear*10000))
    subset1 <- subset(se$SalaryY, se$Education == input$Education)
    highersubset <- subset(se$SalaryY, se$Education == input$Education & se$SalaryY>est)
    PR <- (1 - (length(highersubset) / length(subset1)))*100
    PR <- round(PR,1)
  })
  output$aPRInd <- renderText({
    paste("#Your Estimated Salary is higher than ", "<font color=\"#FF0000\"><b>", IndPRdup(),"%", "</b></font>", "of people in the same industry.")
    
  })
  output$aPREdu <- renderText({
    paste("#Your Estimated Salary is higher than ", "<font color=\"#FF0000\"><b>", EduPRdup(),"%", "</b></font>", "of people with the same education level as you.")
  })
  
  #2-4 Recommendation
  bestcomp <- reactive({
    req(input$aIndustry)
    subset1 <- se[which(se$Industry==input$aIndustry),]
    asdf <- subset1 %>% 
      group_by(Company) %>% 
      summarize(avg=mean(SalaryY))
    ordered <- asdf[order(-asdf$avg),]
    bestcomp <- lapply(ordered[1,1], as.character)
  })
  
  bestsal <- reactive({
    req(input$aIndustry)
    subset1 <- se[which(se$Industry==input$aIndustry),]
    asdf <- subset1 %>% 
      group_by(Company) %>% 
      summarize(avg=mean(SalaryY))
    ordered <- asdf[order(-asdf$avg),]
    bestcomp <- lapply(ordered[1,2], as.character)
  })
  
  output$bestcomp <- renderText({
    paste("Best Company in Current Industry: ", bestcomp())
  })
  
  output$bestsal <- renderText({
    paste("Salary in month: ", bestsal())
  })
  
  ###The 3rd Tab
  #3-1 Input & BarChart & TotalSalary
  observeEvent(
    input$Industry,
    updateSelectizeInput(session, "Company",  "",
                         choices = se$Company [se$Industry == input$Industry])
  )
  observeEvent(
    input$Industry2,
    updateSelectizeInput(session, "Company2",  "",
                         choices = se$Company [se$Industry == input$Industry2
                                               & se$Company != input$Company])
  )
  observeEvent(
    input$Industry3,
    updateSelectizeInput(session, "Company3",  "",
                         choices = se$Company [se$Industry == input$Industry3
                                               & se$Company != input$Company
                                               & se$Company != input$Company2])
  )
  
  P1 <- reactive({
    req(input$Industry, input$Company, input$Education, input$Year, input$Hour, input$Fatigue,
        input$Recommend, input$GrowthHelp, input$CVHelp)
    base <- se %>% lm(SalaryY ~ Industry + Company + Education + Year + Hour + Fatigue + Recommend
                      + GrowthHelp + CVHelp, .)
    i <- data.frame(Industry = input$Industry, Company = input$Company, Education = input$Education,
                    Year = input$Year, Hour = input$Hour, Fatigue = as.factor(input$Fatigue),
                    Recommend = as.factor(input$Recommend), GrowthHelp = as.factor(input$GrowthHelp),
                    CVHelp = as.factor(input$CVHelp))
    pred <- predict(base, i)
  })
  P12 <- reactive({
    req(input$Industry2, input$Company2, input$Education, input$Year, input$Hour, input$Fatigue,
        input$Recommend, input$GrowthHelp, input$CVHelp)
    base <- se %>% lm(SalaryY ~ Industry + Company + Education + Year + Hour + Fatigue + Recommend
                      + GrowthHelp + CVHelp, .)
    i <- data.frame(Industry = input$Industry2, Company = input$Company2, Education = input$Education,
                    Year = input$Year, Hour = input$Hour, Fatigue = as.factor(input$Fatigue),
                    Recommend = as.factor(input$Recommend), GrowthHelp = as.factor(input$GrowthHelp),
                    CVHelp = as.factor(input$CVHelp))
    pred <- predict(base, i)
  })
  P13 <- reactive({
    req(input$Industry3, input$Company3, input$Education, input$Year, input$Hour, input$Fatigue,
        input$Recommend, input$GrowthHelp, input$CVHelp)
    base <- se %>% lm(SalaryY ~ Industry + Company + Education + Year + Hour + Fatigue + Recommend
                      + GrowthHelp + CVHelp, .)
    i <- data.frame(Industry = input$Industry3, Company = input$Company3, Education = input$Education,
                    Year = input$Year, Hour = input$Hour, Fatigue = as.factor(input$Fatigue),
                    Recommend = as.factor(input$Recommend), GrowthHelp = as.factor(input$GrowthHelp),
                    CVHelp = as.factor(input$CVHelp))
    pred <- predict(base, i)
  })
  
  G <- reactive({
    req(input$Industry, input$Company, input$Education, input$Hour, input$Fatigue)
    sb <- se %>% lm(GrowthRate ~ Industry + Company + Hour + Fatigue + Education, .)
    sd <- data.frame(Industry = input$Industry, Company = input$Company, Education = input$Education,
                     Hour = input$Hour, Fatigue = as.factor(input$Fatigue))
    predG <- predict(sb, sd)
  })
  G2 <- reactive({
    req(input$Industry2, input$Company2, input$Education, input$Hour, input$Fatigue)
    sb <- se %>% lm(GrowthRate ~ Industry + Company + Hour + Fatigue + Education, .)
    sd <- data.frame(Industry = input$Industry2, Company = input$Company2, Education = input$Education,
                     Hour = input$Hour, Fatigue = as.factor(input$Fatigue))
    predG <- predict(sb, sd)
  })
  G3 <- reactive({
    req(input$Industry3, input$Company3, input$Education, input$Hour, input$Fatigue)
    sb <- se %>% lm(GrowthRate ~ Industry + Company + Hour + Fatigue + Education, .)
    sd <- data.frame(Industry = input$Industry3, Company = input$Company3, Education = input$Education,
                     Hour = input$Hour, Fatigue = as.factor(input$Fatigue))
    predG <- predict(sb, sd)
  })
  Sum <- reactive({
    req(input$WorkYear)
    g <- G()
    i <- 2.73
    r <- (1+g/100)/(1+i/100)
    Pt <- Gamma()*P1()*(1-r^(input$WorkYear))/(1-r)*10000
    Pt <- Pt %>% ceiling()
  })
  Sum2 <- reactive({
    req(input$WorkYear)
    g <- G2()
    i <- 2.73
    r <- (1+g/100)/(1+i/100)
    Pt <- Gamma()*P12()*(1-r^(input$WorkYear))/(1-r)*10000
    Pt <- Pt %>% ceiling()
  })
  Sum3 <- reactive({
    req(input$WorkYear)
    g <- G3()
    i <- 2.73
    r <- (1+g/100)/(1+i/100)
    Pt <- Gamma()*P13()*(1-r^(input$WorkYear))/(1-r)*10000
    Pt <- Pt %>% ceiling()
  })
  
  output$Compare <- renderPlot({
    req(Sum(), Sum2(), Sum3())
    com <- c(Sum()/10000, Sum2()/10000, Sum3()/10000)
    name <- c(input$Company, input$Company2, input$Company3)
    barplot(com, main = "Comparison", names.arg = name, col="darkblue")
  })
  
  output$TotalSalary <- renderText({
    Sum <- Sum() %>% format(., big.mark=",", scientific=FALSE)
    paste("Estimated Total Salary:", "<font color=\"#FF0000\"><b>", Sum,"($ NTD)", "</b></font>")
  })
  output$TotalSalary2 <- renderText({
    Sum2 <- Sum2() %>% format(., big.mark=",", scientific=FALSE)
    paste("Estimated Total Salary:", "<font color=\"#FF0000\"><b>", Sum2,"($ NTD)", "</b></font>")
  })
  output$TotalSalary3 <- renderText({
    Sum3 <- Sum3() %>% format(., big.mark=",", scientific=FALSE)
    paste("Estimated Total Salary:", "<font color=\"#FF0000\"><b>", Sum3, "($ NTD)", "</b></font>")
  })
  #3-2 Filter Data
  output$SalaryTable <- renderTable({
    se %>% filter(input$Industry == se$Industry & input$Company == se$Company)
  })
  output$SalaryTable2 <- renderTable({
    se %>% filter(input$Industry2 == se$Industry & input$Company2 == se$Company)
  })
  output$SalaryTable3 <- renderTable({
    se %>% filter(input$Industry3 == se$Industry & input$Company3 == se$Company)
  })
  #3-3 Distribution & PR
  Indust <- reactive({
    if(input$choice == 1){
      Ind <- input$Industry 
    } else if(input$choice == 2){
      Ind <- input$Industry2
    } else if(input$choice == 3){
      Ind <- input$Industry3
    }
  })
  IndSal <- reactive({
    if(input$choice == 1){
      Sal <- Sum() 
    } else if(input$choice == 2){
      Sal2 <- Sum2()
    } else if(input$choice == 3){
      Sal3 <- Sum3()
    }
  })
  output$densityind <- renderPlot({
    subset1 <- subset(se$SalaryY, se$Industry == Indust())
    abc <- density(subset1)
    point <- (IndSal() / (input$WorkYear*10000))
    plot(abc,
         main = "Current Industry",
         xlab = "Salary(in millions)",
         ylab = "Density"
    )
    abline(v= point)
  })
  output$densityedu <- renderPlot({
    subset1 <- subset(se$SalaryY, se$Education == input$Education)
    abc <- density(subset1)
    point <- (IndSal() / (input$WorkYear*10000))
    plot(abc,
         main = "Current Education Level",
         xlab = "Salary(in millions)",
         ylab = "Density"
    )
    abline(v= point)
  })
  
  IndPR <- reactive({
    req(Indust())
    est <- (IndSal() / (input$WorkYear*10000))
    subset1 <- subset(se$SalaryY, se$Industry == Indust())
    highersubset <- subset(se$SalaryY, se$Industry == Indust() & se$SalaryY>est)
    PR <- (1 - (length(highersubset) / length(subset1)))*100
    PR <- round(PR,1)
  })
  EduPR <- reactive({
    req(input$Education)
    est <- (IndSal() / (input$WorkYear*10000))
    subset1 <- subset(se$SalaryY, se$Education == input$Education)
    highersubset <- subset(se$SalaryY, se$Education == input$Education & se$SalaryY>est)
    PR <- (1 - (length(highersubset) / length(subset1)))*100
    PR <- round(PR,1)
  })
  output$PRInd <- renderText({
    paste("#Your Estimated Salary is higher than ", "<font color=\"#FF0000\"><b>", IndPR(),"%", "</b></font>", "of people in the same industry.")
  })
  output$PREdu <- renderText({
    paste("#Your Estimated Salary is higher than ", "<font color=\"#FF0000\"><b>", EduPR(),"%", "</b></font>", "of people with the same education level as you.")
  })
  
  ###The 4th Tab
  output$mpgPlot <- renderPlot({
    par(mfrow=c(2,1), mar = c(5, 40, 2, 1))
    boxplot(se$SalaryY~se$Industry,
            col = "#74AADB", pch = 19, horizontal = T, las = 2, main = "Industry Comparison",
            xlab = "Salary per year (10,000)")
    boxplot(se$SalaryY~se$Education,
            col = "#75AADB", pch = 19 , horizontal = T, las = 2, main = "Education Level Comparison",
            xlab = "Salary per year (10,000)")}, height = 1000, width = 1000)
  
  ###The 5th Tab
  output$data <- renderTable({
    se
  })
  
}

shinyApp(ui = ui, server = server)