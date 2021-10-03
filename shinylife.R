library(shiny)
library(dplyr)
library(readr)
require(DMwR)

se <- read.csv("C:\\Users\\carl_dis2003\\Desktop\\customerlv\\SalaryF.csv")
se <- knnImputation(se)

se$Fatigue <- se$Fatigue %>% round() %>% as.factor()
se$Recommend <- se$Recommend %>% round() %>% as.factor()
se$GrowthHelp <- se$GrowthHelp %>% round() %>% as.factor()
se$CVHelp <- se$CVHelp %>% round() %>% as.factor()

#############################實驗室##################################

#####################################################################
ui <- navbarPage("Personal Life-Value Estimation",
                 tabPanel("Dimission Rate & Job Satisfaction", 
                          selectInput("Q1", "一旦找到一份更好的工作，我會離開這家公司",
                                      c("Yes" = 0.2, "No" = 0)),
                          selectInput("Q2", "我在本公司以外積極地找工作",
                                      c("Yes" = 0.2, "No" = 0)),
                          selectInput("Q3", "我在認真地考慮辭去這份工作",
                                      c("Yes" = 0.2, "No" = 0)),
                          selectInput("Q4", "我經常想辭去這份工作",
                                      c("Yes" = 0.2, "No" = 0)),
                          selectInput("Q5", "我認為五年內我會一直在這家公司工作", 
                                      c("Yes" = 0, "No" = 0.2)),
                          textOutput("r123")
                 ),
                 
                 tabPanel("Job & Personal Life-Value",
                          sidebarLayout(
                            
                            sidebarPanel(
                              selectizeInput("Industry", "您所在的產業 (Target Industry)：", se$Industry),
                              selectizeInput("Company", "您想進入的公司 (Target Company)：", se$Company),
                              numericInput("WorkYear", "您想要在此公司待幾年 (Years you plan to work) ：",
                                           value = 1, min = 1, max = 46),
                              selectInput("Education", "您的學歷 (Your Education Level) ：", se$Education),
                              selectInput("Year", "您目前已在在這產業工作幾年(Your current working experience)[years] ：", 
                                          c("0~1", "1~2", "2~3", "4~5", "5~7", "7~10", "10~15", "15~20", "20~30", "30+")),
                              selectInput("Hour", "您所能接受的每週工時 (The Weekly working hours you can accept)[Hours/week]：", 
                                          c("30以下", "30~40", "40~45", "45~50", "50~55", "55~60", "60~70", "70~80",
                                            "80~90", "90~100", "120+")),
                              sliderInput("Fatigue", "您對於工作疲勞的接受度 (Your tolerance to Fatigue at work):",
                                          min = 1, max = 8, value = 5),
                              sliderInput("Recommend", "您覺得在職員工對於公司的看法重要嗎 (Inportance of Current staff recommendation)：",
                                          min = 1, max = 5, value = 3),
                              sliderInput("GrowthHelp", "這份工作對你未來事業的成長是否重要 (The job's importance to your Career Growth)：",
                                          min = 1, max = 5, value = 3),
                              sliderInput("CVHelp","這份工作對你履歷的幫助是否重要 (The job's importance to your resume)：",
                                          min = 1, max = 5, value = 3)
                            ),
                            mainPanel(
                              tabsetPanel(
                              tabPanel("Estimated Salary",textOutput("TotalSalary")),
                              tabPanel("Employee Salaries",tableOutput("SalaryTable")),
                              tabPanel("Distribution",plotOutput("densityind"),
                                                textOutput("PRInd"),
                                                 plotOutput("densityedu"),
                                                textOutput("PREdu"))
                              
                              
                              )
                            )                
                          )
                 ),
                 
                 tabPanel("SalaryPlot", 
                          plotOutput("mpgPlot")
                 ),
                 
                 tabPanel("RawData", tableOutput("data")
                 )
)

server <- function(input, output, session){
  
  output$data <- renderTable({
    se
  })
  
  output$r123 <- renderText({
    Gamma() %>% paste("Gamma = ",.)
  })
  
  Gamma <- reactive({
    Q1 <- input$Q1 %>% as.numeric()
    Q2 <- input$Q2 %>% as.numeric()
    Q3 <- input$Q3 %>% as.numeric()
    Q4 <- input$Q4 %>% as.numeric()
    Q5 <- input$Q5 %>% as.numeric()
    Qsum <- Q1+Q2+Q3+Q4+Q5
  })
  
  observeEvent(                     
    input$Industry,
    updateSelectizeInput(session, "Company",  "您想進入的公司 (Target Company)：",
                         choices = se$Company [se$Industry == input$Industry])
  )
  
  output$SalaryTable <- renderTable({
    se %>% filter(input$Industry == se$Industry & input$Company == se$Company)
  })
  
  P1 <- reactive({
    req(input$Industry, input$Company, input$Education, input$Year, input$Hour, input$Fatigue, 
        input$Recommend, input$GrowthHelp, input$CVHelp)
    base <- se %>% lm(SalaryY ~ Industry + Company + Education + Year + Hour + Fatigue + Recommend
                      + GrowthHelp + CVHelp, .)
    i <- data.frame(Industry = input$Industry, Company = input$Company, Education = input$Education, 
                    Year = input$Year, Hour = input$Hour, Fatigue = as.factor(input$Fatigue), 
                    Recommend = as.factor(input$Recommend), GrowthHelp = as.factor(input$GrowthHelp), CVHelp = as.factor(input$CVHelp))
    pred <- predict(base, i)
  })
  
  G <- reactive({
    req(input$Industry, input$Company, input$Education, input$Hour, input$Fatigue)
    sb <- se %>% lm(GrowthRate ~ Industry + Company + Hour + Fatigue + Education, .)
    sd <- data.frame(Industry = input$Industry, Company = input$Company, Education = input$Education, 
                     Hour = input$Hour, Fatigue = as.factor(input$Fatigue))
    predG <- predict(sb, sd) 
  })
  
  Sum <- reactive({
    req(input$WorkYear)
    g <- G()
    i <- 2.73
    r <- (1+g/100)/(1+i/100)
    Pt <- Gamma()*P1()*(1-r^(input$WorkYear))/(1-r)*10000
  })

  
  output$densityind <- renderPlot({
    subset1 <- subset(se$SalaryY, se$Industry == input$Industry)
    abc <- density(subset1)
    point <- (Sum() / (input$WorkYear*10000))
    plot(abc,
         main = "Salary Distribution in Current Industry",
         xlab = "Salary(in millions)",
         ylab = "Density"
    )
    abline(v= point)

    
  })
  
  output$densityedu <- renderPlot({
    subset1 <- subset(se$SalaryY, se$Education == input$Education)
    abc <- density(subset1)
    point <- (Sum() / (input$WorkYear*10000))
    plot(abc,
         main = "Salary Distribution in Current Education",
         xlab = "Salary(in millions)",
         ylab = "Density"
    )
    abline(v= point)

    
  })
    
  IndPR <- reactive({
    req(input$Industry)
    est <- (Sum() / (input$WorkYear*10000))
    subset1 <- subset(se$SalaryY, se$Industry == input$Industry)
    highersubset <- subset(se$SalaryY, se$Industry == input$Industry & se$SalaryY>est)
    PR <- (1 - (length(highersubset) / length(subset1)))*100
    PR <- round(PR,1)
  })
  
  EduPR <- reactive({
    req(input$Education)
    est <- (Sum() / (input$WorkYear*10000))
    subset1 <- subset(se$SalaryY, se$Education == input$Education)
    highersubset <- subset(se$SalaryY, se$Education == input$Education & se$SalaryY>est)
    PR <- (1 - (length(highersubset) / length(subset1)))*100
    PR <- round(PR,1)
  })
  
  output$PRInd <- renderText({
    paste("Your Estimated Salary is higher than ", IndPR(),"% of people")
  })
  
  output$PREdu <- renderText({
    paste("Your Estimated Salary is higher than ", EduPR(),"% of people")
  })
  
  
  output$TotalSalary <- renderText({
      paste("Estimated Total Salary: ", Sum())
  })
  
  output$mpgPlot <- renderPlot({
    par(mfrow=c(2,1))
    boxplot(se$SalaryY~se$Industry,
            col = "#75AADB", pch = 19 , horizontal = T)
    boxplot(se$SalaryY~se$Education,
            col = "#75AADB", pch = 19 , horizontal = T)
  }, height = 1000, width = 500)
  
}
shinyApp(ui = ui, server = server)

