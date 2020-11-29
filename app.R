#install.packages("dplyr")
#install.packages('tidyverse')
options(shiny.sanitize.errors = F) ## Error 내용 확인가능


library(shiny)
library(tidyverse)
library(dplyr)
library(showtext)

#한글패치 나눔고딕체
font_add_google(name = "Nanum Gothic")
showtext_auto()
showtext_opts(dpi = 100)

#install github 해서 iris 불러오듯이 데이터파일 불러올수있게 만듬
data1 <- readRDS(file = 'long_1128_1.RDS')#매출액 데이터 파일 불러오기
data2 <- readRDS(file = 'long_1128_2.RDS')#부채 데이터 파일 불러오기

#github에서 csv파일을 받아와서 샤이니 실행할때마다 연동하는 방법도 있음

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("company salse analysis"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      textInput("text", label = h3("회사명을 정확하게 입력하세요"), value = "EX)삼성전자"),

      hr(),

      submitButton('submit')
    ),

    # Show a plot of the generated distribution
    mainPanel(
      uiOutput("mainUI")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$value <- renderPrint({ input$text })

  # 그래프 그리기
  output$barplot1 <- renderPlot({

    data1 %>%
      filter(회사 == input$text) %>%
      ggplot(aes(x = year, y = sales , fill = year)) +
      geom_col() +
      ggtitle("[Salse volume]")+
      theme_bw()

  })


  output$barplot2 <- renderPlot({

    data2 %>%
      filter(회사 == input$text) %>%
      ggplot(aes(x = year, y = debt , fill = year)) +
      geom_col() +
      ggtitle("[Debt volume]")+
      theme_bw()

  })

  #출력

  output$mainUI <- renderUI({

    tabsetPanel(tabPanel("Sales Volume",plotOutput("barplot1")),
                tabPanel("Debt Volume",plotOutput("barplot2")))

  })


}

# Run the application
shinyApp(ui = ui, server = server)
