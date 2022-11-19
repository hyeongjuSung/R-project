#------------------------
# 9-1 처음 만나는 샤이니
#------------------------

#---# [1단계: 샤이니 기본구조의 이해]
install.packages("shiny")  
library(shiny)  # install.packages("shiny")  
ui <- fluidPage("사용자 인터페이스")  # 구성 1: ui
server <- function(input, output, session){}  # 구성 2: server
shinyApp(ui, server)  # 구성 3: 실행

#---# [2단계: 샤이니가 제공하는 샘플 확인하기]
library(shiny)    # 라이브러리 등록
runExample()      # 샘플 보여주기
runExample("01_hello")   # 첫 번째 샘플 실행하기

#---# [3단계: 01_hello 샘플의 사용자 인터페이스 부분]
library(shiny)       # 라이브러리 등록
ui <- fluidPage(     # 사용자 인터페이스 시작: fluidPage 정의
  titlePanel("샤이니 1번 샘플"),  # 타이틀 입력
  #---# 레이아웃 구성: 사이드바 패널 + 메인패널 
  sidebarLayout(
    sidebarPanel(  # 사이드바 패널 시작
      #--- 입력값: input$bins 저장
      sliderInput(inputId = "bins",         # 입력 아이디  
                  label = "막대(bin)갯수:",  # 텍스트 라벨  
                  min = 1, max = 50,        # 선택 범위(1-50)
                  value = 30)),             # 기본 선택 값 30
    mainPanel(   # 메인패널 시작
      #---# 출력값: output$distPlot 저장
      plotOutput(outputId = "distPlot"))  # 차트 출력
  ))

#---# [4단계: 01_hello 샘플의 서버 부분]
server <- function(input, output, session){
  #---# 랜더링한 플롯을 output 인자의 distPlot에 저장
  output$distPlot <- renderPlot({
    x <- faithful$waiting # 분출대기시간 정보 저장
    #---# input$bins을 플롯으로 랜더링
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    #---# 히스토그램 그리기 (맥 사용자 폰트 추가 필요)
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "다음 분출때까지 대기시간(분)",  
         main = "대기시간 히스토그램")
  })
}
#---# 실행
shinyApp(ui, server)
rm(list = ls())  # 메모리 정리하기 

#---------------------
# 9-2 입력과 출력하기
#---------------------

#---# [1단계: 데이터 입력]
library(shiny) 
ui <- fluidPage(   
  sliderInput("range", "연비", min = 0, max = 35, value = c(0, 10)), # 입력

server <- function(input, output, session){
  output$value <- renderText((input$range[1] + input$range[2]))} 

shinyApp(ui, server)