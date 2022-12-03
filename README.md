R-project 성형주
=============
2022-2 실무프로젝트 수업 내용 정리
-------------
## [11월 30일]
### 서울시 아파트 실거래 애플리케이션 만들기
> 1. 라이브러리 불러오기
```R
library(devtools); library(sf); library(purrr); library(dplyr); library(DT) 
library(rgdal); library(lattice); library(latticeExtra); library(lubridate)  
library(ggplot2); library(ggfortify); library(ggrepel); library(showtext)  
library(leaflet); library(leaflet.extras); library(raster); library(shiny)  
library(mapview); library(mapedit); library(grid)
```
> 2. 한글 글꼴 설정하기
```R
require(showtext)
font_add_google(name='Nanum Gothic', regular.wt=400, bold.wt=700)
showtext_auto()
showtext_opts(dpi=112)
```
> 3. 데이터 불러오기
- 배포할때 setwd()는 반드시 주석처리 해야함
```R
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
grid <- st_read("./01_code/sigun_grid/seoul.shp") 
bnd <- st_read("./01_code/sigun_bnd/seoul.shp")  
load("./06_geodataframe/06_apt_price.rdata")      
load("./07_map/07_kde_high.rdata")                
load("./07_map/07_kde_hot.rdata")     
```
> 4. 마커 클러스터링 설정
- quantile()로 극단치 제거
```R
pcnt_10 <- as.numeric(quantile(apt_price$py, probs = seq(.1, .9, by = .1))[1]) 
pcnt_90 <- as.numeric(quantile(apt_price$py, probs = seq(.1, .9, by = .1))[9]) 
load("./01_code/circle_marker/circle_marker.rdata")                            
circle.colors <- sample(x=c("red","green","blue"), size=1000, replace=TRUE)    
```
> 5. 그리드 필터링하기
```R
grid <- as(grid, "Spatial") 
grid <- as(grid, "sfc") 
grid <- grid[which(sapply(st_contains(st_sf(grid), apt_price), length)>0)]
```
> 6. 사용자 화면 만들기
```R
ui <- fluidPage(
  #---#
  fluidRow(
    column(9, selectModUI("selectmap"), div(style = "height:45px")),
    column(3, sliderInput("range_time", "Construction Year", sep = "", min = 1960, 
                          max = 2021, value = c(1970, 2020)),
              sliderInput("range_area", "Area", sep = "", min = 0, 
                          max = 350, value = c(0, 200)), )), 
  #---#
  tabsetPanel(
    tabPanel("Chart",
      column(4, h5("Price Range", align = "center"), 
             plotOutput("density", height=300),), 
      column(4, h5("Price Trends", align = "center"), 
             plotOutput("regression", height=300)), 
      column(4, h5("PCA", 
             align = "center"), plotOutput("pca", height=300)), ),
    tabPanel("Table", DT::dataTableOutput("table")) 
))
```
> 7. 서버 만들기
- subset()으로 범위를 선택 후 reactive() 반응식으로 저장
```R
server <- function(input, output, session){
# 슬라이드 범위 선택 필터링
  all = reactive({
    all = subset(apt_price, con_year >= input$range_time[1] & 
                            con_year <= input$range_time[2] & 
                            area >= input$range_area[1] & 
                            area <= input$range_area[2])
    return(all)})
# 지도 그리기
  g_sel <- callModule(selectMod, "selectmap",
    leaflet() %>% 
      addTiles(options = providerTileOptions(minZoom = 9, maxZoom = 18)) %>% 
      addRasterImage(raster_high, 
        colors = colorNumeric(c("blue", "green", "yellow","red"), 
        values(raster_high), na.color = "transparent"), 
        opacity = 0.4, group = "2021 High Price") %>%
      addRasterImage(raster_hot, 
        colors = colorNumeric(c("blue", "green", "yellow","red"), 
        values(raster_hot), na.color = "transparent"), 
        opacity = 0.4, group = "2021 Hot Spot") %>%
      addLayersControl(baseGroups = c("2021 High Price", "2021 Hot Spot"), 
        options = layersControlOptions(collapsed = FALSE)) %>%
      addPolygons(data=bnd, weight = 3, stroke = T, 
        color = "red", fillOpacity = 0) %>%
      addCircleMarkers(data = apt_price, lng =unlist(map(apt_price$geometry,1)), 
        lat = unlist(map(apt_price$geometry,2)), radius = 10, stroke = FALSE, 
        fillOpacity = 0.6, fillColor = circle.colors, weight=apt_price$py, 
        clusterOptions = markerClusterOptions(iconCreateFunction=JS(avg.formula))) %>% 
      leafem::addFeatures(st_sf(grid), layerId = ~seq_len(length(grid)), color = 'grey'))
# 반응 결과 필터링
  rv <- reactiveValues(intersect=NULL, selectgrid=NULL)  # 반응식 초깃값 NULL
  observe({
    gs <- g_sel() 
    rv$selectgrid <- st_sf(grid[as.numeric(gs[which(gs$selected==TRUE),"id"])])
    if(length(rv$selectgrid) > 0){
      rv$intersect <- st_intersects(rv$selectgrid, all())
      rv$sel       <- st_drop_geometry(apt_price[apt_price[unlist(rv$intersect[1:10]),],])
    } else {
      rv$intersect <- NULL
    }
  })
# 차트 그리기1: 확률 밀도 함수
  output$density <- renderPlot({
    if (nrow(rv$intersect) == 0)  
      return(NULL)
    max_all  <- density(all()$py)  ; max_all <- max(max_all$y)
    max_sel  <- density(rv$sel$py) ; max_sel <- max(max_sel$y)
    plot_high  <- max(max_all, max_sel)
    avg_all <- mean(all()$py)
    avg_sel <- mean(rv$sel$py)
    plot(stats::density(all()$py), xlab=NA, ylab=NA, ylim=c(0, plot_high),
         col="blue", lwd=3, main= NA)
    abline(v = avg_all, lwd = 2, col = "blue", lty=2)
    text(avg_all + (avg_all)*0.13, plot_high * 0.1, 
         sprintf("%.0f", avg_all), srt=0.2, col = "blue")
    lines(stats::density(rv$sel$py), ylim=c(0, plot_high), 
          col="red", lwd=3, main= NA)
    abline(v = avg_sel, lwd = 2, col = "red", lty=2)
    text(avg_sel + (avg_sel)*0.13, plot_high * 0.3, 
         sprintf("%.0f", avg_sel), srt=0.2, col = "red")
  })
# 회귀분석
  output$regression <- renderPlot({
    if (nrow(rv$intersect) == 0) 
      return(NULL)
    all <- aggregate(all()$py, by=list(all()$ym),mean)
    sel <- aggregate(rv$sel$py, by=list(rv$sel$ym),mean)
    fit_all <- lm(all$x ~ all$Group.1)   
    fit_sel <- lm(sel$x ~ sel$Group.1)   
    coef_all <- round(summary(fit_all)$coefficients[2], 1) * 365  
    coef_sel <- round(summary(fit_sel)$coefficients[2], 1) * 365 
    grob_1 <- grobTree(textGrob(paste0("All: ",              
                coef_all), x=0.05,  y=0.84, hjust=0,
                gp=gpar(col="blue", fontsize=13)))
    grob_2 <- grobTree(textGrob(paste0("Sel: ", 
                coef_sel), x=0.05,  y=0.95, hjust=0,
                gp=gpar(col="red", fontsize=16, fontface="bold")))
    gg <- ggplot(sel, aes(x=Group.1, y=x, group=1)) +
      geom_smooth(color= "red",size=1.5, se=F) + xlab("Year")+ ylab("Price") +
      theme(axis.text.x=element_text(angle=90)) +
      stat_smooth(method='lm', linetype = "dashed", se=F) +
      theme_bw()
    gg + geom_smooth(data=all, aes(x=Group.1, y=x, group=1, se=F), 
      color="blue", size=1, se=F) +
      annotation_custom(grob_1) + 
      annotation_custom(grob_2)
  })
# 주성분 분석
  output$pca <- renderPlot({
    if (nrow(rv$intersect) == 0)  
      return(NULL)
    pca_01 <- aggregate(list(rv$sel$con_year, rv$sel$floor, 
      rv$sel$py, rv$sel$area), by=list(rv$sel$apt_nm), mean)
    colnames(pca_01) <- c("apt_nm", "new", "floor","price", "area") 
    m <- prcomp(~ new + floor + price + area, data= pca_01, scale=T)
    autoplot(m, size=NA, loadings.label=T, loadings.label.size=4)+
      geom_label_repel(aes(label=pca_01$apt_nm), size=3, alpha = 0.7, family="Nanum Gothic")
  })
# 테이블 그리기
  output$table <- DT::renderDataTable({
    dplyr::select(rv$sel, ymd, addr_1, apt_nm, price, area, floor, py) %>% 
      arrange(desc(py))}, extensions = 'Buttons',
      options = list(dom = 'Bfrtip', scrollY = 300, scrollCollapse = T, 
      paging = TRUE, buttons = c('excel'))) 
}
```
> 8. 애플리케이셔 실행하기
```R
shinyApp(ui, server)
```
11장 - 애플리케이션 배포하기
### 배포 준비하기
> 1. 샤이니 클라우드 서비스 사용을 위해 [shinyapps](https://www.shinyapps.io/) 접속
> 2. 회원가입 및 로그인 후 계정 설정화면에서 계정이름 입력
> 3. R스튜디오를 배포 계정에 연결하기 위해 메뉴 Tools-Global Options-Publishing-connect Shinyapps.io 선택하여 복사한 계정정보 입력
> 4. app.R 파일에서 setwd()문 주석처리 후 File-Publish로 파일 올리기
> 5. Publish 선택 후 업로드가 완료되면 애플리케이션 자동 실행
## [11월 23일]
### 반응형 웹 애플리케이션 만들기
> 1. 데이터 준비하기
- 반응성: 입력값이 변경될 때 서버가 자동으로 변화를 감지하여 출력값을 렌더링 후 갱신하는 것
- DT 패키지: 데이터 테이블을 편리하게 다룰 수 있는 패키지
- mpg: ggplot2 패키지에 포함된 자동차 연비 테스트 결과 테이터 세트 불러옴
```R
library(DT)      # install.packages("DT")
library(ggplot2) # install.packages("ggplot2")
mpg <- mpg
head(mpg)
```
> 2. 반응식 작성하기
- sliderInput() 으로 연비 범위 슬라이드 생성
- subset() 으로 반응식 결과 저장
- 반응식 결과를 사용할 때 뒤에 호출연산자 ()를 붙여야 함
```R
library(shiny) 
ui <- fluidPage(
  sliderInput("range", "연비", min = 0, max = 35, value = c(0, 10)), # 입력
  DT::dataTableOutput("table"))   # 출력

server <- function(input, output, session){
  #---# 반응식
  cty_sel = reactive({  
    cty_sel = subset(mpg, cty >= input$range[1] & cty <= input$range[2])
    return(cty_sel)})    
  #---# 반응결과 렌더링
  output$table <- DT::renderDataTable(cty_sel()) }

shinyApp(ui, server)
```
### 레이아웃 정의하기
> 3. 단일 페이지 레이아웃
- 샤이니에서 레이아웃은 제한된 화면 안에 입력 위젯과 출력 결과를 배치하는 방식을 의미
- 그리드라는 규격화 된 레이아웃 선호
- 그리드 방식을 사용하려면 ui()에서 fluidPage() - fluidRow() - column() 순으로 화면을 정의
```R
library(shiny)
#---# 전체 페이지 정의
ui <- fluidPage(  
  #---# 행 row 구성 정의
  fluidRow(    
    #---# 첫번째 열: 붉은색(red) 박스로 높이 450 픽셀, 폭 9
    column(9, div(style = "height:450px;border: 4px solid red;","폭 9")),
    #---# 두번째 열: 보라색(purple) 박스로 높이 450 픽셀, 폭 3
    column(3, div(style = "height:450px;border: 4px solid purple;","폭 3")),
    #---# 세번째 열: 파란색(blue) 박스로 높이 400 픽셀, 폭 12
    column(12, div(style = "height:400px;border: 4px solid blue;","폭 12"))))
server <- function(input, output, session) {}
shinyApp(ui, server)
```
> 4. 탭 페이지 추가하기
- tabsetPanel()로 탭 패널을 추가
```R
library(shiny)
ui <- fluidPage(
  fluidRow(
    column(9, div(style = "height:450px;border: 4px solid red;","폭 9")),
    column(3, div(style = "height:450px;border: 4px solid red;","폭 3")),
    #---# 탭패널 1~2번 추가 
    tabsetPanel(
      tabPanel("탭1",   
               column(4, div(style = "height:300px;border: 4px solid red;","폭 4")),
               column(4, div(style = "height:300px;border: 4px solid red;","폭 4")),           
               column(4, div(style = "height:300px;border: 4px solid red;","폭 4")), ),              
      tabPanel("탭2", div(style = "height:300px;border: 4px solid blue;","폭 12")))))
server <- function(input, output, session) {}
shinyApp(ui, server)
```
10장 - 데이터 분석 애플리케이션 개발하기
### 반응형 지도 만들기
> 1. 데이터 불러오기
- leaflet: 반응형 지도를 만드는 자바스크립트 라이브러리
```R
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
load("./06_geodataframe/06_apt_price.rdata")      # 아파트 실거래 데이터
library(sf)
bnd <- st_read("./01_code/sigun_bnd/seoul.shp")   # 서울시 경계선
load("./07_map/07_kde_high.rdata")    # 최고가 래스터 이미지
load("./07_map/07_kde_hot.rdata")     # 급등지역 래스터 이미지
grid <- st_read("./01_code/sigun_grid/seoul.shp")   # 서울시 그리드
```
> 2. 마커 클러스터링 설정
- 데이터가 왜곡되는 것을 방지하고자 quantile()로 지점 특정
```R
pcnt_10 <-as.numeric(quantile(apt_price$py, probs=seq(.1,.9,by=.1))[1])   # 하위10%
pcnt_90 <-as.numeric(quantile(apt_price$py, probs=seq(.1,.9,by=.1))[9])   # 상위10%
load("./01_code/circle_marker/circle_marker.rdata")   # 마커 클러스터링 함수
circle.colors <- sample(x=c("red","green","blue"), size=1000, replace=TRUE)
```
> 3. 반응형 지도 만들기
- 래스터 이미지 레이어를 바꿀 수 있도록 addLayersControl()로 변경 스위치 추가
```R
library(leaflet)
library(purrr)
library(raster)
leaflet() %>% 
  #---# 기본 맵 설정: 오픈스트리트맵
  addTiles(options = providerTileOptions(minZoom = 9, maxZoom = 18)) %>% 
  #---# 최고가 지역 KDE 
  addRasterImage(raster_high, 
    colors = colorNumeric(c("blue", "green","yellow","red"), 
    values(raster_high), na.color = "transparent"), opacity = 0.4, 
    group = "2021 최고가") %>%
  #---# 급등 지역 KDE 
  addRasterImage(raster_hot, 
    colors = colorNumeric(c("blue", "green","yellow","red"), 
    values(raster_hot), na.color = "transparent"), opacity = 0.4, 
    group = "2021 급등지") %>%
  #---# 레이어 스위치 메뉴
  addLayersControl(baseGroups = c("2021 최고가", "2021 급등지"), 
    options = layersControlOptions(collapsed = FALSE)) %>%   
  #---# 서울시 외곽 경계선
  addPolygons(data=bnd, weight = 3, stroke = T, color = "red", 
    fillOpacity = 0) %>%
  #---# 마커 클러스터링
  addCircleMarkers(data = apt_price, lng =unlist(map(apt_price$geometry,1)), 
    lat = unlist(map(apt_price$geometry,2)), radius = 10, stroke = FALSE, 
    fillOpacity = 0.6, fillColor = circle.colors, weight=apt_price$py, 
    clusterOptions = markerClusterOptions(iconCreateFunction=JS(avg.formula))) 
```
### 지도 애플리케이션 만들기
> 4. 그리드 필터링하기
- 그리드 데이터는 sf형이음로 필요한 부분만 잘라내기위해 sp형으로 바꾼 후 변환
```R
grid <- st_read("./01_code/sigun_grid/seoul.shp")       # 그리드 불러오기
grid <- as(grid, "Spatial") ; grid <- as(grid, "sfc")   # 변환
grid <- grid[which(sapply(st_contains(st_sf(grid),apt_price),length) > 0)]   # 필터링
plot(grid)   # 그리드 확인
```
> 2. 반응형 지도 모듈화하기
- leaflet 기반 반응형 지도 생성
- addFeatures()로 그리드 지도 레이어가 추가
- 지도를 m이라는 변수로 저장
```R
m <- leaflet() %>% 
  #---# 기본 맵 설정: 오픈스트리트맵
  addTiles(options = providerTileOptions(minZoom = 9, maxZoom = 18)) %>% 
  #---# 최고가 지역 KDE 
  addRasterImage(raster_high, 
    colors = colorNumeric(c("blue", "green","yellow","red"), 
    values(raster_high), na.color = "transparent"), opacity = 0.4, 
    group = "2021 최고가") %>%
  #---# 급등 지역 KDE 
  addRasterImage(raster_hot, 
    colors = colorNumeric(c("blue", "green","yellow","red"), 
    values(raster_hot), na.color = "transparent"), opacity = 0.4, 
    group = "2021 급등지") %>%
  #---# 레이어 스위치 메뉴
  addLayersControl(baseGroups = c("2021 최고가", "2021 급등지"),
    options = layersControlOptions(collapsed = FALSE)) %>%   
  #---# 서울시 외곽 경계선
  addPolygons(data=bnd, weight = 3, stroke = T, color = "red", 
    fillOpacity = 0) %>%
  #---# 마커 클러스터링
  addCircleMarkers(data = apt_price, lng =unlist(map(apt_price$geometry,1)), 
    lat = unlist(map(apt_price$geometry,2)), radius = 10, stroke = FALSE, 
    fillOpacity = 0.6, fillColor = circle.colors, weight=apt_price$py, 
    clusterOptions = markerClusterOptions(iconCreateFunction=JS(avg.formula))) %>%
  #---# 그리드
  leafem::addFeatures(st_sf(grid), layerId= ~seq_len(length(grid)), color = 'grey')
m
```
## [11월 16일]
9장 - 샤이니 입문하기
### 처음 만나는 샤이니
> 1. 샤이니 기본 구조 이해하기
- R은 분석 결과를 웹 애플리케이션으로 구현할 수 있는 샤이니라는 패키지 제공
```R
library(shiny)  # install.packages("shiny")  
ui <- fluidPage("사용자 인터페이스")  # 구성 1: ui
server <- function(input, output, session){}  # 구성 2: server
shinyApp(ui, server)  # 구성 3: 실행
```
> 2. 샘플 실행해보기
- 데이터 분석은 명령형과 반응형 방식으로 구분
- 명령형은 데이터 분석을 단계별로 진행(R 데이터 분석)
- 반응형은 분석을 진행하다가 특정한 조건이 바뀌면 되돌아가 다시 분석(샤이니 애플리케이션)
```R
library(shiny)    # 라이브러리 등록
runExample()      # 샘플 보여주기
runExample("01_hello")   # 첫 번째 샘플 실행하기
```
> 3. 사용자 인터페이스 부분
- fluidPage() 로 단일 페이지 화면 생성
```R
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
```
> 4. 서버 부분
- server()는 ui()의 input$bins 데이터를 받아 분석한 후 output$distPlot로 전달
- session은 여러 사람이 동시에 샤이니를 이용할 경우 독립성을 확보하는 역할을 수행
```R
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
```
### 입력과 출력하기
> 5. 입력받기 input$~
- 샤이니는 입력 조건을 바꿔서 서버의 계산을 거쳐 출력 결과로 전달하는 과정 중요
- 아래의 코드는 ui의 입력함수만 정의하고 이를 분석해 다시 돌려주는 server()의 출력함수를 정의하지 않았으므로 슬라이더를 조작해도 변화 X
```R
library(shiny) 
ui <- fluidPage(   
  sliderInput("range", "연비", min = 0, max = 35, value = c(0, 10))) # 입력

server <- function(input, output, session){}  # 반응 없음

shinyApp(ui, server)  # 실행
```
> 6. 출력하기 output$~
- renderText() 내에서 입력값을 더한 다음 output$value에 저장
- 이 값이 textOutput로 연결되어 계산된 값을 화면에 출력
```R
library(shiny) 
ui <- fluidPage(
  sliderInput("range", "연비", min = 0, max = 35, value = c(0, 10)), # 입력
  textOutput("value"))  

server <- function(input, output, session){
  output$value <- renderText((input$range[1] + input$range[2]))} 

shinyApp(ui, server)
```
## [11월 9일]
7장 - 분석 주제를 지도로 시각화하기
> 6. 불필요한 부분 자르기
- crop() 함수로 외곽선을 기준으로 래스터 이미지를 잘라냄
```R
bnd <- st_read("./01_code/sigun_bnd/seoul.shp")    # 서울시 경계선 불러오기
raster_high <- crop(raster_high, extent(bnd))      # 외곽선 자르기
crs(raster_high) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 + towgs84=0,0,0") # 좌표계 정의
plot(raster_high)  # 지도확인
plot(bnd, col=NA, border = "red", add=TRUE)
```
> 7. 지도 그리기
```R
library(rgdal)    # install.packages("rgdal")
library(leaflet)  # install.packages("leaflet")
leaflet() %>% 
  #---# 베이스맵 불러오기
  addProviderTiles(providers$CartoDB.Positron) %>% 
  #---# 서울시 경계선 불러오기
  addPolygons(data = bnd, weight = 3, color= "red", fill = NA) %>% 
  #---# 레스터 이미지 불러오기
  addRasterImage(raster_high, 
   colors = colorNumeric(c("blue", "green","yellow","red"), 
   values(raster_high), na.color = "transparent"), opacity = 0.4) 
```
> 8. 평균 가격 정보 저장하기
```R
dir.create("07_map")  # 새로운 폴더 생성
save(raster_high, file="./07_map/07_kde_high.rdata") # 저장
rm(list = ls()) # 메모리 정리  
```
### 요즘 뜨는 지역은 어디일까?
> 1. 데이터 준비하기
```R
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) # 작업폴더 설정
load("./06_geodataframe/06_apt_price.rdata")     # 실거래 불러오기
grid <- st_read("./01_code/sigun_grid/seoul.shp")  # 서울시 1km 그리드 불러오기
apt_price <-st_join(apt_price, grid, join = st_intersects)  # 실거래 + 그리드 공간결합
head(apt_price, 2)
```
> 2. 이전/이후 데이터 세트 만들기
```R
kde_before <- subset(apt_price, ymd < "2021-07-01")  # 이전 데이터 필터링
kde_before <- aggregate(kde_before$py, by=list(kde_before$ID),mean)  # 평균가격
colnames(kde_before) <- c("ID", "before")   # 컬럼명 변경

kde_after  <- subset(apt_price, ymd > "2021-07-01")  # 이후 데이터 필터링
kde_after <- aggregate(kde_after$py, by=list(kde_after$ID),mean) # 평균가격 
colnames(kde_after) <- c("ID", "after")  # 컬럼명 변경

kde_diff <- merge(kde_before, kde_after, by="ID")    # 이전 + 이후 데이터 결합
kde_diff$diff <- round((((kde_diff$after-kde_diff$before)/
                           kde_diff$before)* 100), 0) # 변화율 계산

head(kde_diff, 2) # 변화율 확인
```
> 3. 가격이 오른 지역 찾기
```R
library(sf)        # install.packages("sf")
kde_diff <- kde_diff[kde_diff$diff > 0,]    # 상승지역만 추출
kde_hot <- merge(grid, kde_diff,  by="ID")  # 그리드에 상승지역 결합
library(ggplot2)   # install.packages("ggplot2")
library(dplyr)     # install.packages("dplyr")
kde_hot %>%        # 그래프 시각화
  ggplot(aes(fill = diff)) + 
  geom_sf() + 
  scale_fill_gradient(low = "white", high = "red")
```
> 4. 4~7 기타 지도 작업
```R
library(sp)   # install.packages("sp")
kde_hot_sp <- as(st_geometry(kde_hot), "Spatial") # sf형 => sp형 변환
x <- coordinates(kde_hot_sp)[,1]  # 그리드 x, y 좌표 추출
y <- coordinates(kde_hot_sp)[,2] 

l1 <- bbox(kde_hot_sp)[1,1] - (bbox(kde_hot_sp)[1,1]*0.0001) # 그리드 기준 경계지점 설정
l2 <- bbox(kde_hot_sp)[1,2] + (bbox(kde_hot_sp)[1,2]*0.0001)
l3 <- bbox(kde_hot_sp)[2,1] - (bbox(kde_hot_sp)[2,1]*0.0001)
l4 <- bbox(kde_hot_sp)[2,2] + (bbox(kde_hot_sp)[1,1]*0.0001)

library(spatstat)  # install.packages("spatstat")
win <- owin(xrange=c(l1,l2), yrange=c(l3,l4))  # 경계지점 기준 외곽선 만들기(bounding-box)
plot(win)                                      # 확인
rm(list = c("kde_hot_sp", "apt_price", "l1", "l2", "l3", "l4")) # 메모리 정리

p <- ppp(x, y, window=win, marks=kde_hot$diff) # 경계창 위에 좌표값 포인트 생성
d <- density.ppp(p, weights=kde_hot$diff,      # 포인트를 커널밀도 함수로 변환
                 sigma = bw.diggle(p), 
                 kernel = 'gaussian')
plot(d)   # 확인
rm(list = c("x", "y", "win","p")) # 변수 정리

d[d < quantile(d)[4] + (quantile(d)[4]*0.1)] <- NA  # 노이즈 제거
library(raster)         # install.packages("raster")
raster_hot <- raster(d) # 레스터 변환
plot(raster_hot) #  확인

bnd <- st_read("./01_code/sigun_bnd/seoul.shp") # 서울시 경계선 불러오기
raster_hot <- crop(raster_hot, extent(bnd))            # 외곽선 클리핑
crs(raster_hot) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84  
                        +towgs84=0,0,0")  # 좌표계 정의
plot(raster_hot)   #  확인
plot(bnd, col=NA, border = "red", add=TRUE)
```
> 5. 지도 그리기
```R
library(leaflet)   # install.packages("leaflet")
leaflet() %>%
  #---# 베이스맵 불러오기
  addProviderTiles(providers$CartoDB.Positron) %>% 
  #---# 서울시 경계선 불러오기
  addPolygons(data = bnd, weight = 3, color= "red", fill = NA) %>% 
  #---# 레스터 이미지 불러오기
  addRasterImage(raster_hot, 
   colors = colorNumeric(c("blue", "green", "yellow","red"), 
   values(raster_hot), na.color = "transparent"), opacity = 0.4)
```
> 6. 평균 가격 변화율 정보 저장하기
```R
save(raster_hot, file="./07_map/07_kde_hot.rdata") # 저장하기
rm(list = ls())      # 메모리 정리하기 
```
### 우리 동네가 옆 동네보다 비쌀까?
> 1. 데이터 준비하기
```R
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) # 작업폴더 설정
load("./06_geodataframe/06_apt_price.rdata")   # 실거래 불러오기
load("./07_map/07_kde_high.rdata")    # 최고가 레스터 이미지
load("./07_map/07_kde_hot.rdata")     # 급등지 레스터 이미지

library(sf)    # install.packages("sf") 
bnd <- st_read("./01_code/sigun_bnd/seoul.shp")    # 서울시 경계선
grid <- st_read("./01_code/sigun_grid/seoul.shp")  # 서울시 1km 그리드 불러오기
```
> 2. 마커 클러스터링 옵션 설정하기
- avg.fomula 라는 마커 클러스터링용 자바스크립트 실행
```R
#---# 이상치 설정(하위 10%, 상위 90% 지점)
pcnt_10 <- as.numeric(quantile(apt_price$py, probs = seq(.1, .9, by = .1))[1])
pcnt_90 <- as.numeric(quantile(apt_price$py, probs = seq(.1, .9, by = .1))[9])
#---# 마커 클러스터링 함수 등록
load("./01_code/circle_marker/circle_marker.rdata")
#---# 마커 클러스터링 컬러 설정: 상, 중, 하
circle.colors <- sample(x=c("red","green","blue"),size=1000, replace=TRUE)
```
> 3. 마커 클러스터링 시각화하기
- addCircleMarkers() 로 마커 클러스터링 기능 추가
- addLayersControl() 옵션으로 두 이미지 중 어떤 이미지를 나타낼 것인지 선택
```R
library(purrr)  # install.packages("purrr")
leaflet() %>% 
  #---# 오픈스트리트맵 불러오기
  addTiles() %>%  
  #---# 서울시 경계선 불러오기
  addPolygons(data = bnd, weight = 3, color= "red", fill = NA) %>%
  #---# 최고가 레스터 이미지 불러오기
  addRasterImage(raster_high, 
    colors = colorNumeric(c("blue","green","yellow","red"), values(raster_high), 
    na.color = "transparent"), opacity = 0.4, group = "2021 최고가") %>% 
  #---# 급등지 레스터 이미지 불러오기
  addRasterImage(raster_hot, 
    colors = colorNumeric(c("blue", "green", "yellow","red"), values(raster_hot), 
    na.color = "transparent"), opacity = 0.4, group = "2021 급등지") %>%   
  #---# 최고가 / 급등지 선택 옵션 추가하기
  addLayersControl(baseGroups = c("2021 최고가", "2021 급등지"), options = layersControlOptions(collapsed = FALSE)) %>%
  #---# 마커 클러스터링 불러오기
  addCircleMarkers(data = apt_price, lng =unlist(map(apt_price$geometry,1)), 
                   lat = unlist(map(apt_price$geometry,2)), radius = 10, stroke = FALSE, 
                   fillOpacity = 0.6, fillColor = circle.colors, weight=apt_price$py, 
                   clusterOptions = markerClusterOptions(iconCreateFunction=JS(avg.formula))) 

#---# 메모리 정리하기 
rm(list = ls())  
```
## [11월 2일]
7장 - 분석 주제를 지도로 시각화하기
### 어느 지역이 제일 비쌀까?
> 1. 지역별 평균 가격 구하기
- st_read() 함수로 읽어온 서울시 그리드 파일을 st_join() 함수로 특성이 서로 다른 두 테이터를 결합
- 결합 옵션은 2개의 형상이 교차하는지를 판별하는 st_intersects 사용
- aggregate() 함수로 그리드 내에 거래된 평당 거래가를 취합
```R
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) # 작업폴더 설정
load("./06_geodataframe/06_apt_price.rdata")   # 실거래 불러오기
library(sf)    # install.packages("sf") 
grid <- st_read("./01_code/sigun_grid/seoul.shp")     # 서울시 1km 그리드 불러오기
apt_price <-st_join(apt_price, grid, join = st_intersects)  # 실거래 + 그리드 결합
head(apt_price)

kde_high <- aggregate(apt_price$py, by=list(apt_price$ID), mean) # 그리드별 평균가격(평당) 계산
colnames(kde_high) <- c("ID", "avg_price")   # 컬럼명 변경
head(kde_high, 2)     # 확인
```
> 2. 평균 가격 정보 표시하기
- merge() 함수로 그리드 정보를 평균가격에 공간 결합
```R
kde_high <- merge(grid, kde_high,  by="ID")   # ID 기준으로 결합
library(ggplot2) # install.packages("ggplot2")
library(dplyr)   # install.packages("dplyr")
kde_high %>% ggplot(aes(fill = avg_price)) + # 그래프 시각화
  geom_sf() + 
  scale_fill_gradient(low = "white", high = "red")
```
> 3. 지도 경계 그리기
- 데이터가 집중된 곳을 찾기 위해 커널 밀도 추정 이용
- coordinates() 함수로 각 그리드의 중심좌표를 추출
- bbox() 함수로 11부터 14까지 외곽 끝 지점을 나타내는 좌표 4개 추출
- owin() 함수로 외곽 좌표를 연결하는 지도 경계선 생성
```R
library(sp) # install.packages("sp")
kde_high_sp <- as(st_geometry(kde_high), "Spatial")    # sf형 => sp형 변환
x <- coordinates(kde_high_sp)[,1]  # 그리드 x, y 좌표 추출
y <- coordinates(kde_high_sp)[,2] 

l1 <- bbox(kde_high_sp)[1,1] - (bbox(kde_high_sp)[1,1]*0.0001) # 그리드 기준 경계지점 설정
l2 <- bbox(kde_high_sp)[1,2] + (bbox(kde_high_sp)[1,2]*0.0001)
l3 <- bbox(kde_high_sp)[2,1] - (bbox(kde_high_sp)[2,1]*0.0001)
l4 <- bbox(kde_high_sp)[2,2] + (bbox(kde_high_sp)[1,1]*0.0001)

library(spatstat)  # install.packages("spatstat")
win <- owin(xrange=c(l1,l2), yrange=c(l3,l4)) # 지도 경계선 생성
plot(win)         # 지도 경계선 확인
rm(list = c("kde_high_sp", "apt_price", "l1", "l2", "l3", "l4")) # 변수 정리
```
> 4. 밀도 그래프 표시하기
- 커널 밀도 추정을 위해 지도 경계선 내의 포인트 분포 데이터로 커널 밀도 계산
- ppp() 함수로 위도와 경도를 포인트로 변환
- density.ppp() 함수로 생성한 포인트를 연속된 곡선을 가지는 커널로 변환
```R
p <- ppp(x, y, window=win)  # 경계창 위에 좌표값 포인트 생성
d <- density.ppp(p, weights=kde_high$avg_price, # 포인트를 커널밀도 함수로 변환
                 sigma = bw.diggle(p), 
                 kernel = 'gaussian')  
plot(d)   # 확인
rm(list = c("x", "y", "win","p")) # 변수 정리
```
> 5. 래스터 이미지로 변환하기
- quantile() 전체 데이터를 순서대로 정렬할때 지점을 알려주는 함수
```R
d[d < quantile(d)[4] + (quantile(d)[4]*0.1)] <- NA   # 노이즈 제거
library(raster)      #  install.packages("raster")
raster_high <- raster(d)  # 레스터 변환
plot(raster_high)
```
## [10월 26일]
### 주소와 좌표 결합하기
> 1. 데이터 불러오기
```R
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
load("./04_preprocess/04_preprocess.rdata")    # 주소 불러오기
load("./05_geocoding/05_juso_geocoding.rdata") # 좌표 불러오기
```
> 2. 주소와 좌표 결합하기
- 데이터 결합을 위해 left_join() 함수 사용
```R
# install.packages('dplyr')
library(dplyr)   
apt_price <- left_join(apt_price, juso_geocoding, 
                       by = c("juso_jibun" = "apt_juso")) # 결합
apt_price <- na.omit(apt_price)   # 결측치 제거
```
### 지오 데이터프레임 만들기
> 3. 지오 데이터프레임 생성하기
- x, y 의 공간좌표 설정을 위해 coordinates() 함수 사용
- 좌표가 어떤 좌표계를 참조하는지 정의하기 위해 proj4string() 함수 사용
- 공간 데이터를 편리하게 다루기 위해 st_as_sf() 함수를 이용해 지오 데이터프레임으로 변환
```R
library(sp)    # install.packages('sp')
coordinates(apt_price) <- ~coord_x + coord_y    # 좌표값 할당
proj4string(apt_price) <- "+proj=longlat +datum=WGS84 +no_defs" # 좌표계(CRS) 정의
library(sf)    # install.packages('sf')
apt_price <- st_as_sf(apt_price)     # sp형 => sf형 변환
```
> 4. 지오 데이터프레임 시각화
- 데이터프레임 시각화를 위해 plot() 함수 사용
- 빈 캔버스를 그리기 위해 leaflet() 함수 사용
- 기본 지도인 오픈스트리트맵을 불러오기 위해 addTiles() 함수 사용
```R
plot(apt_price$geometry, axes = T, pch = 1)        # 플롯 그리기 
library(leaflet)   # install.packages('leaflet')   # 지도 그리기
leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(data=apt_price[1:1000,], label=~apt_nm) # 일부분(1000개)만 그리기
```
> 5. 지오 데이터프레임 저장하기
```R
dir.create("06_geodataframe")   # 새로운 폴더 생성
save(apt_price, file="./06_geodataframe/06_apt_price.rdata") # rdata 저장
write.csv(apt_price, "./06_geodataframe/06_apt_price.csv")   # csv 저장
```
## [10월 12일]
### 전처리 데이터 저장하기
> 1. 필요한 칼러만 추출하기
- select() 함수로 필요한 컬럼만 추출
```R
apt_price <- apt_price %>% select(ymd, ym, year, code, addr_1, apt_nm, 
              juso_jibun, price, con_year,  area, floor, py, cnt) # 칼럼 추출
head(apt_price, 2)  # 확인
```
> 2. 전처리 데이터 저장하기
```R
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
dir.create("./04_preprocess")   # 새로운 폴더 생성
save(apt_price, file = "./04_preprocess/04_preprocess.rdata") # 저장
write.csv(apt_price, "./04_preprocess/04_preprocess.csv") 
```
5장 - 카카오맵 API로 지오 코딩하기
### 지오 코딩 준비하기
> 3. 카카오 로컬 API 키 발급받기
- [Kakao Developers 접속](https://developers.kakao.com/)
- 로그인 후 내 애플리케이션 메뉴 클릭
- 애플리케이션 추가하기 메뉴 클릭
- 앱 이름 및 사업자명 입력 후 저장
- 발급된 REST API 키 확인
> 4. 중복된 주소 제거하기
- duplicated() 함수로 중복되는 주소 제거
```R
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
load( "./04_preprocess/04_preprocess.rdata")  # 실거래 불러오기          
apt_juso <- data.frame(apt_price$juso_jibun)  # 주소컬럼만 추출
apt_juso <- data.frame(apt_juso[!duplicated(apt_juso), ]) # unique 주소 추출 
head(apt_juso, 2)   # 추출결과 확인
```
### 주소를 좌표로 변환하는 지오 코딩
> 5. 지오 코딩 준비하기
- 앞에서 발급받은 REST API 키 입력
```R
add_list <- list()   # 빈 리스트 생성
cnt <- 0             # 반복문 카운팅 초기값 설정
kakao_key = ""       # 인증키
```
> 6. 지오 코딩하기
- httr 패키지: http로 자료 요청
- rjson 패키지: 응답 결과인 JSON 형 자료 처리
- data.table 패키지: 좌표를 테이블로 저장
- dplyr 패키지: 파이프라인 사용
- 서비스URL, 질의, 헤더 3가지 요소로 요청 URL 구성
- fromJSON() 함수로 JSON 데이터를 읽어 위도와 경도만 추출
```R
# install.packages('httr')
# install.packages('RJSONIO')
# install.packages('data.table')
# install.packages('dplyr')
library(httr)       
library(RJSONIO)    
library(data.table)  
library(dplyr) 

for(i in 1:nrow(apt_juso)){ 
  #---# 에러 예외처리 구문 시작
  tryCatch(
    {
      #---# 주소로 좌표값 요청
      lon_lat <- GET(url = 'https://dapi.kakao.com/v2/local/search/address.json',
                     query = list(query = apt_juso[i,]),
                     add_headers(Authorization = paste0("KakaoAK ", kakao_key)))
      #---# 위경도만 추출하여 저장
      coordxy <- lon_lat %>% content(as = 'text') %>% RJSONIO::fromJSON()
      #---# 반복횟수 카운팅
      cnt = cnt + 1
      #---# 주소, 경도, 위도 정보를 리스트로 저장
      add_list[[cnt]] <- data.table(apt_juso = apt_juso[i,], 
        coord_x = coordxy$documents[[1]]$x, 
        coord_y = coordxy$documents[[1]]$y)
      #---# 진행상황 알림 메시지
      message <- paste0("[", i,"/",nrow(apt_juso),"] 번째 (", 
       round(i/nrow(apt_juso)*100,2)," %) [", apt_juso[i,] ,"] 지오코딩 중입니다: 
       X= ", add_list[[cnt]]$coord_x, " / Y= ", add_list[[cnt]]$coord_y)
      cat(message, "\n\n")
      #---# 예외처리 구문 종료
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
  )
}
```
> 7. 지오 코딩 결과 저장하기
- rbindlist() 함수로 리스트형 자료를 데이터프레임형으로 변환 및 문자형 좌표를 숫자로 변환
```R
juso_geocoding <- rbindlist(add_list)   # 리스트를 데이터프레임 변환
juso_geocoding$coord_x <- as.numeric(juso_geocoding$coord_x) # 좌표값 숫자형 변환
juso_geocoding$coord_y <- as.numeric(juso_geocoding$coord_y)
juso_geocoding <- na.omit(juso_geocoding)   # 결측치 제거
dir.create("./05_geocoding")   # 새로운 폴더 생성
save(juso_geocoding, file="./05_geocoding/05_juso_geocoding.rdata") # 저장
write.csv(juso_geocoding, "./05_geocoding/05_juso_geocoding.csv")
```
6장 - 지오 데이터프레임 만들기
### 좌표계와 지오 데이터 포맷
- 좌표계: 타원체의 실제 좌푯값을 표현하기 위해 투영 과정을 거쳐 보정하는 기준
- EPSG: 좌표계를 표준화하고자 부여한 코드
- R의 데이터프레임은 기하학 특성의 위치 정보를 저장하기 적합한 포맷이 아니기 때문에 공간 분석에 한계
- 이러한 문제를 해결하기 위해 sp 패키지 공개
- 그러나, 데이터 일부를 편집하거나 수정하기 어렵다는 한계가 있어 이를 극복하고자 sf 패키지 공개됨
- sp 패키지는 기하학 정보를 처리할 때 유리
- sf 패키지는 부분적인 바이너리 정보 처리가 빠름
- 상황에 따라 sp 및 sf 패키지를 서로 변환하여 사용
## [10월 5일]
### 자료 정리: 자료 통합하기
> 1. 통합 데이터 저장하기
```R
dir.create("./03_integrated")   # 새로운 폴더 생성
save(apt_price, file = "./03_integrated/03_apt_price.rdata") # 저장
write.csv(apt_price, "./03_integrated/03_apt_price.csv")   
```
4장 - 전처리: 데이터를 알맞게 다듬기
### 불필요한 정보 지우기
> 2. 수집한 데이터 불러오기
```R
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
options(warn=-1) # 경고메시지 

load("./03_integrated/03_apt_price.rdata")  # 실거래 자료 불러오기
head(apt_price, 2)                          # 확인
```
> 3. 결측값과 공백 제거하기
- 결측값은 보통 NA(Not Available) 로 표현
- is.na() 함수와 table() 함수를 사용하여 NA가 몇 개 포함되었는지 확인 가능
- 결측값을 제거하기 위해 na.omit() 함수 사용
- 데이터 앞이나 뒤에 있는 빈데이터안 공백을 데이터 수집과정에서 필요없는 정보이므로 제거 필요
- 공백 제거를 위해 str_trim() 함수 사용
- 전체 데이터프레임 대상으로 공백을 제거할 때는 특정한 단위로 함수를 쉽게 호출해주는 apply() 함수를 사용
```R
table(is.na(apt_price))         # 현재 결측값 확인

apt_price <- na.omit(apt_price) # 결측값 제거
table(is.na(apt_price))         # 결측값 제거 확인

head(apt_price$price, 2)      # 현재 결측값 확인  

library(stringr)              # 문자열 처리 패키지 실행
apt_price <- as.data.frame(apply(apt_price, 2, str_trim)) # 공백 제거
head(apt_price$price, 2)                                  # 확인
```
### 항목별 데이터 다듬기
> 4. 매매 연월일 만들기
- 문자형 데이터는 계산할 수 없어 분석할 때 제약이 있으므로 날짜나 숫자처럼 계산할 수 있는 형태로 변환 필요
- 월 단위 날짜 형식(YYYY-DD)으로 변환을 위해 floor_date() 함수 사용
```R
# install.packages("lubridate") # 날짜형 데이터 패키지
library(lubridate)
# install.packages("dplyr") # 파이프라인 연산자(%>%) 사용을 위한 패키지
library(dplyr)      
apt_price <- apt_price %>% mutate(ymd=make_date(year, month, day))  # 연월일
apt_price$ym <- floor_date(apt_price$ymd, "month")                  # 연월
head(apt_price, 2)   
```
> 5. 매매가 변환하기
- sub() 함수를 이용해 쉼표 제거
- as.numeric() 함수로 문자를 숫자로 변환
```R
head(apt_price$price, 3) 

apt_price$price <- apt_price$price %>% sub(",","",.) %>% as.numeric() # 매매가 변환(문자 → 숫자)
head(apt_price$price, 3)  # 확인
```
> 6. 주소 조합하기
- gsub() 함수로 여는 괄호 "(" 부터 시작하는 문자 제거
- 코드에서 특수문자 사용을 위해 "\\" 기호 사용
- "." 은 이후 문자 의미
- "*" 은 모든 문자 의미
```R
head(apt_price$apt_nm, 30)  # 아파트 이름 현황

apt_price$apt_nm <- gsub("\\(.*","", apt_price$apt_nm) # 괄호이후 삭제
head(apt_price$apt_nm, 30)                             # 아파트 이름 확인

loc <- read.csv("./sigun_code.csv", fileEncoding="UTF-8")  # 지역코드 불러오기

apt_price <- merge(apt_price, loc, by = 'code')         # 지역명 결합하기
apt_price$juso_jibun <- paste0(apt_price$addr_2, " ", apt_price$dong," ",

                               apt_price$jibun," ",apt_price$apt_nm) # 주소조합
head(apt_price, 2)                                      # 확인
```
> 7. 건축연도, 전용면적 변환하기
- 문자형인 건축 연도를 계산할 수 있도록 전처리 작업 수행
- round() 함수는 숫자형 데이터의 소수점 이하를 반올림
- round(0) 으로 소수점 제거
```R
head(apt_price$con_year, 3)

apt_price$con_year <- apt_price$con_year %>% as.numeric()   # 건축연도 숫자변환
head(apt_price$con_year, 3)   # 건축연도 확인

head(apt_price$area, 3)   # 확인

apt_price$area <- apt_price$area %>% as.numeric() %>% round(0)  # 전용면적 숫자변환
head(apt_price$area, 3)          # 확인
```
> 8. 평당 매매가 만들기
```R
apt_price$py <- round(((apt_price$price/apt_price$area) * 3.3), 0) # 평당가격 계산
head(apt_price$py, 3)           # 확인
```
> 9. 층수 변환하기
- 절대값 함수 abs() 를 이용하여 모두 양수로 변환
```R
min(apt_price$floor)   # 확인

apt_price$floor <- apt_price$floor %>% as.numeric() %>% abs() # 층수 숫자변환
min(apt_price$floor)

apt_price$cnt <- 1   # 모든 거래 건수에 숫자 1 할당
head(apt_price, 2)   # 확인
```
## [09월 28일]
### 크롤러 제작
> 1. 자료 요청하고 응답받기
```R
for(i in 1:length(url_list)){   # 요청목록(url_list) 반복
  raw_data[[i]] <- xmlTreeParse(url_list[i], useInternalNodes = TRUE,encoding = "utf-8") # 결과 저장
  root_Node[[i]] <- xmlRoot(raw_data[[i]])	# xmlRoot로 추출
```
> 2. 전체 거래 건수 확인하기
```R
  items <- root_Node[[i]][[2]][['items']]  # 전체 거래내역(items) 추출
  size <- xmlSize(items)                   # 전체 거래 건수 확인  
```
> 3. 개별 거래 내역 추출하기
```R
  item <- list()  # 전체 거래내역(items) 저장 임시 리스트 생성
  item_temp_dt <- data.table()  # 세부 거래내역(item) 저장 임시 테이블 생성
  Sys.sleep(.1)  # 0.1초 멈춤
  for(m in 1:size){  # 전체 거래건수(size)만큼 반복
    #---# 세부 거래내역 분리   
    item_temp <- xmlSApply(items[[m]],xmlValue)
    item_temp_dt <- data.table(year = item_temp[4],     # 거래연도 
                               month = item_temp[7],    # 거래월
                               day = item_temp[8],      # 거래일
                               price = item_temp[1],    # 거래금액
                               code = item_temp[12],    # 지역코드
                               dong_nm = item_temp[5],  # 법정동
                               jibun = item_temp[11],   # 지번
                               con_year = item_temp[3], # 건축연도 
                               apt_nm = item_temp[6],   # 아파트 이름   
                               area = item_temp[9],     # 전용면적
                               floor = item_temp[13])   # 층수 
    item[[m]] <- item_temp_dt}    # 분리된 거래내역 순서대로 저장
  apt_bind <- rbindlist(item)     # 통합 저장
```
> 4. 응답 내역 저장하기
- 트래픽 제한(1000건)으로 컴파일시 25건만 접근하도록 코드 수정 필요
```R
datelist <- seq(from = as.Date('2021-01-01'), # 시작
                to   = as.Date('2021-04-30'), # 종료(수정) - 트래픽 초과 문제로 날짜 수정
                by    = '1 month')            # 단위
```
```R
  region_nm <- subset(loc, code== str_sub(url_list[i],115, 119))$addr_1 # 지역명 추출
  month <- str_sub(url_list[i],130, 135)   # 연월(YYYYMM) 추출
  path <- as.character(paste0("./02_raw_data/", region_nm, "_", month,".csv")) # 저장위치 설정
  write.csv(apt_bind, path)     # csv 파일로 저장
  msg <- paste0("[", i,"/",length(url_list), "] 수집한 데이터를 [", path,"]에 저장 합니다.") # 알림 메시지
  cat(msg, "\n\n")
```
### 자료 정리: 자료 통합하기
> 5. CSV 파일 통합하기
- 불필요하게 패키지 재설치를 하지 않도록 install.packages 는 주석처리
```R
setwd(dirname(rstudioapi::getSourceEditorContext()$path))  # 작업폴더 설정
files <- dir("./02_raw_data")    # 폴더 내 모든 파일 이름 읽기
# install.packages("plyr")
library(plyr)             
apt_price <- ldply(as.list(paste0("./02_raw_data/", files)), read.csv) # 모든 파일 하나로 결합
tail(apt_price, 2)  # 전처 거래 데이터를 몇 건 합쳤는지 확인
```
## [09월 21일]
### 요청 목록 생성
> 1. 요청 목록 만들기
```R
url_list <- list() # 빈 리스트 만들기
cnt <-0	           # 반복문의 제어 변수 초기값 0으로 설정
```
> 2. 요청 목록 채우기
- 요청목록: 프로토콜 + 주소 + 포트번호 + 리소스 경로 + 요청내역 등의 정보로 구성
```R
for(i in 1:nrow(loc)){           # 외부반복: 25개 자치구
  for(j in 1:length(datelist)){  # 내부반복: 12개월
    cnt <- cnt + 1               # 반복누적 카운팅
    #---# 요청 목록 채우기 (25*12=300)
    url_list[cnt] <- paste0("http://openapi.molit.go.kr:8081/OpenAPI_ToolInstallPackage/service/rest/RTMSOBJSvc/getRTMSDataSvcAptTrade?",
                            "LAWD_CD=", loc[i,1],         # 지역코드
                            "&DEAL_YMD=", datelist[j],    # 수집월
                            "&numOfRows=", 100,           # 한번에 가져올 최대 자료 수
                            "&serviceKey=", service_key)  # 인증키
  } 
  Sys.sleep(0.1)   # 0.1초간 멈춤
  msg <- paste0("[", i,"/",nrow(loc), "]  ", loc[i,3], " 의 크롤링 목록이 생성됨 => 총 [", cnt,"] 건") # 알림메시지
  cat(msg, "\n\n") 
}
```
- paste() & paste0()
```R
1. paste(): 나열된 원소 사이에 공백을 두고 결과값을 출력
- ex) paste(1,2,3,4)
      -> [1] "1 2 3 4"
1-1. sep: paste() 에 나열된 각각의 원소 사이에 옵션을 적용하여 구분, paste0() 와 결과값이 동일
1.2. collapse: 결과값이 두개 이상일 때, 각각의 결과값에 옵션을 주어 이어붙일 때 사용

2. paste0(): 나열된 원소 사이에 공백없이 출력
- ex) paste0('a','b','c','d','e')
      -> [1] "abcde"
```
> 3. 요청 목록 확인하기
```R
length(url_list)                # 요청목록 개수 확인
browseURL(paste0(url_list[1]))  # 정상작동 확인
```
### 크롤러 제작
> 4. 임시 저장 리스트 만들기
- install.packages 명령으로 각 패키지를 설치하고 library() 함수로 불러옴
- 불필요하게 패키지 재설치를 하지 않도록 install.packages 는 주석처리 
```R
# install.packages("XML")
# install.packages("data.table")
# install.packages("stringr")
library(XML)
library(data.table)
library(stringr)

raw_data <- list()        # xml 임시 저장소
root_Node <- list()       # 거래내역 추출 임시 저장소
total <- list()           # 거래내역 정리 임시 저장소
dir.create("02_raw_data") # 새로운 폴더 만들기
```
## [09월 14일]
> 1. Visual Studio 내 R 개발환경 구축
```
- Visual Studio 에서 R Extension 설치
- 설정->확장->R->Rpath: Windows 에 C:\Program Files\R\R-4.2.1\bin\x64 입력
```
3장 - 자료 수집: API 크롤러 만들기
### 크롤링 준비
> 2. 작업 폴더 설정하기
- [실습파일 다운로드](https://drive.google.com/file/d/10Cvmme8oxQ9upMMnPwn07V9MXenYjKeD/view)
```R
install.packages("rstudioapi")   # rstudioapi 설치                         
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) # 작업폴더 설정
getwd()   # 작업 폴더 확인
```
> 3. 수집 대상 지역 설정하기
```R
loc <- read.csv("./sigun_code.csv", fileEncoding="UTF-8")  #  지역코드
loc$code <- as.character(loc$code) # 행정구역명 문자 변환     
head(loc, 2) # 확인
```
> 4. 수집 기간 설정하기
```R
datelist <- seq(from = as.Date('2021-01-01'), # 시작
                to   = as.Date('2021-12-31'), # 종료
                by    = '1 month')            # 단위
datelist <- format(datelist, format = '%Y%m') # 형식변환(YYYY-MM-DD => YYYYMM) 
datelist[1:3]                                 # 확인
```
> 5. 인증키 입력하기
```R
service_key <- "공공테이터포털에서 발급받은 인증키 입력">  # 인증키 입력
```
## [09월 07일]
2장 - 자료 수집 전에 알아야 할 내용
> 1. [공공데이터포털 접속](https://www.data.go.kr)
- 국토교통부_아파트매매 실거래자료 검색(오픈 API탭) 및 활용신청
> 2. 요청메시지 확인
- http://openapi.molit.go.kr:8081/OpenAPI_ToolInstallPackage/service/rest/RTMSOBJSvc/getRTMSDataSvcAptTrade?LAWD_CD=11110&DEAL_YMD=201512&serviceKey=서비스키
> 3. 텍스트 마이닝
- 비정형 텍스트에서 의미있는 정보를 찾아내는 mining 기술
> 4. 워드 클라우드
- installed.packages() 를 통해 설치된 패키지 확인
- install.packages("wordcloud") 를 통해 wordcloud 패키지 설치
- source를 클릭하여 실행