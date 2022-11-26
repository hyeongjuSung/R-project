#----------------------------------
# 10-1 기초: 인터렉티브 지도 만들기
#----------------------------------

#---# [1단계: 데이터 불러오기]
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
load("./06_geodataframe/06_apt_price.rdata")      # 아파트 실거래 데이터
library(sf)
bnd <- st_read("./01_code/sigun_bnd/seoul.shp")   # 서울시 경계선
load("./07_map/07_kde_high.rdata")    # 최고가 래스터 이미지
load("./07_map/07_kde_hot.rdata")     # 급등지역 래스터 이미지
grid <- st_read("./01_code/sigun_grid/seoul.shp")   # 서울시 그리드

#---# [2단계: 마커클러스터링 설정]
pcnt_10 <-as.numeric(quantile(apt_price$py, probs=seq(.1,.9,by=.1))[1])   # 하위10%
pcnt_90 <-as.numeric(quantile(apt_price$py, probs=seq(.1,.9,by=.1))[9])   # 상위10%
load("./01_code/circle_marker/circle_marker.rdata")   # 마커 클러스터링 함수
circle.colors <- sample(x=c("red","green","blue"), size=1000, replace=TRUE)

#---# [3단계: 반응형 지도 만들기]
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


#-------------------------------
# 10-2 지도 애플리케이션 만들기
#------------------------------

#---# [1단계: 그리드 필터링]
grid <- st_read("./01_code/sigun_grid/seoul.shp")       # 그리드 불러오기
grid <- as(grid, "Spatial") ; grid <- as(grid, "sfc")   # 변환
grid <- grid[which(sapply(st_contains(st_sf(grid),apt_price),length) > 0)]   # 필터링
plot(grid)   # 그리드 확인

#---# [2단계: 반응형 지도 모듈화]
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

