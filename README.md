R-project 성형주
=============
2022-2 실무프로젝트 수업 내용 정리
-------------
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