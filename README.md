R-project 성형주
=============
2022-2 실무프로젝트 수업 내용 정리
-------------
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
3. 요청 목록 확인하기
```R
length(url_list)                # 요청목록 개수 확인
browseURL(paste0(url_list[1]))  # 정상작동 확인
```
### 크롤러 제작
4. 임시 저장 리스트 만들기
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