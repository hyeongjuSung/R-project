#------------------------------------
# 03-1 크롤링 준비
#------------------------------------  

#---# [1단계: 작업폴더 설정하기]
# install.packages("rstudioapi")   # rstudioapi 설치                         
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) # 작업폴더 설정
getwd()   # 확인

#---# [2단계: 수집 대상지역 설정]
loc <- read.csv("./sigun_code.csv", fileEncoding="UTF-8")  #  지역코드
loc$code <- as.character(loc$code) # 행정구역명 문자 변환     
head(loc, 2) # 확인

#---# [3단계: 수집 기간 설정]
datelist <- seq(from = as.Date('2021-01-01'), # 시작
                to   = as.Date('2021-12-31'), # 종료
                by    = '1 month')            # 단위
datelist <- format(datelist, format = '%Y%m') # 형식변환(YYYY-MM-DD => YYYYMM) 
datelist[1:3]                                 # 확인

#---# [4단계: 인증키 입력하기]
service_key <- ""  # 인증키 입력

#------------------------------------
# 3-2 요청목록 생성
#------------------------------------  

#---# [1단계: 요청목록 만들기]
url_list <- list() # 빈 리스트 만들기
cnt <-0	           # 반복문의 제어 변수 초기값 설정

#---# [2단계: 요청목록 채우기]
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

#---# [3단계: 요청 목록 동작 확인]
length(url_list)                # 요청목록 개수 확인
browseURL(paste0(url_list[1]))  # 정상작동 확인


#----------------
# 3-3 크롤링 실행
#----------------  

#---# [1단계: 임시 저장 리스트 생성]
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
