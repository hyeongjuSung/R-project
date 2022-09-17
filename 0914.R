#------------------------------------
# 03-1 크롤링 준비
#------------------------------------  

#---# [1단계: 작업폴더 설정하기]
install.packages("rstudioapi")   # rstudioapi 설치                         
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