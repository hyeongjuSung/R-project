R-project 성형주
=============
2022-2 실무프로젝트 수업 내용 정리
-------------
## [09월 14일]
> 1. Visual Studio 내 R 개발환경 구축
```
- Visual Studio 에서 R Extension 설치
- 설정->확장->R->Rpath: Windows 에 C:\Program Files\R\R-4.2.1\bin\x64 입력
```
크롤링 준비
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