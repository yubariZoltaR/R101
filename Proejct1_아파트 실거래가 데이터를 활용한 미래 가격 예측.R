setwd('C:/Users/im050/Desktop/R')
region_cd <- read.csv('region_cd.csv')

library(dplyr)
library(ggplot2)
library(data.table)
library(XML)
install.packages("showtext")
library(showtext)
font_add_google('Noto Sans KR', 'notosanskr')
library(gridExtra)
install.packages("TTR")
library(TTR)

LAWD_CD <- region_cd %>% filter(region %like% "서울특별시")
LAWD_CD <- LAWD_CD[,2]

temp <- merge(c(2010:2020), c(1:12))
temp$y <- if_else(temp$y<10, paste0(0,temp$y), as.character(temp$y))
DEAL_YMD <- paste0(temp$x, temp$y) %>% as.integer()

df <- NULL
API_URL <- "http://openapi.molit.go.kr/OpenAPI_ToolInstallPackage/service/rest/RTMSOBJSvc/getRTMSDataSvcAptTradeDev?serviceKey="

API_KEY <- "FfGshzVRwyofy3cMrqANdiOTPtIHDN%2B6NpQ3WzYUw2ywjOwkd2OpSU6UtTPA5esi29Uz%2FMPZVePB11uLZ7pEOg%3D%3D"
NUM_OF_ROWS <- 1000

for (i in LAWD_CD) {
  print(i)
  for (l in DEAL_YMD) {
    url <- paste0(API_URL,API_KEY,"&pageNo=",1,"&numOfRows=",NUM_OF_ROWS,"&LAWD_CD=",i,"&DEAL_YMD=",l)
    raw.data <- xmlTreeParse(url, useInternalNodes = TRUE, encoding = "utf-8")
    xml_rootNode <- try(xmlRoot(raw.data))
    xml_result <- xmlToDataFrame(xml_rootNode[[2]][['items']])
    df <- rbind(df, xml_result)
  }
}
rawdf <- df
usedf <- rawdf
colnames(usedf) = c("거래금액","건축년도","년","도로명","도로명건물본번호코드","도로명건물부번호코드","도로명시군구코드","도로명일련번호코드","도로명지상지하코드","도로명코드","법정동","법정동본번코드","법정동부번코드","법정동시군구코드","법정동읍면동코드","법정동지번코드","아파트","월","일","일련번호","전용면적","지번","지역코드","층","해제사유발생일","해제여부")
View(usedf)
str(usedf)

usedf$년 <- as.integer(usedf$년)
usedf$월 <- as.integer(usedf$월)
usedf$거래금액 <- gsub(',','',usedf$거래금액) %>% as.integer()
usedf$법정동 <- gsub(' ','', usedf$법정동)
usedf$전용면적 <- as.integer(usedf$전용면적)


usedf$qrt <- ifelse(usedf$월 < 4, 'Q1', ifelse(usedf$월 < 7, 'Q2', ifelse(usedf$월 < 10, 'Q3', 'Q4')))
usedf$yyyyqrt <- paste0(usedf$년, usedf$qrt)
usedf$평수 <- round(usedf$전용면적/3.3)
usedf$평단가 <- usedf$거래금액/usedf$평수
usedf.1 <- usedf %>% group_by(yyyyqrt) %>% summarise(평균평단가 = mean(평단가))

theme_set(theme_grey(base_family = 'notosanskr'))
ggplot(usedf.1, aes(x=yyyyqrt, y=평균평단가, group=1)) +
  geom_line() + xlab('년도/분기') + ylab('평균 가격(만원)') +
  ggtitle("서울 아파트 평당 가격 변화 추이") +
  theme(axis.text.x=element_text(angle=90, size=5, family = "notosanskr")) +
  stat_smooth(method='lm') +
  ylim(0, max(usedf.1$평균평단가))

#Hypothesis 1 : 서울전체아파트와 강남 아파트의 가격은 변화 양상이 다를 것이다. 

dong <- '반포동'
usedf.2 <- usedf %>% filter(법정동== dong)
usedf.2 <- usedf.2 %>% group_by(yyyyqrt) %>% summarise(평균평단가 = mean(평단가))
usedf.2.plot <- ggplot(usedf.2, aes(x=yyyyqrt, y=평균평단가, group=1))+
  geom_line() + xlab('년도/분기') + ylab('평균 가격(만원)') +
  ggtitle(paste0(dong," 아파트 평당 가격 변화 추이")) +
  theme(axis.text.x=element_text(angle=90, size=5)) + stat_smooth(method='lm') +
  ylim(0, max(usedf.2$평균평단가))

dong <- '서초동'
usedf.3 <- usedf %>% filter(법정동== dong)
usedf.3 <- usedf.3 %>% group_by(yyyyqrt) %>% summarise(평균평단가 = mean(평단가))
usedf.3.plot <- ggplot(usedf.3, aes(x=yyyyqrt, y=평균평단가, group=1))+
  geom_line() + xlab('년도/분기') + ylab('평균 가격(만원)') +
  ggtitle(paste0(dong," 아파트 평당 가격 변화 추이")) +
  theme(axis.text.x=element_text(angle=90, size=5)) + stat_smooth(method='lm') +
  ylim(0, max(usedf.3$평균평단가))

dong <- '삼성동'
usedf.4 <- usedf %>% filter(법정동== dong)
usedf.4 <- usedf.4 %>% group_by(yyyyqrt) %>% summarise(평균평단가 = mean(평단가))
usedf.4.plot <- ggplot(usedf.4, aes(x=yyyyqrt, y=평균평단가, group=1))+
  geom_line() + xlab('년도/분기') + ylab('평균 가격(만원)') +
  ggtitle(paste0(dong," 아파트 평당 가격 변화 추이")) +
  theme(axis.text.x=element_text(angle=90, size=5)) + stat_smooth(method='lm') +
  ylim(0, max(usedf.4$평균평단가))

dong <- '압구정동'
usedf.5 <- usedf %>% filter(법정동== dong)
usedf.5 <- usedf.5 %>% group_by(yyyyqrt) %>% summarise(평균평단가 = mean(평단가))

usedf.5.plot <- ggplot(usedf.5, aes(x=yyyyqrt, y=평균평단가, group=1))+
  geom_line() + xlab('년도/분기') + ylab('평균 가격(만원)') +
  ggtitle(paste0(dong," 아파트 평당 가격 변화 추이")) +
  theme(axis.text.x=element_text(angle=90, size=5)) + stat_smooth(method='lm') +
  ylim(0, max(usedf.5$평균평단가))

grid.arrange(usedf.2.plot, usedf.3.plot, usedf.4.plot, usedf.5.plot, nrow=2, ncol=2)

#Hypothesis 2 : 강남 아파트의 가격 변화를 따라서 후행적으로 가격이 변화하는 지역이 있을 것이다. 
# 가정 : 2020년 가격 하락세는 일시적 현상이며 이전 수준을 회복할 것이다. -> 2020년 이전 데이터로 탐색 & 추세가 비슷한 것을 찾기

usedf.fil <- usedf %>% filter(!yyyyqrt %in% c('2020Q1','2020Q2','2020Q3','2020Q4'))
View(usedf.fil)
dong.list <- unique(usedf$법정동)
usedf.ts <- NULL


for (i in dong.list) {
  print(i)
  try(temp <- merge(i, usedf.fil %>% filter(법정동 == i) %>%
                  group_by(yyyyqrt) %>%
                  summarise(평균평단가 = mean(평단가)) %>%
                  mutate(ma3 = runMean(평균평단가, 3))))
  usedf.ts <- rbind(usedf.ts, temp) %>% na.omit()
}

dong.list <- usedf.ts %>% group_by(x) %>% 
  summarise(cnt = n_distinct(yyyyqrt)) %>% 
  filter (cnt == 38) %>% select(x)
usedf.ts.banpo <- usedf.ts %>% filter(x == '반포동')
usedf.ts <- usedf.ts %>% filter(x %in% dong.list$x, x != '반포동')

usedf.trend <- list()
for (i in dong.list$x) {
  print(i)
  temp <- usedf.ts %>% filter(x == i) %>% select(ma3) %>% as.matrix()
  usedf.trend[[i]] <- temp[,1]
}
usedf.trend <- as.data.frame(do.call(cbind,usedf.trend))

usedf.trend.banpo <- usedf.ts.banpo[-38,] %>% select(ma3) #반포동만 마지막분기 제거
colnames(usedf.trend.banpo) <- '반포동'

usedf.trend <- usedf.trend[-1,] #전체 동 데이터에서 첫번재 데이터를 빼줌
usedf.trend <- cbind(usedf.trend, usedf.trend.banpo)

usedf.cor <- cor(usedf.trend)

hc <- hclust(as.dist(usedf.cor), method = 'ward.D2')
plot(hc, hang = -1, cex = 0.35)

count(dong.list)
for (i in 1:203){
  print(i)
  if(rownames(usedf.cor)[i] == "반포동"){
    break
  } 
}

rownames(usedf.cor)[203]
cor.banpo <- usedf.cor[203,1:203] %>% as.data.frame()
View(cor.banpo)

cor.banpo <- arrange(cor.banpo,desc(.))
View(cor.banpo)
#rownames(cor.banpo)[2]

 
par(mfrow=(c(2,3)))
plot(usedf.trend$반포동, type = 'l')
plot(usedf.trend$양평동5가, type = 'l')
plot(usedf.trend$사당동, type = 'l')
plot(usedf.trend$문래동6가, type = 'l')
plot(usedf.trend$가락동, type = 'l') #가락동과 가산동은 유사도가 떨어지는 데이터임. 
plot(usedf.trend$가산동, type = 'l')

#반포동과 유사한 패턴의 지역 미래 가격 예측. 반포동은 급격하게 하락했다가 회복. 다른 지역 역시 2020년 초 거래 가격 수준으로 회복할 것으로 예측. 위 예측을 바탕으로 추가 자료 조사를 통해 투자 지역 선정

# 특징적인 서울 아파트 가격흐름의 파악. 군집이 어떻게 묶이는지 확인, 군집별 가격흐름파악
# cf)https://rpubs.com/doris/734561
