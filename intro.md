##  R 처음

일단 막 따라하기

#### 1\. 회사별 평균 연비 높은순 정렬

dplyr 패키지의 mpg 데이터를 활용

``` r
install.packages("dplyr")
install.packages(""ggplot2")
library(dplyr)
library(ggplot2)
head(mpg)
mean(mpg$hwy)
max(mpg$hwy)
hist(mpg$hwy)

mpg %>% 
  group_by(manufacturer) %>%  # 제조사 별로 분리
  summarise(mean.hwy=mean(hwy)) %>% # hwy 평균 산출
  arrange(desc(mean.hwy))
```

#### 2\. 회사별 평균 연비 높은순 정렬

``` r
mpg %>% 
  filter(manufacturer=="ford") %>% 
   group_by(model) %>% 
   arrange(desc(hwy))
```

#### 3\. 단순회귀분석 & 그래프 제작
배기량과 고속도로연비와의 상관관계

``` r
lm.mpg <- lm(data=mpg, hwy ~ displ)
summary(lm.mpg)
qplot(data = mpg, x = displ, y = hwy) 
qplot(data = mpg, x = hwy, y = drv, geom = "boxplot", colour = drv)  # geom=은 그래프형식, colour은 drv별로 색깔구분
```

#### 4\. 변수와 함수

``` r
a <- 1
b <- 2
c <- c(1,2,3)
d <- c(1:10)
e <- seq(1,10)
f <- seq(1,10, by=2) # 간격 2씩 늘어나면서, 1,3,5,7,9
g <- "hi"
g
h <- c("이","것","은")
mean(a)
max(a)
paste(h, collapse = " ") #빈칸으로 문자 이어붙이기(값은 저장안됨)
h <- paste(h, collapse = ",") #"이,것,은" 을 h에 저장
```

#### 5\. 데이터

``` r
micro <- c(91,93,81,39) #미시점수
macro <- c(48,68,18,100) #거시점수
df_midterm <- data.frame(micro, macro)
df_midterm
class <- c(1,1,2,2) #반 추가
df_midterm <- data.frame(micro, macro, class)
mean(df_midterm$micro) #모든 미시성적 평균 출력
```
#### 6\. 외부데이터

``` r
install.packages("readxl")
library(readxl)
df_finalexam <- read_excel("C:/Users/im050/Desktop/R/finalexam.xlsx",sheet=1, col_names = T) #col_names는 첫번째줄에 변수명이 있는가, 아닐경우 F
data1 <- read.csv("C:/Users/im050/Desktop/R/csv_exam.csv", header = T) #자체 내장함수 사용 
write.csv(df_finalexam, file = "csv_finalexam.csv") #csv로 저장하기
```
#### 7\. 데이터파악하기, 수정하기

``` r
head(data1) # 앞에서 6행
head(data1,10) #앞에서 10행
tail(data1) # 뒤에서 6행
View(data1) # 뷰어창에서 확인하기
dim(data1) # 행,열 개수
str(data1) # 데이터 속성 확인
summary(data1) # 요약통계

mpg <- as.data.frame(ggplot2::mpg) #mpg 데이터를 dataframe형태로
mpg <- rename(mpg, kousoku = hwy) # mpg데이터의 hwy변수를 kousoku로 수정 (새 변수명 = 원래 변수명)
mpg$ratio <- mpg$cty/mpg$hwy*100 # cty, hwy변수를 활용해 파생변수 ratio를 생성
mpg$group <- ifelse(mpg$ratio > 0.8, "A", "B") #ifelse활용해서 파생변수 group 생성
table(mpg$group) # 빈도 확인 
qplot(mpg$group)
```
#### 8\. 데이터가공하기
exam 데이터활용 (데이터에는 id, class, micro, macro, stats 변수가 존재한다)
``` r
exam %>% filter(class == 1) #filter 함수는 행을 추출한다. 1반인 학생만 출력
exam %>% filter(class != 1) #1반이 아닌경우를 출력
exam %>% filter(micro > 50) # 미시 점수가 50점 초과
exam %>% filter(class == 2 & macro >= 50) # 2반 and 거시 50점 이상
exam %>% filter(micro <= 20 | macro <= 20) # 미시가 20점 이하 or 거시가 20점 이하
groupA <- exam %>% filter(class %in% c(1,3,5)) # 1,3,5반 추출해서 groupA에 할당
groupB <- mpg %>% filter(manufacturer %in% c("chevrolet", "ford", "honda")) #mpg데이터에서 제조사가 다음에 해당하는 것 추출 후 할당

exam %>% select(class, micro, stats) # 필요한 변수만 추출
exam %>% select(-macro) # 특정 변수 제외

exam %>% arrange(macro) # 거시기준 오름차순 정렬
exam %>% arrange(desc(micro)) # 미시기준 내림차순 정렬
exam %>% arrange(class, stats) #정렬기준 여러가지
exam %>% mutate(total = micro + macro + stats) #총합 파생변수 추가














