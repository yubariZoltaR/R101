##  Hypothesis Testing

분할표 -> chi-square test   
아니면서 그룹개수>2 -> ANOVA   
아니면서 데이터개수>30 -> z-test   
아니면서 대응표본 -> 대응표본 t-test   
아님 -> t-test

#### 1\. t-test
H0 : groupA and groupB have same mean in height.
H1 : mean(groupA) < mean(groupB)


``` r
raw01 <- read.csv("test01.csv", header = TRUE) #sample size : 3 
unique(raw01$group) #그룹이름 확인
groupA <- raw01[raw01$group=='A',1:2]
groupB <- raw01[raw01$group=='B',1:2]
mean(groupA[,2])
mean(groupB[,2])

shapiro.test(groupA[,2]) #데이터의 정규성검정 Null Hypothesis : Dataset follows Normal Distribution
qqnorm(groupA[,2])
qqline(groupA[,2]) 
#as a result p-value=1 : cannot reject Null Hypothesis

var.test(groupA[,2], groupB[,2]) #분산 동질성검정 Null Hypothesis : Variance of both group is identical
#as a result p-value=0.5xx : cannot reject Null Hypothesis

t.test(groupA[,2],groupB[,2], alternative="less", var.equal = TRUE) #alternative : Alternative Hypothesis : groupA < groupB (less) / var.equal : identical variance
#as a result p-value=0.1x : cannot reject Null Hypothesis, but when it comes to 10 sample size, Null Hypothesis is rejected.
```

#### 2\. 대응표본 t-test
주어진 csv데이터는 before, after로 나뉘어진 데이터
H0 : sales stayed even though there was marketing project
H1 : sales increased
``` r
raw02 <- read.csv("test02.csv", header = TRUE) 
groupAA <- raw02[,1]
groupBB <- raw02[,2]

d = groupAA - groupBB
shapiro.test(d) #데이터의 정규성검정
qqnorm(d)
qqline(d)

t.test(groupAA, groupBB, alternative="less", paired = TRUE)
```

#### 3\. z-test (data>30)
H0 : groupA and groupB have same mean in height.
H1 : mean(groupA) < mean(groupB)

참조) z-test function
``` r
z.test <- function(x1, x2){
n_x1 = length(x1)
n_x2 = length(x2)
mean_x1 = mean(x1)
mean_x2 = mean(x2)
cat("\n")
cat("\tTwo Sample z-test\n")
cat("\n")
cat("mean of x1:", mean_x1, "\n")
cat("mean of x2:", mean_x2, "\n")
var_x1 = var(x1)
var_x2 = var(x2)
z = (mean_x1 - mean_x2)/sqrt((var_x1/n_x1)+(var_x2/n_x2))
abs_z = abs(z)
cat("z =", abs_z, "\n")
p_value = 1-pnorm(abs_z)
cat("p-value =", p_value)
```

test

``` r
raw03 <- read.csv("test03.csv", header= TRUE)
groupz1 <- raw03[raw03$group=='A',1:2]
groupz2 <- raw03[raw03$group=='B',1:2]
mean(groupz1[,2])
mean(groupz2[,2])

var.test(groupz1[,2], groupz2[,2])

z.test(groupz1[,2], groupz2[,2])
```

#### 4\. ANOVA test (group >2)
f통계량 = (집단간오차 / (집단개수-1)) / (집단내오차 / (전체데이터수 - 집단개수))
``` r
library(lawstat)
raw04 <- read.csv("test04.csv", header= TRUE)
groupf1 <- raw04[raw04$group=='A',1:2]
groupf2 <- raw04[raw04$group=='B",1:2]
groupf3 <- raw04[raw04$group=='C',1:2]
mean(groupf1[,2])
mean(groupf2[,2])
mean(groupf3[,2])

shapiro.test(groupf1[,2]) #groupf2,groupf3에 대해서도 반복
qqnorm(groupf1[,2])
qqline(groupf1[,2])

levene.test(raw04$height, raw04$group)
bartlett.test(height~group, data=raw04)

rawAnova <- aov(height~group, data=raw04)
summary(rawAnova)
```

#### 5\. chi-square 
카이제곱통계량 = (관측값 - 기대값)^2 / 기대값
![image](https://user-images.githubusercontent.com/80673078/111430768-301f1e80-873e-11eb-924e-a41d7229340a.png)
``` r
raw05 <- read.csv("test05.csv", header = TRUE)
rawTable <- table(raw05)
chisq.test(rawTable, correct=FALSE) # 셀 기대도수 >5이면 FALSE, 5보다 작으면 TRUE
```



