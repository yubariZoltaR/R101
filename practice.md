##  R 처음

일단 막 따라하기

#### 1\. 회사별 평균 연비 높은순 정렬

dplyr 패키지의 mpg 데이터를 활용

``` r
install.packages("dplyr")
install.packages(""ggplot2")
library(dplyr)
library(ggplot2)
mpg %>% 
  group_by(manufacturer) %>% 
  summarise(mean.hwy=mean(hwy)) %>% 
  arrange(desc(mean.hwy))
```
