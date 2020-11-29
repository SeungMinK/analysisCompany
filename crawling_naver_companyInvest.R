#install.packages("stringr")
#install.packages("dplyr")
#install.packages('tidyverse')
#install.packages("")
#install.packages("shiny")


library(tqk)
library(tidyverse) # 8개의 package가 있음
library(httr)
library(rvest)
library(jsonlite)
library(stringr)
library(dplyr)




results <- data.frame() #매출액 저장하기 위한 데이터 프레임
results_2 <- data.frame() # 부채비율 저장하기 위한 데이터 프레임

i<-1

n<-nrow(x=codes)


for(i in 1:n){

  cat(str_glue('현재 {i}번째 회사 정보 수집중 !'),'\n')
  res<-GET(url='https://finance.naver.com/item/main.nhn',
           query = list('code'=codes$code[i]))

  tryCatch(expr={
  tbl <- res %>%
    read_html(encoding = 'EUC-KR')%>%
    html_node(css ='#content > div.section.cop_analysis > div.sub_section >
            table')%>%
    html_table(fill = TRUE)

  colnames(x=tbl)[1:1] <- str_c('total')
  colnames(x=tbl)[2:5] <- str_c('실적',2017:2020)

  df2<-
    tbl %>%
    select(1:5)%>%
    slice(c(3))%>%
    mutate(회사 = codes$name[i])

  results <-rbind(results,df2)

  Sys.sleep(time = 0.5L)
  }, error = function(e) cat('---> 에러 발생!\n'))
}

results$실적2017 <- str_remove(string = results$실적2017,pattern = ',')# , 지우기
results$실적2018 <- str_remove(string = results$실적2018,pattern = ',')# , 지우기
results$실적2019 <- str_remove(string = results$실적2019,pattern = ',')# , 지우기
results$실적2020 <- str_remove(string = results$실적2020,pattern = ',')# , 지우기


results$실적2017 <- as.integer(x = results$실적2017)# 정수로 바꾸기
results$실적2018 <- as.integer(x = results$실적2018)# 정수로 바꾸기
results$실적2019 <- as.integer(x = results$실적2019)# 정수로 바꾸기
results$실적2020 <- as.integer(x = results$실적2020)# 정수로 바꾸기

#dplyr
results <- results %>% select(-total) # 쉬프트+컨트롤+m = 파이프라인 단축키

#
long <-  results %>%
  gather(key = year, value = sales, -회사) %>%
  arrange(회사)

#rm(wide) # wide 데이터 파일 지우기



#slice(9) 부채비율 찾기
i<-1
for(i in 1:n){

  cat(str_glue('현재 {i}번째 회사 정보 수집중 !'),'\n')
  res<-GET(url='https://finance.naver.com/item/main.nhn',
           query = list('code'=codes$code[i]))

  tryCatch(expr={
    tbl <- res %>%
      read_html(encoding = 'EUC-KR')%>%
      html_node(css ='#content > div.section.cop_analysis > div.sub_section >
            table')%>%
      html_table(fill = TRUE)

    colnames(x=tbl)[1:1] <- str_c('total')
    colnames(x=tbl)[2:5] <- str_c('실적',2017:2020)

    df<-
      tbl %>%
      select(1:5)%>%
      slice(c(9))%>% #부채 비율 debt
      mutate(회사 = codes$name[i])

    results_2 <-rbind(results_2,df)

    Sys.sleep(time = 0.5L)
  }, error = function(e) cat('---> 에러 발생!\n'))
}


results_2$실적2017 <- str_remove(string = results_2$실적2017,pattern = ',')# , 지우기
results_2$실적2018 <- str_remove(string = results_2$실적2018,pattern = ',')# , 지우기
results_2$실적2019 <- str_remove(string = results_2$실적2019,pattern = ',')# , 지우기
results_2$실적2020 <- str_remove(string = results_2$실적2020,pattern = ',')# , 지우기


results_2$실적2017 <- as.integer(x = results_2$실적2017)# 정수로 바꾸기
results_2$실적2018 <- as.integer(x = results_2$실적2018)# 정수로 바꾸기
results_2$실적2019 <- as.integer(x = results_2$실적2019)# 정수로 바꾸기
results_2$실적2020 <- as.integer(x = results_2$실적2020)# 정수로 바꾸기

#dplyr
results_2 <- results_2 %>% select(-total) # 쉬프트+컨트롤+m = 파이프라인 단축키

#
long_2 <-  results_2 %>%
  gather(key = year, value = debt, -회사) %>%
  arrange(회사)




long %>%
  filter(회사 == '고바이오랩') %>%
  ggplot(aes(x = year, y = sales , fill = year)) +
  geom_col() +
  ggtitle("[매출액]")+
  theme_bw()

long_2 %>%
  filter(회사 == '고바이오랩') %>%
  ggplot(aes(x = year, y = debt , fill = year)) +
  geom_col() +
  ggtitle("[부채]")+
  theme_bw()

saveRDS(object = results, file = 'results_1128_1.RDS')#데이터 파일 저장하기
saveRDS(object = results_2, file = 'results_1128_2.RDS')#데이터 파일 저장하기
saveRDS(object = long, file = 'long_1128_1.RDS')#데이터 파일 저장하기
saveRDS(object = long_2, file = 'long_1128_2.RDS')#데이터 파일 저장하기

