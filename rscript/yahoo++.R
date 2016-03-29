##yahoo
library(rvest)
library(plyr)
library(XML)
library(RCurl)
library(SnowballC)
library(cluster)   
library(XML)
library(RCurl)

links_data_yahoo = c()
for(i in 1:200){
  url <- paste('https://tw.answers.yahoo.com/search/search_result?p=%E7%A7%91%E5%A4%A7&s=', i, '&sort=new',sep='')
  title_css = read_html(url) %>% html_nodes("a") %>% html_attr('href')
  title_css = title_css[which(grepl('question',title_css) & !grepl('login',title_css))]
  links_data_yahoo = c(links_data_yahoo,title_css)
  gc() #�O��������
  print(paste0('yahoo��',i,'��'))
  Sys.sleep(runif(1,2,5))
}

temp_yahoo_data = {}
##�N��X�����}�i�檦��
for(i in 1:length(links_data_yahoo)){
  url = paste0('https://tw.answers.yahoo.com',links_data_yahoo[i])
  title_css = read_html(url) %>% html_nodes("span") %>% html_text()
  temp <- iconv(title_css,'utf8')
  
  temp_yahoo_data = c(temp_yahoo_data,temp)
  ##which contains ���I
  gc() #�O��������
  print(paste0('yahoo��',i,'��  ',i/length(links_data_yahoo)*100,'%'))
  Sys.sleep(runif(1,2,5))
}