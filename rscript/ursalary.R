rm(list = ls()) #去除工作空間中所有物件
gc() #記憶體釋放
#path<-"C:\\Documents and Settings\\wjhong\\桌面\\網路爬蟲"
setwd(path)
start.time<-Sys.time()

library(rvest)
library(plyr)

result_export_all = data.frame('職務代碼'=numeric(),'職務名稱'=character(),'1年以下'=character(),'1~3年'=character(),'3~5年'=character(),'5~7年'=character(),'7年以上'=character(),'工作內容'=character(),'相關專業證照'=character(),'未來職涯發展'=character(),stringsAsFactors=F)
##利用列表取得各文章id
temp_link = c()
for(x in 1:10){
  url <- paste('http://www.ursalary.com/search.php?&p=',x,sep='')
  
  #result_export = data.frame('職務代碼'=numeric(),'職務名稱'=character(),'1年以下'=character(),'1~3年'=character(),'3~5年'=character(),'5~7年'=character(),'7年以上'=character(),'工作內容'=character(),'相關專業證照'=character(),'未來職涯發展'=character(),stringsAsFactors=F)
  
  title_css = read_html(url) %>% html_nodes("a") %>% html_attr("href")
  title_css = unique(title_css)
  title_css = title_css[which(grepl('detail',title_css))]
  title_css = paste0('http://www.ursalary.com/',title_css)
  temp_link = c(temp_link, title_css)
  sleep_time = runif(1,2,5)
  print(paste0('第',x,'頁列表link抓取完成'))
  print(paste0('loading... 暫停',sleep_time,'秒'))
  Sys.sleep(sleep_time)
  gc() #記憶體釋放
}
##使用抓出來的link作爬蟲
for(i in 1:length(temp_link)){
  url <- temp_link[i]
  title_css = read_html(url) %>% html_nodes("ul li") %>% html_text()
  utf8_text_title <- toUTF8(title_css) ## 將捉下來的標題轉成 UTF8
}


write.csv(result_NT,'爬蟲.csv',row.names=F)
