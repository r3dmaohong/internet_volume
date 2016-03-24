rm(list = ls()) #去除工作空間中所有物件
gc() #記憶體釋放
path<-"C:\\Documents and Settings\\wjhong\\桌面\\網路爬蟲"
setwd(path)
start.time<-Sys.time()

library(rvest)
library(tmcn)
library(plyr)


temp = read.csv(file.choose(),stringsAsFactors=F)
temp_url = temp[,2]
temp$連結結果 = ''
#result_export_all = data.frame('職務代碼'=numeric(),'職務名稱'=character(),'1年以下'=character(),'1~3年'=character(),'3~5年'=character(),'5~7年'=character(),'7年以上'=character(),'工作內容'=character(),'相關專業證照'=character(),'未來職涯發展'=character(),stringsAsFactors=F)

for(x in 1:length(temp_url)){
  url <- temp_url[x]
  
  #result_export = data.frame('職務代碼'=numeric(),'職務名稱'=character(),'1年以下'=character(),'1~3年'=character(),'3~5年'=character(),'5~7年'=character(),'7年以上'=character(),'工作內容'=character(),'相關專業證照'=character(),'未來職涯發展'=character(),stringsAsFactors=F)
  
  ##span class="mobileShow"
  title_css = read_html(url) %>% html_nodes("td") %>% html_text()
  utf8_text_title <- toUTF8(title_css ) ## 將捉下來的標題轉成 UTF8
  ##.listContent
  temp$連結結果[x] = utf8_text_title[1]
  gc() #記憶體釋放
  print(utf8_text_title[1])
  sec = runif(1,2,3)
  print(paste0('loading... 稍等',sec,'秒'))
  Sys.sleep(sec)
}
