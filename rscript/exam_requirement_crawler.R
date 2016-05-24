rm(list = ls()) #去除工作空間中所有物件
gc() #記憶體釋放
#path<-"D:\\abc\\wjhong\\projects\\school_performence_analysis"
#setwd(path)

library(httr)
library(rvest)
library(tmcn)

options(java.parameters = "-Xmx4000m")
jgc <- function()
{
  .jcall("java/lang/System", method = "gc")
}    
##getting urls
all_links = c()
for(i in 1:19){
  if(i<10){
    url = paste0('http://campus4.ncku.edu.tw/uac/cross_search/class_info/D0',i,'.html')
    
    ##有些頁有問題, 加入此encoding
    total_css = read_html(url,encoding="ISO-8859-1")
    link_css = total_css %>% html_nodes("td a") %>% html_attr("href")
    link_css = unlist(lapply(link_css,function(link_css){
      unlist(strsplit(link_css,'/'))[length(unlist(strsplit(link_css,'/')))]
    }))
    
    links = paste0('http://campus4.ncku.edu.tw/uac/cross_search/dept_info/',link_css)
    
    all_links = c(all_links,links)
    cat(paste0('\r',i/19*100,'%'))
    gc()
    Sys.sleep(runif(1,2,5))
  }else{
    url = paste0('http://campus4.ncku.edu.tw/uac/cross_search/class_info/D',i,'.html')
    
    ##有些頁有問題, 加入此encoding
    total_css = read_html(url,encoding="ISO-8859-1")
    link_css = total_css %>% html_nodes("td a") %>% html_attr("href")
    link_css = unlist(lapply(link_css,function(link_css){
      unlist(strsplit(link_css,'/'))[length(unlist(strsplit(link_css,'/')))]
    }))    
    links = paste0('http://campus4.ncku.edu.tw/uac/cross_search/dept_info/',link_css)
    gc()
    all_links = c(all_links,links)
    cat(paste0('\r','抓取各頁內部網址 : ',format(round(i/19*100,2),nsmall=2),'%',paste(replicate(50, " "), collapse = "")))
    Sys.sleep(runif(1,2,5))
  }
  
}

##getting datas
output_df = data.frame('學校'=character(),'學系'=character(),'連結'=character(),'代碼'=character(),'學科能力測驗及英語聽力檢定標準'=character(),'學科能力測驗及英語聽力檢定標準2'=character(),'指定考試採計科目及方法'=character(),'同分參酌方式1'=character(),'同分參酌方式2'=character(),'同分參酌方式3'=character(),'選系說明'=character(),stringsAsFactors=F)
#total_css <-html(url,encoding="gb2312")

all_links = unique(all_links)
error = 0
x=1
for(i in 1:length(all_links)){
#for(i in 1:92){
  tryCatch({
    total_css = read_html(all_links[i],encoding="big5")
    content_css = total_css %>% html_nodes("tr td") %>% html_text()
    title_css = total_css %>% html_nodes("title") %>% html_text()
    ##有亂碼問題
    ##content_css = iconv(content_css,'utf8')
    content_css = toUTF8(content_css)
    content_css = gsub(' ','',content_css)
    #content_css = gsub('    ','',content_css)
    title_css = toUTF8(title_css)
    cat(paste0('\r',title_css,' : ',format(round(i/length(all_links)*100,2),nsmall=2),'%',paste(replicate(50, " "), collapse = "")))
    title_css = unlist(strsplit(title_css,' -'))[1]
    
    Sys.sleep(runif(1,2,4))
    
    output_df[x,] = c(title_css,content_css[1],all_links[i],unlist(strsplit(unlist(strsplit(all_links[i],'/')),'[.]'))[length(unlist(strsplit(unlist(strsplit(all_links[i],'/')),'[.]')))-1],content_css[2],content_css[9],content_css[3],content_css[5],content_css[8],content_css[11],content_css[6])
    
    if(is.na(output_df[x-1,1])){
      output_df[x-1,] = c('錯誤','錯誤',all_links[i-1],unlist(strsplit(unlist(strsplit(all_links[i-1],'/')),'[.]'))[length(unlist(strsplit(unlist(strsplit(all_links[i-1],'/')),'[.]')))-1],'錯誤','錯誤','錯誤','錯誤','錯誤','錯誤','錯誤')
    }
    gc()
    x = x + 1
  }, error = function(e) {
    #output_df[x,] <<- c('錯誤','錯誤',all_links[i],unlist(strsplit(unlist(strsplit(all_links[i],'/')),'[.]'))[length(unlist(strsplit(unlist(strsplit(all_links[i],'/')),'[.]')))-1],'錯誤','錯誤','錯誤','錯誤','錯誤','錯誤','錯誤')
    x <<- x + 1
    error <<- error + 1
    print(paste0(error,'筆錯誤'))
    #print(paste0('錯誤率為 ', round(error/length(all_links),2)))
    gc()
  })
}

getwd()
write.csv(output_df,'test.csv',row.names=F)
input_df = read.csv('test.csv',stringsAsFactors=F)
for(i in 1:ncol(input_df)){
  ##沒fixed無法
  input_df[,i] = gsub('<U+00A0>','',input_df[,i],fixed=T)
  #input_df[,i] = gsub('\n','',input_df[,i])
  input_df[,i] = gsub('　','',input_df[,i])
}
write.csv(output_df,'test3.csv',row.names=F)

input_df = read.csv('test3.csv',stringsAsFactors=F)

result_df=data.frame("學校名稱"=character(),"系組名稱（簡章）"=character(),"簡章介紹（連結）"=character(),"簡章編碼"=character(),"105年學測、英聽檢定標準:國文"=character(),"105年學測、英聽檢定標準:英文"=character(),"105年學測、英聽檢定標準:數學"=character(),"105年學測、英聽檢定標準:社會"=character(),"105年學測、英聽檢定標準:自然"=character(),"105年學測、英聽檢定標準:總級分"=character(),"105年學測、英聽檢定標準:英聽"=character(),"105年指考採計標準權重:國文"=character(),"105年指考採計標準權重:英文"=character(),"105年指考採計標準權重:數學乙"=character(),"105年指考採計標準權重:歷史"=character(),"105年指考採計標準權重:地理"=character(),"105年指考採計標準權重:公民"=character(),"105年指考採計標準權重:數學甲"=character(),"105年指考採計標準權重:物理"=character(),"105年指考採計標準權重:化學"=character(),"105年指考採計標準權重:生物"=character(),"105年指考採計標準權重:術科"=character(),"同分參酌方式:科目1"=character(),"同分參酌方式:科目2"=character(),"同分參酌方式:科目3"=character(),"選系說明"=character(),stringsAsFactors=F)

for(i in 1:nrow(input_df)){
  result_df[i,1] = input_df[i,1]
  result_df[i,2] = input_df[i,2]
  result_df[i,3] = input_df[i,3]
  result_df[i,4] = input_df[i,4]
  if(grepl('國文',input_df[i,5])){
    result_df[i,5] = substr(input_df[i,5],unlist(gregexpr(pattern ='國文',input_df[i,5]))+3,unlist(gregexpr(pattern ='國文',input_df[i,5]))+4)
  }
  if(grepl('英文',input_df[i,5])){
    result_df[i,6] = substr(input_df[i,5],unlist(gregexpr(pattern ='英文',input_df[i,5]))+3,unlist(gregexpr(pattern ='英文',input_df[i,5]))+4)
  }
  if(grepl('數學',input_df[i,5])){
    result_df[i,7] = substr(input_df[i,5],unlist(gregexpr(pattern ='數學',input_df[i,5]))+3,unlist(gregexpr(pattern ='數學',input_df[i,5]))+4)
  }
  if(grepl('社會',input_df[i,5])){
    result_df[i,8] = substr(input_df[i,5],unlist(gregexpr(pattern ='社會',input_df[i,5]))+3,unlist(gregexpr(pattern ='社會',input_df[i,5]))+4)
  }
  if(grepl('自然',input_df[i,5])){
    result_df[i,9] = substr(input_df[i,5],unlist(gregexpr(pattern ='自然',input_df[i,5]))+3,unlist(gregexpr(pattern ='自然',input_df[i,5]))+4)
  }
  if(grepl('總級分',input_df[i,5])){
    result_df[i,10] = substr(input_df[i,5],unlist(gregexpr(pattern ='總級分',input_df[i,5]))+4,unlist(gregexpr(pattern ='總級分',input_df[i,5]))+5)
  }
  if(grepl('Ｂ',input_df[i,6])){
    result_df[i,11] = 'B'
  }else if(grepl('Ｃ',input_df[i,6])){
    result_df[i,11] = 'C'
  }else if(grepl('Ａ',input_df[i,6])){
    result_df[i,11] = 'A'
  }else{
    
  }
  
  if(grepl('國文',input_df[i,7])){
    result_df[i,12] = substr(input_df[i,7],unlist(gregexpr(pattern ='國文',input_df[i,7]))+3,unlist(gregexpr(pattern ='國文',input_df[i,7]))+6)
  }
  if(grepl('英文',input_df[i,7])){
    result_df[i,13] = substr(input_df[i,7],unlist(gregexpr(pattern ='英文',input_df[i,7]))+3,unlist(gregexpr(pattern ='英文',input_df[i,7]))+6)
  }
  if(grepl('數學乙',input_df[i,7])){
    result_df[i,14] = substr(input_df[i,7],unlist(gregexpr(pattern ='數學乙',input_df[i,7]))+4,unlist(gregexpr(pattern ='數學乙',input_df[i,7]))+7)
  }
  if(grepl('歷史',input_df[i,7])){
    result_df[i,15] = substr(input_df[i,7],unlist(gregexpr(pattern ='歷史',input_df[i,7]))+3,unlist(gregexpr(pattern ='歷史',input_df[i,7]))+6)
  }
  if(grepl('地理',input_df[i,7])){
    result_df[i,16] = substr(input_df[i,7],unlist(gregexpr(pattern ='地理',input_df[i,7]))+3,unlist(gregexpr(pattern ='地理',input_df[i,7]))+6)
  }
  if(grepl('公民',input_df[i,7])){
    result_df[i,17] = substr(input_df[i,7],unlist(gregexpr(pattern ='公民',input_df[i,7]))+6,unlist(gregexpr(pattern ='公民',input_df[i,7]))+9)
  }
  if(grepl('數學甲',input_df[i,7])){
    result_df[i,18] = substr(input_df[i,7],unlist(gregexpr(pattern ='數學甲',input_df[i,7]))+4,unlist(gregexpr(pattern ='數學甲',input_df[i,7]))+7)
  }
  if(grepl('物理',input_df[i,7])){
    result_df[i,19] = substr(input_df[i,7],unlist(gregexpr(pattern ='物理',input_df[i,7]))+3,unlist(gregexpr(pattern ='物理',input_df[i,7]))+6)
  }
  if(grepl('化學',input_df[i,7])){
    result_df[i,20] = substr(input_df[i,7],unlist(gregexpr(pattern ='化學',input_df[i,7]))+3,unlist(gregexpr(pattern ='化學',input_df[i,7]))+6)
  }
  if(grepl('生物',input_df[i,7])){
    result_df[i,21] = substr(input_df[i,7],unlist(gregexpr(pattern ='生物',input_df[i,7]))+3,unlist(gregexpr(pattern ='生物',input_df[i,7]))+6)
  }
  if(grepl('術科',input_df[i,7])){
    result_df[i,22] = substr(input_df[i,7],unlist(gregexpr(pattern ='術科',input_df[i,7]))+7,unlist(gregexpr(pattern ='術科',input_df[i,7]))+10)
  }
  result_df[i,23] = input_df[i,8]
  result_df[i,24] = input_df[i,9]
  result_df[i,25] = input_df[i,10]
  result_df[i,26] = input_df[i,11]
}
for(i in 1:ncol(result_df)){
  result_df[which(is.na(result_df[,i])),i] = '---'
}

write.csv(result_df,'test4.csv',row.names=F)

library(XML)
library(RCurl)

get_url = getURL(url,encoding = "gb2312")
get_url_parse = htmlParse(get_url, encoding = "gb2312")

get_url = getURL(url,encoding = "gbk")
get_url_parse = htmlParse(get_url, encoding = "gbk")
