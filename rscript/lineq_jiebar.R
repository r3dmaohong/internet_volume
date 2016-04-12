##lineq jieba

library(rvest)
library(plyr)
library(XML)
library(RCurl)
library(SnowballC)
library(cluster)   
library(XML)
library(RCurl)

lineq_crawler_jiebar <- function(link,forum_name,min,max,start.time){
  forum_name = forum_name
  links_data_lineq = {}
  url = link
  
  for(i in min:max){
    url <- paste(link, i, sep='')
    title_css = read_html(url) %>% html_nodes("p") %>% html_nodes("a") %>% html_attr('href')
    links_data_lineq = c(links_data_lineq,title_css)
    gc() #記憶體釋放
    cat("\r lineq 第 ",i, '頁 ==>' ,i/max*100, '% completed                              ',paste(replicate(100, " "), collapse = ""))
    #print(paste0('lineq第',i,'頁'))
    Sys.sleep(runif(1,2,5))
  }
  cat("\n ")
  temp_lineq_data = {}
  
  for(i in 1:length(links_data_lineq)){
    tryCatch({
      url = paste0('http://lineq.tw',links_data_lineq[i])
      title_css = read_html(url) %>% html_nodes("p") %>% html_text()
      temp <- iconv(title_css,'utf8')
      
      print(paste0(substr(temp[2],1,10),'...'))
      temp_lineq_data = c(temp_lineq_data,temp)
      ##which contains 落點
      gc() #記憶體釋放
      
      cat("\r lineq 第",i, '筆 ==>',i/length(links_data_lineq)*100, '% completed                              ',paste(replicate(100, " "), collapse = ""))
      #print(paste0('linq第',i,'筆  ',i/length(links_data_lineq)*100,'%'))
      Sys.sleep(runif(1,2,5))
    },error=function(e){
      
    })
    
  }
  cat("\n ")
  
  title_css = read_html(url) %>% html_nodes(".header_time") %>% html_text()
  recent <- iconv(title_css,'utf8')
  recent = strsplit(recent,' ')[[1]][1]
  recent = gsub('[.]','',recent)
  
  last = gsub('-','',strsplit(toString(Sys.time()),' ')[[1]][1])
  
  dir.create(forum_name, showWarnings = FALSE)
  write.csv(recent_lineq_data,paste0(forum_name,'/',forum_name,'_',last,'_',recent,'.csv'))
  
  lineq_data = temp_lineq_data
  jiebar_n(forum_name,lineq_data,recent,last)
}

##'http://lineq.tw/search/question?q=%E6%B1%82%E8%81%B7%20%E6%9C%8D%E5%8B%99%E6%A5%AD&sort=date&sel=all'
#print(iconv(read_html(url) %>% html_nodes(".description_title") %>% html_text(),'utf8'))



