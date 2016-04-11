##yahoo
library(rvest)
library(plyr)
library(XML)
library(RCurl)
library(SnowballC)
library(cluster)   
library(XML)
library(RCurl)

yahoo_crawler_jiebar <- function(link,forum_name,min,max,start.time){
  forum_name = forum_name
  links_data_yahoo = {}
  url = link
  
  for(i in min:max){
    url <- paste(link, i, '&sort=new',sep='')
    title_css = read_html(url) %>% html_nodes("a") %>% html_attr('href')
    title_css = title_css[which(grepl('question',title_css) & !grepl('login',title_css))]
    links_data_yahoo = c(links_data_yahoo,title_css)
    gc() #記憶體釋放
    cat("\r yahoo 第 ",i, '頁 ',i/max*100, '% completed                              ')
    #print(paste0('yahoo第',i,'頁'))
    Sys.sleep(runif(1,2,5))    
  }
  cat("\n ")
  temp_yahoo_data = {}
  ##將抓出的網址進行爬蟲
  for(i in 1:length(links_data_yahoo)){
    tryCatch({
    url = paste0('https://tw.answers.yahoo.com',links_data_yahoo[i])
    title_css = read_html(url) %>% html_nodes("span") %>% html_text()
    temp <- iconv(title_css,'utf8')
    
    temp_yahoo_data = c(temp_yahoo_data,temp)
    ##which contains 落點
    gc() #記憶體釋放
    cat("\r yahoo 第",i, '筆 ==> ',i/length(links_data_yahoo)*100, '% completed                              ')
    #print(paste0('yahoo第',i,'筆  ',i/length(links_data_yahoo)*100,'%'))
    Sys.sleep(runif(1,2,5))
    },error=function(e){
      
    })
  }
  cat("\n ")
  
  last=links_data_yahoo[which(grepl('qid',links_data_yahoo))]
  last = last[length(last)]
  last = gsub("[^0-9]", "",last)
  last = substr(last,1,8)
  
  recent = gsub('-','',strsplit(toString(Sys.time()),' ')[[1]][1])
  
  dir.create(forum_name, showWarnings = FALSE)
  write.csv(temp_yahoo_data,paste0(forum_name,'/',forum_name,'_',recent,'_',last,'.csv'))
  
  yahoo_data = temp_yahoo_data
  ##yahoo_data = read.csv(file.choose(),stringsAsFactors=F)
  ##yahoo_data = yahoo_data[,2]
  jiebar_n(forum_name,yahoo_data,recent,last)
  
}

