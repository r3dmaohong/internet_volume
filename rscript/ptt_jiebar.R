##ptt jieba
library(rvest)
library(plyr)
library(XML)
library(RCurl)
library(SnowballC)
library(cluster)   
library(XML)
library(RCurl) 

ptt_crawler_jiebar <- function(link,forum_name,min,max,start.time){
  links_data_ptt = {}
  ##先讀取各頁文章的網址
  #forum_name = substr(link,unlist(gregexpr(pattern ='bbs',link))+4,unlist(gregexpr(pattern ='index',link))-2)
  
  print(forum_name)
  for(i in min:max){
    tmp <- paste(i, '.html', sep='')
    #https://www.ptt.cc/bbs/Salary/index1896.html
    #'https://www.ptt.cc/bbs/ServiceInfo/index'
    url <- paste(link, tmp, sep='')
    title_css = read_html(url) %>% html_nodes(".title") %>% html_nodes("a") %>% html_attr('href')
    links_data_ptt = c(links_data_ptt,title_css)
    gc() #記憶體釋放
    cat("\r ptt 第 ",i, '頁 ',(i-min+1)/(max-min+1)*100, '% completed ',paste(replicate(50, " "), collapse = ""))
    #print(paste0(forum_name,' ptt第',i,'頁'))
    Sys.sleep(runif(1,2,5))
  }
  cat("\n ")
  ##剔除部相關之網址(挑選時已替除，不過在判斷一次)
  #links_data_ptt =  links_data_ptt[which(grepl(forum_name,links_data_ptt))]
  
  ptt_data = {}
  ##將抓出的網址進行爬蟲
  for(i in 1:length(links_data_ptt)){
    tryCatch({
      url = paste0('https://www.ptt.cc',links_data_ptt[i])
      title_css = read_html(url) %>% html_nodes("#main-content") %>% html_text()
      utf8_text_title <- iconv(title_css,'utf8')
      
      ##去除id
      title_css1 = read_html(url) %>% html_nodes("span") %>% html_text()
      utf8_text_title1 <- iconv(title_css1,'utf8')
      
      id_delete = utf8_text_title1[which(grepl(': ',utf8_text_title1))-1]
      id_delete = c(id_delete, utf8_text_title1[1:8])
      id_delete = id_delete[which(!is.na(id_delete))]
      for(x in 1:length(id_delete)){
        utf8_text_title=gsub(id_delete[x],'',utf8_text_title)
      }
      
      temp = utf8_text_title
      
      ##: 前兩個去掉
      ptt_data = c(ptt_data,temp)
      ##which contains 落點
      gc() #記憶體釋放
      Sys.sleep(runif(1,2,5))
      #print(paste0(forum_name, ' ptt第',i,'筆  ',i/length(links_data_ptt)*100,'%'))
      cat("\r ptt 第 ",i, '筆 ==>',i/length(links_data_ptt)*100, '% completed   ',paste(replicate(50, " "), collapse = ""))
    }, error = function(e) {
      print(paste0(forum_name, ' ptt第',i,'筆 失敗 ',i/length(links_data_ptt)*100,'%'))
      Sys.sleep(runif(1,2,5))
    })
    
  }
  cat("\n ")
  print(paste0(forum_name,' : ',length(ptt_data),'筆'))
  
  dir.create(forum_name, showWarnings = FALSE)
  write.csv(ptt_data,paste0(forum_name,'/',forum_name,'_',min,'_',max,'.csv'))
  
  jiebar_n(forum_name,ptt_data,min,max)
}

