path<-choose.dir()
setwd(path)
start.time<-Sys.time()

library(rvest)
library(plyr)
library(XML)
library(RCurl)
library(data.table) 
library(xlsx)
library(httr)
library(tmcn)

query = '%E8%8F%AF%E8%88%AA+site:https://www.ptt.cc/bbs/Gossiping/'

url = paste0('https://www.google.com.tw/search?q=',query,'&oq=',query,'&aqs=chrome..69i57.3455j0j4&sourceid=chrome&ie=UTF-8&start=')

uastring <- "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2228.0 Safari/537.36"

total_links = {}
for(i in 45:60){
  link = paste0(url,i,'0')
  gc() #記憶體釋放      
  session <- html_session(link, user_agent(uastring))
  link_css = read_html(session) %>% html_nodes("._Rm") %>% html_text()
  total_links = c(total_links,link_css)
  #print(total_links)
  print(i)
  sleep_time = runif(1,15,25)
  print(paste0('休息',sleep_time,'sec...'))
  Sys.sleep(sleep_time)
}
total_links = total_links[which(!grepl('index',total_links))]
total_links = unique(total_links)

ptt_data = data.frame('Date'=character(),'Content'=character(),stringsAsFactors=F)
xrow = 1
links_data_ptt = total_links
##將抓出的網址進行爬蟲


for(i in 1:length(links_data_ptt)){
  tryCatch({
    url = links_data_ptt[i]
    #total_css = read_html(url) 
    session = rvest::html_session(url = url)
    form = session %>% html_node("form") %>% html_form() 
    session_redirected = rvest::submit_form(session = session, form = form )
        
    total_css = session_redirected
    content_css = total_css %>% html_nodes("#main-content") %>% html_text()
    utf8_text_content <- iconv(content_css,'utf8')
    if(is.na(utf8_text_content)){
      utf8_text_content <- toUTF8(content_css)
    }
    
    date_css = total_css %>% html_nodes(".article-meta-value") %>% html_text()
    date_css = date_css[4]
    
    ##去除id
    id_css = total_css %>% html_nodes("span") %>% html_text()
    utf8_text_id <- iconv(id_css,'utf8')
    
    if(is.na(utf8_text_id)){
      utf8_text_id <- toUTF8(id_css)
    }
    
    id_delete = utf8_text_id[which(grepl(': ',utf8_text_id))-1]
    id_delete = c(id_delete, utf8_text_id[1:8])
    id_delete = id_delete[which(!is.na(id_delete))]
    for(x in 1:length(id_delete)){
      utf8_text_content=gsub(id_delete[x],'',utf8_text_content)
    }
    
    #temp = content_css
    ptt_data[xrow,] = c(date_css, utf8_text_content)
    xrow = xrow + 1
    
    gc() #記憶體釋放
    Sys.sleep(runif(1,2,5))
    #print(paste0(forum_name, ' ptt第',i,'筆  ',i/length(links_data_ptt)*100,'%'))
    cat("\r ptt 第 ",i, '筆 ==>',i/length(links_data_ptt)*100, '% completed   ',paste(replicate(50, " "), collapse = ""))
  }, error = function(e) {
    #cat("\n ")
    #print(paste0(forum_name, ' ptt第',i,'筆 失敗 ',i/length(links_data_ptt)*100,'%'))
    #Sys.sleep(runif(1,2,5))
    print(e)
    if(grepl('Timeout was reached',e)){
      Sys.sleep(runif(1,10,12))
      i <<- i-1
      print('錯誤有處理 : Timeout was reached')
    }else{
      print('錯誤沒處理')
      Sys.sleep(runif(1,10,12))
    }
  })
  
}
cat("\n ")

write.csv(ptt_data,'line華航.csv',row.names=F)
