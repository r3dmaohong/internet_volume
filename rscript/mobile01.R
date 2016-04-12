library(httr)
library(rvest)
library(tmcn)
# Attempt to crawl LinkedIn, requires useragent to access Linkedin Sites
##設定user agent 才能爬mobile01
uastring <- "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2228.0 Safari/537.36"
links_list = c()

max = 1##300
for(i in 1:max){
  tryCatch({
    session <- html_session(paste0("http://www.mobile01.com/topiclist.php?f=651&p=",i), user_agent(uastring))
    link_css = read_html(session ) %>% html_nodes(".topic_gen") %>% html_attr('href')
    links = paste0('http://www.mobile01.com/',link_css)
    links_list = c(links_list,links)
    
    gc() #記憶體釋放
    cat("\r mobile01 第 ",i, '頁 ==>' ,i/max*100, '% completed                              ',paste(replicate(100, " "), collapse = ""))
    #print(paste0('lineq第',i,'頁'))
    Sys.sleep(runif(1,2,5))
    
  },error=function(e){
    
  })
}
cat("\n ")

conversation_data = c()
for(j in 1:length(links_list)){
  tryCatch({
    session <- html_session(links_list[j], user_agent(uastring))
    ##頁數?
    page_css = read_html(session) %>% html_nodes(".numbers") %>% html_text()
    page_css = toUTF8(page_css)
    page_max = as.numeric(substr(page_css,unlist(gregexpr('共',page_css))+1,unlist(gregexpr('頁',page_css))[2]-1))
    
    Sys.sleep(runif(1,2,5))
    
    for(i in 1:page_max){
      url = paste0(links_list[i],'&p=',i)
      session <- html_session(url, user_agent(uastring))
      ##文章內容
      text_css = read_html(session ) %>% html_nodes(".single-post-content") %>% html_text()
      text_css = iconv(text_css,'utf8')
      text_css = text_css[which(!is.na(text_css))]
      text_css = unique(text_css)
      conversation_data = c(conversation_data, text_css)
      
      Sys.sleep(runif(1,2,5))
    }
    
    cat("\r mobile01 第 ",j, '筆 ==>' ,j/length(links_list)*100, '% completed                              ',paste(replicate(100, " "), collapse = ""))
},error=function(e){
  
})
}


#page_max = max(as.numeric(iconv(page_css,'utf8'))[which(!is.na(as.numeric(iconv(page_css,'utf8'))))])
#session <- html_session("http://www.mobile01.com/topiclist.php?f=651&p=1", user_agent(uastring))
#title_css = read_html(session ) %>% html_nodes(".topic_gen") %>% html_text()
##文章列表標題
#utf8_text_title <- iconv(title_css,'utf8') ## 將捉下來的標題轉成 UTF8
##文章link





#http://stat4701.github.io/edav/2015/04/02/rvest_tutorial/
#form <- html_form(session)[[1]]
#form <- set_values(form, keywords = "Data Science", location="New York")

##new_url <- submit_geturl(session,form)
#new_session <- html_session(new_url, user_agent(uastring))
#jobtitle <- new_session %>% html_nodes(".job [itemprop=title]") %>% html_text
#company <- new_session %>% html_nodes(".job [itemprop=name]") %>% html_text
#location <- new_session %>% html_nodes(".job [itemprop=addressLocality]") %>% html_text
#description <- new_session %>% html_nodes(".job [itemprop=description]") %>% html_text
#url <- new_session %>% html_nodes(".job [itemprop=title]") %>% html_attr("href")
#url <- paste(url, ')', sep='')
#url <- paste('[Link](', url, sep='')
#df <- data.frame(jobtitle, company, location, url)

#df %>% kable