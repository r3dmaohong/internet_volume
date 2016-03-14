library(httr)
library(rvest)
library(tmcn)
# Attempt to crawl LinkedIn, requires useragent to access Linkedin Sites
##設定user agent 才能爬mobile01
uastring <- "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2228.0 Safari/537.36"
session <- html_session("http://www.mobile01.com/forumtopic.php?c=21", user_agent(uastring))
title_css = read_html(session ) %>% html_nodes(".topic_gen") %>% html_text()
##文章列表標題
utf8_text_title <- toUTF8(title_css) ## 將捉下來的標題轉成 UTF8
##文章link
link_css = read_html(session ) %>% html_nodes(".topic_gen") %>% html_attr('href')
links_list = paste0('http://www.mobile01.com/',link_css)




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