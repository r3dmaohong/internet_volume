lineq_crawler_jiebar('http://lineq.tw/search/question?q=%E8%8F%AF%E8%88%AA&sort=date&sel=all&page=','lineq華航',,70,start.time)

link='http://lineq.tw/search/question?q=%E8%8F%AF%E8%88%AA&sort=date&sel=all&page='
forum_name = 'lineq華航'
min = 1
max = 70

forum_name = forum_name
links_data_lineq = {}
url = link
for(i in min:max){
  url <- paste(link, i, sep='')
  title_css = read_html(url) %>% html_nodes("p") %>% html_nodes("a") %>% html_attr('href')
  if(toString(title_css)!=''){
    links_data_lineq = c(links_data_lineq,title_css)
    gc() #記憶體釋放
    cat("\r lineq 第 ",i, '頁') #,i/max*100, '% completed',paste(replicate(50, " "), collapse = ""))
    #print(paste0('lineq第',i,'頁'))
    Sys.sleep(runif(1,2,5))
  }else{
    break
  }
  
}
cat("\n ")
links_data_lineq = unique(links_data_lineq)

print(paste0('已爬到最底頁 : ', max , '頁' ))
##temp_lineq_data = {}

lineq_data = data.frame('Date'=character(),'Content'=character(),stringsAsFactors=F)
xrow = 1

for(i in 659:length(links_data_lineq)){
  tryCatch({
    url = paste0('http://lineq.tw',links_data_lineq[i])
    total_css = read_html(url)
    content_css = c(total_css %>% html_nodes(".question_content .content_text") %>% html_text(), total_css %>% html_nodes(".reply_content .content_text") %>% html_text())
    content_utf8 <- iconv(content_css,'utf8')
    
    #文章日期
    date_css = total_css %>% html_nodes(".header_time") %>% html_text()
    date_utf8 <- iconv(date_css,'utf8')
    
    #nowdate = substr(gsub('-','.',Sys.time()),1,unlist(gregexpr(pattern =':',Sys.time()))[length(unlist(gregexpr(pattern =':',Sys.time())))]-1)
    if(grepl('小時',date_utf8)){
      time = as.numeric(unique(unlist(regmatches(date_utf8, gregexpr("[0-9]+", date_utf8)))))
      date_utf8 = substr(gsub('-','.',Sys.time() - time*60*60),1,unlist(gregexpr(pattern =':',Sys.time() - time*60*60))[length(unlist(gregexpr(pattern =':',Sys.time() - time*60*60)))]-1)
      
    }else if(grepl('分',date_utf8)){
      time = as.numeric(unique(unlist(regmatches(date_utf8, gregexpr("[0-9]+", date_utf8)))))
      date_utf8 = substr(gsub('-','.',Sys.time() - time*60),1,unlist(gregexpr(pattern =':',Sys.time() - time*60))[length(unlist(gregexpr(pattern =':',Sys.time() - time*60)))]-1)
      
    }else if(grepl('天',date_utf8)){
      time = as.numeric(unique(unlist(regmatches(date_utf8, gregexpr("[0-9]+", date_utf8)))))
      date_utf8 = paste0(gsub('-','.',Sys.Date() - time)," 00:00")
    }else{
    }
    content_utf8 =  paste0(content_utf8,collapse=';:;:;')
    lineq_data[xrow,] = c(date_utf8,content_utf8)
    xrow = xrow + 1 
    ##which contains 落點
    gc() #記憶體釋放
    
    print(paste0("lineq 第",i, '筆 ==> ',i/length(links_data_lineq)*100, '% completed ',paste(replicate(50, " "), collapse = "")))
    #print(paste0('linq第',i,'筆  ',i/length(links_data_lineq)*100,'%'))
    Sys.sleep(runif(1,2,5))
  },error=function(e){
    if(grepl('Timeout was reached',e)){
      Sys.sleep(runif(1,10,12))
      i <<- i-1
      print('錯誤有處理 : Timeout was reached')
    }else{
      print('錯誤沒處理')
      print(e)
      Sys.sleep(runif(1,10,12))
    }

    
 })
  
}
cat("\n ")

write.csv(lineq_data,'line華航.csv',row.names=F)
