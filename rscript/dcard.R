##https://www.dcard.tw/api/post/all/85846
##http://stackoverflow.com/questions/28418770/using-rvest-or-httr-to-log-in-to-non-standard-forms-on-a-webpage

##People Talking About This
rm(list = ls()) #去除工作空間中所有物件
path = 'D:\\abc\\wjhong\\projects\\internet_volume'
setwd(path)
list = c()
library("httr")
##爬json
x = 1
list_df = data.frame('類型'=character(),'標題'=character(),'內容'=character(),stringsAsFactors=F)
min = 1200000
max = 1220000
for(i in min:max){
  tryCatch({
    url <- paste0("https://www.dcard.tw/api/post/all/",i)
    a <- GET(url)
    #content(a)
    a = as(a, "character")
    a = gsub('"','',a)
    list = c(list,a)
    document <- fromJSON(file=url, method='C')
    print(i)
    print(paste0('    類別:',document$forum_name))
    print(paste0('    標題:',document$version[[1]]$title))
    gc()
    #暫存一下好了怕當機
    #length(document$comment)
    content_list = c()
    for(j in 1:length(document$comment)){
      content_list = c(content_list,document$comment[[j]]$version[[1]]$content)}
    content_list = paste(content_list,collapse=' ;; ')
    
    #list_df = rbind(list_df,c(substr(forum_name,12,nchar(forum_name)),substr(title_,7,nchar(title_)),as.character(a)))
    list_df[x,1:3] = c(document$forum_name,document$version[[1]]$title,content_list)
    x = x + 1
    write.csv(list_df,paste0('output/',min,'_',max,'_Dcard.csv'),row.names=F)
    sec = runif(1,2,5)
    print(paste0('loading... 稍等',sec,'秒'))
    Sys.sleep(sec)
  }, error=function(e){
    print(paste0('   Dcard',': ',i,' 失敗'))
    sec = runif(1,1,3)
    print(paste0('loading... 稍等',sec,'秒'))
    Sys.sleep(sec)
    gc()
  })
}
##取出要的論壇
#list = list[which(grepl('forum_name:男女',list))]
##"forum_name\":\"新生季\"

##json cleaning
#library(rjson)
#url <- paste0("https://www.dcard.tw/api/post/all/",i)
#document <- fromJSON(file=url, method='C')
#document$version[[1]]$title
#document$forum_name
#document$comment[[4]]$version[[1]]$content
