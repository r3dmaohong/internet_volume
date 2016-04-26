##https://www.dcard.tw/api/post/all/85846
##http://stackoverflow.com/questions/28418770/using-rvest-or-httr-to-log-in-to-non-standard-forms-on-a-webpage

##People Talking About This
rm(list = ls()) #去除工作空間中所有物件
path = 'D:\\abc\\wjhong\\projects\\internet_volume'
setwd(path)
list = c()
library(httr)
library(rjson)
##爬json

dcard_crawler <- function(minmax){
  list_df = data.frame('類型'=character(),'標題'=character(),'日期'=character(),'文章ID'=character(),'文章內容'=character(),'回覆內容'=character(),stringsAsFactors=F)
  #3937229
  #1000000
  x=1
  #minmax = 1000000
  while(minmax <= 9999999999){
    ##怕變e
    min  = format(minmax, scientific = FALSE)
    max = format(minmax + 10000, scientific = FALSE)
    for(i in min:max){
      tryCatch({
        ##怕變e
        i = format(i, scientific = FALSE)
        url <- paste0("https://www.dcard.tw/api/post/all/",i)
        a <- GET(url)
        #content(a)
        a = as(a, "character")
        a = gsub('"','',a)
        a = gsub('\n',',,,',a)
        a = gsub('\r',',,,',a)
        list = c(list,a)
        document <- fromJSON(file=url, method='C')
        print(i)
        
        gc()
        #暫存一下好了怕當機
        #length(document$comment)
        content_list = c()
        ##$version[[1]]$content
        content = document$version[[1]]$content
        content = gsub('\n',',,,',content)
        content = gsub('\r',',,,',content)
        content = iconv(content, "UTF8", sub="")
        
        title = document$version[[1]]$title
        title = iconv(title, "UTF8", sub="")
        
        for(j in 1:length(document$comment)){
          content_list = c(content_list,document$comment[[j]]$version[[1]]$content)
        }
        content_list = paste(content_list,collapse=' ;; ')
        content_list = gsub('\n',',,,',content_list)
        content_list = gsub('\r',',,,',content_list)
        ##把非utf8的東西先去除
        content_list = iconv(content_list, "UTF8", sub="")
        
        
        #list_df = rbind(list_df,c(substr(forum_name,12,nchar(forum_name)),substr(title_,7,nchar(title_)),as.character(a)))
        list_df[x,1:6] = c(document$forum_name,title,document$version[[1]]$createdAt,i,content,content_list)
        x = x + 1
        list_df = list_df[,1:6]
        write.csv(list_df,paste0('output/',min,'_',max,'_Dcard.csv'),row.names=F)
        
        print(paste0('    類別:',document$forum_name))
        print(paste0('    標題:',title))
        print(paste0('    日期:',document$version[[1]]$createdAt))
        
        sec = runif(1,2,5)
        print(paste0('loading... 稍等',sec,'秒'))
        Sys.sleep(sec)
      }, error=function(e){
        print(paste0('   Dcard',': ',i,' 失敗'))
        sec = runif(1,2,3)
        print(paste0('loading... 稍等',sec,'秒'))
        Sys.sleep(sec)
        gc()
      })
    }
    minmax = minmax + 10001
  }
}

dcard_crawler(1304654)



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
