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
    print(paste0('yahoo第',i,'頁'))
    Sys.sleep(runif(1,2,5))    
  }
  temp_yahoo_data = {}
  ##將抓出的網址進行爬蟲
  for(i in 1:length(links_data_yahoo)){
    url = paste0('https://tw.answers.yahoo.com',links_data_yahoo[i])
    title_css = read_html(url) %>% html_nodes("span") %>% html_text()
    temp <- iconv(title_css,'utf8')
    
    temp_yahoo_data = c(temp_yahoo_data,temp)
    ##which contains 落點
    gc() #記憶體釋放
    print(paste0('yahoo第',i,'筆  ',i/length(links_data_yahoo)*100,'%'))
    Sys.sleep(runif(1,2,5))
  }
  
  
  write.csv(temp_yahoo_data,paste0('yahoo/',forum_name,'_',min,'_',max,'.csv'))
  
  yahoo_data = temp_yahoo_data
  library(jiebaR)
  cutter = worker()
  jieba_yahoo = {}
  yahoo_data = tolower(yahoo_data)
  for(i in 1:length(yahoo_data)){
    temp = segment(yahoo_data[i], cutter)
    jieba_yahoo = c(jieba_yahoo,temp)
    print(paste0('jiebar :',i/length(yahoo_data)*100,'%'))
  }
  ##去除單字
  jieba_yahoo = jieba_yahoo[which(nchar(jieba_yahoo)>1)]
  ##去除數值與id
  jieba_yahoo = jieba_yahoo[which(!grepl('[0-9]',jieba_yahoo))]
  
  
  jieba_yahoo_df = as.data.frame(jieba_yahoo)
  jieba_yahoo_cdf = ddply(jieba_yahoo_df , c('jieba_yahoo'), nrow)
  jieba_yahoo_cdf = jieba_yahoo_cdf[order(-jieba_yahoo_cdf$V1),]
  #write.csv(jieba_yahoo_cdf,paste0('output/yahoo/',format(Sys.time(), "%Y_%d_%b"),'jieba_yahoo_output_tolower_temp.csv'),row.names=F)
  
  ##不知為何沒tolower.. once again
  jieba_yahoo_cdf[,1] = tolower(jieba_yahoo_cdf[,1])
  jieba_yahoo_cdf = ddply(jieba_yahoo_cdf , c('jieba_yahoo'), summarize, sum(V1))
  jieba_yahoo_cdf = jieba_yahoo_cdf[order(-jieba_yahoo_cdf$..1,jieba_yahoo_cdf$jieba_yahoo),]
  
  write.csv(jieba_yahoo_cdf,paste0('yahoo/',format(Sys.time(), "%Y_%d_%b"),'jieba',forum_name,'_',min,'_',max,'.csv'),row.names=F)
  
  ##之前收到的手動填寫公司名稱
  temp = read.csv('D:\\abc\\wjhong\\projects\\school_performence_analysis\\__處理後公司名稱.csv',stringsAsFactors=F)
  temp = temp[,1]
  temp = tolower(temp)
  
  inter_list= intersect(jieba_yahoo_cdf[,1],temp)
  yahoo2 = jieba_yahoo_cdf[which(jieba_yahoo_cdf[,1] %in% inter_list),]
  write.csv(yahoo2,paste0(start.time,'/',forum_name,'_',min,'_',max,'交集結果.csv'),row.names=F)
  
}




forum_name = 'yahoo'
min=1
max=200

