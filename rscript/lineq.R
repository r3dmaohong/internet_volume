##lineq jieba

library(rvest)
library(plyr)
library(XML)
library(RCurl)
library(SnowballC)
library(cluster)   
library(XML)
library(RCurl)

#gc
jgc <- function()
{
  gc()
  .jcall("java/lang/System", method = "gc")
}  

lineq_crawler_jiebar <- function(link,forum_name,min,max,start.time){
  forum_name = forum_name
  links_data_lineq = {}
  url = link
  
  for(i in min:max){
    url <- paste(link, i, sep='')
    title_css = read_html(url) %>% html_nodes("p") %>% html_nodes("a") %>% html_attr('href')
    links_data_lineq = c(links_data_lineq,title_css)
    gc() #記憶體釋放
    print(paste0('lineq第',i,'頁'))
    Sys.sleep(runif(1,2,5))
  }
  
  temp_lineq_data = {}
  
  for(i in 1:length(links_data_lineq)){
    url = paste0('http://lineq.tw',links_data_lineq[i])
    title_css = read_html(url) %>% html_nodes("p") %>% html_text()
    temp <- iconv(title_css,'utf8')
    
    print(paste0(substr(temp[2],1,10),'...'))
    temp_lineq_data = c(temp_lineq_data,temp)
    ##which contains 落點
    gc() #記憶體釋放
    print(paste0('linq第',i,'筆  ',i/length(links_data_lineq)*100,'%'))
    Sys.sleep(runif(1,2,5))
  }
  write.csv(temp_lineq_data,paste0('lineq/',forum_name,'_',min,'_',max,'.csv'))
  
  lineq_data = temp_lineq_data
  library(jiebaR)
  cutter = worker()
  jieba_lineq = {}
  lineq_data = tolower(lineq_data)
  for(i in 1:length(lineq_data)){
    temp = segment(lineq_data[i], cutter)
    jieba_lineq = c(jieba_lineq,temp)
    print(paste0('jiebar :',i/length(lineq_data)*100,'%'))
  }
  ##去除單字
  jieba_lineq = jieba_lineq[which(nchar(jieba_lineq)>1)]
  ##去除數值與id
  jieba_lineq = jieba_lineq[which(!grepl('[0-9]',jieba_lineq))]
  
  
  jieba_lineq_df = as.data.frame(jieba_lineq)
  jieba_lineq_cdf = ddply(jieba_lineq_df , c('jieba_lineq'), nrow)
  jieba_lineq_cdf = jieba_lineq_cdf[order(-jieba_lineq_cdf$V1),]
  #write.csv(jieba_lineq_cdf,paste0('output/lineq/',format(Sys.time(), "%Y_%d_%b"),'jieba_lineq_output_tolower_temp.csv'),row.names=F)
  
  ##不知為何沒tolower.. once again
  jieba_lineq_cdf[,1] = tolower(jieba_lineq_cdf[,1])
  jieba_lineq_cdf = ddply(jieba_lineq_cdf , c('jieba_lineq'), summarize, sum(V1))
  jieba_lineq_cdf = jieba_lineq_cdf[order(-jieba_lineq_cdf$..1,jieba_lineq_cdf$jieba_lineq),]
  
  write.csv(jieba_lineq_cdf,paste0('lineq/',format(Sys.time(), "%Y_%d_%b"),'jieba',forum_name,'_',min,'_',max,'.csv'),row.names=F)
  
  ##之前收到的手動填寫公司名稱
  temp = read.csv('D:\\abc\\wjhong\\projects\\school_performence_analysis\\__處理後公司名稱.csv',stringsAsFactors=F)
  temp = temp[,1]
  temp = tolower(temp)
  
  inter_list= intersect(jieba_lineq_cdf[,1],temp)
  lineq2 = jieba_lineq_cdf[which(jieba_lineq_cdf[,1] %in% inter_list),]
  write.csv(lineq2,paste0(start.time,'/',forum_name,'_',min,'_',max,'交集結果.csv'),row.names=F)
  
}

##'http://lineq.tw/search/question?q=%E6%B1%82%E8%81%B7%20%E6%9C%8D%E5%8B%99%E6%A5%AD&sort=date&sel=all'
#print(iconv(read_html(url) %>% html_nodes(".description_title") %>% html_text(),'utf8'))


##將抓出的網址進行爬蟲



min=1
max=170




