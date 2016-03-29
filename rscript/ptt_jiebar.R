##ptt jieba
library(rvest)
library(plyr)
library(XML)
library(RCurl)
library(SnowballC)
library(cluster)   
library(XML)
library(RCurl)

# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#gc
jgc <- function()
{
  gc()
  .jcall("java/lang/System", method = "gc")
}  

ptt_crawler_jiebar <- function(link,min,max){
  links_data_ptt = {}
  ##先讀取各頁文章的網址
  forum_name = substr(link,unlist(gregexpr(pattern ='bbs',link))+4,unlist(gregexpr(pattern ='index',link))-2)
  print(forum_name)
  for(i in min:max){
    tmp <- paste(i, '.html', sep='')
    #https://www.ptt.cc/bbs/Salary/index1896.html
    #'https://www.ptt.cc/bbs/ServiceInfo/index'
    url <- paste(link, tmp, sep='')
    title_css = read_html(url) %>% html_nodes(".title") %>% html_nodes("a") %>% html_attr('href')
    links_data_ptt = c(links_data_ptt,title_css)
    gc() #記憶體釋放
    print(paste0(forum_name,' ptt第',i,'頁'))
    Sys.sleep(runif(1,2,5))
  }
  ##剔除部相關之網址(挑選時已替除，不過在判斷一次)

  links_data_ptt =  links_data_ptt[which(grepl(forum_name,links_data_ptt))]
  
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
      for(x in 1:length(id_delete)){
        utf8_text_title=gsub(id_delete[x],'',utf8_text_title)
      }
      
      temp = utf8_text_title
      
      ##: 前兩個去掉
      ptt_data = c(ptt_data,temp)
      ##which contains 落點
      gc() #記憶體釋放
      Sys.sleep(runif(1,2,5))
      print(paste0('ptt第',i,'筆  ',i/length(links_data_ptt)*100,'%'))
    }, error = function(e) {
      print(paste0('ptt第',i,'筆 失敗 ',i/length(links_data_ptt)*100,'%'))
      Sys.sleep(runif(1,2,5))
    })
    
  }
  print(paste0(forum_name,' : ',length(ptt_data),'筆'))
  
  write.csv(ptt_data,paste0(forum_name,'_',min,'_',max,'.csv'))
  
  
  library(jiebaR)
  cutter = worker()
  jieba_ptt = {}
  ptt_data = tolower(ptt_data)
  for(i in 1:length(ptt_data)){
    temp = segment(ptt_data[i], cutter)
    jieba_ptt = c(jieba_ptt,temp)
    print(paste0('jiebar :',i/length(ptt_data)*100,'%'))
  }
  ##去除單字
  jieba_ptt = jieba_ptt[which(nchar(jieba_ptt)>1)]
  ##去除數值與id
  jieba_ptt = jieba_ptt[which(!grepl('[0-9]',jieba_ptt))]
  
  
  jieba_ptt_df = as.data.frame(jieba_ptt)
  jieba_ptt_cdf = ddply(jieba_ptt_df , c('jieba_ptt'), nrow)
  jieba_ptt_cdf = jieba_ptt_cdf[order(-jieba_ptt_cdf$V1),]
  #write.csv(jieba_ptt_cdf,paste0('output/ptt/',format(Sys.time(), "%Y_%d_%b"),'jieba_ptt_output_tolower_temp.csv'),row.names=F)
  
  ##不知為何沒tolower.. once again
  jieba_ptt_cdf[,1] = tolower(jieba_ptt_cdf[,1])
  jieba_ptt_cdf = ddply(jieba_ptt_cdf , c('jieba_ptt'), summarize, sum(V1))
  jieba_ptt_cdf = jieba_ptt_cdf[order(-jieba_ptt_cdf$..1,jieba_ptt_cdf$jieba_ptt),]
  
  write.csv(jieba_ptt_cdf,paste0('output/ptt/',format(Sys.time(), "%Y_%d_%b"),'jieba',forum_name,'_',min,'_',max,'.csv'),row.names=F)
  
  #之前收到的手動填寫公司名稱
  temp = read.csv('D:\\abc\\wjhong\\projects\\school_performence_analysis\\__處理後公司名稱.csv',stringsAsFactors=F)
  temp = temp[,1]
  temp = tolower(temp)
  
  inter_list= intersect(jieba_ptt_cdf[,1],temp)
  ptt2 = jieba_ptt_cdf[which(jieba_ptt_cdf[,1] %in% inter_list),]
  write.csv(ptt2,paste0(forum_name,'_',min,'_',max,'交集結果.csv'),row.names=F)
}

