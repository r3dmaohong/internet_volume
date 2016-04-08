##lineq jieba

library(rvest)
library(plyr)
library(XML)
library(RCurl)
library(SnowballC)
library(cluster)   
library(XML)
library(RCurl)

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
  
  title_css = read_html(url) %>% html_nodes(".header_time") %>% html_text()
  recent <- iconv(title_css,'utf8')
  recent = strsplit(recent,' ')[[1]][1]
  recent = gsub('[.]','',recent)
  
  last = gsub('-','',strsplit(toString(Sys.time()),' ')[[1]][1])
  
  write.csv(recent_lineq_data,paste0('lineq/',forum_name,'_',last,'_',recent,'.csv'))
  
  lineq_data = temp_lineq_data
  
  library(jiebaR)
  ##匯入詞庫
  tmp = read.csv('D:\\abc\\wjhong\\projects\\school_performence_analysis\\__處理後公司名稱.csv',stringsAsFactors=F)
  temp = unique(c(tmp$company,tmp$最終比對結果))
  tmp2 = read.csv('D:\\abc\\wjhong\\projects\\school_performence_analysis\\學校名稱正規化表格.csv',stringsAsFactors=F)
  temp = unique(c(temp,tmp2$trim後原始,tmp2$對應表))
  word_DB = tolower(temp)
  #write.table(temp, file="D:\\abc\\wjhong\\projects\\合併詞庫.txt", row.names=FALSE, col.names=FALSE)
  
  #cutter = worker(type  = "mix"，user  = "D:/somefile.xxx")
  #cutter = worker()
  cutter=worker()
  #sapply(temp,function(x) new_user_word(cutter,x,"n"))
  for(xj in 1:length(word_DB)){
    new_user_word(cutter,word_DB[xj],"n")
  }
  
  
  
  jieba_lineq = {}
  lineq_data = tolower(lineq_data)
  
  #remove all punctuation 
  #remove all punctuation except comma[^[:alnum:],]
  lineq_data = gsub('[^[:alnum:]]','',lineq_data)
  
  tryCatch({
    for(i in 1:length(lineq_data)){
      temp = segment(lineq_data[i], cutter)
      jieba_lineq = c(jieba_lineq,temp)
      print(paste0('jiebar :',i/length(lineq_data)*100,'%'))
    }
  }, error = function(e) {
    conditionMessage(e) # 這就會是"demo error"
  })
  
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
  
  write.csv(jieba_lineq_cdf,paste0('lineq/',format(Sys.time(), "%Y_%m_%d_%H_%M_%OS"),'jieba',forum_name,'_',last,'_',recent,'.csv'),row.names=F)
  
  tmp = tmp[,c('company','最終比對結果')]
  tmp2 = tmp2[,1:2]
  colnames(tmp) = c('before','after')
  colnames(tmp2) = c('before','after')
  tmp3 = rbind(tmp,tmp2)
  
  inter_list= intersect(jieba_lineq_cdf[,1],word_DB)
  lineq2 = jieba_lineq_cdf[which(jieba_lineq_cdf[,1] %in% inter_list),]
  
  ##讀取剔除表
  word_remove = read.table("D:\\abc\\wjhong\\projects\\internet_volume\\應剔除字串.txt")
  word_remove = word_remove[,1]
  lineq2 = lineq2[which(!(lineq2[,1] %in% word_remove)),]
  
  write.csv(lineq2,paste0(start.time,'/',forum_name,'_',last,'_',recent,'交集結果.csv'),row.names=F)
  path<-"D:\\abc\\wjhong\\projects\\internet_volume\\output"
  setwd(path)  
  print(paste0('lineq ',forum_name,'爬蟲與分析結束'))
}

##'http://lineq.tw/search/question?q=%E6%B1%82%E8%81%B7%20%E6%9C%8D%E5%8B%99%E6%A5%AD&sort=date&sel=all'
#print(iconv(read_html(url) %>% html_nodes(".description_title") %>% html_text(),'utf8'))



