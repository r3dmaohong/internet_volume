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
  
  last=links_data_yahoo[which(grepl('qid',links_data_yahoo))]
  last = last[length(last)]
  last = gsub("[^0-9]", "",last)
  last = substr(last,1,8)
  
  recent = gsub('-','',strsplit(toString(Sys.time()),' ')[[1]][1])
  
  write.csv(temp_yahoo_data,paste0('yahoo/',forum_name,'_',recent,'_',last,'.csv'))
  
  yahoo_data = temp_yahoo_data
  ##yahoo_data = read.csv(file.choose(),stringsAsFactors=F)
  ##yahoo_data = yahoo_data[,2]
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
  
  
  
  jieba_yahoo = {}
  yahoo_data = tolower(yahoo_data)
  ##瓂\xa2\026
  #remove all punctuation 
  #remove all punctuation except comma[^[:alnum:],]
  yahoo_data = gsub('[^[:alnum:]]','',yahoo_data)
  
  tryCatch({
    for(i in 1:length(yahoo_data)){
      temp = segment(yahoo_data[i], cutter)
      jieba_yahoo = c(jieba_yahoo,temp)
      print(paste0('jiebar :',i/length(yahoo_data)*100,'%'))
    }
  }, error = function(e) {
    conditionMessage(e) # 這就會是"demo error"
  })

  
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
  
  write.csv(jieba_yahoo_cdf,paste0('yahoo/',format(Sys.time(), "%Y_%m_%d_%H_%M_%OS"),'jieba',forum_name,'_',recent,'_',last,'.csv'),row.names=F)
  
  tmp = tmp[,c('company','最終比對結果')]
  tmp2 = tmp2[,1:2]
  colnames(tmp) = c('before','after')
  colnames(tmp2) = c('before','after')
  tmp3 = rbind(tmp,tmp2)
  
  inter_list= intersect(jieba_yahoo_cdf[,1],word_DB)
  yahoo2 = jieba_yahoo_cdf[which(jieba_yahoo_cdf[,1] %in% inter_list),]
  
  ##讀取剔除表
  word_remove = read.table("D:\\abc\\wjhong\\projects\\internet_volume\\應剔除字串.txt")
  word_remove = word_remove[,1]
  yahoo2 = yahoo2[which(!(yahoo2[,1] %in% word_remove)),]
  
  write.csv(yahoo2,paste0(start.time,'/',forum_name,'_',recent,'_',last,'交集結果.csv'),row.names=F)
  path<-"D:\\abc\\wjhong\\projects\\internet_volume\\output"
  setwd(path)
  
  print(paste0('yahoo知識+ ',forum_name,'爬蟲與分析結束'))
}

