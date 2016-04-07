rm(list = ls()) #去除工作空間中所有物件
gc() #記憶體釋放
path<-"D:\\abc\\wjhong\\projects\\internet_volume\\output"
setwd(path)
start.time<-Sys.time()
start.time = gsub(":","_",start.time)

mainDir = paste0('D:\\abc\\wjhong\\projects\\internet_volume\\output\\',start.time)
dir.create(file.path(mainDir), showWarnings = FALSE)
dir.create(file.path(paste0(mainDir,'\\union_output')), showWarnings = FALSE)

source('D:\\abc\\wjhong\\projects\\internet_volume\\rscript\\ptt_jiebar.R', print.eval  = TRUE)
source('D:\\abc\\wjhong\\projects\\internet_volume\\rscript\\lineq_jiebar.R', print.eval  = TRUE)
source('D:\\abc\\wjhong\\projects\\internet_volume\\rscript\\yahoo++_jiebar.R', print.eval  = TRUE)

##科大
if(TRUE){
  yahoo_crawler_jiebar('https://tw.answers.yahoo.com/search/search_result?p=科大&s=','yahoo科大',1,200,start.time)
  
  ##730 848
  #https://www.ptt.cc/bbs/V_ScHooL/index848.html
  ptt_crawler_jiebar('https://www.ptt.cc/bbs/V_ScHooL/index',730,848,start.time)
}

##服務業
if(FALSE){
  
  ptt_crawler_jiebar('https://www.ptt.cc/bbs/Finance/index',750,887,start.time)
  
  ptt_crawler_jiebar('https://www.ptt.cc/bbs/ServiceInfo/index',310,417,start.time)
  
  ptt_crawler_jiebar('https://www.ptt.cc/bbs/Salesperson/index',650,762,start.time)
  ptt_crawler_jiebar('https://www.ptt.cc/bbs/unemployed/index',50,113,start.time)
  ptt_crawler_jiebar('https://www.ptt.cc/bbs/hairdo/index',1050,1291,start.time)
  ptt_crawler_jiebar('https://www.ptt.cc/bbs/Depstore/index',900,946,start.time)
  ptt_crawler_jiebar('https://www.ptt.cc/bbs/Salary/index',1200,1921,start.time)
  ptt_crawler_jiebar('https://www.ptt.cc/bbs/Broker/index',150,211,start.time)
  ptt_crawler_jiebar('https://www.ptt.cc/bbs/Therapist/index156.html',110,156,start.time)
  ptt_crawler_jiebar('https://www.ptt.cc/bbs/pharmacist/index',229,329,start.time)
  ptt_crawler_jiebar('https://www.ptt.cc/bbs/Nurse/index',1000,1150,start.time)
  ptt_crawler_jiebar('https://www.ptt.cc/bbs/medache/index',430,476,start.time)
}

#lineq_crawler_jiebar(link,forum_name,min,max,start.time)
#http://lineq.tw/search/question?q=%E8%90%BD%E9%BB%9E&sort=date&sel=all&page=

#yahoo_crawler_jiebar(link,forum_name,min,max,start.time)
#yahoo_crawler_jiebar('https://tw.answers.yahoo.com/search/search_result?p=%E7%A7%91%E5%A4%A7&s=','yahootest',1,1,start.time)
#https://tw.answers.yahoo.com/search/search_result?p=%E7%A7%91%E5%A4%A7&s=

##整合所有檔案
path<-paste0("D:\\abc\\wjhong\\projects\\internet_volume\\output\\",start.time)
setwd(path)
csv_list = list.files(pattern="*.csv")
for(i in 1:length(csv_list)){
  temp = read.csv(csv_list[i],stringsAsFactors=F)
  colnames(temp) = c('詞彙','次數')
  if(i==1){
    all_temp = temp
  }else{
    all_temp = rbind(all_temp,temp)
  }
}

library(plyr)
temp = ddply(all_temp , '詞彙', summarize, 總次數=sum(次數))
temp = temp[order(-temp$總次數),]

now = format(Sys.time(), "%Y_%m_%d_%H_%M_%OS")

write.csv(temp,paste0('union_output/',now,'整合詞彙結果.csv'),row.names=F)
