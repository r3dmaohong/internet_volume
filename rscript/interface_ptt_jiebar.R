rm(list = ls()) #去除工作空間中所有物件
gc() #記憶體釋放
path<-"D:\\abc\\wjhong\\projects\\internet_volume\\output"
setwd(path)
start.time<-Sys.time()

source('D:\\abc\\wjhong\\projects\\internet_volume\\rscript\\ptt_jiebar.R', print.eval  = TRUE)

ptt_crawler_jiebar('https://www.ptt.cc/bbs/Finance/index',750,887)

ptt_crawler_jiebar('https://www.ptt.cc/bbs/ServiceInfo/index',310,417)

ptt_crawler_jiebar('https://www.ptt.cc/bbs/Salesperson/index',650,762)
ptt_crawler_jiebar('https://www.ptt.cc/bbs/unemployed/index',50,113)
ptt_crawler_jiebar('https://www.ptt.cc/bbs/hairdo/index',1050,1291)
ptt_crawler_jiebar('https://www.ptt.cc/bbs/Depstore/index',900,946)
ptt_crawler_jiebar('https://www.ptt.cc/bbs/Salary/index',1200,1921)
ptt_crawler_jiebar('https://www.ptt.cc/bbs/Broker/index',150,211)
ptt_crawler_jiebar('https://www.ptt.cc/bbs/Therapist/index156.html',110,156)
ptt_crawler_jiebar('https://www.ptt.cc/bbs/pharmacist/index',229,329)
ptt_crawler_jiebar('https://www.ptt.cc/bbs/Nurse/index',1000,1150)
ptt_crawler_jiebar('https://www.ptt.cc/bbs/medache/index',430,476)


##整合所有檔案
#path<-"D:\\abc\\wjhong\\projects\\internet_volume\\output"
#setwd(path)
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

now = Sys.time()
now = gsub(':','_',gsub(' ','_',now))

write.csv(temp,paste0(now,'整合詞彙結果.csv'),row.names=F)
