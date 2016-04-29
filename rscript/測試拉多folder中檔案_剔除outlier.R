path<-paste0("D:\\abc\\wjhong\\projects\\internet_volume\\output\\爬蟲分詞後資料")
setwd(path)

folder_list = list.dirs(path = path, full.names = TRUE, recursive = TRUE)

total_list = c()
for(i in 2:length(folder_list)){
  csv_list = list.files(path=folder_list[i] ,full.names=T ,pattern="*.csv")
  csv_list = csv_list[which(grepl('名詞',csv_list))]
  total_list = c(total_list,csv_list)
}

for(i in 1:length(total_list)){
  temp = read.csv(total_list[i],stringsAsFactors=F)
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
write.csv(temp,paste0('D:\\abc\\wjhong\\projects\\internet_volume\\output\\爬蟲分詞後資料\\',now,'整合名詞詞彙結果.csv'),row.names=F)


#y= temp$總次數[!temp$總次數 %in% boxplot.stats(temp$總次數)$out]
