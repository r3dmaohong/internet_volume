##華航
path = choose.dir()
setwd(path)
line <- read.csv('line華航.csv',stringsAsFactors = F)
ptt <- read.csv('ptt華航.csv',stringsAsFactors = F)

colnames(line)
colnames(ptt)

head(line[,1])
tail(line[,1])
line$Date = substr(line$Date,1,10)
grep('華航',line$Content)
head(line$Content)

#install.packages("jiebaR")
Sys.setenv(homeDir="D:/洪維均")
library(jiebaR)

cutter=worker()

new_user_word(cutter,'華航',"n")
new_user_word(cutter,'這是一個分隔用的代號',"n")
line$Content = gsub(';:;:;','這是一個分隔用的代號',line$Content)

res = cutter[line$Content]
res = as.data.frame(table(res),stringsAsFactors = F) 
wd_freq <- res$Freq[which(res$res=='華航')]
reply_freq <- res$Freq[which(res$res=='這是一個分隔用的代號')]


ptt$Content = gsub('\n推','這是一個分隔用的代號',ptt$Content,fixed=T)
ptt$Content = gsub('\n→','這是一個分隔用的代號',ptt$Content,fixed=T)
ptt$Content = gsub('\n噓','這是一個分隔用的代號',ptt$Content,fixed=T)
res = cutter[ptt$Content]
res = as.data.frame(table(res),stringsAsFactors = F) 
wd_freq2 <- res$Freq[which(res$res=='華航')]
reply_freq2 <- res$Freq[which(res$res=='這是一個分隔用的代號')]

total_wd_freq = wd_freq + wd_freq2
total_reply_freq = reply_freq + reply_freq2

line_month <- substr(line$Date,1,7)
tmp <- as.data.frame(table(line_month),stringsAsFactors = F)
tmp$word_freq = 0
tmp$reply_freq = 0

for(i in 1:nrow(tmp)){
  res = cutter[line$Content[which(grepl(tmp$line_month[i],line$Date))]]
  res = as.data.frame(table(res),stringsAsFactors = F) 
  tmp$word_freq[i] <- res$Freq[which(res$res=='華航')]
  tmp$reply_freq[i] <- res$Freq[which(res$res=='這是一個分隔用的代號')]
}
colnames(tmp)[2] = 'article_freq'



write.csv(tmp,'line歷年狀況.csv',row.names=F)

##
ptt$year = substr(ptt$Date,21,24)
ptt$month = substr(ptt$Date,5,7)
ptt$month[which(ptt$month=='Apr')] = 4
ptt$month[which(ptt$month=='Dec')] = 12
ptt$month[which(ptt$month=='Feb')] = 2
ptt$month[which(ptt$month=='Jan')] = 1
ptt$month[which(ptt$month=='Jun')] = 6
ptt$month[which(ptt$month=='Mar')] = 3
ptt$month[which(ptt$month=='May')] = 5
ptt$month[which(ptt$month=='Nov')] = 11
ptt$month[which(ptt$month=='Oct')] = 10
ptt$month[which(ptt$month=='Sep')] = 9

for(i in 1:length(ptt$month)){
  if(!is.na(ptt$month[i])){
    if(nchar(ptt$month[i])==1){
      ptt$month[i] = paste0('0',ptt$month[i])
    }
  }
}

ptt$Date <- paste0(ptt$year,'.',ptt$month)
ptt_month <- paste0(ptt$year,'.',ptt$month)

tmp <- as.data.frame(table(ptt_month),stringsAsFactors = F)
tmp$word_freq = 0
tmp$reply_freq = 0

for(i in 1:nrow(tmp)){
  res = cutter[ptt$Content[which(grepl(tmp$ptt_month[i],ptt$Date))]]
  res = as.data.frame(table(res),stringsAsFactors = F) 
  tmp$word_freq[i] <- res$Freq[which(res$res=='華航')]
  tmp$reply_freq[i] <- res$Freq[which(res$res=='這是一個分隔用的代號')]
}
colnames(tmp)[2] = 'article_freq'


write.csv(tmp,'ptt歷年狀況.csv',row.names=F)
