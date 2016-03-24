rm(list = ls()) #�h���u�@�Ŷ����Ҧ�����
gc() #�O��������
path<-"D:\\abc\\wjhong\\projects\\internet_volume"
setwd(path)
start.time<-Sys.time()

library(rvest)
library(plyr)
library(XML)
library(RCurl)
library(SnowballC)
library(cluster)   
library(data.table) 
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

links_data_ptt = {}
##��Ū���U���峹�����}
for(i in 300:415){
  tmp <- paste(i, '.html', sep='')
  #https://www.ptt.cc/bbs/Salary/index1896.html
  url <- paste('https://www.ptt.cc/bbs/ServiceInfo/index', tmp, sep='')
  title_css = read_html(url) %>% html_nodes(".title") %>% html_nodes("a") %>% html_attr('href')
  links_data_ptt = c(links_data_ptt,title_css)
  gc() #�O��������
  print(paste0('ptt��',i,'��'))
  Sys.sleep(runif(1,2,5))
}
##�簣�����������}(�D��ɤw�����A���L�b�P�_�@��)
links_data_ptt =  links_data_ptt[which(grepl('/bbs/ServiceInfo/',links_data_ptt))]

ptt_data = {}
##�N��X�����}�i�檦��
for(i in 1:length(links_data_ptt)){
  tryCatch({
    url = paste0('https://www.ptt.cc',links_data_ptt[i])
    title_css = read_html(url) %>% html_nodes("#main-content") %>% html_text()
    utf8_text_title <- toUTF8(title_css)
    
    ##�h��id
    title_css1 = read_html(url) %>% html_nodes("span") %>% html_text()
    utf8_text_title1 <- toUTF8(title_css1)
    id_delete = utf8_text_title1[which(grepl(': ',utf8_text_title1))-1]
    id_delete = c(id_delete, utf8_text_title1[1:8])
    for(x in 1:length(id_delete)){
      utf8_text_title=gsub(id_delete[x],'',utf8_text_title)
    }
    
    luodian = utf8_text_title#[which(grepl('���I',utf8_text_title))]
    
    ##: �e��ӥh��
    ptt_data = c(ptt_data,luodian)
    ##which contains ���I
    gc() #�O��������
    Sys.sleep(runif(1,2,5))
    print(paste0('ptt��',i,'��  ',i/length(links_data_ptt)*100,'%'))
  }, error = function(e) {
    print(paste0('ptt��',i,'�� ���� ',i/length(links_data_ptt)*100,'%'))
    Sys.sleep(runif(1,2,5))
  })
  
}
length(ptt_data) #1189
write.csv(ptt_data,'ptt_serviceinfo_371_415.csv')


library(jiebaR)
cutter = worker()
jieba_ptt = {}
ptt_data = tolower(ptt_data)
for(i in 1:length(ptt_data)){
  temp = segment(ptt_data[i], cutter)
  jieba_ptt = c(jieba_ptt,temp)
  print(paste0('jiebar :',i/length(ptt_data)*100,'%'))
}
##�h����r
jieba_ptt = jieba_ptt[which(nchar(jieba_ptt)>1)]
##�h���ƭȻPid
jieba_ptt = jieba_ptt[which(!grepl('[0-9]',jieba_ptt))]


jieba_ptt_df = as.data.frame(jieba_ptt)
jieba_ptt_cdf = ddply(jieba_ptt_df , c('jieba_ptt'), nrow)
jieba_ptt_cdf = jieba_ptt_cdf[order(-jieba_ptt_cdf$V1),]
#write.csv(jieba_ptt_cdf,paste0('output/ptt/',format(Sys.time(), "%Y_%d_%b"),'jieba_ptt_output_tolower_temp.csv'),row.names=F)

##��������Stolower.. once again
jieba_ptt_cdf[,1] = tolower(jieba_ptt_cdf[,1])
jieba_ptt_cdf = ddply(jieba_ptt_cdf , c('jieba_ptt'), summarize, sum(V1))
jieba_ptt_cdf = jieba_ptt_cdf[order(-jieba_ptt_cdf$..1,jieba_ptt_cdf$jieba_ptt),]

write.csv(jieba_ptt_cdf,paste0('output/ptt/',format(Sys.time(), "%Y_%d_%b"),'jieba_ptt_output_tolower_last.csv'),row.names=F)

##���e���쪺��ʶ�g���q�W��
temp = read.csv(file.choose(),stringsAsFactors=F)
temp = temp[,1]
temp = tolower(temp)

inter_list= intersect(jieba_ptt_cdf[,1],temp)
ptt2 = jieba_ptt_cdf[which(jieba_ptt_cdf[,1] %in% inter_list),]
write.csv(ptt2,'���ե涰���G.csv',row.names=F)