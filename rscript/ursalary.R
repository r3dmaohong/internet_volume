rm(list = ls()) #�h���u�@�Ŷ����Ҧ�����
gc() #�O��������
#path<-"C:\\Documents and Settings\\wjhong\\�ୱ\\��������"
setwd(path)
start.time<-Sys.time()

library(rvest)
library(plyr)

result_export_all = data.frame('¾�ȥN�X'=numeric(),'¾�ȦW��'=character(),'1�~�H�U'=character(),'1~3�~'=character(),'3~5�~'=character(),'5~7�~'=character(),'7�~�H�W'=character(),'�u�@���e'=character(),'�����M�~�ҷ�'=character(),'����¾�P�o�i'=character(),stringsAsFactors=F)
##�Q�ΦC�����o�U�峹id
temp_link = c()
for(x in 1:10){
  url <- paste('http://www.ursalary.com/search.php?&p=',x,sep='')
  
  #result_export = data.frame('¾�ȥN�X'=numeric(),'¾�ȦW��'=character(),'1�~�H�U'=character(),'1~3�~'=character(),'3~5�~'=character(),'5~7�~'=character(),'7�~�H�W'=character(),'�u�@���e'=character(),'�����M�~�ҷ�'=character(),'����¾�P�o�i'=character(),stringsAsFactors=F)
  
  title_css = read_html(url) %>% html_nodes("a") %>% html_attr("href")
  title_css = unique(title_css)
  title_css = title_css[which(grepl('detail',title_css))]
  title_css = paste0('http://www.ursalary.com/',title_css)
  temp_link = c(temp_link, title_css)
  sleep_time = runif(1,2,5)
  print(paste0('��',x,'���C��link�������'))
  print(paste0('loading... �Ȱ�',sleep_time,'��'))
  Sys.sleep(sleep_time)
  gc() #�O��������
}
##�ϥΧ�X�Ӫ�link�@����
for(i in 1:length(temp_link)){
  url <- temp_link[i]
  title_css = read_html(url) %>% html_nodes("ul li") %>% html_text()
  utf8_text_title <- toUTF8(title_css) ## �N���U�Ӫ����D�ন UTF8
}


write.csv(result_NT,'����.csv',row.names=F)