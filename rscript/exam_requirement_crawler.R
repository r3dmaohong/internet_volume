rm(list = ls()) #�h���u�@�Ŷ����Ҧ�����
gc() #�O��������
#path<-"D:\\abc\\wjhong\\projects\\school_performence_analysis"
#setwd(path)

library(httr)
library(rvest)
library(tmcn)

options(java.parameters = "-Xmx4000m")
jgc <- function()
{
  .jcall("java/lang/System", method = "gc")
}    
##getting urls
all_links = c()
for(i in 1:19){
  if(i<10){
    url = paste0('http://campus4.ncku.edu.tw/uac/cross_search/class_info/D0',i,'.html')
    
    ##���ǭ������D, �[�J��encoding
    total_css = read_html(url,encoding="ISO-8859-1")
    link_css = total_css %>% html_nodes("td a") %>% html_attr("href")
    link_css = unlist(lapply(link_css,function(link_css){
      unlist(strsplit(link_css,'/'))[length(unlist(strsplit(link_css,'/')))]
    }))
    
    links = paste0('http://campus4.ncku.edu.tw/uac/cross_search/dept_info/',link_css)
    
    all_links = c(all_links,links)
    cat(paste0('\r',i/19*100,'%'))
    gc()
    Sys.sleep(runif(1,2,5))
  }else{
    url = paste0('http://campus4.ncku.edu.tw/uac/cross_search/class_info/D',i,'.html')
    
    ##���ǭ������D, �[�J��encoding
    total_css = read_html(url,encoding="ISO-8859-1")
    link_css = total_css %>% html_nodes("td a") %>% html_attr("href")
    link_css = unlist(lapply(link_css,function(link_css){
      unlist(strsplit(link_css,'/'))[length(unlist(strsplit(link_css,'/')))]
    }))    
    links = paste0('http://campus4.ncku.edu.tw/uac/cross_search/dept_info/',link_css)
    gc()
    all_links = c(all_links,links)
    cat(paste0('\r','����U���������} : ',format(round(i/19*100,2),nsmall=2),'%',paste(replicate(50, " "), collapse = "")))
    Sys.sleep(runif(1,2,5))
  }
  
}

##getting datas
output_df = data.frame('�Ǯ�'=character(),'�Ǩt'=character(),'�s��'=character(),'�N�X'=character(),'�Ǭ��O����έ^�yť�O�˩w�з�'=character(),'�Ǭ��O����έ^�yť�O�˩w�з�2'=character(),'���w�Ҹձĭp��ؤΤ�k'=character(),'�P���Ѱu�覡1'=character(),'�P���Ѱu�覡2'=character(),'�P���Ѱu�覡3'=character(),'��t����'=character(),stringsAsFactors=F)
#total_css <-html(url,encoding="gb2312")

all_links = unique(all_links)
error = 0
x=1
for(i in 1:length(all_links)){
#for(i in 1:92){
  tryCatch({
    total_css = read_html(all_links[i],encoding="big5")
    content_css = total_css %>% html_nodes("tr td") %>% html_text()
    title_css = total_css %>% html_nodes("title") %>% html_text()
    ##���ýX���D
    ##content_css = iconv(content_css,'utf8')
    content_css = toUTF8(content_css)
    content_css = gsub(' ','',content_css)
    #content_css = gsub('    ','',content_css)
    title_css = toUTF8(title_css)
    cat(paste0('\r',title_css,' : ',format(round(i/length(all_links)*100,2),nsmall=2),'%',paste(replicate(50, " "), collapse = "")))
    title_css = unlist(strsplit(title_css,' -'))[1]
    
    Sys.sleep(runif(1,2,4))
    
    output_df[x,] = c(title_css,content_css[1],all_links[i],unlist(strsplit(unlist(strsplit(all_links[i],'/')),'[.]'))[length(unlist(strsplit(unlist(strsplit(all_links[i],'/')),'[.]')))-1],content_css[2],content_css[9],content_css[3],content_css[5],content_css[8],content_css[11],content_css[6])
    
    if(is.na(output_df[x-1,1])){
      output_df[x-1,] = c('���~','���~',all_links[i-1],unlist(strsplit(unlist(strsplit(all_links[i-1],'/')),'[.]'))[length(unlist(strsplit(unlist(strsplit(all_links[i-1],'/')),'[.]')))-1],'���~','���~','���~','���~','���~','���~','���~')
    }
    gc()
    x = x + 1
  }, error = function(e) {
    #output_df[x,] <<- c('���~','���~',all_links[i],unlist(strsplit(unlist(strsplit(all_links[i],'/')),'[.]'))[length(unlist(strsplit(unlist(strsplit(all_links[i],'/')),'[.]')))-1],'���~','���~','���~','���~','���~','���~','���~')
    x <<- x + 1
    error <<- error + 1
    print(paste0(error,'�����~'))
    #print(paste0('���~�v�� ', round(error/length(all_links),2)))
    gc()
  })
}

getwd()
write.csv(output_df,'test.csv',row.names=F)
input_df = read.csv('test.csv',stringsAsFactors=F)
for(i in 1:ncol(input_df)){
  ##�Sfixed�L�k
  input_df[,i] = gsub('<U+00A0>','',input_df[,i],fixed=T)
  #input_df[,i] = gsub('\n','',input_df[,i])
  input_df[,i] = gsub('�@','',input_df[,i])
}
write.csv(output_df,'test3.csv',row.names=F)

input_df = read.csv('test3.csv',stringsAsFactors=F)

result_df=data.frame("�ǮզW��"=character(),"�t�զW�١]²���^"=character(),"²�����С]�s���^"=character(),"²���s�X"=character(),"105�~�Ǵ��B�^ť�˩w�з�:���"=character(),"105�~�Ǵ��B�^ť�˩w�з�:�^��"=character(),"105�~�Ǵ��B�^ť�˩w�з�:�ƾ�"=character(),"105�~�Ǵ��B�^ť�˩w�з�:���|"=character(),"105�~�Ǵ��B�^ť�˩w�з�:�۵M"=character(),"105�~�Ǵ��B�^ť�˩w�з�:�`�Ť�"=character(),"105�~�Ǵ��B�^ť�˩w�з�:�^ť"=character(),"105�~���ұĭp�з��v��:���"=character(),"105�~���ұĭp�з��v��:�^��"=character(),"105�~���ұĭp�з��v��:�ƾǤA"=character(),"105�~���ұĭp�з��v��:���v"=character(),"105�~���ұĭp�з��v��:�a�z"=character(),"105�~���ұĭp�з��v��:����"=character(),"105�~���ұĭp�з��v��:�ƾǥ�"=character(),"105�~���ұĭp�з��v��:���z"=character(),"105�~���ұĭp�з��v��:�ƾ�"=character(),"105�~���ұĭp�з��v��:�ͪ�"=character(),"105�~���ұĭp�з��v��:�N��"=character(),"�P���Ѱu�覡:���1"=character(),"�P���Ѱu�覡:���2"=character(),"�P���Ѱu�覡:���3"=character(),"��t����"=character(),stringsAsFactors=F)

for(i in 1:nrow(input_df)){
  result_df[i,1] = input_df[i,1]
  result_df[i,2] = input_df[i,2]
  result_df[i,3] = input_df[i,3]
  result_df[i,4] = input_df[i,4]
  if(grepl('���',input_df[i,5])){
    result_df[i,5] = substr(input_df[i,5],unlist(gregexpr(pattern ='���',input_df[i,5]))+3,unlist(gregexpr(pattern ='���',input_df[i,5]))+4)
  }
  if(grepl('�^��',input_df[i,5])){
    result_df[i,6] = substr(input_df[i,5],unlist(gregexpr(pattern ='�^��',input_df[i,5]))+3,unlist(gregexpr(pattern ='�^��',input_df[i,5]))+4)
  }
  if(grepl('�ƾ�',input_df[i,5])){
    result_df[i,7] = substr(input_df[i,5],unlist(gregexpr(pattern ='�ƾ�',input_df[i,5]))+3,unlist(gregexpr(pattern ='�ƾ�',input_df[i,5]))+4)
  }
  if(grepl('���|',input_df[i,5])){
    result_df[i,8] = substr(input_df[i,5],unlist(gregexpr(pattern ='���|',input_df[i,5]))+3,unlist(gregexpr(pattern ='���|',input_df[i,5]))+4)
  }
  if(grepl('�۵M',input_df[i,5])){
    result_df[i,9] = substr(input_df[i,5],unlist(gregexpr(pattern ='�۵M',input_df[i,5]))+3,unlist(gregexpr(pattern ='�۵M',input_df[i,5]))+4)
  }
  if(grepl('�`�Ť�',input_df[i,5])){
    result_df[i,10] = substr(input_df[i,5],unlist(gregexpr(pattern ='�`�Ť�',input_df[i,5]))+4,unlist(gregexpr(pattern ='�`�Ť�',input_df[i,5]))+5)
  }
  if(grepl('��',input_df[i,6])){
    result_df[i,11] = 'B'
  }else if(grepl('��',input_df[i,6])){
    result_df[i,11] = 'C'
  }else if(grepl('��',input_df[i,6])){
    result_df[i,11] = 'A'
  }else{
    
  }
  
  if(grepl('���',input_df[i,7])){
    result_df[i,12] = substr(input_df[i,7],unlist(gregexpr(pattern ='���',input_df[i,7]))+3,unlist(gregexpr(pattern ='���',input_df[i,7]))+6)
  }
  if(grepl('�^��',input_df[i,7])){
    result_df[i,13] = substr(input_df[i,7],unlist(gregexpr(pattern ='�^��',input_df[i,7]))+3,unlist(gregexpr(pattern ='�^��',input_df[i,7]))+6)
  }
  if(grepl('�ƾǤA',input_df[i,7])){
    result_df[i,14] = substr(input_df[i,7],unlist(gregexpr(pattern ='�ƾǤA',input_df[i,7]))+4,unlist(gregexpr(pattern ='�ƾǤA',input_df[i,7]))+7)
  }
  if(grepl('���v',input_df[i,7])){
    result_df[i,15] = substr(input_df[i,7],unlist(gregexpr(pattern ='���v',input_df[i,7]))+3,unlist(gregexpr(pattern ='���v',input_df[i,7]))+6)
  }
  if(grepl('�a�z',input_df[i,7])){
    result_df[i,16] = substr(input_df[i,7],unlist(gregexpr(pattern ='�a�z',input_df[i,7]))+3,unlist(gregexpr(pattern ='�a�z',input_df[i,7]))+6)
  }
  if(grepl('����',input_df[i,7])){
    result_df[i,17] = substr(input_df[i,7],unlist(gregexpr(pattern ='����',input_df[i,7]))+6,unlist(gregexpr(pattern ='����',input_df[i,7]))+9)
  }
  if(grepl('�ƾǥ�',input_df[i,7])){
    result_df[i,18] = substr(input_df[i,7],unlist(gregexpr(pattern ='�ƾǥ�',input_df[i,7]))+4,unlist(gregexpr(pattern ='�ƾǥ�',input_df[i,7]))+7)
  }
  if(grepl('���z',input_df[i,7])){
    result_df[i,19] = substr(input_df[i,7],unlist(gregexpr(pattern ='���z',input_df[i,7]))+3,unlist(gregexpr(pattern ='���z',input_df[i,7]))+6)
  }
  if(grepl('�ƾ�',input_df[i,7])){
    result_df[i,20] = substr(input_df[i,7],unlist(gregexpr(pattern ='�ƾ�',input_df[i,7]))+3,unlist(gregexpr(pattern ='�ƾ�',input_df[i,7]))+6)
  }
  if(grepl('�ͪ�',input_df[i,7])){
    result_df[i,21] = substr(input_df[i,7],unlist(gregexpr(pattern ='�ͪ�',input_df[i,7]))+3,unlist(gregexpr(pattern ='�ͪ�',input_df[i,7]))+6)
  }
  if(grepl('�N��',input_df[i,7])){
    result_df[i,22] = substr(input_df[i,7],unlist(gregexpr(pattern ='�N��',input_df[i,7]))+7,unlist(gregexpr(pattern ='�N��',input_df[i,7]))+10)
  }
  result_df[i,23] = input_df[i,8]
  result_df[i,24] = input_df[i,9]
  result_df[i,25] = input_df[i,10]
  result_df[i,26] = input_df[i,11]
}
for(i in 1:ncol(result_df)){
  result_df[which(is.na(result_df[,i])),i] = '---'
}

write.csv(result_df,'test4.csv',row.names=F)

library(XML)
library(RCurl)

get_url = getURL(url,encoding = "gb2312")
get_url_parse = htmlParse(get_url, encoding = "gb2312")

get_url = getURL(url,encoding = "gbk")
get_url_parse = htmlParse(get_url, encoding = "gbk")