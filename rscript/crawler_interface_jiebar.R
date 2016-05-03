rm(list = ls()) #�h���u�@�Ŷ����Ҧ�����
gc() #�O��������
path<-"D:\\abc\\wjhong\\projects\\internet_volume\\output"
setwd(path)
start.time = gsub(":","_",Sys.time())

mainDir = paste0('D:\\abc\\wjhong\\projects\\internet_volume\\output\\',start.time)
dir.create(file.path(mainDir), showWarnings = FALSE)
dir.create(file.path(paste0(mainDir,'\\union_output')), showWarnings = FALSE)
dir.create(file.path(paste0(mainDir,'\\����')), showWarnings = FALSE)

##�U����
source('D:\\abc\\wjhong\\projects\\internet_volume\\rscript\\ptt_jiebar.R', print.eval  = TRUE)
source('D:\\abc\\wjhong\\projects\\internet_volume\\rscript\\lineq_jiebar.R', print.eval  = TRUE)
source('D:\\abc\\wjhong\\projects\\internet_volume\\rscript\\yahoo++_jiebar.R', print.eval  = TRUE)

##jiebar����
source('D:\\abc\\wjhong\\projects\\internet_volume\\rscript\\function\\jiebar������X.R', print.eval  = TRUE)

##��j
if(TRUE){
  yahoo_crawler_jiebar('https://tw.answers.yahoo.com/search/search_result?p=��j&s=','yahoo��j',1,200,start.time)
  
  ##730 848
  #https://www.ptt.cc/bbs/V_ScHooL/index848.html
  ptt_crawler_jiebar('https://www.ptt.cc/bbs/V_ScHooL/index','ptt V_ScHooL',730,,start.time)
  ptt_crawler_jiebar('https://www.ptt.cc/bbs/graduate/index','ptt graduate',1200,,start.time)
  
  #lineq_crawler_jiebar('http://lineq.tw/tag/369/recent?page=','lineq��¾�Ш|',1,start.time)
  lineq_crawler_jiebar('http://lineq.tw/tag/369/recent?page=','lineq��¾�Ш|',,,start.time)
}

##�A�ȷ~
if(FALSE){
  ptt_crawler_jiebar('https://www.ptt.cc/bbs/Finance/index',,750,,start.time)
  ptt_crawler_jiebar('https://www.ptt.cc/bbs/ServiceInfo/index',,310,,start.time)
  ptt_crawler_jiebar('https://www.ptt.cc/bbs/Salesperson/index',,650,,start.time)
  ptt_crawler_jiebar('https://www.ptt.cc/bbs/unemployed/index',,50,,start.time)
  ptt_crawler_jiebar('https://www.ptt.cc/bbs/hairdo/index',,1050,,start.time)
  ptt_crawler_jiebar('https://www.ptt.cc/bbs/Depstore/index',,900,,start.time)
  ptt_crawler_jiebar('https://www.ptt.cc/bbs/Salary/index',,1200,,start.time)
  ptt_crawler_jiebar('https://www.ptt.cc/bbs/Broker/index',,150,,start.time)
  ptt_crawler_jiebar('https://www.ptt.cc/bbs/Therapist/index',,110,,start.time)
  ptt_crawler_jiebar('https://www.ptt.cc/bbs/pharmacist/index',,229,,start.time)
  ptt_crawler_jiebar('https://www.ptt.cc/bbs/Nurse/index',,1000,,start.time)
  ptt_crawler_jiebar('https://www.ptt.cc/bbs/medache/index',,430,,start.time)
  
  
  lineq_crawler_jiebar('http://lineq.tw/tag/1647/recent?page=','lineq��|',,,start.time)
  lineq_crawler_jiebar('http://lineq.tw/tag/1127/recent?page=','lineq����',,,start.time)
  lineq_crawler_jiebar('http://lineq.tw/tag/182/recent?page=','lineq�ɲ߯Z',,,start.time)
}

#lineq_crawler_jiebar(link,forum_name,min,max,start.time)
#http://lineq.tw/search/question?q=%E8%90%BD%E9%BB%9E&sort=date&sel=all&page=

#yahoo_crawler_jiebar(link,forum_name,min,max,start.time)
#yahoo_crawler_jiebar('https://tw.answers.yahoo.com/search/search_result?p=%E7%A7%91%E5%A4%A7&s=','yahootest',1,1,start.time)
#https://tw.answers.yahoo.com/search/search_result?p=%E7%A7%91%E5%A4%A7&s=

##��X�Ҧ��ɮ�
sc_or_com <- function(n){
  if(n=='�Ǯ�'){
    ##�ǮմN�ഫ�M�᭫�s��@��freq
    path<-paste0("D:\\abc\\wjhong\\projects\\internet_volume\\output\\",start.time)
    setwd(path)
    csv_list = list.files(pattern="*.csv")
    for(i in 1:length(csv_list)){
      temp = read.csv(csv_list[i],stringsAsFactors=F)
      colnames(temp) = c('���J','����')
      if(i==1){
        all_temp = temp
      }else{
        all_temp = rbind(all_temp,temp)
      }
    }
    
    tmp2 = read.csv('D:\\abc\\wjhong\\projects\\school_performence_analysis\\�ǮզW�٥��W�ƪ���.csv',stringsAsFactors=F)
    tmp2[,1] = tolower(tmp2[,1])
    all_temp$out=''
    for(i in 1:nrow(all_temp)){
      if(toString(tmp2[which(tmp2[,1]==all_temp[i,1]),2])!=''){
        all_temp[i,1] = tmp2[which(tmp2[,1]==all_temp[i,1]),2][1]
        print(all_temp[i,1])
      }else if(toString(tmp2[which(tmp2[,2]==all_temp[i,1]),2])!=''){
        
      }else{
        all_temp$out[i] = 1
      }
    }
    ##�@����
    write.csv(all_temp[which(all_temp$out==1),],'����\\���C�J���j�ǦW�٨Ѭd��.csv',row.names=F)
    
    all_temp = all_temp[which(all_temp$out!=1),]
    
    
    library(plyr)
    temp = ddply(all_temp , '���J', summarize, �`����=sum(����))
    temp = temp[order(-temp$�`����),]
    
    now = format(Sys.time(), "%Y_%m_%d_%H_%M_%OS")
    
    write.csv(temp,paste0('union_output/',now,'��X���J���G.csv'),row.names=F)
    temp = temp[which(temp[,1]!=''),]
    return(temp)
  }else if(n=='���q'){
    ##���q�N������ഫ�F
    ##�H�K�����D
    path<-paste0("D:\\abc\\wjhong\\projects\\internet_volume\\output\\",start.time)
    setwd(path)
    csv_list = list.files(pattern="*.csv")
    for(i in 1:length(csv_list)){
      temp = read.csv(csv_list[i],stringsAsFactors=F)
      colnames(temp) = c('���J','����')
      if(i==1){
        all_temp = temp
      }else{
        all_temp = rbind(all_temp,temp)
      }
    }
    
    library(plyr)
    temp = ddply(all_temp , '���J', summarize, �`����=sum(����))
    temp = temp[order(-temp$�`����),]
    
    now = format(Sys.time(), "%Y_%m_%d_%H_%M_%OS")
    dir.create('union_output', showWarnings = FALSE)
    write.csv(temp,paste0('union_output/',now,'��X���J���G.csv'),row.names=F)
    
    comp_name = read.csv('D:\\abc\\wjhong\\projects\\school_performence_analysis\\__�B�z�᤽�q�W��.csv',stringsAsFactors=F)
    tmp = temp
    tmp$���� = ''
    for(i in 1:nrow(tmp)){
      if(toString(which(tolower(comp_name$company)==tmp$���J[i]))!=''){
        tmp$����[i] = comp_name$�̲פ�ﵲ�G[which(tolower(comp_name$company)==tmp$���J[i])][1]
      }else if(toString(which(tolower(comp_name$�̲פ�ﵲ�G)==tmp$���J[i]))!=''){
        tmp$����[i] = comp_name$�̲פ�ﵲ�G[which(tolower(comp_name$�̲פ�ﵲ�G)==tmp$���J[i])][1]
      }else{
        tmp$����[i] = ''
      }
    }
    school_name = read.csv('D:\\abc\\wjhong\\projects\\school_performence_analysis\\�ǮզW�٥��W�ƪ���.csv',stringsAsFactors=F)
    for(i in 1:nrow(tmp)){
      if(toString(which(tolower(school_name$trim���l)==tmp$���J[i]))!='' & tmp$����[i]==''){
        tmp$����[i] = school_name$������[which(tolower(school_name$trim���l)==tmp$���J[i])][1]
      }else if(toString(which(tolower(school_name$������)==tmp$���J[i]))!='' & tmp$����[i]==''){
        tmp$����[i] = school_name$������[which(tolower(school_name$������)==tmp$���J[i])][1]
      }else{
      }
    }
    remove_text = read.table("D:\\abc\\wjhong\\projects\\internet_volume\\���簣�r��.txt")
    tmp = tmp[which(!tmp[,1] %in% remove_text$V1),]
    ##�O�d��r��
    write.csv(tmp,paste0('union_output/',now,'�O�d��r�ꤽ�q��ӵ��J�W���ഫ�ᵲ�G.csv'),row.names=F)
    
    tmp = tmp[which(tmp$����!=''),]
    tmp = ddply(tmp , '����', summarize, �`����=sum(�`����))
    tmp = tmp[order(-tmp$�`����),]
    tmp = tmp[which(tmp$����!="�L�k�P�_"),]
    
    write.csv(tmp,paste0('union_output/',now,'���q��ӵ��J�W���ഫ�ᵲ�G.csv'),row.names=F)
    
    return(temp)
  }else{
    n <- readline(prompt="��J[�Ǯ�] or [���q]: ")
    sc_or_com(n)
  }
}

n <- readline(prompt="��J[�Ǯ�] or [���q]: ")
output <- sc_or_com(n)



##�j�ǦW��
college = read.table('D:\\abc\\wjhong\\projects\\internet_volume\\�j�ǦW��.txt')

##�줣�b�j�ǦW�椺
nc = output[which(!(output[,1] %in% college[,1])),]
write.csv(nc,'union_output/20160425��j��X���J���G.csv',row.names=F)


##���q
comp = read.table('D:\\abc\\wjhong\\projects\\internet_volume\\���簣�r��.txt')

##�줣�b�j�ǦW�椺
nc = output[which(!(output[,1] %in% comp[,1])),]
write.csv(nc,'union_output/20160408�A�ȷ~��X���J���G.csv',row.names=F)


##��ʾ�z��
if(F){
  path<-paste0("D:\\abc\\wjhong\\projects\\internet_volume\\output\\���q��ʾ�z�U�ɮ�")
  setwd(path)
  csv_list = list.files(pattern="*.csv")
  for(i in 1:length(csv_list)){
    temp = read.csv(csv_list[i],stringsAsFactors=F)
    colnames(temp) = c('���J','����')
    if(i==1){
      all_temp = temp
    }else{
      all_temp = rbind(all_temp,temp)
    }
  }
  
  library(plyr)
  temp = ddply(all_temp , '���J', summarize, �`����=sum(����))
  temp = temp[order(-temp$�`����),]
  
  now = format(Sys.time(), "%Y_%m_%d_%H_%M_%OS")
  dir.create('union_output', showWarnings = FALSE)
  write.csv(temp,paste0('union_output/',now,'��X���J���G.csv'),row.names=F)
  
  comp_name = read.csv('D:\\abc\\wjhong\\projects\\school_performence_analysis\\__�B�z�᤽�q�W��.csv',stringsAsFactors=F)
  tmp = temp
  tmp$���� = ''
  for(i in 1:nrow(tmp)){
    if(toString(which(tolower(comp_name$company)==tmp$���J[i]))!=''){
      tmp$����[i] = comp_name$�̲פ�ﵲ�G[which(tolower(comp_name$company)==tmp$���J[i])][1]
    }else if(toString(which(tolower(comp_name$�̲פ�ﵲ�G)==tmp$���J[i]))!=''){
      tmp$����[i] = comp_name$�̲פ�ﵲ�G[which(tolower(comp_name$�̲פ�ﵲ�G)==tmp$���J[i])][1]
    }else{
      tmp$����[i] = ''
    }
  }
  #school_name = read.csv('D:\\abc\\wjhong\\projects\\school_performence_analysis\\�ǮզW�٥��W�ƪ���.csv',stringsAsFactors=F)
  #for(i in 1:nrow(tmp)){
  #  if(toString(which(tolower(school_name$trim���l)==tmp$���J[i]))!='' & tmp$����[i]==''){
  #    tmp$����[i] = school_name$������[which(tolower(school_name$trim���l)==tmp$���J[i])][1]
  #  }else if(toString(which(tolower(school_name$������)==tmp$���J[i]))!='' & tmp$����[i]==''){
  #    tmp$����[i] = school_name$������[which(tolower(school_name$������)==tmp$���J[i])][1]
  #  }else{
  #  }
  #}
  remove_text = read.table("D:\\abc\\wjhong\\projects\\internet_volume\\���簣�r��.txt")
  tmp = tmp[which(!tmp[,1] %in% remove_text$V1),]
  ##�O�d��r��
  write.csv(tmp,paste0('union_output/',now,'�O�d��r�ꤽ�q��ӵ��J�W���ഫ�ᵲ�G.csv'),row.names=F)
  
  tmp = tmp[which(tmp$����!=''),]
  tmp = ddply(tmp , '����', summarize, �`����=sum(�`����))
  tmp = tmp[order(-tmp$�`����),]
  tmp = tmp[which(tmp$����!="�L�k�P�_"),]
  
  write.csv(tmp,paste0('union_output/',now,'���q��ӵ��J�W���ഫ�ᵲ�G.csv'),row.names=F)
}