rm(list = ls()) #去除工作空間中所有物件
gc() #記憶體釋放
path<-choose.dir()
if(!grepl('output',path)){
  dir.create(paste0(path,'\\output'), showWarnings = FALSE)
  path <- paste0(path,'\\output')
}

setwd(path)
start.time = gsub(":","_",Sys.time())

mainDir = paste0(path,start.time)
dir.create(file.path(mainDir), showWarnings = FALSE)
dir.create(file.path(paste0(mainDir,'\\union_output')), showWarnings = FALSE)
dir.create(file.path(paste0(mainDir,'\\紀錄')), showWarnings = FALSE)

##各網站
source('..\\rscript\\ptt_jiebar.R', print.eval  = TRUE)
source('..\\rscript\\lineq_jiebar.R', print.eval  = TRUE)
source('..\\rscript\\yahoo++_jiebar.R', print.eval  = TRUE)

##jiebar提取
source('..\\rscript\\function\\jiebar分詞輸出.R', print.eval  = TRUE)

if(T){
  lineq_crawler_jiebar('http://lineq.tw/search/question?q=%E8%8F%AF%E8%88%AA&sort=date&sel=all&page=','lineq華航',,70,start.time)
  ptt_crawler_jiebar('https://www.ptt.cc/bbs/Gossiping/index',,750,,start.time)
}


##科大
if(F){
  yahoo_crawler_jiebar('https://tw.answers.yahoo.com/search/search_result?p=科大&s=','yahoo科大',1,200,start.time)
  
  ##730 848
  #https://www.ptt.cc/bbs/V_ScHooL/index848.html
  ptt_crawler_jiebar('https://www.ptt.cc/bbs/V_ScHooL/index','ptt V_ScHooL',730,,start.time)
  ptt_crawler_jiebar('https://www.ptt.cc/bbs/graduate/index','ptt graduate',1200,,start.time)
  
  #lineq_crawler_jiebar('http://lineq.tw/tag/369/recent?page=','lineq技職教育',1,start.time)
  lineq_crawler_jiebar('http://lineq.tw/tag/369/recent?page=','lineq技職教育',,,start.time)
}

##服務業
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
  
  
  lineq_crawler_jiebar('http://lineq.tw/tag/1647/recent?page=','lineq醫院',,,start.time)
  lineq_crawler_jiebar('http://lineq.tw/tag/1127/recent?page=','lineq飯店',,,start.time)
  lineq_crawler_jiebar('http://lineq.tw/tag/182/recent?page=','lineq補習班',,,start.time)
}

#lineq_crawler_jiebar(link,forum_name,min,max,start.time)
#http://lineq.tw/search/question?q=%E8%90%BD%E9%BB%9E&sort=date&sel=all&page=

#yahoo_crawler_jiebar(link,forum_name,min,max,start.time)
#yahoo_crawler_jiebar('https://tw.answers.yahoo.com/search/search_result?p=%E7%A7%91%E5%A4%A7&s=','yahootest',1,1,start.time)
#https://tw.answers.yahoo.com/search/search_result?p=%E7%A7%91%E5%A4%A7&s=

##整合所有檔案
sc_or_com <- function(n,path=paste0("D:\\abc\\wjhong\\projects\\internet_volume\\output\\",start.time)){
  if(n=='學校'){
    ##學校就轉換然後重新算一次freq
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
    
    tmp2 = read.csv('D:\\abc\\wjhong\\projects\\school_performence_analysis\\學校名稱正規化表格.csv',stringsAsFactors=F)
    tmp2[,1] = tolower(tmp2[,1])
    ##英文踢掉好了太含糊
    tmp2 = tmp2[which(!grepl("[a-z]",tmp2[,1])),]
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
    ##作紀錄
    write.csv(all_temp[which(all_temp$out==1),],'紀錄\\未列入之大學名稱供查看.csv',row.names=F)
    
    all_temp = all_temp[which(all_temp$out!=1),]
    
    
    library(plyr)
    temp = ddply(all_temp , '詞彙', summarize, 總次數=sum(次數))
    temp = temp[order(-temp$總次數),]
    
    now = format(Sys.time(), "%Y_%m_%d_%H_%M_%OS")
    
    write.csv(temp,paste0('union_output/',now,'整合詞彙結果.csv'),row.names=F)
    temp = temp[which(temp[,1]!=''),]
    return(temp)
  }else if(n=='公司'){
    ##公司就不對照轉換了
    ##以免有問題
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
    dir.create('union_output', showWarnings = FALSE)
    write.csv(temp,paste0('union_output/',now,'整合詞彙結果.csv'),row.names=F)
    
    comp_name = read.csv('D:\\abc\\wjhong\\projects\\school_performence_analysis\\__處理後公司名稱.csv',stringsAsFactors=F)
    tmp = temp
    tmp$對應 = ''
    for(i in 1:nrow(tmp)){
      if(toString(which(tolower(comp_name$company)==tmp$詞彙[i]))!=''){
        tmp$對應[i] = comp_name$最終比對結果[which(tolower(comp_name$company)==tmp$詞彙[i])][1]
      }else if(toString(which(tolower(comp_name$最終比對結果)==tmp$詞彙[i]))!=''){
        tmp$對應[i] = comp_name$最終比對結果[which(tolower(comp_name$最終比對結果)==tmp$詞彙[i])][1]
      }else{
        tmp$對應[i] = ''
      }
    }
    school_name = read.csv('D:\\abc\\wjhong\\projects\\school_performence_analysis\\學校名稱正規化表格.csv',stringsAsFactors=F)
    for(i in 1:nrow(tmp)){
      if(toString(which(tolower(school_name$trim後原始)==tmp$詞彙[i]))!='' & tmp$對應[i]==''){
        tmp$對應[i] = school_name$對應表[which(tolower(school_name$trim後原始)==tmp$詞彙[i])][1]
      }else if(toString(which(tolower(school_name$對應表)==tmp$詞彙[i]))!='' & tmp$對應[i]==''){
        tmp$對應[i] = school_name$對應表[which(tolower(school_name$對應表)==tmp$詞彙[i])][1]
      }else{
      }
    }
    remove_text = read.table("D:\\abc\\wjhong\\projects\\internet_volume\\應剔除字串.txt")
    tmp = tmp[which(!tmp[,1] %in% remove_text$V1),]
    ##保留原字串
    write.csv(tmp,paste0('union_output/',now,'保留原字串公司對照詞彙名稱轉換後結果.csv'),row.names=F)
    
    tmp = tmp[which(tmp$對應!=''),]
    tmp = ddply(tmp , '對應', summarize, 總次數=sum(總次數))
    tmp = tmp[order(-tmp$總次數),]
    tmp = tmp[which(tmp$對應!="無法判斷"),]
    
    write.csv(tmp,paste0('union_output/',now,'公司對照詞彙名稱轉換後結果.csv'),row.names=F)
    
    return(temp)
  }else{
    n <- readline(prompt="輸入[學校] or [公司]: ")
    sc_or_com(n)
  }
}

n <- readline(prompt="輸入[學校] or [公司]: ")
output <- sc_or_com(n,'D:\\abc\\wjhong\\projects\\internet_volume\\output\\2016-04-07 14_06_45')



##大學名單
college = read.table('D:\\abc\\wjhong\\projects\\internet_volume\\大學名單.txt')

##抓不在大學名單內
nc = output[which(!(output[,1] %in% college[,1])),]
write.csv(nc,'union_output/20160615-2科大整合詞彙結果.csv',row.names=F)


##公司
comp = read.table('D:\\abc\\wjhong\\projects\\internet_volume\\應剔除字串.txt')

##抓不在大學名單內
nc = output[which(!(output[,1] %in% comp[,1])),]
write.csv(nc,'union_output/20160408服務業整合詞彙結果.csv',row.names=F)


##手動整理的
if(F){
  path<-paste0("D:\\abc\\wjhong\\projects\\internet_volume\\output\\公司手動整理各檔案")
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
  dir.create('union_output', showWarnings = FALSE)
  write.csv(temp,paste0('union_output/',now,'整合詞彙結果.csv'),row.names=F)
  
  comp_name = read.csv('D:\\abc\\wjhong\\projects\\school_performence_analysis\\__處理後公司名稱.csv',stringsAsFactors=F)
  tmp = temp
  tmp$對應 = ''
  for(i in 1:nrow(tmp)){
    if(toString(which(tolower(comp_name$company)==tmp$詞彙[i]))!=''){
      tmp$對應[i] = comp_name$最終比對結果[which(tolower(comp_name$company)==tmp$詞彙[i])][1]
    }else if(toString(which(tolower(comp_name$最終比對結果)==tmp$詞彙[i]))!=''){
      tmp$對應[i] = comp_name$最終比對結果[which(tolower(comp_name$最終比對結果)==tmp$詞彙[i])][1]
    }else{
      tmp$對應[i] = ''
    }
  }
  #school_name = read.csv('D:\\abc\\wjhong\\projects\\school_performence_analysis\\學校名稱正規化表格.csv',stringsAsFactors=F)
  #for(i in 1:nrow(tmp)){
  #  if(toString(which(tolower(school_name$trim後原始)==tmp$詞彙[i]))!='' & tmp$對應[i]==''){
  #    tmp$對應[i] = school_name$對應表[which(tolower(school_name$trim後原始)==tmp$詞彙[i])][1]
  #  }else if(toString(which(tolower(school_name$對應表)==tmp$詞彙[i]))!='' & tmp$對應[i]==''){
  #    tmp$對應[i] = school_name$對應表[which(tolower(school_name$對應表)==tmp$詞彙[i])][1]
  #  }else{
  #  }
  #}
  remove_text = read.table("D:\\abc\\wjhong\\projects\\internet_volume\\應剔除字串.txt")
  tmp = tmp[which(!tmp[,1] %in% remove_text$V1),]
  ##保留原字串
  write.csv(tmp,paste0('union_output/',now,'保留原字串公司對照詞彙名稱轉換後結果.csv'),row.names=F)
  
  tmp = tmp[which(tmp$對應!=''),]
  tmp = ddply(tmp , '對應', summarize, 總次數=sum(總次數))
  tmp = tmp[order(-tmp$總次數),]
  tmp = tmp[which(tmp$對應!="無法判斷"),]
  
  write.csv(tmp,paste0('union_output/',now,'公司對照詞彙名稱轉換後結果.csv'),row.names=F)
}
