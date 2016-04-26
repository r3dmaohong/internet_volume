##jiebar分詞輸出
##包含名詞提取
jiebar_n <- function(forum_name,x_data,recent,last){
  library(jiebaR)
  library(plyr)
  ##x為yahoo lineq ptt等等..
  ##匯入詞庫
  tmp = read.csv('D:\\abc\\wjhong\\projects\\school_performence_analysis\\__處理後公司名稱.csv',stringsAsFactors=F)
  temp = unique(c(tmp$company,tmp$最終比對結果))
  tmp2 = read.csv('D:\\abc\\wjhong\\projects\\school_performence_analysis\\學校名稱正規化表格.csv',stringsAsFactors=F)
  temp = unique(c(temp,tmp2$trim後原始,tmp2$對應表))
  word_DB = tolower(temp)
  #write.table(temp, file="D:\\abc\\wjhong\\projects\\合併詞庫.txt", row.names=FALSE, col.names=FALSE)
  
  #cutter = worker(type  = "mix"，user  = "D:/somefile.xxx")
  #cutter = worker()
  cutter=worker("tag")
  #sapply(temp,function(x) new_user_word(cutter,x,"n"))
  for(xj in 1:length(word_DB)){
    new_user_word(cutter,word_DB[xj],"n")
  }
  
  ##抓名詞出來
  get_noun = function(x){
    stopifnot(inherits(x,"character"))
    index = names(res) %in% c("n","nr","nr1","nr2","nrj","nrf","ns","nsf","nt","nz","nl","ng")
    x[index]
  }
  
  jieba_x = {}
  jieba_x_noun = {}
  x_data = tolower(x_data)
  ##瓂\xa2\026
  #remove all punctuation 
  #remove all punctuation except comma[^[:alnum:],]
  #x_data = x_data[,2]
  x_data = gsub('[^[:alnum:]]','',x_data)
  
  for(i in 1:length(x_data)){
    tryCatch({
      temp = segment(x_data[i], cutter)
      res = cutter[x_data[i]]
      #get_noun(res)
      jieba_x = c(jieba_x,temp)
      jieba_x_noun = c(jieba_x_noun, get_noun(res))
      #print(paste0('jiebar :',i/length(x_data)*100,'%'))
      cat("\r ",forum_name," jiebar : ",i/length(x_data) * 100, '% completed',paste(replicate(50, " "), collapse = ""))
    }, error = function(e) {
      conditionMessage(e) # 這就會是"demo error"
    })
  }
  
  cat("\n ")
  
  
  ##資料篩選提取
  data_ep <- function(x){
    ##去除單字
    x = x[which(nchar(x)>1)]
    ##去除數值與id
    x = x[which(!grepl('[0-9]',x))]
    
    
    x_df = as.data.frame(x)
    x_cdf = ddply(x_df , c('x'), nrow)
    x_cdf = x_cdf[order(-x_cdf$V1),]
    #write.csv(x_cdf,paste0('output/x/',format(Sys.time(), "%Y_%d_%b"),'x_output_tolower_temp.csv'),row.names=F)
    
    ##不知為何沒tolower.. once again
    x_cdf[,1] = tolower(x_cdf[,1])
    x_cdf = ddply(x_cdf , c('x'), summarize, sum(V1))
    x_cdf = x_cdf[order(-x_cdf$..1,x_cdf$x),]
    
    return(x_cdf)
  }
  
  jieba_x_cdf = data_ep(jieba_x)
  jieba_x_n_cdf = data_ep(jieba_x_noun)
  
  dir.create('爬蟲分詞後資料', showWarnings = FALSE)
  dir.create(paste0('爬蟲分詞後資料/',forum_name), showWarnings = FALSE)
  write.csv(jieba_x_cdf,paste0('爬蟲分詞後資料/',forum_name,'/',format(Sys.time(), "%Y_%m_%d_%H_%M_%OS"),'jieba',forum_name,'_',recent,'_',last,'.csv'),row.names=F)
  write.csv(jieba_x_n_cdf,paste0('爬蟲分詞後資料/',forum_name,'/',format(Sys.time(), "%Y_%m_%d_%H_%M_%OS"),'jieba名詞',forum_name,'_',recent,'_',last,'.csv'),row.names=F)
  
  tmp = tmp[,c('company','最終比對結果')]
  tmp2 = tmp2[,1:2]
  colnames(tmp) = c('before','after')
  colnames(tmp2) = c('before','after')
  tmp3 = rbind(tmp,tmp2)
  
  if(exists('jieba_x_cdf')){
    inter_list= intersect(jieba_x_cdf[,1],word_DB)
    x2 = jieba_x_cdf[which(jieba_x_cdf[,1] %in% inter_list),]
    
    ##讀取剔除表
    word_remove = read.table("D:\\abc\\wjhong\\projects\\internet_volume\\應剔除字串.txt")
    word_remove = word_remove[,1]
    x2 = x2[which(!(x2[,1] %in% word_remove)),]
    
    #dir.create(, showWarnings = FALSE)
    write.csv(x2,paste0("D:\\abc\\wjhong\\projects\\internet_volume\\output\\",start.time,'/',forum_name,'_',recent,'_',last,'交集結果.csv'),row.names=F)
    path<-"D:\\abc\\wjhong\\projects\\internet_volume\\output"
    setwd(path)
    print(paste0(forum_name,'爬蟲與分析結束'))
  }else if(exists('jieba_x_n_cdf')){
    inter_list= intersect(jieba_x_n_cdf[,1],word_DB)
    x2 = jieba_x_n_cdf[which(jieba_x_n_cdf[,1] %in% inter_list),]
    
    ##讀取剔除表
    word_remove = read.table("D:\\abc\\wjhong\\projects\\internet_volume\\應剔除字串.txt")
    word_remove = word_remove[,1]
    x2 = x2[which(!(x2[,1] %in% word_remove)),]
    
    #dir.create(, showWarnings = FALSE)
    write.csv(x2,paste0("D:\\abc\\wjhong\\projects\\internet_volume\\output\\",start.time,'/',forum_name,'_',recent,'_',last,'交集結果.csv'),row.names=F)
    path<-"D:\\abc\\wjhong\\projects\\internet_volume\\output"
    setwd(path)
    print(paste0(forum_name,'爬蟲與分析結束'))
  }else{
    print("分析失敗")
  }
  
}
