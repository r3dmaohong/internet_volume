rm(list = ls()) #去除工作空間中所有物件
gc() #記憶體釋放
#path<-"C:\\Documents and Settings\\wjhong\\桌面\\網路爬蟲"
setwd(path)
start.time<-Sys.time()

library(rvest)
library(plyr)


##利用列表取得各文章id
temp_link = c()
for(x in 0:500){
  url <- paste('http://www.ursalary.com/search.php?&p=',x,sep='')
  
  title_css = read_html(url) %>% html_nodes("a") %>% html_attr("href")
  title_css = unique(title_css)
  title_css = title_css[which(grepl('detail',title_css))]
  title_css = paste0('http://www.ursalary.com/',title_css)
  temp_link = c(temp_link, title_css)
  sleep_time = runif(1,2,5)
  print(paste0('第',x,'頁列表link抓取完成'))
  print(paste0('loading... 暫停',sleep_time,'秒'))
  Sys.sleep(sleep_time)
  gc() #記憶體釋放
}
#使用抓出來的link作爬蟲
for(i in 1:length(temp_link)){
  url <- temp_link[i]
  title_css = read_html(url) %>% html_nodes("ul li") %>% html_text()
  utf8_text_title <- toUTF8(title_css) ## 將捉下來的標題轉成 UTF8
  ##各式雜亂，以grepl判斷標題？
}


#write.csv(result_NT,'爬蟲.csv',row.names=F)



###
temp_data = c()
max=580

##這部分是只抓列表資料
for(x in 1:max){
  url <- paste('http://www.ursalary.com/search.php?&p=',x,sep='')
  title_css = read_html(url) %>% html_nodes("td a") %>% html_text()
  title_css = iconv(title_css,'utf8')
  temp_data = c(temp_data,title_css)
  sleep_time = runif(1,2,5)
  cat("\r ursalary 第 ",x, '頁 ',x/max*100, '% completed 休息',sleep_time,'秒                              ')
  Sys.sleep(sleep_time)
}
cat("\n ")
result_export = data.frame("公司"=character(),"職務"=character(),"薪水"=character(),"年資"=character(),"流量"=character(),stringsAsFactors=F)
x = 1
for(i in seq(1,length(temp_data),by=5)){
  min = i
  max = i + 4
  temp =  temp_data[min:max]
  result_export[x,] = temp
  x = x + 1
}
temp = sapply(result_export[,5],function(x){
  if(grepl('/',x)){
    x = as.numeric(substr(x,1,unlist(gregexpr('/',x))-2))
  }else{
    x = x
  }
  return(x)
})

temp = as.numeric(temp)

result_export[,5] = temp

nrow(result_export[which(result_export$流量>100),])

library(plyr)
temp = ddply(result_export,'職務',nrow)
result_job = temp[order(-temp$V1),]

temp = ddply(result_export,'公司',nrow)
result_comp = temp[order(-temp$V1),]

write.csv(result_export,'ursalary整體資料.csv',row.names=F)
write.csv(result_comp,'ursalary公司資料.csv',row.names=F)
write.csv(result_job,'ursalary職務資料.csv',row.names=F)
