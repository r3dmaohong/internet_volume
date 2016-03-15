rm(list = ls()) #去除工作空間中所有物件
gc() #記憶體釋放
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
library(tm)
library(tmcn)
library(Rwordseg)
#library(wordcloud)
library(compiler)
##修正termdocumentmatrix問題用
source(paste0(path,'\\rscript\\function\\error_solve_termdocumentmatrix.R'), print.eval  = TRUE)
# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#gc
jgc <- function()
{
  gc()
  .jcall("java/lang/System", method = "gc")
}  

links_data_ptt = {}
##先讀取各頁文章的網址
for(i in 300:415){
  tmp <- paste(i, '.html', sep='')
  #https://www.ptt.cc/bbs/Salary/index1896.html
  url <- paste('https://www.ptt.cc/bbs/ServiceInfo/index', tmp, sep='')
  title_css = read_html(url) %>% html_nodes(".title") %>% html_nodes("a") %>% html_attr('href')
  links_data_ptt = c(links_data_ptt,title_css)
  gc() #記憶體釋放
  print(paste0('ptt第',i,'頁'))
  Sys.sleep(runif(1,2,5))
}
##剔除部相關之網址(挑選時已替除，不過在判斷一次)
links_data_ptt =  links_data_ptt[which(grepl('/bbs/ServiceInfo/',links_data_ptt))]

ptt_data = {}
##將抓出的網址進行爬蟲
for(i in 1:length(links_data_ptt)){
  tryCatch({
    url = paste0('https://www.ptt.cc',links_data_ptt[i])
    title_css = read_html(url) %>% html_nodes("#main-content") %>% html_text()
    utf8_text_title <- toUTF8(title_css)
    
    ##去除id
    title_css1 = read_html(url) %>% html_nodes("span") %>% html_text()
    utf8_text_title1 <- toUTF8(title_css1)
    id_delete = utf8_text_title1[which(grepl(': ',utf8_text_title1))-1]
    id_delete = c(id_delete, utf8_text_title1[1:8])
    for(x in 1:length(id_delete)){
      utf8_text_title=gsub(id_delete[x],'',utf8_text_title)
    }
    
    luodian = utf8_text_title#[which(grepl('落點',utf8_text_title))]
    
    ##: 前兩個去掉
    ptt_data = c(ptt_data,luodian)
    ##which contains 落點
    gc() #記憶體釋放
    Sys.sleep(runif(1,2,5))
    print(paste0('ptt第',i,'筆  ',i/length(links_data_ptt)*100,'%'))
  }, error = function(e) {
    print(paste0('ptt第',i,'筆 失敗 ',i/length(links_data_ptt)*100,'%'))
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
##去除單字
jieba_ptt = jieba_ptt[which(nchar(jieba_ptt)>1)]
##去除數值與id
jieba_ptt = jieba_ptt[which(!grepl('[0-9]',jieba_ptt))]


jieba_ptt_df = as.data.frame(jieba_ptt)
jieba_ptt_cdf = ddply(jieba_ptt_df , c('jieba_ptt'), nrow)
jieba_ptt_cdf = jieba_ptt_cdf[order(-jieba_ptt_cdf$V1),]
write.csv(jieba_ptt_cdf,paste0(format(Sys.time(), "%Y_%d_%b"),'jieba_ptt_output_tolower_temp.csv'),row.names=F)

##不知為何沒tolower.. once again
jieba_ptt_cdf[,1] = tolower(jieba_ptt_cdf[,1])
jieba_ptt_cdf = ddply(jieba_ptt_cdf , c('jieba_ptt'), summarize, sum(V1))
jieba_ptt_cdf = jieba_ptt_cdf[order(-jieba_ptt_cdf$..1,jieba_ptt_cdf$jieba_ptt),]

write.csv(jieba_ptt_cdf,paste0(format(Sys.time(), "%Y_%d_%b"),'jieba_ptt_output_tolower_last.csv'),row.names=F)


#ptt
if(FALSE){
  print('ptt 進行落點文字探勘與計算中')
{
    #for(x in 1:length(ptt_data)){
    #  ptt_data[x] = substr(ptt_data[x],unlist(gregexpr(pattern ='問題',ptt_data[x]))[1],nchar(ptt_data[x]))
    #  }
    review_text <- paste(ptt_data, collapse=" ")
    review_text <- gsub("\n", "  ", review_text)
    
    review_source <- VectorSource(review_text)
    d.corpus <- Corpus(review_source)
    
    d.corpus <- tm_map(d.corpus, removePunctuation) 
    d.corpus <- tm_map(d.corpus, removeNumbers) 
    d.corpus <- tm_map(d.corpus, content_transformer(tolower))
    d.corpus <- tm_map(d.corpus, function(word) {
      gsub("[0-9]", " ", word)
    })
    
    d.corpus <- tm_map(d.corpus, segmentCN, nature = TRUE)
    #library(compiler)
    #system.time(cmpfun(d.corpus <- tm_map(d.corpus, segmentCN, nature = TRUE)))
    
    #insertWords(c("壹壹壹壹","???博","落???分析",'??????网','台北??????大???','新???人','行?????????班','大???甘???','大???生甘???','q八八八','大???科技大???四技二???榜???查???','得胜者文教'))
    myStopWords <- c(stopwordsCN())
    d.corpus <- tm_map(d.corpus, removeWords, myStopWords)
    d.corpus <- tm_map(d.corpus, removeWords, stopwords("english")) 
    d.corpus <- tm_map(d.corpus, PlainTextDocument)
    ##修改取出文字長度
    tdm <- TermDocumentMatrixCN(d.corpus, control = list(wordLengths = c(2, Inf)))
    
    m1 <- as.matrix(tdm)
    v <- sort(rowSums(m1), decreasing = TRUE)
    d <- data.frame(word = names(v), freq = v)
    d$word <- as.character(d$word)
    jgc()
    
    #d_ptt_luodian = d[which(d$word=='大學生甘單' | d$word=='新鮮人' | d$word=='統博' | d$word=='壹壹壹壹' | d$word=='樂學網' | d$word=='行動補習班' | d$word=='大學甘單' | d$word=='大學生甘單' | d$word=='q八八八' | d$word=='大學科技大學四技二專榜單查詢' | d$word=='得勝者文教'),]
    #d_ptt_luodian = rbind(d_ptt_luodian,c('落點相關文章筆數',length(links_data_ptt)))
    write.csv(d,'ptt工作爬蟲詞頻.csv',row.names=F)
    #write.csv(d_ptt_luodian,'ptt工作爬蟲詞頻篩選後.csv',row.names=F)
  }
}















{
  ######################################################################
  ##  注意!
  ##  先安裝好所需的套件
  options( java.parameters = "-Xmx1g" )
  library("tm")
  ##  有關中文的套件: Rwordseg tmcn
  ##  用指令會有無法安裝的可能(滿常發生)
  ##  直接到 https://r-forge.r-project.org/R/?group_id=1571 及
  ##         https://r-forge.r-project.org/R/?group_id=1054 下載 .zip 檔
  ##  放入 R 的套件資料夾    
  ##  注意:載入 Rwordseg 前需要先載入 rJava 套件
  ##設定ram給rjava以處理錯誤
  ##options(java.parameters = "-Xmx2048m")
  options( java.parameters = "-Xmx1g" )
  library("rJava")
  options( java.parameters = "-Xmx1g" )
  library("Rwordseg")
  options( java.parameters = "-Xmx1g" )
  library("tmcn")
  library("slam")
  ######################################################################
  ##  Product TermDocumentMatrix for Chinese on R after version 3.0.2
  ##
  ##  Modified command "words" on package NLP 
  wordsCN<-function(x,...){
    words<-unlist(segmentCN(x$content))
    return(words)
  }
  ##  Modified command "termFreq" on package tm
  termFreqCN<-
    function (doc, control = list()) 
    {
      stopifnot(inherits(doc, "TextDocument"), is.list(control))
      .tokenize <- control$tokenize
      if (is.null(.tokenize) || identical(.tokenize, "wordsCN")) 
        .tokenize <- wordsCN
      else if (identical(.tokenize, "MC")) 
        .tokenize <- MC_tokenizer
      else if (identical(.tokenize, "scan")) 
        .tokenize <- scan_tokenizer
      else if (NLP::is.Span_Tokenizer(.tokenize)) 
        .tokenize <- NLP::as.Token_Tokenizer(.tokenize)
      if (is.function(.tokenize)) 
        txt <- .tokenize(doc)
      else stop("invalid tokenizer")
      .tolower <- control$tolower
      if (is.null(.tolower) || isTRUE(.tolower)) 
        .tolower <- tolower
      if (is.function(.tolower)) 
        txt <- .tolower(txt)
      .removePunctuation <- control$removePunctuation
      if (isTRUE(.removePunctuation)) 
        .removePunctuation <- removePunctuation
      else if (is.list(.removePunctuation)) 
        .removePunctuation <- function(x) do.call(removePunctuation, 
                                                  c(list(x), control$removePunctuation))
      .removeNumbers <- control$removeNumbers
      if (isTRUE(.removeNumbers)) 
        .removeNumbers <- removeNumbers
      .stopwords <- control$stopwords
      if (isTRUE(.stopwords)) 
        .stopwords <- function(x) x[is.na(match(x, stopwords(meta(doc, 
                                                                  "language"))))]
      else if (is.character(.stopwords)) 
        .stopwords <- function(x) x[is.na(match(x, control$stopwords))]
      .stemming <- control$stemming
      if (isTRUE(.stemming)) 
        .stemming <- function(x) stemDocument(x, meta(doc, "language"))
      or <- c("removePunctuation", "removeNumbers", "stopwords", 
              "stemming")
      nc <- names(control)
      n <- nc[nc %in% or]
      for (name in sprintf(".%s", c(n, setdiff(or, n)))) {
        g <- get(name)
        if (is.function(g)) 
          txt <- g(txt)
      }
      if (is.null(txt)) 
        return(setNames(integer(0), character(0)))
      dictionary <- control$dictionary
      tab <- if (is.null(dictionary)) 
        table(txt)
      else table(factor(txt, levels = dictionary))
      if (names(tab[1])=="") tab <- tab[-1]
      bl <- control$bounds$local
      if (length(bl) == 2L && is.numeric(bl)) 
        tab <- tab[(tab >= bl[1]) & (tab <= bl[2])]
      nc <- nchar(names(tab), type = "chars")
      wl <- control$wordLengths
      lb <- if (is.numeric(wl[1])) wl[1] else 3
      ub <- if (is.numeric(wl[2])) wl[2] else Inf
      tab <- tab[(nc >= lb) & (nc <= ub)]
      storage.mode(tab) <- "integer"
      class(tab) <- c("term_frequency", class(tab))
      tab
    }
  
  ## Useful for TermDocumentMatrix
  TermDocumentMatrix_classes <-
    c("TermDocumentMatrix", "simple_triplet_matrix")
  ## Useful for TermDocumentMatrix
  .TermDocumentMatrix <-
    function(x, weighting)
    {
      x <- as.simple_triplet_matrix(x)
      if(!is.null(dimnames(x)))
        names(dimnames(x)) <- c("Terms", "Docs")
      class(x) <- TermDocumentMatrix_classes
      ## <NOTE>
      ## Note that if weighting is a weight function, it already needs to
      ## know whether we have a term-document or document-term matrix.
      ##
      ## Ideally we would require weighting to be a WeightFunction object
      ## or a character string of length 2.  But then
      ##   dtm <- DocumentTermMatrix(crude,
      ##                             control = list(weighting =
      ##                                            function(x)
      ##                                            weightTfIdf(x, normalize =
      ##                                                        FALSE),
      ##                                            stopwords = TRUE))
      ## in example("DocumentTermMatrix") fails [because weightTfIdf() is
      ## a weight function and not a weight function generator ...]
      ## Hence, for now, instead of
      ##   if(inherits(weighting, "WeightFunction"))
      ##      x <- weighting(x)
      ## use
      if(is.function(weighting))
        x <- weighting(x)
      ## and hope for the best ...
      ## </NOTE>
      else if(is.character(weighting) && (length(weighting) == 2L))
        attr(x, "weighting") <- weighting
      else
        stop("invalid weighting")
      x
    }
  ##  Modified command "TermDocumentMatrix" on package tm
  ##  and defined "TermDocumentMatrixCN"
  TermDocumentMatrixCN<-
    function (x, control = list()) 
    {
      stopifnot(is.list(control))
      tflist <- lapply(unname(content(x)), termFreqCN, control)
      tflist <- lapply(tflist, function(y) y[y > 0])
      v <- unlist(tflist)
      i <- names(v)
      allTerms <- sort(unique(as.character(if (is.null(control$dictionary)) i else control$dictionary)))
      i <- match(i, allTerms)
      j <- rep(seq_along(x), sapply(tflist, length))
      docs <- as.character(meta(x, "id", "local"))
      if (length(docs) != length(x)) {
        warning("invalid document identifiers")
        docs <- NULL
      }
      m <- simple_triplet_matrix(i = i, j = j, v = as.numeric(v), 
                                 nrow = length(allTerms), ncol = length(x), dimnames = list(Terms = allTerms, 
                                                                                            Docs = docs))
      bg <- control$bounds$global
      if (length(bg) == 2L && is.numeric(bg)) {
        rs <- row_sums(m > 0)
        m <- m[(rs >= bg[1]) & (rs <= bg[2]), ]
      }
      weighting <- control$weighting
      if (is.null(weighting)) 
        weighting <- weightTf
      .TermDocumentMatrix(m, weighting)
    }
}
