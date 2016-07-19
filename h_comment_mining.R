req_packs <-
  c(
    "openxlsx","tm","tmcn","tmcn.word2vec","Rwordseg","RColorBrewer","wordcloud","snow","slam","topicmodels",
    "ggplot2","SnowballC","cluster","fpc","ape","plotrix","optparse","arules"
  )

library(optparse)
option_list <- list(
  make_option(
    c("-t", "--thread_num"), type = "integer", default = 4,
    help = "Number of threads to execute [default %default]"
  ),
  make_option(
    c("-r","--wordcut_read"), type = "logical",default = TRUE,
    help = "Read the word-cut result from file [default \"%default\"]"
  ),
  make_option(c("-p","--work_path"), type = "character",
              help = "Specify the working path"),
  make_option(c("-i","--input_data"), type = "character",
              help = "Specify the input data file path"),
  make_option(
    c("-m","--min_word_length"), type = "integer", default = 1,
    help = "Minimum word length for doc term matrix processing"
  )
)
# get command line options, if help option encountered print help and exit,
# otherwise if options not found on command line then set defaults,
opt <- parse_args(OptionParser(option_list=option_list))
nThreads <- opt$thread_num
enable_wordcut_read <- opt$wordcut_read
dtm_min_word_len <- opt$min_word_length
if ( length(opt$work_path) ) {
  work_dir <- opt$work_path
}
if ( length(opt$input_data) ) {
  input_file <- opt$input_data
}

dtm_min_word_len <- 2
input_file_aggr <- NULL
input_file_con_aggr <- NULL
ad_uuid_aggr <- NULL
ad_plid_aggr <- NULL
samp_aggr <- NULL
topic_com_aggr <- NULL
topic_word_aggr <- NULL
star_name_freq_stat <- NULL
topic_com_count_stat <- NULL
comment_count_stat <- NULL
nTmpSum <- 0
nWordAggr <- 0
dtm_mat <- NULL

source("~/h_com_mining_config.R") # string/path variables definitions
setwd(work_dir)
# input_file_list <-
#   c(
#     "result_8-5_uniq_utf-8.csv","result_15_mobile_utf-8.csv","result_16_mobile_utf-8.csv","result_17_mobile_utf-8.csv","result_18_mobile_utf-8.csv","Combo"
#   )
# input_file_tags <- c("2015/8/5","2015/8/15","2015/8/16","2015/8/17","2015/8/18","2015-8-(5,15-18)")
input_file_list <-
  t( read.table(data_list_file, stringsAsFactors = F) )
input_file_tags <-
  t( read.table(data_tags_file, stringsAsFactors = F) )
colnames(input_file_list) <- input_file_tags
nInLen <- length(input_file_list)
if (tolower(input_file_list[nInLen]) == "combo") {
  enable_combine <- TRUE 
}else {
  enable_combine <- FALSE
}

topic_file_list <-
  t( read.table(topic_list_file, stringsAsFactors = F) )
topic_file_tags <-
  t( read.table(topic_tags_file, stringsAsFactors = F) )
colnames(topic_file_list) <- topic_file_tags
nTopLen <- length(topic_file_list)
topic_com_aggr <- vector("list",nTopLen)

#comments <- comments[comments$content != ""] #not working
ad_phrases <-
  unlist (read.table(
    AdPhFile,stringsAsFactors = F,fill = T
  ))
ad_phrases <- ad_phrases[ad_phrases != ""]
foul_phrases <-
  unlist (read.table(
    FoulPhFile,stringsAsFactors = F,fill = T
  ))
#mystopwords <- unlist (read.table("StopWords.txt",stringsAsFactors=F,fileEncoding="GBK",,fill=T))
mystopwords <-
  unlist (read.table(
    StopWordsFile, encoding = "UTF-8",stringsAsFactors = F
  ))

con <- file(SynWordsFile, encoding = "UTF-8")
synonyms <- readLines(con)
synonyms <- synonyms[!sapply(synonyms, is.null)]
synonyms_ss <- strsplit(synonyms,"\\s+")
# read the synonyms dict from file
readFirstField <- function(x) {
  ret <- NULL
  #cat(paste(x[1],"\n"))
  grep_res <- grep("^\\s*#", x[1])
  # skip comment
  if (length(grep_res)) {
    #cat(grep_res)
    return(ret)
  }
  x[[1]][1] # return first column
}
removeFirstField <- function(x) {
  ret <- NULL
  grep_res <- grep("^\\s*#", x[1])
  # skip comment
  if (length(grep_res)) {
    #cat(grep_res)
    return(ret)
  }
  nLen <- length(x)
  index <- 1
  while (index <= nLen) {
    if (index != 1)
      ret <- c(ret,x[index])
    index <- index + 1
  }
  ret
}
syn_names <-
  lapply(synonyms_ss, readFirstField) # first column as subsitution word
syn_names <- syn_names[!sapply(syn_names, is.null)]
synonyms <- lapply(synonyms_ss, removeFirstField)
synonyms <- synonyms[!sapply(synonyms, is.null)]
names(synonyms) <- syn_names
close(con)

cat_names <- function(x) {
  orig_name <- names(x)
  #cat(orig_name,"\n")
  line <- strsplit(unlist(x),"\\s")
  len <- length(line)
  syn_reg <- "("
  if (len>1) {
    for (i in seq(1,len-1,by = 2)) {
      if (is.null(line[i])) browser()
      if (is.null(line[i+1])) browser()
#       cat(line[i],"\n")
#       cat(line[i+1],"\n")
      syn_reg <-
        paste(syn_reg,paste(line[i],"|",line[i + 1],"|",sep = ""),sep = "")
      #browser()
    }
  } else {
    syn_reg <- paste(syn_reg,line[1],"|",sep = "")
  }
  syn_reg <- paste(syn_reg,paste(orig_name,")",sep = ""),sep = "")
  #browser()
  syn_reg
}
syn_len <- length(synonyms)
synonyms_reg <- vector("list",syn_len)
for(i in 1:syn_len){
  synonyms_reg[i] <- cat_names(synonyms[i])
  
}


library(Rwordseg)
con <- file(SegWordsFile, encoding = "UTF-8")
seg_words <- readLines(con)
insertWords(seg_words,save=TRUE)
close(con)

# WARNING: THIS LINE BELOW WILL FREEZE THE SYSTEM!!!! Func will return out of memory cause file's too big
# installDict(paste(dict_dir,"/standard.txt",sep = ""), dictname = "standard.txt") #搜狗标准词库 
installDict(
  paste(dict_dir,"/meme_words.txt",sep = ""), dictname = "meme_words.txt"
) #搜狗拼音网络流行新词词库
installDict(
  paste(dict_dir,"/celebrities.scel",sep = ""), dictname = "celebrities.scel", dicttype = "scel"
) #搜狗拼音名人词库
installDict(
  paste(dict_dir,"/singer.scel",sep = ""), dictname = "singer.scel", dicttype = "scel"
) #搜狗拼音歌手词库
installDict(
  paste(dict_dir,"/korean fan.scel",sep = ""), dictname = "korean fan.scel", dicttype = "scel"
) #搜狗韩粉必备词库
installDict(
  paste(dict_dir,"/korean meme.scel",sep = ""), dictname = "korean meme.scel", dicttype = "scel"
) #搜狗韩国流行语词库
installDict(paste(dict_dir,"/popular.txt",sep = ""), dictname = "popular.txt") #搜狗拼音热词词典
installDict(
  paste(dict_dir,"/hua_qian_gu.scel",sep = ""), dictname = "花千骨词库大全.scel",dicttype =
    "scel"
) #搜狗拼音花千骨词典
installDict(paste(dict_dir,"/xuan_feng_shao_nv.scel",sep = ""), dictname = "旋风少女.scel",dicttype =
              "scel") #搜狗拼音旋风少女词典
installDict(
  paste(dict_dir,"/baba_qu_na_er_di_san_ji.scel",sep = ""), dictname = "《爸爸去哪儿第三季》词库.scel",dicttype =
    "scel"
) #搜狗拼音"爸3"词典
installDict(
  paste(dict_dir,"/re_men_zong_yi.scel",sep = ""), dictname = "re_men_zong_yi.scel",dicttype =
    "scel"
) #搜狗拼音"热门综艺"词典
installDict(
  paste(dict_dir,"/gang_tai_ou_xiang_ju.scel",sep = ""), dictname = "gang_tai_ou_xiang_ju.scel",dicttype =
    "scel"
) #搜狗拼音"港台偶像剧"词典

con <- file(NounWordsFile)
star_names <- readLines(con)
close(con)

library(tools)
library("openxlsx")

print_wordcut_line <-
  function(word_list, file_name, append = TRUE, sep = " ") {
    cat(unlist(word_list), file = "", sep = " ", fill = TRUE)
  }

batch.start.time <- Sys.time()

for (nInFileIndex in 1:nInLen){
  start.time <- Sys.time()
  compute_combine_flag <- enable_combine && nInFileIndex == nInLen
  if (!enable_combine || !compute_combine_flag) {
    input_file <- input_file_list[nInFileIndex]
    setwd(work_dir)
    #comments<-read.xlsx(input_file,sheet=1)#读xlsx数据,不加UTF-8 <-unique(data$content)#去掉一样的评论
    read.start.time <- Sys.time()
    comments <- read.csv(
      input_file, sep = ",", quote = "\"", header = TRUE, fileEncoding = "UTF-8",stringsAsFactors = FALSE
    )
    read.end.time <- Sys.time()
    read.time.taken <- read.end.time - read.start.time
    cat(paste("读取",input_file,"耗时:"))
    print(read.time.taken)
    cat("\n")
    #rl <- readLines(file(input_file, encoding = "UTF-8"))
    #length(rl)
    comments$content <- as.character(comments$content)
    
    input_file_aggr <- c(input_file_aggr, list(comments))
    input_file_con_aggr <- c(input_file_con_aggr, comments$content)
    sub_dir <- file_path_sans_ext(input_file)
  } else if ( compute_combine_flag ) {
    sub_dir <- input_file_tags[nInLen]
  }
  sub_path <- paste(work_dir,"/",sub_dir,sep = "")
  if (!dir.exists(sub_path))
    dir.create(sub_path,showWarnings = T)  # create sub dir according to input file name
  setwd(sub_path)
  
  nDropRows <- 0
  all_words <- NULL
  wordcut_res_file <- "word_cut_result_h.txt"
  if (enable_wordcut_read && file.exists(wordcut_res_file)) {
    sample.clean <- readLines(wordcut_res_file)
  } else if (compute_combine_flag) {
    sample.clean <- samp_aggr
    sink(wordcut_res_file)
    for (i in 1:length(sample.clean))
      print_wordcut_line(sample.clean[[i]], wordcut_res_file)
    sink()
    print("分词后单词数:")
    print(nWordAggr)
    nWordSeg <-  nWordAggr
    nDropRows <- length(ad_uuid_aggr)
    out_file <- paste(input_file_tags[[nInLen]],".csv",sep = "")
    setwd(work_dir)
    if (!file.exists(out_file)) {
      write.start.time <- Sys.time()
      for (nComIdx in 1:length(input_file_aggr)) {
        if (nComIdx == 1) {
          write.table(input_file_aggr[[nComIdx]],out_file,sep = ",", row.names = FALSE)
        } else {
          write.table(
            input_file_aggr[[nComIdx]],out_file,sep = ",", col.names = FALSE, row.names = FALSE, append = T
          )  
        }
      }
      write.end.time <- Sys.time()
      write.time.taken <- write.end.time - write.start.time
      cat(paste("写入合并文件",out_file,"耗时："))
      print(write.time.taken)
      cat("\n")
    }
    setwd(sub_path)
  } else {
    segword.start.time <- Sys.time()
    library(Rwordseg)
    # 设置人名识别
    segment.options(isNameRecognition = TRUE)
    
    library(snow)
    #     ## 进行分词
    #     if (compute_combine_flag) {
    #       system.time (sample.words <-
    #                      segmentCN(input_file_con_aggr, returnType = 'tm', nosymbol = TRUE))
    #     } else{
    #       system.time (sample.words <-
    #                      segmentCN(
    #                        comments$content, returnType = 'tm', nosymbol = TRUE
    #                      ))
    #     }
    if (length(sample.words) > seg_mt_limit) {
      library(snow)
      clus <- makeCluster(nThreads)
      clusterExport(clus,"seg_words")
      detach("package:Rwordseg") #jvm can't be forked!!  check out:http://stackoverflow.com/questions/24337383/unloading-rjava-and-or-restarting-jvm
      snow.time(sample.words <- parLapply(clus, comments$content, function(x) {
        library(Rwordseg)
        #.jinit()
        #insertWords(seg_words) # hang the worker!!!!!!!!
        segment.options(isNameRecognition = TRUE) # 设置人名识别
        segmentCN(x, returnType = 'tm', nosymbol = TRUE) 
      }))
      stopCluster(clus)}
    else{    system.time (sample.words <-
                            segmentCN(
                              comments$content, returnType = 'tm', nosymbol = TRUE
                            ))}
#     system.time (sample.words <-
#                    segmentCN(
#                      comments$content, returnType = 'tm', nosymbol = TRUE
#                    ))
    nTmpSum <- 0
    for (nSamLine in 1:length(sample.words)) {
      ss_res <- strsplit(sample.words[[nSamLine]][1],"\\s+")
      #cat( length(ss_res[[1]]) )
      nTmpSum <- nTmpSum + length(ss_res[[1]])
    }
    print("分词后单词数:")
    print(nTmpSum)
    nWordSeg <- nTmpSum
    nWordAggr <- nWordAggr + nTmpSum
    segword.end.time <- Sys.time()
    segword.time.taken <- segword.end.time - segword.start.time
    cat("分词计算耗时:")
    print(segword.time.taken)
    cat("\n")
    system.time (write.table(as.matrix(sample.words),"sample_segmentCN.txt"))
    
    #library(stringr)
    # 去掉英文字符或者门牌号码或者数字
    word_preproc <- function(words) {
      words <- gsub("\\/", "", words)
      #words <- gsub("的|是|在|和|也|如果|同时|当", "", words)
      #word <- gsub("[0-9０１２３４５６７８９]+","",words)
      #word <- gsub("[a-zA-Z]+","",words)
      words <- gsub(pattern = '([[:upper:]])', perl = TRUE, replacement = '\\L\\1', words) # tolower
      words <- gsub("\r?(\n|\r)+","",words)
      #words <- gsub("粑","爸",words)
      #words <- gsub("老子","我",words)
      words <- gsub(" +", " ", words)
      words <- gsub("jason xu", "jason_xu", words) # 完美假期特殊人名替换,同一人名两个词替换一个词
      words <- gsub("3w", "翁炜炜", words) # 完美假期特殊人名替换,Rwordseg无法对3w进行正确分词
      #words[nchar(words) >= 2]
      #str_replace_all(words, "[\r\n]", "")
    }
    
    prep.start.time <- Sys.time()
    library(snow)
    clus <- makeCluster(nThreads)
    clusterExport(clus,"word_preproc")
    # 对分词的进行过滤，并返回两个字以上的词
    snow.time (sample.words <-
                 parLapply(clus, sample.words, word_preproc))
    # remove filtered words' empty entries -> ""
    sample.words <-
      parLapply(clus, sample.words, function(x)
        x <- x[x != ""])
    stopCluster(clus)
    prep.end.time <- Sys.time()
    prep.time.taken <- prep.end.time - prep.start.time
    cat("文本预处理耗时：")
    print(prep.time.taken)
    cat("\n")
    system.time (write.table(as.matrix(sample.words),"sample_words.txt"))
    
    #library(stringr)
    FindRecords = function(adword, word_list, out_file = del_out_file) {
      nLine <- length(word_list)
      rec_list <- NULL
      row_nums <- NULL
      adword_list <- NULL
      line_list <- NULL
      # library(doParallel)
      # clus <- makeCluster(nThreads)
      # registerDoParallel(clus)
      # foreach (i=1:nRec)  %dopar% {
      for (i in 1:nLine) {
        #browser()
        line <-
          paste(word_list[[i]],collapse = " ") # convert char vector to one SINGLE string
        #cat(paste(adword,"match",line))
        #cat("\n")
        found <- grepl(adword, line, perl = TRUE)
        #found <- grep(adword, line)
        #found <- found[found==TRUE]
        if (found)
          #if (length(found))
        {
          #row_nums[[length(row_nums)+1]] <- i
          #row_nums[[j]] <- -i
          row_nums <- c(row_nums,i)
          adword_list <- c(adword_list,adword)
          line_list <- c(line_list,line)
          rec_list <- c( rec_list,list(c(i,adword,line)) )
          cat(paste(adword,"->"))
          cat(paste(i,"th:"))
          cat(line)
          cat("\n")
        }
      }
      #stopCluster(clus)
      # nDel <- length(row_nums)
      # cat(row_nums)
      # cat("\n")
      # cat(paste(nDel,"rows to del\n"))
      # cat(paste(nRec,"rows."))
      # cat("\n")
      #rec_list <- c( rec_list,list(row_nums,adword_list,line_list) )
      rec_list
    }
    
    dAd.start.time <- Sys.time()
    del_out_file <- "deleted_content.txt"
    clus <- makeCluster(nThreads)
    clusterExport(clus,"FindRecords",out_file = del_out_file)
    snow.time ( rec_list <- parLapply(clus, ad_phrases, FindRecords, sample.words) )
#     system.time (rec_list <-
#                    lapply(ad_phrases, FindRecords, sample.words))
#     stopCluster(clus)
    rec_list <-
      rec_list[!sapply(rec_list,is.null)]
    rec_list_un <- unlist(rec_list,recursive = F)
    rec_row_num <- as.integer(lapply(rec_list_un,readFirstField))
    #rownames(rec_list) <- c("row_num","adword","content") # not working
    row_nums <- rec_row_num
    row_nums <- sort(unique(unlist(row_nums)))
    nDropRows <- length(row_nums)
    if (nDropRows)
    {
      ad_uuid_mat <- as.matrix(comments$uuid)
      ad_plid_mat <- as.matrix(comments$plid)
      ad_uuid <- ad_uuid_mat[c(row_nums)]
      ad_plid <- ad_plid_mat[c(row_nums)]
      ad_uuid_aggr <- c(ad_uuid_aggr, ad_uuid)
      ad_plid_aggr <- c(ad_plid_aggr, ad_plid)
      sink("Ad_ids.txt")
      cat("row_num\tuuid\tplid")
      cat("\n")
      for (row_idx in 1:nDropRows) {
        cat(row_nums[row_idx],ad_uuid[row_idx],ad_plid[row_idx],sep = "\t")
        cat("\n")
      }
      sink()
      sink("Ad_uuid.txt")
      cat(unique(sort(ad_uuid)),sep = '\n')
      cat("\n")
      sink()
      records_mat <- as.matrix(sample.words)
      #dim(records_mat)
      # remove specified AdWords rows
      records_mat <- records_mat[-c(unlist(row_nums)),]
      #dim(records_mat)
      sample.words <- records_mat
      
      # NOT working!
      # for ( i in 1:length(sample.words) )
      # {
      # if ( i %in% row_nums )
      # {
      # cat(paste(i,":",sample.words[[i]],"\n",sep=""))
      # sample.words[[i]] <- NULL
      # cat(paste(sample.words[[i]],"\n",sep=""))
      # }
      # }
    }
    sample.words <- sample.words[lapply(sample.words, length) > 0]
    dAd.end.time <- Sys.time()
    dAd.time.taken <- dAd.end.time - dAd.start.time
    cat("删除广告评论耗时：")
    print(dAd.time.taken)
    cat("\n")
    sink(del_out_file)
    for (nRecIdx in 1:length(rec_list_un)){
        cat(unlist(rec_list_un[[nRecIdx]],recursive = F),fill = T,sep = '->')
    }
    sink()
    
    # replace_stopword <- function(stopword, word_list){
    # #browser()
    # line <- paste(word_list,collapse=" ")
    # if ( grepl("[[:punct:]]", stopword) )
    # {
    # #browser()
    # #grep bug: see http://r.789695.n4.nabble.com/Errors-on-Windows-with-grep-fixed-TRUE-on-UTF-8-strings-td4704073.html
    # stopword <- paste("\\",stopword,sep="")
    # grep_line <- gsub(stopword," ", line)
    # }
    # else
    # grep_line <- gsub(stopword," ",line)
    # if (grep_line != line)
    # {
    # cat(paste(stopword,": ",grep_line,sep=""))
    # cat("\n")
    # cat(line)
    # cat("\n\n")
    # }
    # strsplit(line, "\\s+")
    # }
    GrepFoulPhrases <- function(word_list, phrases_list) {
      #browser()
      #cat(stopword)
      nPhrase <- length(phrases_list)
      line <- paste(word_list,collapse = " ")
      grep_line <- line
      if (grep_line != "")
      {
        for (i in 1:nPhrase) {
          if (length(grep_line))
            grep_line <- gsub(phrases_list[[i]]," ",grep_line)
        }
        if (grep_line != line)
        {
          cat(grep_line)
          cat("\n")
          cat(line)
          cat("\n\n")
        }
      }
      #cat(line)
      #cat("\n")
      #browser()
      strsplit(grep_line, "\\s+")
    }
    
    grepf.start.time <- Sys.time()
    #         # remove Stop Phrases(filthy/cursing phrases)
    #         clus <- makeCluster(nThreads,out_file="foul_line_content.txt")
    #         #exp_fun <- c("GrepFoulPhrases")
    #         snow.time( sample.words <- parLapply(clus, sample.words, GrepFoulPhrases, mystopwords) ) # not working!!! invalid regular expression '(', reason 'Missing ')''
    #         stopCluster(clus)
    sink("foul_line_content.txt")
    sample.clean <-
      lapply(sample.words, GrepFoulPhrases, foul_phrases)
    sink()
    grepf.end.time <- Sys.time()
    grepf.time.taken <- grepf.end.time - grepf.start.time
    cat("删除污秽用语耗时：")
    print(grepf.time.taken)
    cat("\n")
    system.time (write.table(as.matrix(sample.words),"sample_words_cut_phrases.txt"))
    
    removeStopWords = function(line,word_list) {
      #browser()
      ret = character(0)
      index <- 1
      line <- strsplit(line[[1]],"\\s+")
      it_max <- length(line)
      #       cat(it_max)
      #       cat("\n")
      while (index <= it_max) {
        if (length(word_list[word_list == line[index]]) < 1)
          ret <- c(ret,line[index])
        index <- index + 1
      }
      #line <- paste(line,collapse = " ")
      ret
    }
    swords.start.time <- Sys.time()
    #sample.clean <- lapply( sample.clean, removeStopWords, mystopwords )
    clus <- makeCluster(nThreads)
    exp_fun <- c("removeStopWords")
    clusterExport(clus,exp_fun)
    snow.time( sample.clean <- parLapply(clus, sample.clean, removeStopWords, mystopwords) )
    ## remove empty entries and empty rows
    sample.clean <-
      parLapply(clus, sample.clean, function(x)
        x <- unlist(x))
    stopCluster(clus)
    sample.clean <- sample.clean[lapply(sample.clean,length)>0]
    swords.end.time <- Sys.time()
    swords.time.taken <- swords.end.time - swords.start.time
    cat("删除停用词耗时:")
    print(swords.time.taken)
    cat("\n")
    system.time ( write.table(as.matrix(sample.clean),"sample_clean_rsw.txt") )
    
    StemSynonym = function(line,stemword_list) {
      #browser()
      #cat(stopword)
      nStem <- length(stemword_list)
      #line <- paste(word_list,collapse = " ")
      grep_line <- line
      if (grep_line != "")
      {
        for (i in 1:nStem) {
          if (length(grep_line))
            grep_line <- gsub(synonyms_reg[[i]],names(stemword_list)[i],grep_line)
        }
        if (grep_line != line)
        {
          cat(grep_line)
          cat("\n")
          cat(line)
          cat("\n\n")
        }
      }
      #cat(line)
      #cat("\n")
      #browser()
      strsplit(grep_line, "\\s+")
    }
    
#     StemSynonym = function(line,stemword_list) {
#       index <- 1
#       #line <- strsplit(line[[1]],"\\s+")
#       it_max <-
#         length(line) # this fucking index scheme is killing me!!!!!!
#       #if ( it_max == 1) browser()
#       #cat(it_max)
#       #cat("\n")
#       nStemLen <- length(stemword_list)
#       # cat(nStemLen)
#       # cat("\n")
#       sub_res <- ret <- character(0)
#       sub_names <- names(stemword_list)
#       while (index <= it_max) {
#         #cat(line[index])
#         #cat("\n")
#         #cat(paste(index,":",line[index]," ",sep=""))
#         break_flag <- FALSE
#         sub_res <- line[index]
#         for (i in 1:nStemLen) {
#           nWord <- length(stemword_list[[i]])
#           for (j in 1:nWord) {
#             #cat( paste(":",stemword_list[[i]][j],"\n") )
#             if (is.na(line[index]))
#               browser()
#             if (is.na(stemword_list[[i]][j]))
#               browser()
#             if (is.na(line[index] == stemword_list[[i]][j]))
#               browser()
#             # cat(line[index])
#             # cat(" vs ")
#             # cat(stemword_list[[i]][j])
#             # cat("\n")
#             if (line[index] == stemword_list[[i]][j]) {
#               sub_res <- sub_names[[i]]
#               cat(paste(
#                 stemword_list[[i]][j],"->",sub_names[[i]],":",
#                 paste(line,collapse = " "),"\n"
#               ))
#               break_flag = TRUE
#               break
#             }
#             # sub_res <- gsub( synonyms[[i]][j], sub_names[[i]], line[index] ) # fixed=TRUE not working for multi-bytes string!
#             # if ( sub_res != line[index] )
#             # {
#             #   cat( paste(synonyms[[i]][j],"->",sub_names[[i]],":",
#             #     paste(line,collapse=" "),"\n") )
#             #   break_flag=TRUE
#             #   break
#             # }
#           }
#           if (break_flag)
#             break
#         }
#         ret <- c(ret, sub_res)
#         index <- index + 1
#       }
#       #cat(paste("index:",index,"it_max",it_max,"\n"))
#       #browser()
#       ret
#     }
    
    stem.start.time <- Sys.time()
    sink("stem_syn_content.txt")
    #     # these codes below are only for debug purposes!
    #     for ( i in 1:length(sample.clean) ) {
    #       cat(paste(i,":",sample.clean[[i]],"\n"))
    #       stem_word_list <- StemSynonym(sample.clean[[i]],synonyms)
    #       stem_word_list <- paste(stem_word_list,collapse=" ")
    #       cat(stem_word_list)
    #       cat("\n")
    # #       if ( stem_word_list!=sample.clean[[i]] ) {
    # #         cat(stem_word_list)
    # #         browser()
    # #         sample.clean[[i]] <- stem_word_list
    # #         cat( paste(i,"th:",paste(stem_word_list,collapse=" "),"\n",sep="") )
    # #       }
    #     }
    sink()
    clus <- makeCluster(nThreads,out_file = "stem_syn_content.txt")
    exp_var <- c("StemSynonym","synonyms","synonyms_reg")
    clusterExport(clus,exp_var)
    snow.time(sample.clean <-
                parLapply(clus, sample.clean, StemSynonym, synonyms))
    stopCluster(clus)
#     sample.clean <- lapply(sample.clean, StemSynonym, synonyms)
    stem.end.time <- Sys.time()
    stem.time.taken <- stem.end.time - stem.start.time
    cat("替换同义词耗时：")
    print(stem.time.taken)
    cat("\n")
    system.time (write.table(as.matrix(sample.clean),"sample_clean_stem.txt"))
    
    # stat topic comments count
    topic_row_list <- NULL
    topic_com_count_list <- NULL
    t_stat.start.time <- Sys.time()
    clus <- makeCluster(nThreads)
    clusterExport(clus,"FindRecords")
    for (nTopIdx in 1:nTopLen) {
      file <- paste(work_dir,"/",topic_file_list[[nTopIdx]],sep = "")
      keyword_list <- unlist(read.table(file,stringsAsFactors = F,fill = T))
      keyword_list <- keyword_list[keyword_list != ""]
      topic_word_aggr <- c(topic_word_aggr,list(keyword_list))
      snow.time ( rec_list <- parLapply(clus, keyword_list, FindRecords, sample.clean) )
#       system.time (rec_list <-
#                      lapply(keyword_list, FindRecords, sample.clean))
      rec_list <-
        rec_list[!sapply(rec_list,is.null)]
      rec_list_un <- unlist(rec_list,recursive = F)
      row_nums <- as.integer(lapply(rec_list_un,readFirstField))
      row_nums <- sort(unique(unlist(row_nums)))
      topic_com_count_list <- c(topic_com_count_list,length(row_nums))
      topic_row_list <- c(topic_row_list,list(row_nums))
      sink(paste(topic_file_tags[[nTopIdx]],"_content",".txt",sep = ""))
      for (nRecIdx in 1:length(rec_list_un)){
        cat(unlist(rec_list_un[[nRecIdx]],recursive = F),fill = T,sep = '->')
      }
      sink()
      topic_com_aggr[[nTopIdx]] <- c(topic_com_aggr[[nTopIdx]],"\n",rec_list_un)
    }
    stopCluster(clus)
    sample.clean<- sample.clean[lapply(sample.clean, length) > 0]
    topic_com_count_stat <- c(topic_com_count_stat,list(topic_com_count_list))
    t_stat.end.time <- Sys.time()
    t_stat.time.taken <- t_stat.end.time - t_stat.start.time
    cat("统计主题相关评论耗时：")
    print(t_stat.time.taken)
    cat("\n")
    
    samp_aggr <- c(samp_aggr,sample.clean)
  } # end else
  nTmpSum <- 0
  for (nSamLine in 1:length(sample.clean)) {
    nTmpSum <- nTmpSum + length(sample.clean[[nSamLine]])
  }
  print("过滤后单词数:")
  print(nTmpSum)
  nWordClean <- nTmpSum
  
  
  sink(wordcut_res_file)
  #lapply(sample.clean, print_wordcut_line, file=wordcut_res_file) # extra lines added by lapply func!!
  for (i in 1:length(sample.clean))
    print_wordcut_line(sample.clean[[i]], wordcut_res_file)
  sink()
  
  tm.start.time <- Sys.time()
  library(tm)
  #vec_src <- lapply(sample.clean, function(x) x <- paste(x,collapse=" "))
  #corpus <- Corpus(VectorSource(vec_src))
  corpus <- Corpus(VectorSource(sample.clean))
  library(SnowballC)
  corpus <- tm_map(corpus, stripWhitespace)   # *Stripping whitespace
  corpus <-
    tm_map(corpus, stemDocument)   # *Removing common word endings* (e.g., "ing", "es")
  
  #corpus <- tm_map(corpus, removePunctuation)
  #corpus <- tm_map(corpus, removeNumbers)
  #corpus <- tm_map(corpus, content_transformer(tolower))
  #system.time(corpus <- tm_map(corpus, removeWords, stopwords("english")))
  library(tmcn)
  #system.time(corpus <- tm_map(corpus, removeWords, stopwordsCN()))
  corpus <- tm_map(corpus, PlainTextDocument)
  
  system.time(dtm <-
                DocumentTermMatrix(corpus, control = list(wordLengths = c(2, Inf))))
  #   system.time(dtm <-
  #                 DocumentTermMatrix(corpus, control = list(
  #                   wordLengths = c(2, Inf), stopwords = mystopwords
  #                 )))
  
  
  #   low_freq <- 50
  #   write.table(findFreqTerms(dtm,low_freq),file = "freq_term.txt",row.name =F)
  
  library(slam)
  term.statistics <- function(dtm) {
    #dtm = dtm[row_sums(dtm) > 0,col_sums(dtm) > 0]    # get rid of empty rows/columns
    vocabulary = colnames(dtm)
    data.frame(
      term = vocabulary,
      characters = nchar(vocabulary),
      number = grepl("[0-9]", vocabulary),
      nonalpha = grepl("\\W", vocabulary),
      termfreq = col_sums(dtm),
      docfreq = col_sums(dtm > 0),
      reldocfreq = col_sums(dtm > 0) / nDocs(dtm),
      tfidf = tapply(dtm$v / row_sums(dtm)[dtm$i], dtm$j, mean) * log2(nDocs(dtm) /
                                                                         col_sums(dtm > 0))
    )
  }
  all_words <- term.statistics(dtm)
  tm.end.time <- Sys.time()
  tm.time.taken <- tm.end.time-tm.start.time
  cat("tm操作耗时：")
  print(tm.time.taken)
  cat("\n")
  
  library(Matrix)
  #dense <- sparseMatrix(dtm$i,dtm$j,x=dtm$v) mat <- sparseMatrix(i=dtm$i, j=dtm$j, x=dtm$v,dims=c(dtm$nrow, dtm$ncol))
  as.matrix.simple_triplet_matrix <- 
    function (x, ...) 
    {
      nr <- x$nrow
      nc <- x$ncol
      ## old line: y <- matrix(vector(typeof(x$v), nr * nc), nr, nc)
      y <- matrix(0, nr, nc)  ## 
      y[cbind(x$i, x$j)] <- x$v
      dimnames(y) <- x$dimnames
      y
    } # circument the interger overflow problem of old line when the matrix is too big
  #   dtm_mat <- as.matrix(dtm)
  #   dim(dtm_mat)
  #system.time( write.table(dtm_mat,"dtm.txt") )
  
  freq.start.time <- Sys.time()
  word_freq <- all_words$termfreq
  names(word_freq) <- all_words$term
  length(word_freq)
  ord <- order(word_freq, decreasing = TRUE)
  high_freq <- word_freq[head(ord,n = 10L)]
  sink("term_freq.txt")
  cat( paste(names(word_freq[ord]),word_freq[ord],"\n") )
  sink()
  high_wf <- data.frame(word = names(high_freq), freq = high_freq)
  wf <- data.frame(word = names(word_freq), freq = word_freq)
  head(high_wf)
  #   sink('freq_terms_cor.txt')
  #   print( findAssocs(dtm, names(high_freq), corlimit = 0.5) ) # very slow when dtm is large!!!
  #   sink()
  
  #plot term frequency histogram
  library(ggplot2)
  freq_limit <- median(word_freq)
  #x11()
  png("word_freq_histo.png",width = 1200,height = 800)
  p <- ggplot(subset(high_wf, high_freq > freq_limit), aes(word,freq))
  p <- p + geom_bar(stat = "identity")
  p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  p
  dev.off()
  #   # system.time( write.csv(dtm_mat, file="dtm.csv") )
  
  #word_freq<- all_words$termfreq
  for (nTopIdx in 1:nTopLen){
    topic_freq <- word_freq[unlist(topic_word_aggr[[nTopIdx]])]
    names(topic_freq) <- topic_word_aggr[[nTopIdx]] # incase of na value in word_freq
    sink(paste(topic_file_tags[[nTopIdx]],"_freq.txt",sep = ""))
    print( sort(topic_freq,decreasing = T),na.print ="0" )
    sink()
    topic_freq
  }
  freq.end.time <- Sys.time()
  freq.time.taken <- freq.end.time - freq.start.time
  cat("主题相关词频统计耗时：")
  print(freq.time.taken)
  cat("\n")
  
  tfidf.start.time <- Sys.time()
  library(slam)
  summary(col_sums(dtm))
  #term_tfidf  <- tapply(dtm$v/row_sums( dtm)[ dtm$i],   dtm$j,  mean)
  term_tfidf  <-
    tapply(dtm$v / row_sums(dtm)[dtm$i],   dtm$j,  mean) *
    log2(nDocs(dtm) / col_sums(dtm  >  0))
  summary(term_tfidf)
  
  dtm  <-  dtm[,  term_tfidf  >=  0.1]
  dtm  <-  dtm[row_sums(dtm)  >  0,]
  tfidf.end.time <- Sys.time()
  tfidf.time.taken <- tfidf.end.time - tfidf.start.time
  cat("tfidf计算耗时：")
  print(tfidf.time.taken)
  cat("\n")
  
  if (enable_lda_per_file || compute_combine_flag){
    lda.start.time <- Sys.time()
    library(topicmodels)
    library(snow)
    k <- 10
    SEED <- 2010
    clus <- makeCluster(nThreads)
    export_names <- c("dtm","k","SEED","LDA","CTM")
    clusterExport(clus, export_names)
    sample_TM_tasks <- list(VEM <-
                              function() {
                                return (LDA(dtm, k = k, control = list(seed = SEED)))
                              },
                            VEM_fixed <-
                              function() {
                                return (LDA(
                                  dtm, k = k,control = list(estimate.alpha = FALSE, seed = SEED)
                                ))
                              },
                            Gibbs <-
                              function() {
                                return (LDA(
                                  dtm, k = k, method = "Gibbs",control = list(
                                    seed = SEED, burnin = 1000,thin = 100, iter = 1000
                                  )
                                ))
                              } #,
                            #CTM <- function() { return ( CTM(dtm, k = k, ,control = list(seed = SEED,var = list(tol = 10^-4), em = list(tol = 10^-3))) ) }
    )
    snow.time(sample_TM <-
                clusterApply(clus, sample_TM_tasks, function(f)
                  f()))
    list_names <- c("VEM","VEM_fixed","Gibbs") #,"CTM")
    names(sample_TM) <- list_names
    stopCluster(clus)
    
    sapply(sample_TM[1:2], slot, "alpha")
    sapply(sample_TM, function(x)
      mean(apply(posterior(x)$topics, 1, function(z)
        - sum(z * log(z)))))
    
    method <- "Gibbs"
    #最可能的主题文档
    Topic  <-  topics(sample_TM[[method]],  1)
    #table(Topic) #  Error: class(objId) == "jobj" is not TRUE 
    #每个Topc前5个Term
    Terms  <-  terms(sample_TM[[method]],  5)
    Terms[,  1:k]
    write.table(t(Terms), file = "topic_terms.txt", col.names = FALSE)
    lda.end.time <- Sys.time()
    lda.time.taken <- lda.end.time - lda.start.time
    cat("LDA计算耗时：")
    print(lda.time.taken)
    cat("\n")
    
    topic_index <- 1:k
    term_prob_file <- "term_prob.txt"
    max_words <- 50
    #按各主题画wordcloud
    library(RColorBrewer)
    library(wordcloud)
    sample.cloud <- function(topic_idx, maxwords = 100) {
      topic_words = posterior(sample_TM[[method]])$terms[topic_idx,]
      topic_words = sort(topic_words, decreasing = T)[1:maxwords]
      topic_words = topic_words[!is.na(topic_words)]
      write.table(topic_words, paste("topic ", topic_idx,"_" ,term_prob_file, sep = ""))
      
      png(paste("topic ", topic_idx, ".png", sep = ""), width = 800, height = 800)
      wordcloud(
        names(topic_words), topic_words, scale = c(4, 1.5), min.freq = 2, max.words = maxwords, colors = rainbow(maxwords)
      )
      title(main = paste("topic:", topic_idx))
      dev.off()
    }
    lapply(topic_index, sample.cloud, maxwords = max_words)
  }
  
  dtmss <- removeSparseTerms(dtm,0.99)
  # hierarchical clustering
  library(cluster)
  dis <- dist(t(dtmss), method = "euclidian")
  (hc <- hclust(d = dis, method = "ward.D"))
  png("hot_words_dendogram.png", width = 1600, height = 800)
  
  plot.new()
  title(paste("芒果TV",input_file,"文件全部评论热词分类"))
  plot(hc, main = "芒果TV评论热词分类",hang = -1)
  groups <-
    cutree(hc, k = 5)   # "k=" defines the number of clusters you are using
  rect.hclust(hc, k = 5, border = "red") # draw dendogram with red borders around the 5 clusters
  dev.off()
  
  library(ape)
  # plot(as.phylo(hc), type = "fan", tip.color = hsv(runif(15, 0.65,
  #     0.95), 1, 1, 0.7), edge.color = hsv(runif(10, 0.65, 0.75), 1, 1, 0.7), edge.width = runif(20,
  #     0.5, 3), use.edge.length = TRUE, col = "gray80")
  
  png("hot_words_fan_clusters.png", width = 1600, height = 800)
  mypal = c("#556270", "#4ECDC4", "#1B676B", "#FF6B6B", "#C44D58")
  # cutting dendrogram in 5 clusters
  # plot
  op = par(bg = "#E8DDCB")
  plot.new()
  title("芒果TV评论热词聚类")
  # Size reflects term frequency
  plot(
    as.phylo(hc), type = "fan", tip.color = mypal[groups], label.offset = 1,
    cex = log(all_words$termfreq,10), col = "red"
  )
  dev.off()
  
  library(fpc)
  png("hot_words_kmeans.png",width= 1600,height= 1200)
  clus_num <- 3
  kfit <- kmeans(dis, clus_num)
  clusplot(as.matrix(dis), kfit$cluster, color=T, shade=T, labels=2, lines=0)
  dev.off()
  sink("hot_words_kmeans_res.txt")
  print(lapply(1:clus_num, function(x) {
    (sort(which(kfit$cluster == x),decreasing = T))
  }))
  sink()
  
  png("comments pie chart.png", width = 1200, height = 800 )
  library(plotrix)
  if (compute_combine_flag) {
    nComment <- length(input_file_con_aggr)
  } else {
    nComment <- length(comments$content)
  }
  nRealSample <- length(sample.clean)
  stat_val <- c( nComment-nRealSample-nDropRows, nRealSample,nDropRows )
  lbls <- class_lbls
  pct <- round(stat_val/sum(stat_val)*100)
  lbls <- paste(lbls, pct) # add percents to labels
  lbls <- paste(lbls,"%",sep="") # ad % to labels
  lbls <- paste(lbls,stat_val)
  pie3D(stat_val, labels = lbls, explode=0.1,,main="全站评论分布图")
  dev.off()
  
  lbls <- c("评论总数","有效评论数","广告评论数")
  stat_val <- c( nComment, nRealSample,nDropRows )
  pct <- round(stat_val/nComment*100)
  lbls <- paste(lbls, pct) # add percents to labels
  lbls <- paste(lbls,"%",sep="") # ad % to labels
  ( lbls <- paste(lbls,stat_val) )
  comment_count_stat <- c(comment_count_stat, list(stat_val))
  sink("comments_count.txt")
  print( lbls )
  sink()
  sink("words_count.txt")
  h_lbls <- c("分词后单词数","过滤后单词数","矩阵文档数","矩阵词汇数")
  stat_val <- c(nWordSeg,nWordClean,nDocs(dtm),nTerms(dtm))
  cat(paste(h_lbls,stat_val))
  cat("\n")
  h_lbls <- c("停用词","分词词库","广告短语","污秽短语","同义词")
  stat_val <-
    c(
      length(mystopwords), length(seg_words), length(ad_phrases), length(foul_phrases), length(synonyms)
    )
  cat(paste(h_lbls,stat_val))
  sink()
  
  # terms = term.statistics(dtm)
  # words = terms$term[order(-terms$tfidf)[1:10000]]
  # m_filtered = dtm[, colnames(dtm) %in% words]
  # m_filtered = m_filtered[row_sums(m_filtered) > 0,col_sums(m_filtered) > 0]
  
  # fit = LDA(m_filtered, k=10, method="Gibbs", control=list(iter=500, alpha=.5))
  
  # association analysis
  library(arules)
  arul.start.time <- Sys.time()
  min_support <- (word_freq[ord[50]] / sum(all_words$termfreq))  # 100th frequency's percentage as min support for rules
  cat(paste("min_support=",min_support))
  trans <- as(as.matrix(dtmss),"transactions") #trans <- as(dtm_mat,"transactions")
  system.time(rules <-
                apriori(
                  trans,parameter = list(
                    confidence = 0.01,support = min_support, minlen = 2
                  )
                ))
  top_confidence <-
    sort(rules, decreasing = TRUE, na.last = NA, by = "confidence")
  #detach(package:tm, unload=TRUE) # using arules pack's inspect method
  sink("arules.txt")
  print( arules::inspect(head(top_confidence,100)) )
  sink()
  r_len <- length(rules)
  if (r_len) {
    sub_rules <- rules
    if (r_len>rules_num_limit){
      sub_rules <- head(sort(rules, decreasing = TRUE, na.last = NA, by = "confidence"),25)
    }
    library(arulesViz)
    png("arulesViz.png",width = 3200, height = 1600)
#     sub_item <- unlist(topic_word_aggr[1])
#     sub_rules <- subset(rules,lhs %in% sub_item)
    plot(sub_rules,method = "graph",interactive = FALSE,shading = "confidence", main = "文本关联分析")
    dev.off()
  }
  arul.end.time <- Sys.time()
  arul.time.taken <- arul.end.time - arul.start.time
  cat("关联分析耗时：")
  print(arul.time.taken)
  cat("\n")
  
  if (enable_word2vec_train_per_file || compute_combine_flag) {
    # word2vec distances
    w2v.start.time <- Sys.time()
    library(tmcn.word2vec)
    train_model_file <- "word2vec_train_res.txt"
    word2vec_model <- word2vec(wordcut_res_file, train_model_file)
    w2v.end.time <- Sys.time()
    w2v.time.taken <- w2v.end.time - w2v.start.time
    cat("word2vec训练耗时：")
    print(w2v.time.taken)
    cat("\n")
  }

  end.time <- Sys.time()
  time.taken <- end.time - start.time
  if (!compute_combine_flag) {
    cat( paste("处理",input_file,"文件耗时：") )
  } else {
    cat( paste("处理",input_file_tags[nInLen],"耗时：") )
  }
  print(time.taken)
  cat("\n")
} # end for each input data file
batch.end.time <- Sys.time()
batch.time.taken <- batch.end.time - batch.start.time
cat(paste("总耗时："))
batch.time.taken
cat("\n")

if (compute_combine_flag) {
  #setwd(work_dir)
  star_name_freq_stat_t <-
    lapply(star_name_freq_stat,function(x)
      unlist(x))
  #   write.csv(
  #     star_name_freq_stat, file = "star_name_freq_stat.csv", na = "0" #, row.names = input_file_tags
  #   )
  
  sink("topic_count_stat.txt")
  cat("\t")
  cat(topic_file_tags)
  cat("\n")
  for (nDayIdx in 1:length(topic_com_count_stat)) {
    topic_count_list <- topic_com_count_stat[[nDayIdx]]
    cat("\t")
    cat(unlist(topic_count_list),sep = " ",fill = T)
  }
  sink()
  
  for(nTopIdx in 1:nTopLen) {
    sink(paste(topic_file_tags[[nTopIdx]],"_content",".txt",sep = ""))
    for (nRecIdx in 1:length(topic_com_aggr[[nTopIdx]])) {
        cat(unlist(topic_com_aggr[[nTopIdx]][nRecIdx],recursive = F),fill = T,sep = '->')
    }
    sink()
  }
  
  sink("star_name_freq_stat.txt")
  cat(star_names)
  cat("\n")
  for (i in 1:length(star_name_freq_stat)){
    cat(unlist(star_name_freq_stat[[i]]),sep=" ",fill = T)
  }
  sink()
  sink("comment_count_stat.txt")
  cat(print_lbls)
  cat("\n")
  #   comment_count_stat_t <-
  #     lapply(comment_count_stat,function(x)
  #       unlist(x))
  for (i in 1:length(comment_count_stat)){
    cat(unlist(comment_count_stat[[i]]),sep=" ",fill = T)
  }
  sink()
  #setwd(sub_path)
  
  nLenRow <- length(ad_uuid_aggr)
  sink("Ad_ids_stat.txt")
  cat("uuid\tplid")
  cat("\n")
  for (row_idx in 1:nLenRow) {
    cat(ad_uuid_aggr[row_idx],ad_plid_aggr[row_idx],sep="\t")
    cat("\n")
  }
  sink()
  
  #   # will hang the IDE if model is large!!!!
  #   if (enable_word2vec_dis_calc) {
  #     sink("word2vec_nouns_dis.txt")
  #     for (name_idx in 1:length(star_names)) {
  #       print(star_names[[name_idx]])
  #       print (distance(train_model_file,star_names[[name_idx]]))
  #       cat("\n")
  #     }
  #     sink()
  #   }
}