## 说明
  使用的分词包:*Rwordseg*  
  使用的文本挖掘模型：  
  * *LDA* 主题模型 
  * *k-means*  k均值聚类
  * *word2vec*  
  * *Apriori* 购物篮关联分析
  
## 测试系统环境
  * Centos 6.4 x64
  * Intel(R) Xeon(R) CPU E5-2603 v2  
  * 64GB ram  
  * R 3.2.0 64-bit  

## R依赖包  
```
  req_packs <-
  c(
    "openxlsx","tm","tmcn","tmcn.word2vec","Rwordseg","RColorBrewer","wordcloud","snow","slam","topicmodels",
    "ggplot2","SnowballC","cluster","fpc","ape","plotrix","optparse","arules"
  )
```
  
## 输出目录
  目录名  
```
    wmjq_*  
```
### 示例输出
  目录名  
```
   wmjq_result_29
```
  示例输出文件列表
```
total 2.5M
-rw-r--r-- 1 rstudio2 pf-datanalyze 178K Dec 16  2015 sample_clean_rsw.txt
-rw-r--r-- 1 rstudio2 pf-datanalyze 111K Dec 16  2015 hot_words_fan_clusters.png
-rw-r--r-- 1 rstudio2 pf-datanalyze  56K Dec 16  2015 hot_words_dendogram.png
-rw-r--r-- 1 rstudio2 pf-datanalyze 1.5K Dec 16  2015 topic 6_term_prob.txt
-rw-r--r-- 1 rstudio2 pf-datanalyze 191K Dec 16  2015 sample_words_cut_phrases.txt
-rw-r--r-- 1 rstudio2 pf-datanalyze  58K Dec 16  2015 topic 6.png
-rw-r--r-- 1 rstudio2 pf-datanalyze 1.5K Dec 16  2015 topic 1_term_prob.txt
-rw-r--r-- 1 rstudio2 pf-datanalyze 187K Dec 16  2015 sample_clean_stem.txt
-rw-r--r-- 1 rstudio2 pf-datanalyze  247 Dec 16  2015 wmjq_freq.txt
-rw-r--r-- 1 rstudio2 pf-datanalyze 1.5K Dec 16  2015 topic 8_term_prob.txt
-rw-r--r-- 1 rstudio2 pf-datanalyze 1.3K Dec 16  2015 hot_words_kmeans_res.txt
-rw-r--r-- 1 rstudio2 pf-datanalyze 1.5K Dec 16  2015 topic 4_term_prob.txt
-rw-r--r-- 1 rstudio2 pf-datanalyze 1.5K Dec 16  2015 topic 9_term_prob.txt
-rw-r--r-- 1 rstudio2 pf-datanalyze 193K Dec 16  2015 sample_words.txt
-rw-r--r-- 1 rstudio2 pf-datanalyze  95K Dec 16  2015 arulesViz.png
-rw-r--r-- 1 rstudio2 pf-datanalyze  32K Dec 16  2015 comments pie chart.png
-rw-r--r-- 1 rstudio2 pf-datanalyze  65K Dec 16  2015 topic 5.png
-rw-r--r-- 1 rstudio2 pf-datanalyze  247 Dec 16  2015 star_name_freq.txt
-rw-r--r-- 1 rstudio2 pf-datanalyze  584 Dec 16  2015 topic_terms.txt
-rw-r--r-- 1 rstudio2 pf-datanalyze   84 Dec 16  2015 comments_count.txt
-rw-r--r-- 1 rstudio2 pf-datanalyze 1.4K Dec 16  2015 topic 10_term_prob.txt
-rw-r--r-- 1 rstudio2 pf-datanalyze  50K Dec 16  2015 topic 4.png
-rw-r--r-- 1 rstudio2 pf-datanalyze  265 Dec 16  2015 Ad_uuid.txt
-rw-r--r-- 1 rstudio2 pf-datanalyze  53K Dec 16  2015 topic 10.png
-rw-r--r-- 1 rstudio2 pf-datanalyze 1.5K Dec 16  2015 topic 7_term_prob.txt
-rw-r--r-- 1 rstudio2 pf-datanalyze 180K Dec 16  2015 wmjq_content.txt
-rw-r--r-- 1 rstudio2 pf-datanalyze 2.1K Dec 16  2015 deleted_content.txt
-rw-r--r-- 1 rstudio2 pf-datanalyze 1.5K Dec 16  2015 topic 3_term_prob.txt
-rw-r--r-- 1 rstudio2 pf-datanalyze 1.5K Dec 16  2015 topic 2_term_prob.txt
-rw-r--r-- 1 rstudio2 pf-datanalyze  55K Dec 16  2015 topic 3.png
-rw-r--r-- 1 rstudio2 pf-datanalyze  94K Dec 16  2015 hot_words_kmeans.png
-rw-r--r-- 1 rstudio2 pf-datanalyze  169 Dec 16  2015 words_count.txt
-rw-r--r-- 1 rstudio2 pf-datanalyze  27K Dec 16  2015 term_freq.txt
-rw-r--r-- 1 rstudio2 pf-datanalyze 3.7K Dec 16  2015 arules.txt
-rw-r--r-- 1 rstudio2 pf-datanalyze  50K Dec 16  2015 topic 2.png
-rw-r--r-- 1 rstudio2 pf-datanalyze  50K Dec 16  2015 topic 9.png
-rw-r--r-- 1 rstudio2 pf-datanalyze 111K Dec 16  2015 word_cut_result_h.txt
-rw-r--r-- 1 rstudio2 pf-datanalyze 1.5K Dec 16  2015 topic 5_term_prob.txt
-rw-r--r-- 1 rstudio2 pf-datanalyze  459 Dec 16  2015 Ad_ids.txt
-rw-r--r-- 1 rstudio2 pf-datanalyze  48K Dec 16  2015 topic 1.png
-rw-r--r-- 1 rstudio2 pf-datanalyze   44 Dec 16  2015 Ad_freq.txt
-rw-r--r-- 1 rstudio2 pf-datanalyze 192K Dec 16  2015 sample_segmentCN.txt
-rw-r--r-- 1 rstudio2 pf-datanalyze 240K Dec 16  2015 word2vec_train_res.txt
-rw-r--r-- 1 rstudio2 pf-datanalyze 1.4K Dec 16  2015 Ad_content.txt
-rw-r--r-- 1 rstudio2 pf-datanalyze  18K Dec 16  2015 foul_line_content.txt
-rw-r--r-- 1 rstudio2 pf-datanalyze  52K Dec 16  2015 topic 8.png
-rw-r--r-- 1 rstudio2 pf-datanalyze    0 Dec 16  2015 stem_syn_content.txt
-rw-r--r-- 1 rstudio2 pf-datanalyze  56K Dec 16  2015 topic 7.png

```
 
