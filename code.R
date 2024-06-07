# 设置文件存储路径
setwd("G:/工作/数据分析/2024年6月份/文本挖掘-1000(20240607)")

# 加载和安装包
install.packages(c("tidyverse", "tidytext", "wordcloud", "topicmodels"))
library(tidyverse)
library(tidytext)
library(wordcloud)
library(topicmodels)

# 读取CSV文件
data <- read_csv("data.csv")

# 提取文本数据
texts <- data %>% select(3) %>% pull()

# 读取停用词文件
stopwords_file <- "stopwords.txt"
additional_stopwords <- read_lines(stopwords_file)

# 自定义文本清理函数
clean_text <- function(x) {
  x <- tolower(x)
  x <- str_replace_all(x, "[[:punct:]]", " ")
  x <- str_replace_all(x, "[[:digit:]]", " ")
  x <- str_squish(x)
  return(x)
}

# 应用自定义文本清理函数
texts_cleaned <- sapply(texts, clean_text)

# 创建数据框
df <- data.frame(text = texts_cleaned, stringsAsFactors = FALSE)

# 将文本拆分为词语
df_tokens <- df %>%
  unnest_tokens(word, text)

# 去除停用词
stop_words_custom <- bind_rows(
  stop_words,
  tibble(word = additional_stopwords, lexicon = "custom")
)

df_tokens <- df_tokens %>%
  anti_join(stop_words_custom, by = "word")

# 计算词频
word_freq <- df_tokens %>%
  count(word, sort = TRUE) %>%
  top_n(500, n) # 仅保留前500个高频词

# 导出高频关键词及其频次到CSV文件
write_csv(word_freq, "word_freq.csv")

# 生成词云图
set.seed(1000) # 设置随机种子
wordcloud(words = word_freq$word, freq = word_freq$n, scale = c(3, 0.5), min.freq = 1, colors = brewer.pal(8, "Dark2"))

# 创建文档-词项矩阵，仅保留前500个高频词
dtm <- df_tokens %>%
  filter(word %in% word_freq$word) %>%
  count(document = row_number(), word) %>%
  cast_dtm(document, word, n)

# 设置主题数量
num_topics <- 10

# 使用LDA进行主题建模
lda_model <- LDA(dtm, k = num_topics, control = list(seed = 1000))

# 查看每个主题的关键词
lda_terms <- tidy(lda_model, matrix = "beta") %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# 导出每个主题的高频词到CSV文件
write_csv(lda_terms, "lda_terms.csv")
