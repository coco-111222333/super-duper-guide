# 设置文件存储路径
setwd("D:/HuaweiMoveData/Users/雷桑怀里的猫/Desktop")
#前期数据预处理过程如下，后期合并可删
# 设置文件存储路径
setwd("D:/HuaweiMoveData/Users/雷桑怀里的猫/Desktop")

# 导入必要的库
library(readr)
library(tidyr)
library(ggplot2)
library(dplyr)

# 导入数据集
postings <- read.csv("./postings.csv",header = T)
is.data.frame(postings)

#查看缺失值
sapply(postings, function (x) sum(is.na (x)))

#分析缺失值:
#salary等列缺失值较大
#remote_allowed有两类，即1和NA，因此在最后数据整合后将NA改为0。
distinct(postings, remote_allowed)
#与之相同处理views，applies
#时间列分析。发现时间几乎是从12月到4月。

#（1）首先处理薪资问题
head(postings[,c("max_salary","med_salary","min_salary")],5)
#利用最高薪资和最低薪资来med_salary填充数据
postings$med_salary <- ifelse(is.na(postings$med_salary), (postings$max_salary
                                                           + postings$min_salary) / 2, postings$med_salary)

#重新查看缺失值
sapply(postings,function(x) sum(is.na(x)))
#根据结果可以看到现在med_salary缺失值已经从117569降到87776
#相比其他数据好很多

#导入salaries数据集
#salaries <- read.csv("jobs/salaries.csv")

#利用最高薪资和最低薪资来填充数据
#salaries$med_salary <- ifelse(is.na(salaries$med_salary), (salaries$max_salary
#+ salaries$min_salary) / 2, salaries$med_salary)

#查看缺失
#sapply(salaries,function(x) sum(is.na(x)))

#salaries <- subset(salaries,select =-c(salary_id,max_salary,min_salary,currency,
#                                     compensation_type))

# 使用 left_join 函数合并数据集，并指定你想要合并的列  
#merged_data <- left_join(postings, salaries, by = "job_id") 

#salary  <- merged_data[c("med_salary.x", "med_salary.y")]

#查看缺失值
#sapply(salary,function (x) sum(is.na (x)))
#结果相同因此删除merge_data
#rm(merged_data)
#rm(salary)
#rm(salaries)
#因此不需要合并

#使用相同的方法处理benefits
#导入benefits
benefits <- read.csv("./jobs/benefits.csv")

#删除不需要的列—inferred
benefits <- subset(benefits,select = -c(inferred))

# 使用dplyr包进行分组和聚合  
benefits <- benefits %>%  
  group_by(job_id) %>%  
  summarise(type = toString(unique(type)), .groups = 'drop')  
#合并benefits和postings
postings_benefits <- left_join(postings, benefits, by = "job_id")


#开始处理skill
#根据linkedin页面分析，skill很重要
#导入job_skill
job_skills <- read.csv("./jobs/job_skills.csv")
skills <- read.csv("./mappings/skills.csv")

# 执行左连接  
job_skills <- merge(job_skills, skills, by = "skill_abr", all.x = TRUE) 

#删除不需要的列skill_abr
job_skills <- subset(job_skills,select=-c(skill_abr))

# 查看发现一个job匹配多个skills，因此使用dplyr包进行分组和聚合  
job_skills <- job_skills %>%  
  group_by(job_id) %>%  
  summarise( skill_name = toString(unique(skill_name)), .groups = 'drop')  

# 显示聚合后的结果的前6行  
head(job_skills)

# 使用 left_join 函数合并数据集，并指定合并postings和left_join  
postings_benefits_skills <- left_join(postings_benefits, job_skills, by = "job_id") 



#职位行业数据整理——可以分析那个行业发布的数据最多
#导入数据
job_industries <- read.csv("./jobs/job_industries.csv")
industries <- read.csv("./mappings/industries.csv")
# 执行左连接  
industry <- merge(job_industries, industries, by = "industry_id", all.x = TRUE) 
#删除多余的列industry_id
industry <- subset(industry,select=-c(industry_id))
#查看一下job_id是否是唯一
# 查看job_id是否都是唯一的  
unique_job_id1 <- !any(duplicated(industry$job_id))
unique_job_id1
#结果是false，则存在重复的job_id
#看看是否有重复行
has_duplicates <- anyDuplicated(industry)  

if (has_duplicates > 0) {  
  print("存在重复行")  
} else {  
  print("没有重复行")  
}
#结果是没有重复行，那么就说明一个job_id存在对应多个行业#。
# 使用dplyr包进行分组和聚合 
industry<-  industry%>%  
  group_by(job_id) %>%  
  summarise(industries_name = toString(unique(industry_name)), .groups = 'drop')
#查看industry
head(industry)
#合并postings_skills_benefits)和industry
postings_skills_benefits_industry <- left_join( postings_benefits_skills,industry
                                                ,by = "job_id")



#公司数据整理！！！！
#数据导入
data <- postings_skills_benefits_industry
companies <- read.csv("./companies/companies.csv",header = T)
#删除companies中不需要的列：
companies <- subset(companies,select = -c(state,zip_code,address,city))
#作废company_specialities <- read.csv("companies/company_specialities.csv")
company_industries <- read.csv("./companies/company_industries.csv")
employee_counts <- read.csv("./companies/employee_counts.csv")

#分组聚合=====觉得这个company_specialities没有什么研究价值，有company_industries。
#company_specialities<-  company_specialities%>%  
# group_by(company_id) %>%  
#summarise(type = toString(unique(speciality)), .groups = 'drop')

#因此删除company_specialities数据集

#rm(company_specialities)
data <- left_join(data,companies,by="company_id")
#data <- left_join(data,company_specialities,by="company_id")
#data <- left_join(data,company_industries,by="company_id")

#这里会显示多对多问题，所以company_industries中可能存在一个公司对应不同行业，所以进行聚合处理
company_industries<-  company_industries%>%  
  group_by(company_id) %>%  
  summarise(type = toString(unique(industry)), .groups = 'drop')

#再次运行发现问题解决了！
data <- left_join(data,company_industries,by="company_id")

#连接data和employee_counts数据集时删除必要的列，再连接，删除time_recorded
#employee_counts <- subset(employee_counts,select=-c(time_recorded))
#连接data和employee_counts数据集
#data <- left_join(data,employee_counts,by="company_id")
#结果发现有多对多的关系，看数据发现，一个公司的数据是有不同的时间抽取的，所以这里取每一个的最新抽取时间
#首先对time_recorded进行处理，转换成时间数据
# 加载 dplyr 包（如果需要的话）  
# install.packages("dplyr")  
library(dplyr)  

# 假设 employee_counts$time_recorded 已经是秒级时间戳  
# 将其转换为 POSIXct 日期时间对象  
employee_counts$time_recorded <- as.POSIXct(employee_counts$time_recorded, origin = "1970-01-01", tz = "UTC")  

# 现在，你可以按 company_id 和最新的 time_recorded 选择数据  
latest_counts <- employee_counts %>%  
  group_by(company_id) %>%  
  slice_max(order_by = time_recorded, n = 1) %>%  
  ungroup()  

# 查看结果
#这样就抽取成功了
#再进行连接
#发现还是出现多对多，发现有重复值，删除

latest_counts <- latest_counts %>%  
  distinct(company_id, .keep_all = TRUE)  

# 查看结果  
print(latest_counts)
data <- left_join(data,latest_counts,by="company_id")
#终于不再报多对多了！
#至此所有数据合并完毕-----可喜可贺


#查看所有列的缺失值
sapply(data, function (x) sum(is.na (x)))


#至此工作数据整合完毕
#现在需要做的就是删除不需要的列，整理

#1.首先删除不需要的列
#采用dplyr中的select函数
data<- subset(data,select = -c(job_posting_url,application_url,application_type,
                               expiry,closed_time,skills_desc,listed_time,
                               posting_domain, currency, compensation_type,
                               time_recorded,url,name,sponsored,country,max_salary,min_salary,med_salary,pay_period
))
#说明混淆变量
#industries_name是职位的。type.x是benefit的，type.y是公司的。description.x是职位的，description.y是公司的
#后面会改名

#根据缺失值进行处理
sapply(data, function (x) sum(is.na (x)))
#删除有缺失值的skill_name,industries_name,description.y,type.y,company_id
#employee_count,follower_count
#删除company_id为空的行
data <- data[!is.na(data$company_id), ] 
data <- data[!is.na(data$skill_name), ] 
data <- data[!is.na(data$industries_name), ] 
data <- data[!is.na(data$description.y), ] 
data <- data[!is.na(data$type.y), ] 
data <- data[!is.na(data$employee_count), ] 
data <- data[!is.na(data$follower_count), ] 
data <- data[!is.na(data$views), ] 
#数据已经从123849变为120442，删了将近3000数据


#现在查看一下缺失值
sapply(data, function (x) sum(is.na (x))) 
#company_size有4567行缺失值，进行删除
data <- subset(data,select=-c(company_size))
#查看是否有空值并使用sapply函数计算每列空字符串的数量  
sapply(data, function(x) sum(x == "")) 

#？？pay_period缺失值太大，不管。

#根据结果显示，删除有空值的行~description.x company_name,description.y
# 删除company_name、description.x、description.y 为空字符串的行  
data <- data %>%  
  filter(description.x != "" &  company_name != "" & description.y != "")  

#数据从120442变为119469
# 改变存储日期时间对象  
data$original_listed_time <- as.POSIXct(data$original_listed_time/ 1000, origin = "1970-01-01", tz = "UTC")  


#选取了3月20号到4月5号的一个月数据！


# 导入需要的库
library(lubridate)
start_date <- ymd_hms("2023-12-05 21:08:53")  # 开始日期

# 使用ymd_hms函数解析日期和时间  
end_date <- ymd_hms("2024-04-05 00:00:00")

data <- data[data$original_listed_time>= start_date & data$original_listed_time <= end_date, ]


#缺失值主要集中在各种salary,views, applies,remote_allowed,type.x

#将remote_allowed中的缺失值改为0
data$remote_allowed[is.na(data$remote_allowed)] <- 0

#将applies中的缺失值改为0
data$applies[is.na(data$applies)] <- 0


#再查看所有列的缺失值
sapply(data, function (x) sum(is.na (x)))

#工资不做处理

#改变量名称
colnames(data)[colnames(data) == "description.x"] <- "P_description"   #指向职位描述
colnames(data)[colnames(data) == "description.y"] <- "C_description" #指向公司描述
colnames(data)[colnames(data) == "original_listed_time"] <- "listed_time" #职位发出时间
colnames(data)[colnames(data) == "skill_name"] <- "skills" #技能
colnames(data)[colnames(data) == "industries_name"] <- "P_industry" #指向职位的行业领域
colnames(data)[colnames(data) == "type.x"] <- "benefit_type" #福利
colnames(data)[colnames(data) == "type.y"] <- "C_industry" 

# 查看job_id是否都是唯一的
unique_job_id <- !any(duplicated(data$job_id))
unique_job_id
#结果是true，说明job_id唯一的。

#终于除了salary和pay_period都解决完啦
#前面的数据已经好了就用这个data


# 创建一个包含所有可能福利类型的向量
benefit_types <- c("401(k)", "Child care support", "Commuter benefits", "Dental insurance", 
                   "Disability insurance", "Medical insurance", "Paid paternity leave", 
                   "Paid maternity leave","Pension plan", "Student loan assistance", "Tuition assistance", 
                   "Vision insurance")

# 将缺失值转换为字符串 "NA"，方便在后续的匹配中处理
data$benefit_type[is.na(data$benefit_type)] <- "NA"

# 循环遍历每种福利类型，并添加对应的二分类变量列
for (bt in benefit_types) {  
  # 使用 strsplit() 函数将字符串拆分为单独的福利类型
  data[[bt]] <- ifelse(sapply(strsplit(data$benefit_type, ", "), function(x) bt %in% x), 1, 2)  
}

# 检查数据框的结构
str(data)
#删除原来的benefit_type
data <- subset(data,select=-c(benefit_type))
#至此福利类型结果已完成

table(data$formatted_work_type)
table(data$work_type)
#到工作类型,发现这两列是一个东西，删除formatted_work_type
#删除formatted_work_type
data <- subset(data,select=-c(work_type))
# 查看新数据框的前几行  
head(data,5)

#变量类型转换

#首先各种福利类型是因子变量，工作类型、远程办公、formatted_experience_level
#首先处理formatted_work_type中的空值为unknown
# 将空字符串替换为 "none"
for (i in 1:length(data$formatted_experience_level)) {
  if (data$formatted_experience_level[i] == "") {
    data$formatted_experience_level[i] <- "none"
  }
}  
#我就做到这里啦，成功啦！剩下的各自自己改变一下数据类型（如果需要）


#删除不需要的数据
rm(benefits)
rm(companies)
rm(company_industries)
rm(employee_counts)
rm(industries)
rm(industry)
rm(job_industries)
rm(job_skills)
rm(latest_counts)
rm(postings)
rm(postings_benefits)
rm(postings_benefits_skills)
rm(postings_skills_benefits_industry)
rm(skills)

#至此数据预处理圆满完成！！！开心~


#呜呜呜呜发现要统一类型呢
#先给401（k)改个名
data$`401(k)`<- as.factor(data$`401(k)`)
data$`Child care support` <- as.factor(data$`Child care support`)
data$`Commuter benefits` <- as.factor(data$`Commuter benefits`)
data$`Dental insurance` <- as.factor(data$`Dental insurance`)
data$`Disability insurance` <- as.factor(data$`Disability insurance`)
data$`Medical insurance` <- as.factor(data$`Medical insurance`)
data$`Paid paternity leave` <- as.factor(data$`Paid paternity leave`)
data$`Paid maternity leave` <- as.factor(data$`Paid maternity leave`)
data$`Pension plan` <- as.factor(data$`Pension plan`)
data$`Student loan assistance` <- as.factor(data$`Student loan assistance`)
data$`Tuition assistance` <- as.factor(data$`Tuition assistance`)
data$`Vision insurance` <- as.factor(data$`Vision insurance`)
#给401（k)改个名——查找网上资料是retirement savings plan,其他也改名，后面不好弄空格
colnames(data)[colnames(data) == "401(k)"] <- "Retirement_savings_plan"
colnames(data)[colnames(data) == "Child care support"] <- "Child_care_support"
colnames(data)[colnames(data) == "Commuter benefits"] <- "Commuter_benefits"
colnames(data)[colnames(data) == "Dental insurance"] <- "Dental_insurance"
colnames(data)[colnames(data) == "Disability insurance"] <- "Disability_insurance"
colnames(data)[colnames(data) == "Medical insurance"] <- "Medical_insurance"
colnames(data)[colnames(data) == "Paid paternity leave"] <- "Paid_paternity_leave"
colnames(data)[colnames(data) == "Paid maternity leave"] <- "Paid_maternity_leave"
colnames(data)[colnames(data) == "Pension plan"] <- "Pension_plan"
colnames(data)[colnames(data) == "Student loan assistance"] <- "Student_loan_assistance"
colnames(data)[colnames(data) == "Tuition assistance"] <- "Tuition_assistance"
colnames(data)[colnames(data) == "Vision insurance"] <- "Vision_insurance"

#剩下的
data$formatted_work_type <- as.factor(data$formatted_work_type)
data$remote_allowed <- as.factor(data$remote_allowed)
data$formatted_experience_level <- as.factor(data$formatted_experience_level)
data$views <- as.numeric(data$views)
data$applies <- as.numeric(data$applies)
data$employee_count <- as.numeric(data$employee_count)
data$follower_count <- as.numeric(data$follower_count)
data$C_industry <- as.factor(data$C_industry)

#符合大家代码，改变一下数据的名字
#?merged_data <- data


# 加载 dplyr 包
library(dplyr)

#抽取公司数据的事情！
# 先提取公司的数据，company_id, C_description,C_industry,country,employee_count,follower_count
distinct_companies <- data %>%  
  group_by(job_id) %>%  
  distinct(company_id, C_description,C_industry,employee_count,follower_count) %>%  # 列出你想要保留的所有列名  
  ungroup() 
distinct_companies <- subset(distinct_companies,select=-c(job_id))
# 直接在数据框上使用unique函数
distinct_companies <- unique(distinct_companies)
#查看是否有重复值
unique_distinct_company<- !any(duplicated(distinct_companies$company_id))
unique_distinct_company
# 使用 dplyr 进行汇总
summary_data <- data %>%
  group_by(company_id) %>%
  summarise(
    total_jobs = n(),  # 职位数量
    total_views = sum(views, na.rm = TRUE),  # 浏览量总和
    # 假设您想包含 follower_count 和 employee_count，但这些不在data 中
    # 您需要将它们从相应的数据集中合并进来
  )

company_data <- left_join(distinct_companies,summary_data,by="company_id")
# 查看结果
print(company_data)

# 假设 company_data 是您的数据框，其中包含 employee_count 列
# 定义公司规模的分类标准
size_breaks <- c(0, 10, 50, 200, 500, 1000, 5000, 10000, Inf)
size_labels <- c("0",
                 "1",
                 "2",
                 "3",
                 "4",
                 "5",
                 "6",
                 "7")

# 使用 cut 函数创建 company_size 变量
company_data$company_size <- cut(company_data$employee_count, 
                                 breaks = size_breaks, 
                                 labels = size_labels)


#计算数据框 company_data 中缺失值数量
sapply(company_data, function (x) sum(is.na (x)))
company_data$company_size[is.na(company_data$company_size)] <- 0

# 抽取聚类的数据  
dc <- company_data[, c("company_id","total_jobs", "total_views")]  
# 标准化数据  
dc_scaled <- scale(dc[, -1]) # 不包括company_id  

# 使用NbClust选择最佳聚类数  
library(NbClust)  
nc_dc <- NbClust(dc_scaled, min.nc = 2, max.nc = 15, method = "kmeans")  
barplot(table(nc_dc$Best.nc[1,]), xlab="Number of Clusters", ylab="Number of Criteria", main="Best Number of Clusters")  

# K-Means聚类  
library(cluster)  
library(ggplot2)  
library(factoextra)  
set.seed(123)  

# 肘值计算  
fviz_nbclust(dc_scaled, kmeans, method = "wss") +  
  geom_vline(xintercept = 3, linetype = 2) +  
  labs(title = "Elbow Method")  

# K-Means聚类，假设使用NbClust选定的最佳聚类数  
km <- kmeans(dc_scaled, centers =3, nstart = 20)  

# 添加聚类结果到原始数据框（不包括company_id，因为它通常不用于聚类）  
dc$cluster <- as.factor(km$cluster)  

# 聚类可视化  
fviz_cluster(km, data = dc_scaled) +  
  theme_minimal() +  
  ggtitle("Cluster Visualization")  

# 分析建模  
print(km)  

# 将数据按聚类结果分组（使用标准化后的数据）  
Data.Cluster <- data.frame(dc_scaled, cluster = km$cluster)  
Data1 <- Data.Cluster[which(Data.Cluster$cluster == 1), ]  
Data2 <- Data.Cluster[which(Data.Cluster$cluster == 2), ]  
Data3 <- Data.Cluster[which(Data.Cluster$cluster == 3), ] # 注意：这里假设了聚类数为3  

# 注意：由于数据已经标准化，绘制密度图可能不太直观

# 分群1的概率密度图（这里只是展示如何操作，但结果可能不太有意义）  
par(mfrow = c(1, 2))  
plot(density(Data1[, 1]), col = "pink", main = "Scaled total_jobs")  
plot(density(Data1[, 2]), col = "pink", main = "Scaled total_views")  
#分群2的概率密度图
par(mfrow=c(1,2))
plot(density(Data2[,1]),col="pink",main="total_jobs")
plot(density(Data2[,2]),col="pink",main="total_views")
#分群3的概率密度图
par(mfrow=c(1,2))
plot(density(Data3[,1]),col="pink",main="total_jobs")
plot(density(Data3[,2]),col="pink",main="total_views")


# 轮廓系数  
silhouette_result1 <- silhouette(km$cluster, dist(dc_scaled))  
print(silhouette_result1)  
min_silhouette_result1 <- mean(silhouette_result1[, "sil_width"])  
print(paste("Average Silhouette Width:", min_silhouette_result1))  

#结合数据集
df <- left_join(company_data, dc, by = "company_id") 



#接下来是公司描述文本分析


# 加载和安装包
install.packages(c("tidyverse", "tidytext", "wordcloud", "topicmodels"))
library(tidyverse)
library(tidytext)
library(wordcloud)
library(topicmodels)
library(dplyr)
library(ggplot2)

# 提取文本数据
company_description_text <- company_data %>% select(3) %>% pull()

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
texts_cleaned <- sapply(company_description_text, clean_text)

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

# 取前二十个词并绘制直方图
top_20_word <- word_freq %>%
  arrange(desc(n)) %>%
  head(20)

ggplot(top_20_word,aes(x = word, y = n)) +
  geom_bar(stat = "identity",fill="skyblue") +
  labs(title = "Top 20 Word Frequency Distribution", x = "Words", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 生成词云图
set.seed(123) # 设置随机种子
wordcloud(words = word_freq$word, freq = word_freq$n, scale = c(3, 0.5), 
          min.freq =7, colors = brewer.pal(8, "Dark2"))

#进行主题建模之前，先对测试集和训练集的稀疏性进行计算
# 给word_freq 创建 document 列，作为索引ID
if (!"document" %in% colnames(word_freq)) {
  word_freq <- word_freq %>%
    mutate(document = row_number())
}
# 确保 document 和 word 列是字符类型
word_freq <- word_freq %>%
  mutate(document = as.character(document), word = as.character(word))
str(word_freq)

# 创建训练集和测试集
# 定义训练集和测试集的比例
train_ratio <- 0.8
# 计算训练集和测试集的大小
train_size <- round(train_ratio * nrow(word_freq))
test_size <- nrow(word_freq) - train_size
# 从词项矩阵中随机选择训练集和测试集的词项
train_words <- sample(word_freq$word, train_size)
test_words <- sample(word_freq$word[!word_freq$word %in% train_words], test_size)
# 根据选择的词项提取训练集和测试集的样本
train_set <- word_freq[word_freq$word %in% train_words, ]
test_set <- word_freq[word_freq$word %in% test_words, ]


#LDA() 函数需要接收一个文档-词项矩阵（DTM）作为输入,将训练集和测试集转换为文档-词项矩阵（DTM）
#第一次尝试报错了，因此使用更原始的方法来创建文档-词项矩阵（DTM），而不是依赖于 cast_dtm() 函数

# 获取所有的文档 ID和词项
documents <- unique(c(train_set$document, test_set$document))
terms <- unique(word_freq$word)
# 创建文档-词项矩阵（DTM）
train_dtm <- matrix(0, nrow = length(documents), ncol = length(terms),
                                        dimnames = list(documents, terms))
# 填充训练集的文档-词项矩阵（DTM）
for (i in seq_len(nrow(train_set))) {
    train_dtm[train_set[i, "document"], train_set[i, "word"]] <- train_set[i, "n"]
  }
 # 创建测试集的文档-词项矩阵（DTM）
  test_dtm <- matrix(0, nrow = length(documents), ncol = length(terms),
                                        dimnames = list(documents, terms))
# 填充测试集的文档-词项矩阵（DTM）
for (i in seq_len(nrow(test_set))) {
    test_dtm[test_set[i, "document"], test_set[i, "word"]] <- test_set[i, "n"]
 }
# 将文档-词项矩阵（DTM）转换为普通矩阵
train_dtm <- as.matrix(train_dtm)
test_dtm <- as.matrix(test_dtm)
# 找到在 DTM 中没有任何非零计数的文档
docs_with_no_nonzero <- rowSums(train_dtm) == 0
#尝试在创建DTM之前就删掉那些
library(dplyr)
# 找到在 DTM 中没有任何非零计数的文档
docs_with_no_nonzero <- rowSums(train_dtm) == 0
# 获取这些文档的 ID
docs_to_remove <- names(docs_with_no_nonzero[docs_with_no_nonzero])
# 从原始数据中移除这些文档
train_set_filtered <- train_set %>%
  filter(!document %in% docs_to_remove)
# 创建 Document-Term Matrix（DTM）
train_dtm_filtered <- train_set_filtered %>%
  cast_dtm(document, word, n)
# 检查一下创建的 DTM 是否符合预期
# 检是否存在缺失值
cat("训练集是否存在缺失值:", any(is.na(train_dtm)), "\n")
cat("测试集是否存在缺失值:", any(is.na(test_dtm)), "\n")
# 检查文档-词项矩阵的稀疏性
sparsity <- sum(train_dtm == 0) / length(train_dtm)
cat("训练集的稀疏性:", sparsity, "\n")
sparsity_test <- sum(test_dtm == 0) / length(test_dtm)
cat("测试集的稀疏性:", sparsity_test, "\n")
#接近于1，很稀疏！！
#非常稀疏的训练集，不建议进行主题建模