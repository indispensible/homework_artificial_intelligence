#install.packages("jiebaR") 
#install.packages("wordcloud2",dep=T)
rm(list = ls()) 
setwd("C:/Users/吕港/Desktop/大三上/数据挖掘/大作业/房源数据分析/大作业最终文件")			#设置工作路径

######读入数据和基本处理
esf=read.csv("updateHouse.csv",header=T)   	#读入清洗后的数据
##如果你用的是mac系统，请将上一句代码替换为以下代码
#esf<-read.table(file="esfsj.txt",header=T,sep="\t",fileEncoding="UTF-8") 	#读入清洗后的数据
#par(family="STXihei")   #可实现在统计图中显示中文

#基本处理
esf=esf[,c(-1,-2)]				#去掉序号
n=dim(esf)[1]			  #输出样本量。样本量为35706
summary(esf)		  	#查看数据基本描述

#调整变量单位
esf$单价.元.m2.=esf$单价.元.m2./1000			#价格单位转换成千元
names(esf)[3]<-"单价.千元.m2."      #重命名价格变量

esf$总价.元.=esf$总价.元./10000			#总价单位转换成万元
names(esf)[12]<-"总价.万元."      #重命名价格变量

#调整城区和楼层的因子水平顺序，以便作图输出美观
esf$城区=factor(esf$城区,levels=c("浦东","闵行","宝山","徐汇","普陀","杨浦","长宁","松江","嘉定","黄浦","静安","闸北","虹口","青浦","奉贤","金山","崇明"))
esf$楼层=factor(esf$楼层,levels=c("高层","中层","低层"))

#将建造时间转换为三个level的factor变量
levels(esf$建造时间)[levels(esf$建造时间) %in% c("1936-2001")] <- "1936-2001" #comment:全部code统一使用“＝”或者是“<-”
levels(esf$建造时间)[levels(esf$建造时间) %in% c("2013","2014","2015","2016")] <- "2013-2016年"
levels(esf$建造时间)[levels(esf$建造时间)!=c("1936-2001","2013-2016")] <- "2002-2012年"


######对"介绍"变量开展文本分析
###第一步：分词
#安装和加载文本分析所需的程序包
##如未安装程序包，请先运行以下代码
#install.packages("jiebaR")  #安装程序包

#加载程序包
library(jiebaR)

js=esf$介绍             #将变量“介绍”赋值给新变量“js”
lec=data.frame(js)      #定义词语数据框
head(lec)               #查看前几行，看是否有字符编码问题
n=length(lec[,1])       #获取数据集长度，n=35706  

#文本预处理
res=lec[lec!=" "]                                        #剔除缺失   
res=gsub(pattern="http:[a-zA-Z\\/\\.0-9]+","",res)       #剔除URL
res=gsub(pattern="[我|你|的|了|是]","",res)              #剔除特殊词
res=gsub("[0-9０１２３４５６７８９ < > ~]","",res)       #剔除数字等符号

#分词+频数统计+排序(降序) 
words=unlist(res)                #转化为字符型变量
#View(words)


top <- qseg[words]
#View(top)
top2 <- top[nchar(top)>1] #去除字符长度小于2的词语
toptable <- table(top2) #统计词频
#View(toptable)
top120 <- sort(toptable, decreasing = TRUE)[1:120] #降序排序，并提取出现次数最多的前120个词语
top120 #查看120个词频最高的
#View(top120)

#绘制词云
library(wordcloud)
bmp(file="d:/house.bmp", width = 600, height = 600)
par(bg = "white")
wordcloud(names(top120), top120, colors = rainbow(120), random.order=F)
dev.off()

###第三步：建立标签字典。从top500种提取53个关键词（提取标准是词频大于80），按照字面含义自行拟定7个标签：“交通优势”、“区位优势”、“户型优势”、“家装优势”、“价格优势”、“车位优势”和“产权优势”。关键词及其与标签的对应关系见“关键词对应.xlsx”

#分词+频数统计+排序(降序) 
words=unlist(res)                #转化为字符型变量
mixseg=worker("mix")             #使用混合模型分词方法
word=segment(words,mixseg)       #分词
word=lapply(X=word, FUN=strsplit, " ")        #转化为列表
v=table(unlist(word))            #频数统计
v=sort(v,decreasing = T)         #按词频降序排列

#建立词库
wordsData=data.frame(words=names(v), freq = v)               #建立词库（包括词语和词频）
wordsData=subset(wordsData, nchar(as.character(words))>1)     #过滤掉一个字的词语
wordsData=wordsData[,c(1,3)]        
top500=head(wordsData,500)          

keys=top500[top500[,2]>80,]


###第四步：提取衍生标签变量
as.character(esf$介绍)        #将变量“介绍”转化为字符型

#提取标签变量“交通优势”
esf<-cbind(esf[,],交通优势=seq(0,by=0,length.out = n))              #在数据集最后一列后添加变量“交通优势”
esf$交通优势<-ifelse(regexpr("直通",esf$介绍)>1|regexpr("地铁",esf$介绍)>1|regexpr("沿线",esf$介绍)>1|regexpr("直达",esf$介绍)>1|regexpr("方便",esf$介绍)>1|regexpr("交通",esf$介绍)>1,1,0)           #观测样本在变量“介绍”种若含有关键词“地铁”，其变量“交通优势”的取值为1，否则为0         
esf$交通优势=factor(esf$交通优势,levels=c(0,1),labels=c("否","是"))         #调整因子水平顺序
table(esf$交通优势)/n*100               #展示变量频率分布(%)

#提取标签变量“区位优势”
esf<-cbind(esf[,],区位优势=seq(0,by=0,length.out = n))              #在数据集最后一列后添加变量“区位优势”
esf$区位优势<-ifelse(regexpr("学区",esf$介绍)>1|regexpr("黄金",esf$介绍)>1|regexpr("景观",esf$介绍)>1,1,0)      #观测样本在变量“介绍”种若含有关键词“学区”等，其变量“区位优势”的取值为1，否则为0
esf$区位优势=factor(esf$区位优势,levels=c(0,1),labels=c("否","是"))         #调整因子水平顺序
table(esf$区位优势)/n*100               #展示变量频率分布(%)

#提取标签变量“户型优势”
esf<-cbind(esf[,],户型优势=seq(0,by=0,length.out = n))              #在数据集最后一列后添加变量“户型优势”
esf$户型优势<-ifelse(regexpr("户型",esf$介绍)>1|regexpr("朝南",esf$介绍)>1|regexpr("南北",esf$介绍)>1|regexpr("卧室",esf$介绍)>1|regexpr("阳台",esf$介绍)>1|regexpr("高区",esf$介绍)>1|regexpr("厨卫",esf$介绍)>1|regexpr("全明",esf$介绍)>1|regexpr("客厅",esf$介绍)>1|regexpr("采光",esf$介绍)>1,1,0)    #观测样本在变量“介绍”种若含有关键词“户型”等，其变量“户型优势”的取值为1，否则为0
esf$户型优势=factor(esf$户型优势,levels=c(0,1),labels=c("否","是"))         #调整因子水平顺序
table(esf$户型优势)/n*100               #展示变量频率分布(%)

#提取标签变量“家装优势”
esf<-cbind(esf[,],家装优势=seq(0,by=0,length.out = n))              #在数据集最后一列后添加变量“家装优势”
esf$家装优势<-ifelse(regexpr("精装修",esf$介绍)>1|regexpr("装修",esf$介绍)>1|regexpr("精致",esf$介绍)>1|regexpr("拎包",esf$介绍)>1,1,0)        #观测样本在变量“介绍”种若含有关键词“精装修”等，其变量“家装优势”的取值为1，否则为0
esf$家装优势=factor(esf$家装优势,levels=c(0,1),labels=c("否","是"))         #调整因子水平顺序
table(esf$家装优势)/n*100               #展示变量频率分布(%)

#提取标签变量“价格优势”
esf<-cbind(esf[,],价格优势=seq(0,by=0,length.out = n))              #在数据集最后一列后添加变量“价格优势”
esf$价格优势<-ifelse(regexpr("五年",esf$介绍)>1|regexpr("免税",esf$介绍)>1|regexpr("减税",esf$介绍)>1|regexpr("两年",esf$介绍)>1|regexpr("急售",esf$介绍)>1|regexpr("满五",esf$介绍)>1,1,0)        #观测样本在变量“介绍”种若含有关键词“可按揭”等，其变量“价格优势”的取值为1，否则为0
esf$价格优势=factor(esf$价格优势,levels=c(0,1),labels=c("否","是"))         #调整因子水平顺序
table(esf$价格优势)/n*100               #展示变量频率分布(%)



