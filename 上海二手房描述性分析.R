rm(list = ls())

house<-read.csv("C:/Users/吕港/Desktop/大三上/数据挖掘/大作业/房源数据分析/大作业最终文件/secondHouse.csv")

#观察端点值
house[house$average_price_inlist==max(house$average_price_inlist),]
house[house$average_price_inlist==min(house$average_price_inlist),]
house[house$total_price==max(house$total_price),]
house[house$total_price==min(house$total_price),]
house[house$house_size==max(house$house_size),]
house[house$house_size==min(house$house_size),]
median(house$average_price_inlist)

#整理数据集，卧室数厅数只保留数字，楼层只保留四个类别（低层/中层/高层），删去序号列
house$room_number<-substr(house$room_number,1,1)
house$parlour_number<-substr(house$parlour_number,1,1)
house<-house[house$house_size<300,c(1:14)] #删去了面积超过300m2的豪宅
house<-house[house$average_price_inlist<120000,c(4,13,3,7,9,14,5,12)] #将待用的变量重新组成新的数据框，并且删去了单价超过12万的豪宅
names(house)[1:8]<-c("单价","总价","面积","卧室数","厅数","楼层","建造时间","城区")

#观察各组的情况
library(dplyr)
summarise(group_by(house,楼层),n=n(),price=mean(单价),area=mean(面积)) #分组还算平均
summarise(group_by(house,厅数),n=n(),price=mean(单价),area=mean(面积)) #3厅以上不超过400套，于是将3及3以上合并为2+组
summarise(group_by(house,卧室数),n=n(),price=mean(单价),area=mean(面积))#将5/6/7/8/9合并为4+组
summarise(group_by(house,城区),n=n(),price=mean(单价),area=mean(面积))
summarise(group_by(house,建造时间),n=n(),price=mean(单价),area=mean(面积))

#变量分组调整
house$楼层<-factor(house$楼层,order=TRUE,levels=c("低层","中层","高层"))
house$厅数[house$厅数==3|house$厅数==4|house$厅数==6|house$厅数==7|house$厅数==8|house$厅数==9]<-"2+"
house$厅数<-factor(house$厅数,order=TRUE,levels=c("0","1","2","2+"))
house$卧室数[house$卧室数==5|house$卧室数==6|house$卧室数==7|house$卧室数==8|house$卧室数==9]<-"4+"
house$卧室数<-factor(house$卧室数,order=TRUE,levels=c("1","2","3","4","4+"))
house$城区<-factor(house$城区,order=TRUE,levels=c("浦东","闵行","宝山","徐汇","普陀","杨浦","长宁","松江","嘉定","黄浦","静安","闸北","虹口","青浦","奉贤","金山","崇明"))

#设置通用图像样式
library(ggplot2)
windowsFonts(myFont = windowsFont("微软雅黑"))
my.theme<-theme(
  axis.text.x=element_text(size=25),
  axis.title.x=element_text(size=25,family="myFont"),
  axis.text.y=element_text(size=25),
  axis.title.y=element_text(size=25,family="myFont"),
  legend.position="none"
)
#观察因变量
price<-ggplot(house,aes(单价))
price+geom_histogram(binwidth=10000)+xlab("单位面积价格（元）")+ylab("房源数量")+my.theme
#观察自变量-
area<-ggplot(house,aes(面积))
area+geom_histogram(binwidth=10)+xlim(c(0,300))+xlab("面积（平方米）")+ylab("房源数量")+my.theme
#因变量-自变量关系
price.area<-ggplot(house,aes(log(面积),log(单价)))
price.area+geom_point()+xlab("对数面积")+ylab("对数单位面积价格")+my.theme+geom_smooth(method="lm")
price.bedroom<-ggplot(house,aes(卧室数,单价))
price.bedroom+geom_boxplot(aes(fill=卧室数))+my.theme+ylab("单位面积价格（元）")
last_plot()+scale_x_discrete(labels=c("1","2","3","4","4+"))
price.diner<-ggplot(house,aes(厅数,单价))
price.diner+geom_boxplot(aes(fill=厅数))+my.theme+ylab("单位面积价格（元）")
last_plot()+scale_x_discrete(labels=c("0","1","2","2+"))
price.floor<-ggplot(house,aes(楼层,单价))
price.floor+geom_boxplot(aes(fill=楼层))+my.theme+ylab("单位面积价格（元）")
last_plot()+scale_x_discrete(labels=c("低层","中层","高层"))
price.district<-ggplot(house,aes(城区,单价))
price.district+geom_boxplot(aes(fill=城区))+my.theme+ylab("单位面积价格（元）")+xlab(NULL)
last_plot()+scale_x_discrete(labels=c("浦东","闵行","宝山","徐汇","普陀","杨浦","长宁","松江","嘉定","黄浦","静安","闸北","虹口","青浦","奉贤","金山","崇明"))

