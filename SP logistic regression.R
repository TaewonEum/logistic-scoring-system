library(survival)
install.packages('survivalROC')
library(survivalROC)

getwd()
data1=read.csv('logitdata.csv',header=T)
data1
head(data1)
nrow(data1) #7676�� ������

###19~24���� �̱� û�ҳ����
data1=subset(data1,age>=3)

#########����ġ ���� Ȯ���ϱ�
ncol(data1) #11���� �÷�
nrow(data1)
for(i in 1:11){
	if(sum(is.na(data1[,i]))>1){
	   print(sum(is.na(data1[,i])))
	   print(names(data1[i]))
	}
} 

##����ġ���� �� Ȯ��

###################����ġ ��ü�ϱ� ����ġ ���� ������� ����
data2=na.omit(data1)
nrow(data2)

################### ������ȯ
data2
head(data2)
barplot(table(data2$��ﰨ))
data2$��ﰨ=ifelse(data2$��ﰨ>5,1,0)

######���ΰ��� ���� 3�� ��ġ��
data2$���ΰ���=data2$���ΰ���1+data2$���ΰ���2+data2$���ΰ���3

###����ð� ��������
head(data2)
names(data2[4])='���½ð�'
names(data2[5])='�Ͼ�½ð�'
max(data2[,5])
max(data2[,4])
min(data2[,4])
(data2[,5]+12)-(data2[,4]-12)
sleep=ifelse(data2[,4]<13,data2[,5]-data2[,4],(data2[,5]+12)-(data2[,4]-12))
data2$sleep=sleep
max(data2$sleep)
min(data2$sleep)
head(data2)
#############������ƽ ȸ�ͺм� ����(sp�������� ������->������ ���� ���� �����ϱ�����)
data3=data2[,c(1,2,3,9,10,11,13,14)] #����� ������ ����
head(data3)
table(data3$��ü�ǰ�)
table(data3$���Űǰ�)
table(data3$���ΰ���)

#�߰� ���� ��ȯ
data3$���ΰ���_new=ifelse(data3$���ΰ���<8,0,ifelse(data3$���ΰ���<11,1,2))
data3$����ü��=ifelse(data3$����ü��==1,0,1)
data3$����п�=ifelse(data3$����п�==1,0,1)
data3$���α���=ifelse(data3$���α���==1,0,1)
data3$sleep=ifelse(data3$sleep<7,1,ifelse(data3$sleep<8,2,ifelse(data3$sleep<9,4,6)))
data3=data3[,-7]
head(data3)
table(data3$sleep)
names(data3[5])=c('����غ�')
###
result=glm(��ﰨ~.,data=data3,family=binomial)
summary(result)
new_model=step(result,direction='both')
new_model 

result=glm(��ﰨ~sleep+���Űǰ�+����п�+���ΰ���_new,data=data3,family=binomial)
summary(result)

########�� ��

#vif���
install.packages("car")
library(car)

vif(result) #���� ����

#hoslem show test
library(ResourceSelection)
hoslem.test(result$y,result$fit)

#ROC
library(pROC)
rocplot<-roc(��ﰨ~fitted(result),data=data3)
plot.roc(rocplot,legacy.axes = TRUE)
auc(rocplot)

######������ȯ-> scoring ����

#Logit(�����)=-0.416+0.168(����ð�)-0.6(���Űǰ�)-0.28(���ΰ���) +0.33(����غ�)
#����ð��� �����Ҽ��� ����� Ȯ�� ������
#���Űǰ��� �������� ����� Ȯ�� ������
#���ΰ��谡 �������� ����� Ȯ�� ������
#����غ� �ϰ� �ִ� ����� ����� Ȯ���� ������


#�����Ҹ� ���ַ� �����ϰ� �� ���ֿ� ���� ���� �� ���� B�� ����=risk�� ������
head(data3)
data4=data3[,c(1,3,5,7,8)]
head(data4)
table(data4$���Űǰ�)
###����ð� scoring(B��=risk�� ������)
# B���� �̿��� ���� point ����
# ��ﰨ�� ���� ���ָ� reference�� ��� score��� 
#######total score point ���
p1_���Űǰ�=ifelse(data4$���Űǰ�==4,0,ifelse(data4$���Űǰ�==3,2,ifelse(data4$���Űǰ�==2,4,5)))
p1_���Űǰ� #���Űǰ��� ���� �׷쿡�� ��ﰨ�� ���� ������ �ſ����� ���ָ� reference�� �ΰ� scoring
head(data4)

########total score point ���
table(data4$����п�)
p1_����غ�=ifelse(data4$����п�==0,0,1)

##########total score point
p1_����ð�=ifelse(data4$sleep==1,0,ifelse(data4$sleep==2,1,ifelse(data4$sleep==4,2,3)))

###############total score point
table(data4$���ΰ���_new)
p1_���ΰ���_new=ifelse(data4$���ΰ���_new==3,0,ifelse(data4$���ΰ���==2,1,2))

###set the constant B
#We now define the constant for the points system, or the number of regression units 
#that will correspond to one point. 
#Here, we let B reflect the increase in risk associated with a 2-hours increase in sleep time
B=2*(0.168)

#############data setting(�� ����ġ ���ھ� ����ϱ�����)
head(data4)
��ﰨ=data4$��ﰨ
last_data=cbind(��ﰨ,p1_���Űǰ�)
last_data=cbind(last_data,p1_����غ�,p1_����ð�,p1_���ΰ���_new)

head(last_data)
2*sum(last_data[1,2:5])
last_data=as.data.frame(last_data)

################scoring system
nrow(last_data)
str(last_data)
length(last_data)
result=vector('list',length(1730))
result
for(i in 1:1730){
	risk=-0.416+(-0.6*last_data[i,2])+(0.33*last_data[i,3])+(0.168*last_data[i,4])+(-0.28*last_data[i,5])+(0.336*sum(last_data[i,2:5]))
	print(risk)
	result[[i]]=risk
	result=unlist(result)	
}
result

p_last=vector('list',length(1730))

for(i in 1:1730){
	risk=1/(1+exp(-result[i]))
	p_last[[i]]=risk
	p_last=unlist(p_last)
}
p_last #���� ��ﰨ risk����

