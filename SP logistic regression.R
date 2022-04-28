library(survival)
install.packages('survivalROC')
library(survivalROC)

getwd()
data1=read.csv('logitdata.csv',header=T)
data1
head(data1)
nrow(data1) #7676개 데이터

###19~24세만 뽑기 청소년기준
data1=subset(data1,age>=3)

#########결측치 여부 확인하기
ncol(data1) #11개의 컬럼
nrow(data1)
for(i in 1:11){
	if(sum(is.na(data1[,i]))>1){
	   print(sum(is.na(data1[,i])))
	   print(names(data1[i]))
	}
} 

##결측치포함 열 확인

###################결측치 대체하기 결측치 제거 방법으로 진행
data2=na.omit(data1)
nrow(data2)

################### 변수변환
data2
head(data2)
barplot(table(data2$우울감))
data2$우울감=ifelse(data2$우울감>5,1,0)

######대인관계 변수 3개 합치기
data2$대인관계=data2$대인관계1+data2$대인관계2+data2$대인관계3

###수면시간 변수생성
head(data2)
names(data2[4])='잠드는시각'
names(data2[5])='일어나는시각'
max(data2[,5])
max(data2[,4])
min(data2[,4])
(data2[,5]+12)-(data2[,4]-12)
sleep=ifelse(data2[,4]<13,data2[,5]-data2[,4],(data2[,5]+12)-(data2[,4]-12))
data2$sleep=sleep
max(data2$sleep)
min(data2$sleep)
head(data2)
#############로지스틱 회귀분석 진행(sp적용하지 않은것->유의한 변수 먼저 추출하기위함)
data3=data2[,c(1,2,3,9,10,11,13,14)] #사용할 변수만 추출
head(data3)
table(data3$신체건강)
table(data3$정신건강)
table(data3$대인관계)

#추가 변수 변환
data3$대인관계_new=ifelse(data3$대인관계<8,0,ifelse(data3$대인관계<11,1,2))
data3$진로체험=ifelse(data3$진로체험==1,0,1)
data3$취업학원=ifelse(data3$취업학원==1,0,1)
data3$진로교육=ifelse(data3$진로교육==1,0,1)
data3$sleep=ifelse(data3$sleep<7,1,ifelse(data3$sleep<8,2,ifelse(data3$sleep<9,4,6)))
data3=data3[,-7]
head(data3)
table(data3$sleep)
names(data3[5])=c('취업준비')
###
result=glm(우울감~.,data=data3,family=binomial)
summary(result)
new_model=step(result,direction='both')
new_model 

result=glm(우울감~sleep+정신건강+취업학원+대인관계_new,data=data3,family=binomial)
summary(result)

########모델 평가

#vif계산
install.packages("car")
library(car)

vif(result) #조건 만족

#hoslem show test
library(ResourceSelection)
hoslem.test(result$y,result$fit)

#ROC
library(pROC)
rocplot<-roc(우울감~fitted(result),data=data3)
plot.roc(rocplot,legacy.axes = TRUE)
auc(rocplot)

######변수변환-> scoring 적용

#Logit(우울함)=-0.416+0.168(수면시간)-0.6(정신건강)-0.28(대인관계) +0.33(취업준비)
#수면시간이 증가할수록 우울할 확률 높아짐
#정신건강이 좋을수록 우울할 확률 낮아짐
#대인관계가 좋을수록 우울할 확률 낮아짐
#취업준비를 하고 있는 사람이 우울할 확률이 높아짐


#위험요소를 범주로 구성하고 각 범주에 대한 기준 값 결정 B값 설정=risk의 증가값
head(data3)
data4=data3[,c(1,3,5,7,8)]
head(data4)
table(data4$정신건강)
###수면시간 scoring(B값=risk의 증가값)
# B값을 이용해 최종 point 도출
# 우울감이 낮은 범주를 reference로 잡고 score계산 
#######total score point 계산
p1_정신건강=ifelse(data4$정신건강==4,0,ifelse(data4$정신건강==3,2,ifelse(data4$정신건강==2,4,5)))
p1_정신건강 #정신건강이 좋은 그룹에서 우울감이 낮기 때문에 매우좋음 범주를 reference로 두고 scoring
head(data4)

########total score point 계산
table(data4$취업학원)
p1_취업준비=ifelse(data4$취업학원==0,0,1)

##########total score point
p1_수면시간=ifelse(data4$sleep==1,0,ifelse(data4$sleep==2,1,ifelse(data4$sleep==4,2,3)))

###############total score point
table(data4$대인관계_new)
p1_대인관계_new=ifelse(data4$대인관계_new==3,0,ifelse(data4$대인관계==2,1,2))

###set the constant B
#We now define the constant for the points system, or the number of regression units 
#that will correspond to one point. 
#Here, we let B reflect the increase in risk associated with a 2-hours increase in sleep time
B=2*(0.168)

#############data setting(각 관측치 스코어 계산하기위함)
head(data4)
우울감=data4$우울감
last_data=cbind(우울감,p1_정신건강)
last_data=cbind(last_data,p1_취업준비,p1_수면시간,p1_대인관계_new)

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
p_last #최종 우울감 risk도출


