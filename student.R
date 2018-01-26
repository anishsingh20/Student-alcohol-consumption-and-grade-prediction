#Student alcogol consumption 
stu<-read.csv('student-mat.csv')

#loading essential packages
library(ggplot2)
library(dplyr)
library(tidyr)



#Analysis of Alcohol consumption in Students
#sample of size 395
str(stu)
#count of sexes
table(stu$sex)
#SAMPLE has more females than males -F=208 , M=187

#proportions of sex by age of students
prop.table(table(stu$sex,stu$age))
#for females max prop of females of age 16 & 17,18
#for males max prop of age 15,16,17 
#overall max amount of males and females belong to age 16

prop.table(table(stu$school))
#max students taken from school GP - Gabriel Pereira ,Portugal-88% students



#plotting histogram of age and scaling by coloring by sex
p1=ggplot(aes(x =age, fill=sex) , data = na.omit(stu)) +
  geom_histogram(binwidth=.1) +
  scale_color_brewer(type='div')
#max students belonging to age 16,17,18  & 15
#has more no of females in the sample


#Freq-polygon
ggplot(aes(x=age,color=sex), data  = na.omit(stu)) + 
  geom_freqpoly(binwidth=1) + 
  scale_x_continuous(limits=c(15,20), breaks=seq(15,20,1))


table(stu$age,stu$famsize)

#More males and females both have family size greater than 3
table(stu$sex,stu$famsize)
#barplot -containing x-axes as categorical var
ggplot(aes(x = famsize), data = na.omit(stu)) + 
  geom_bar(aes(fill=sex))


#Pstatus-parents's maritial status, A-'apart', T-'living together'
table(stu$Pstatus)#most students have parents living together

table(stu$famrel,stu$Pstatus,stu$sex)

#almost all students have good family relations nad for maximum students parents live together
ggplot(aes(x = famrel) , data = stu)  +
  geom_bar(aes(fill=sex)) + 
  facet_wrap(~ Pstatus) 

prop.table(table(stu$Pstatus,stu$studytime))
#MAJOR OBSERVARTION-
#1) Majority of students have parents living together
#2)Maximum no of students study 2 to 5 hours weekly and least 10 hours weekly
#2) after checking the proportions of Pstatus and study time-we find out that for students 
#whose parents are together they study more compared to those whose parents who do not live together.
#OBVIOUSLY WE CAN MAKE OUT THAT DUE TO FAMILY ISSUES THEY ARE NOT ABLE TO CONCENTRATE ON THEIR STUDIES


#FINDING OUT WHO STUDIES MORE-MALES OR FEMALES-

table(stu$sex,stu$studytime)
#males study less than females on a weekly basis

ggplot(aes(x=studytime,fill=Pstatus),data= stu) +
  geom_bar() +
  xlab('Weekly Study Time') 



table(stu$failures,stu$sex)
#maximum students have not failed
#converting to factor variable ie- cartegorical value
stu$failures<-factor(stu$failures,labels=c('Never Failed','Failed once',' failed twice','Failed Thrice'))   

table(stu$age,stu$failures)  


table(stu$failures,stu$school)

ggplot(aes(x = age), data = stu )  +
  geom_freqpoly(aes(color = failures),binwidth=1) + 
  scale_x_continuous(limits=c(15,19), breaks = seq(15,19,1)) + 
  coord_trans( y = 'sqrt')
#maximum amount of students have never failed


ggplot(aes(x = failures), data = stu )  +
  geom_bar(aes(fill = sex))




#Particiption in extra -caricullar activities
table(stu$activities,stu$sex)
#More number of males participate in extra caricullar activities than females


#Reason to choose the school-
ggplot(aes(x=reason) , data = stu) + 
  geom_bar() + 
  facet_wrap(~ school  )
#most of the students chose the school with proper and appt courses offered by them and reputation 
#and closer to home as well


#BOTH THE SCHOOLS HAVE MORE NO OF FEMALES 
table(stu$school, stu$sex)

table(stu$reason,stu$sex)
#More no of females went for schools's reputation than males


table(stu$internet,stu$failures)
#most of the students have internet connections at home

#distribution of students who wanted to take higher edu
table(stu$higher,stu$sex)
#Test for checking independence of attributes-correlation b/2 categorical variables-one tailed only
chisq.test(stu$higher,stu$sex)
#X-squared = 7.6859, df = 1, p-value = 0.005565(significant value)
#  Hence one can say that a student choice to go for higher education is related to the 
#  gender of the student

#Student's choice for pursuing higher education vs weekly Studyhours


table(stu$studytime,stu$sex)
#On a whole females weekly study for more hours  than boys in this sample.
#also both study time and gender of students are dependant and related to each other
#X-squared = 50.634, df = 3, p-value = 5.854e-11

table(stu$studytime,stu$higher,stu$sex)
#OBSV-
#Students-All students who are studious -> wants to pursue higher education.
# (usually who study 2-10 hours in a week)
#Also amongst gender only few males (who didn't study at all)
#didn't want to go for higher education.,all females wanted to go for
#higher education leaving 4

ggplot(aes(x = studytime), data =stu) + 
  geom_bar(aes(fill=sex)) + 
  facet_wrap(~ higher)



#DATE-11/2/17

#count of students and their home - RURAL or URBAN area
prop.table(table(stu$address))
#Most students from Urban area.

table(stu$address,stu$studytime,stu$sex)
#Again Females studied more than Males even in RURAL and URBAN Areas
#

ggplot(aes(x = studytime), data =stu) +
  geom_histogram(aes(fill=sex), binwidth=0.1) + 
  facet_wrap(higher ~ address)
#in both rural and Urban areas Females are more studious and hard working

prop.table(table(stu$address,stu$higher))*100

chisq.test(stu$address,stu$higher)
#X-squared = 0.33171, df = 1, p-value = 0.5647
#chi-sq values indicate that both address(RURAL AND URBAN) and Student's choice for higher EDU
# are independent of each other, i .e whether a student is from RURAL area or URBAN , it does
#not affects its choice foe taking up higher education- H0 is accepted

#In rural areas 6% said No for higher edu and rest 93% students from RURAL areas said YES for
#Higher EDU out of total population in RURAL area
#In URABN area 4 % for the Students said NO and rest 95 % Students said YEs for higher EDU 
# out of total population in URBAN area



#2)DOES EDUCATION OF PARENTS AFFECT STUDENTS STUDYING HABITS

prop.table(table(stu$Fedu))

prop.table(table(stu$Medu))
#for maximum students mothers have higher Education i.e 131 students mothers have done higher 
# educated 

#More mothers went for higher education(4) than Fathers of the students in the sample



#3)Family Support in Education and their effect on the children and their Study Habits

table(stu$famsup,stu$sex)
#87 males whose family didnt support them educationally as compared to 66 from FEMALE Students

ggplot(aes(x = studytime),data =stu) + 
  geom_histogram(aes(fill=sex),binwidth=.1) + 
  facet_wrap( ~ famsup) + 
  scale_y_continuous(limits = c(0,150) , breaks = seq(0,150,20)) 

table(stu$famsup,stu$studytime,stu$sex)

#Testing for Independence
chisq.test(stu$studytime,stu$famsup)
#X-squared = 10.605, df = 3, p-value = 0.01407
#A significant p-value less than .05(5%) significance level -which asserts that both studytime
# and family support are dependent on each other 
#The intuition is true i.e students with more family support in education  will study more.Those
# students will be more motivated towards studiying more hours and hard working 

#On a whole Males weekly study time is less than 2 hours and for Females it is more

chisq.test(stu$famsup,stu$activities)
#X-squared = 0, df = 1, p-value = 1, no diff in Observed and expected freq
table(stu$activities,stu$famsup)
#OBSR- students who had educational support from family were all interested in 
# extra curicullar activities, i.e both variables are completely dependent

stu$famsup<-ifelse(stu$famsup=='yes','Family Supports Educationally','Does not Supports')
table(stu$famsup , stu$internet)
chisq.test(stu$internet,stu$famsup)
#There are more Students whose family supports them educationally and have provided 
# Internet access at home for their children



#4) Family Relations of Students with their Parents

prop.table(table(stu$famrel))
#50% of the students in the sample had good family realtion , 26% have Excellent family relations
#2% have worst family relations

table(stu$famrel,stu$famsup)

#family educational support vs family relations
ggplot(aes(x = famrel), data = stu) + 
  geom_bar(aes(fill=famsup)) + 
  xlab('Family Relations(1-worst,5-Excellent)')
#OBSER-If the students had bad family realtions(1 or 2 ) still  their 
# family supported them educationally than not supporting them, but as the family relation improves
# (for EXcellent)->almost equal count for No-support and Educational Support from family.


#Family Educational support vs Father jobs

ggplot(aes(x = Fjob),data = stu) + 
  geom_bar(aes(fill=famsup))

table(stu$famsup,stu$Fjob)

#mother's job vs family educational support
ggplot(aes(x = Mjob),data = stu) + 
  geom_bar(aes(fill=famsup))

#Parents on a whole support their children educationally more than not suppporting 
#irrespective of the job,specially
#fathers and mothers who are Teachers & in Health sector mostly support children educationally

#Parents in Other Jobs and  Doing Services comparatively focus less on children education
# and support less educationally


by(stu$absences,stu$traveltime,summary)

#converting traveltime to factor
stu$traveltime<- stu$traveltime  %>%  factor(labels=c("less than 15min","15 to 30 min","30min to 1 hr",">1hr"))

#QUANTILE ANALYSIS
#boxplot for traveltime(discrete-cagegorical) vs no of absences in school(continious)
ggplot(aes(x = traveltime, y = absences ) , data = stu) + 
  geom_boxplot() + 
  #coord_cartesian to preserve the data points and do not remove them due to limitting 
  coord_cartesian(ylim=c(0,20)) +
  #scalling the y coordinates
  scale_y_continuous(breaks=seq(0,20,4)) + 
  coord_trans(y = 'sqrt') 

#top 5% of students
quantile(stu$absences , c(0.90))
#select() to select particular columns
#filter to filter or subset results of query accordng to the logical condition like SQL
stu %>%select(age,sex , Mjob , Fjob , absences) %>% filter(absences >= 14)


#Classification ALgorithm- Desicion Trees
install.packages('C50')
library(C50)


#shuflling the data
set.seed(9850)
g<-runif(nrow(stu))
#Random and shuffled student dataset
stur<-stu[order(g) , ]

#Creating a  Training  Data Set for training the ML algorithm and make it generate a function
# which maps X(set of input var) to Y-output(target) var

train<-stur[sample(nrow(stur),250),]

#creating 1st model

#target variable(Y) should be a factor(Categorical variable) and a set of 
#input variables(predictor var)
m1<- C5.0(as.factor(Walc) ~ 	Dalc +
          	goout + 
          	failures + 
           	Pstatus +
          	higher +
          	Mjob+
          	activities+
          	studytime+
          	freetime+
          	famsize+
          	internet
          , data = train)

#Start splitting the data selecting the best attribute having the 
# highest Information Gain(G) from the top,then descending down again repeating the
# same procedure till output is classified.

#information gain is the amount of information one can gain from selecting a attr and.
#splitting on it.
#less entropy implies to more info gain
#Entropy value is the measure of randomness = - sum( P(v) log(p(v)))
#P(v) is the probability of observing a value v

m1
summary(m1)

#model2,lesser predictors and but more errors in the model
m2<- C5.0(as.factor(Walc) ~ 	Dalc + goout  , data = train)

m2
summary(m2)

#Plotting the tree for model2-(Only for small decision trees)
plot(m2)








table(stu$health, stu$Dalc)
prop.table(table(stu$health , stu$Dalc))
ggplot(aes(x  = health , y  =  Dalc ) , data = stu) + 
  geom_count()




chisq.test(stu$Walc, stu$goout)
#going out and weekly alcohol consumption are strongly related to each other as
# X-squared = 116.57, df = 16, p-value < 2.2e-16 values are significant and H0 can be rejected
#and we can interpret that both variables are directly dependent.

#installing rpart for decision trees(for classification)
install.packages('rpart')
install.packages('rpart.plot')


require(rpart)
require(rpart.plot)

m1<-rpart(as.factor(Walc) ~ 	Dalc +
            goout + 
            failures + 
            Pstatus +
            higher +
            Mjob+
            activities+
            studytime+
            freetime+
            famsize+
            internet
          , data = train, method='class')

m1
summary(m1)

#loading padkages for attractive decision tree plots
library(RColorBrewer)
library(rattle)


fancyRpartPlot(m1)



m2<-rpart(Walc ~ . , data = train , method = 'class')
m2

summary(m2)
rpart.plot(m2,type=4)

#predicting values for  
p1<-predict(m2 , newdata = stur[250:350,] , type='class' )
p1

#Checking accuracy of the model
#comparing the target(Y) values of test set and predicted 
#Y values(calculated by the model)
table(actual=train[100:200,28] , predicted=p1)







#PREDICTING Students's Grade in math

table(stu$G1)
summary(stu$G1)

ggplot(aes(x = G1), data = stu) + 
  geom_histogram(aes(fill=I('green'),color=I('black')))  + 
  facet_wrap(~sex)

#splitting by gender of students
by(stu$G3, stu$sex,summary)
#somewhat normally distributed(mean and median values same)
#we observe that Male students have scored slightly better scores in first exams
# based on the avg scores,median and quantile values (Q3 and Q1)


ggplot(aes(x = sex , y = G1), data = stu) + 
  geom_boxplot() + 
  scale_y_continuous(breaks=seq(0,20,2))

#Second exams
ggplot(aes(x = sex , y = G2), data = stu) + 
  geom_boxplot() + 
  scale_y_continuous(breaks=seq(0,20,2))


#Final exams
ggplot(aes(x = sex , y = G3), data = stu) + 
  geom_boxplot() + 
  scale_y_continuous(breaks=seq(0,20,2))

#test for independence of attributes
chisq.test(stu$freetime,stu$goout)
#both are dependent on each other, high X^2 value and low p-value, reject H0
#the more the free time the more students will go out with friends


chisq.test(stu$freetime,stu$studytime)
#X-squared = 23.53, df = 12, p-value = 0.02355,significant values, reject H0
#hence we can assert that freetime is related to studytime 
ggplot(aes(x = freetime, y = studytime), data= stu) + 
  geom_count()



chisq.test(stu$freetime,stu$Walc)
#X-squared = 42.081, df = 16, p-value = 0.0003838,H0 rejected
#we conclude that freetime and weekly alcohol consumption are dependent on each other

ggplot(aes(x = freetime, y = Walc), data= stu) + 
  geom_count()



chisq.test(stu$Dalc,stu$failures)
#X-squared = 22.614, df = 12, p-value = 0.03119,reject H0
#Daily alcohol consumption is related to failures


require(rpart)
require(rpart.plot)
#predicting the student's grade


model1<-rpart(G3 ~ . , data = stur[1:150,])
model1
summary(model1)

fancyRpartPlot(model1)

summary(residuals(model1))
#mean diff between actual target and calculated Output(G3) is 0 i.e mean(y(obs)-y(cal))=0

plot(residuals(model1))

plot(predict(model1),residuals(model1))

#predicted values on test set
p1<-predict(model1,newdata=stur[200:350,])

#binding the output(G3) of test set and the predicted(G3) values 
accuracy<-cbind(actual=stur[200:350,33],predicted=p1)
acc1<-as.data.frame(accuracy)

#almost both actual G3 values have same distribution(mean,median)
#and predicted values also somewhat have same distribution(mean,median etc)
summary(acc1$predicted)
summary(acc1$actual)
#plot of actul Y values and predicted target(Y) values
ggplot(aes(x=predicted,y=actual ),data = acc1) +
  geom_point()

#making a new column for residuals(error)
acc1<-mutate(acc1,residuals = actual - predicted)
#standerdized residuals-residual values divided by sd of residuals
acc1<-mutate(acc1,Stdresid = residuals/sd(residuals))

ggplot(aes(x = residuals), data =acc1)  + 
  geom_histogram(color='black',fill='yellow',binwidth=.5)
#residuals are somewhat  normally distributed,slightly +vely skewed

summary(acc1$residuals)
#95 percentile
quantile(acc1$residuals,c(0.95))

ggplot(aes(x = Stdresid),data =acc1) + 
  geom_histogram()

summary(acc1$Stdresid)




#2nd Model
model2<-rpart(G3 ~ goout + absences + studytime + Dalc + Walc + famsup , data = stur[200:350,])

summary(model2)

fancyRpartPlot(model2)

summary(residuals(model2))

#school absences most important variable
predict(model2,newdata=data.frame(goout = 1 ,absences = 10 , Walc = 5, Dalc = 4 , famsup = "no",studytime=4))



#USing C50 package for determing student final Grade and checking its accuracy with Rpart

model3<-C5.0(as.factor(Walc) ~. , data = stur[1:150,])
summary(model3)
myTree2 <- C50:::as.party.C5.0(model3)
plot(model3)

#Allows to explicitly visualize the subtrees based on the node number

library(partykit)
plot(myTree2[10])


p2<-predict(model3,newdata = stur[200:350,])
#returns a vector of predicted Walc consumption

#Creating a df with actual target values and predicted target values to check the accuracy
acc2<-as.data.frame(cbind(Actual=stur[200:350,25:28], Predicted.Walc = p2))



#checking the correct predicted values
filter(acc2, Predicted.Walc==Actual.Walc)
#62 correct predictions which match the actual values
filter(acc2, Predicted.Walc!=Actual.Walc)
#89 wrong predictions made

#checking the frequency distriburtion of predicted and actual Walc values using barplots

ggplot(aes(x = Predicted.Walc),data = acc2) + 
  geom_bar()

ggplot(aes(x = Actual.Walc),data = acc2) + 
  geom_bar()

#counting the no of predictions in each class with actual values
t1<-table(actual=stur[200:350,28],predicted= p2)
#62 correct predictions which match the actual values
dim(t1)
#plotting scatterplot of predicted vs actual values 
ggplot(aes(x=Actual.Walc, y=Predicted.Walc), data = acc2) + 
  geom_jitter() + 
  #smoothening the scatterplot (no need to fit a regression model, only fits data)
  geom_smooth()







#Predicting Failures of students using Rpart

#regression tree
model4<-rpart(failures ~ .  ,data = stur[1:250,],method='anova')
model4
summary(model4)
fancyRpartPlot(model4)
rpart.plot(model4)



printcp(model4)
#xerror is the cross validation error
#xerror lowest for CP = 0.01
#CP=pruning parameter (aka cost-complexity parameter)
#prune tree based on CP values with lease Cross validation error
pruned_model4<-prune(model4,cp=0.0167)

rpart.plot(pruned_model4)
  summary(pruned_model4)

summary(residuals(pruned_model4))  
#residuals are somewhat  normally distributed
hist(resid(pruned_model4))

#Predictions--
pruned_predict<-predict(pruned_model4 , newdata = stur[251:351,])

acc3<-as.data.frame(cbind(actual=stur[251:351,15], predicted=pruned_predict))

summary(acc3$actual)
summary(acc3$predicted)








