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
#Students-All students who are studious want to pursue higher education.
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
#OBSR- students who had no educational support  from family were equally interested in 
# extra curicullar activities as those whose parents supported them, i.e both are independent

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
m1
summary(m1)

#model2,lesser predictors and but more errors in the model
m2<- C5.0(as.factor(Walc) ~ 	Dalc + goout  , data = train)

m2
summary(m2)

#Plotting the tree for model2-(Only for small decision trees)
plot(m2)