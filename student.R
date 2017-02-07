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
#maximum students have not filed
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




