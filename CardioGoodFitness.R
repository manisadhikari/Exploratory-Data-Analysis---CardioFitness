#First R project DEC 18 BACP 

#check for vaiable 
ls()

#Reading file 
options(scipen=999)#to avoid exponential output in graph

#Cardio_Goods=read.csv(file.choose())
getwd()
Cardio_Goods=read.csv("CardioGoodFitness.csv")
attach(Cardio_Goods) # to use features indipendently .
is.na.data.frame(Age) #same as below statement 
sum(is.na(Cardio_Goods$Age))# check missng values 

Cardio_Goods


#inoking Library package for data wrangling , data plotting and knit
library(dplyr)
library(knitr)
library(ggplot2)


##high level overview 

#rows and col
dim(Cardio_Goods)
#structure
str(Cardio_Goods)
#summary
summary(Cardio_Goods)
mean(Cardio_Goods$Age)

sd(Cardio_Goods$Age)
var(Cardio_Goods$Age)
boxplot(Cardio_Goods$Age)


class(Cardio_Goods)
head(Cardio_Goods)
tail(Cardio_Goods)

#Univariate analysis

#summary(Cardio_Goods,Product)

#analyse Product feature
filter_TM195 = filter(Cardio_Goods,Product=='TM195')
filter_TM498 = filter(Cardio_Goods,Product=='TM498')
filter_TM798 = filter(Cardio_Goods,Product=='TM798')
filter_TM195
filter_TM498
filter_TM498

summary(Cardio_Goods)

summary(filter_TM195)

summary(filter_TM498)

summary(filter_TM498)

par(mfrow=c(2,2)) # creating multiple graphs in single pane 

library(ggplot2)

ggplot(Cardio_Goods,aes(x=Product)) +
  geom_bar() + labs(y='count', title = 'Products ')


ggplot(Cardio_Goods,aes(x=filter_TM195$Product)) +
  geom_bar() + labs(y='count', title = 'Products ')

dev.off()




par(mfrow=c(2,2)) # creating multiple graphs in single pane 
plot(Product ~ Age , Cardio_Goods)
plot(filter_TM195$Product ~ Age , filter_TM195)
plot(filter_TM498$Product ~ Age , filter_TM498)
plot(filter_TM798$Product ~ Age , filter_TM798)

##not working  --boxplot(Cardio_Goods$Product, col = "Blue" )
  cor(Age,Product)
attach(Cardio_Goods)

rbind(summary(Cardio_Goods$Age),summary(filter_TM195$Age),
      summary(filter_TM498$Age),summary(filter_TM798$Age))


rbind(summary(Cardio_Goods$Gender),summary(filter_TM195$Gender),
      summary(filter_TM498$Gender),summary(filter_TM798$Gender))

rbind(summary(Cardio_Goods$Usage),summary(filter_TM195$Usage),
      summary(filter_TM498$Usage),summary(filter_TM798$Usage))

hist(Cardio_Goods$Age)
hist(filter_TM195$Age)
hist(filter_TM498$Age)
hist(filter_TM798$Age)

#using by
by(Cardio_Goods,INDICES=Cardio_Goods$Product, FUN = summary)


plot(Age)
plot(filter_TM195$Age)
plot(filter_TM498$Age)
plot(filter_TM798$Age)


summary(Cardio_Goods$Product)
summary(Cardio_Goods$Age)
boxplot(Cardio_Goods$Age)
hist(Cardio_Goods$Age)
plot(density(Cardio_Goods$Age)) # not sure how its working
plot(Cardio_Goods$Age)
summary(Cardio_Goods$Gender)
plot(Cardio_Goods$Gender)
plot(Cardio_Goods$Income)
summary(Cardio_Goods$Education)
plot(density(Cardio_Goods$Education),main='Education')
summary(Cardio_Goods$MaritalStatus)

summary(Cardio_Goods$Usage)
summary(Cardio_Goods$Fitness)
summary(Cardio_Goods$Income)
summary(Cardio_Goods$Miles)
plot(Cardio_Goods$Income)
plot(density(Cardio_Goods$Income),main='INCOme')
#filter_TM98=filter(Cardio_Goods,Cardio_Goods$Product=='TM798')
View(filter_TM98)
hist(filter_TM98$Fitness)
##using tblyr librabry
#convert to local data frame
Cardio_Goods_tbl = tbl_df(Cardio_Goods)
Cardio_Goods_tbl

#Bivariate analysis 
#filter on some condition 
filter(Cardio_Goods_tbl,Product=='TM195' & Gender=='Male')

#select few columns 
select(Cardio_Goods_tbl,  Age,Gender,Usage)

#chaining 
Cardio_Goods_tbl %>% 
  select(Age,Gender) %>% 
  filter(Age > 40)

# Mean age for each product user 
Cardio_Goods_tbl %>% 
  group_by(Product) %>% 
 summarise(Age_mean = mean(Age))
    



  # Mean Usage for each product -
Cardio_Goods_tbl %>% 
  group_by(Product) %>% 
  summarise(Usage_mean = mean(Usage))


# Mean Fitness level for each product 
Cardio_Goods_tbl %>% 
  group_by(Product) %>% 
  summarise(Fitness_mean = mean(Fitness))


# Mean INcome  level for each product user
Cardio_Goods_tbl %>% 
  group_by(Product) %>% 
  summarise(Income_mean = mean(Income))

# Mean Education  level for each product user
Cardio_Goods_tbl %>% 
  group_by(Product) %>% 
  summarise(Edu_mean = mean(Education))

Cardio_Goods_tbl %>% 
  group_by(Product) %>% 
  summarise(Miles_mean = mean(Miles))


# Mean age level for each gender  
Cardio_Goods_tbl %>% 
  group_by(Gender) %>% 
  summarise(Fitness_mean = mean(Age))


# Mean usage  level for each gender
Cardio_Goods_tbl %>% 
  group_by(Gender) %>% 
  summarise(Usage_mean = mean(Usage))


# Mean Income  level for each gender
Cardio_Goods_tbl %>% 
  group_by(Gender) %>% 
  summarise(Inc_mean = mean(Income))


# Mean fitness  level for each gender
Cardio_Goods_tbl %>% 
  group_by(Product) %>% 
  summarise(Fit_mean = mean(Usage))


Cardio_Goods_tbl %>% 
  group_by(Gender) %>% 
  summarise(Fit_mean = mean(Fitness))

# Mean Miles   level for each gender
Cardio_Goods_tbl %>% 
  group_by(Gender) %>% 
  summarise(Miles_mean = mean(Miles))

#Fitness mean per marital status 
Cardio_Goods_tbl %>% 
  group_by(MaritalStatus) %>% 
  summarise(Miles_mean = mean(Fitness))


#INcome mean per marital status 
Cardio_Goods_tbl %>% 
  group_by(MaritalStatus) %>% 
  summarise(Miles_mean = mean(Income))

#Uage mean per marital status 
Cardio_Goods_tbl %>% 
  group_by(MaritalStatus) %>% 
  summarise(Miles_mean = mean(Usage))

#Miles mean per marital status 
Cardio_Goods_tbl %>% 
  group_by(MaritalStatus) %>% 
  summarise(Miles_mean = mean(Miles))

#Edu mean per marital status 
Cardio_Goods_tbl %>% 
  group_by(MaritalStatus) %>% 
  summarise_(Miles_mean = mean(Education))

#summarise each
Cardio_Goods_tbl %>% 
  group_by(MaritalStatus) %>% 
  summarise_each(funs(mean), Income,Usage,Fitness,Education,Age , Miles )

Cardio_Goods_tbl %>% 
  group_by(Product) %>% 
  summarise_each(funs(mean), Income,Usage,Fitness,Education,Age, Miles  )


Cardio_Goods_tbl %>% 
  group_by(Usage) %>% 
  summarise_each(funs(mean), Income,Fitness,Education,Age )


Cardio_Goods_tbl %>% 
  group_by(Gender) %>% 
  summarise_each(funs(mean), Income,Fitness,Education,Age,Usage )


#group by more then 1 column 
Cardio_Goods_tbl %>% 
  group_by(Product,Gender,MaritalStatus) %>% 
  summarise_each(funs(mean), Income,Usage,Fitness,Education,Age, Miles) 
  
#Bivariate - between cont. feature 

plot(Product ~ Miles , Cardio_Goods_tbl)
plot(Product ~ Fitness , Cardio_Goods_tbl)

plot(Age ~ Income , Cardio_Goods_tbl)
plot(Age ~ Product , Cardio_Goods_tbl)
plot(Age ~ MaritalStatus , Cardio_Goods_tbl)
plot(Age ~ Miles , Cardio_Goods_tbl)
plot(Age ~ Education , Cardio_Goods_tbl)
plot(Fitness ~ Age , Cardio_Goods_tbl)
plot(Usage ~ Age , Cardio_Goods_tbl)
plot(Product ~ Income , Cardio_Goods_tbl)
plot(Product ~ Education , Cardio_Goods_tbl)
plot(Fitness ~ Usage , Cardio_Goods_tbl)
plot(Income ~ Fitness , Cardio_Goods_tbl)
plot(Miles ~ MaritalStatus , Cardio_Goods_tbl)
plot(Miles ~ Gender , Cardio_Goods_tbl)



plot(Education ~ Gender , Cardio_Goods_tbl)
plot(MaritalStatus ~ Gender , Cardio_Goods_tbl)
plot(Usage ~ Gender , Cardio_Goods_tbl)
plot(Gender ~ Fitness , Cardio_Goods_tbl)
plot(Education ~ Income , Cardio_Goods_tbl)

plot(Usage  ~ Fitness , Cardio_Goods_tbl)

plot(Income  ~ Usage , Cardio_Goods_tbl)

plot(Miles ~ Gender , Cardio_Goods_tbl)

plot(Miles  ~ Education , Cardio_Goods_tbl)

plot(Miles ~ Education , Cardio_Goods_tbl)
plot(Age ~ Gender , Cardio_Goods_tbl)
plot(Age ~ MaritalStatus , Cardio_Goods_tbl)


x#(Cardio_Goods$Age ~ Cardio_Goods$Usage, Cardio_Goods)
vars(Cardio_Goods$Age)

# ggplot2
install.packages("ggplot2")
library(ggplot2)
str(Cardio_Goods_tbl)

##Categorical variables 
ggplot(Cardio_Goods_tbl,aes(x=Gender,colour="smooth")) +
  geom_bar() + labs(y='count', title = 'Male/Female')
  
ggplot(Cardio_Goods_tbl,aes(x=MaritalStatus)) + geom_bar() + theme_light()

ggplot(Cardio_Goods_tbl,aes(x=Usage)) + geom_bar()+
    labs(y='count of Usage', title = 'Usage Summary')
  
ggplot(Cardio_Goods_tbl,aes(x=Fitness)) + geom_bar()+
   labs(y='count', title = 'Fitness  level')

ggplot(Cardio_Goods_tbl,aes(x=Education)) + geom_bar(alpha=0.4) +
   labs(y='count', title = 'Education Details')

ggplot(Cardio_Goods_tbl,aes(x=Age)) + geom_bar() + 
  labs(y='count', title = 'Age of User')

ggplot(Cardio_Goods_tbl,aes(x=Product)) + geom_bar(alpha=0.4) + 
  labs(y='# of People', title = 'Product Usage')

ggplot(Cardio_Goods_tbl,aes(x=Income)) + geom_boxplot(alpha=0.4) + 
  labs(y='# of People', title = 'Product Usage')


ggplot(Cardio_Goods_tbl,aes(x=Gender, fill=Product)) +
    geom_bar() + labs(y='count', title = 'Product Usage with Gender')

ggplot(Cardio_Goods_tbl,aes(x=Usage, fill=Product)) +
  geom_bar() + labs(y='count', title = 'Usage per product')

ggplot(Cardio_Goods_tbl,aes(x=Fitness, fill=MaritalStatus)) +
  facet_wrap(~ Product) +
  geom_bar() + stat_summary(fun.y = mean())


ggplot(Cardio_Goods_tbl,aes(x=Gender, fill=Product)) +
  facet_wrap(~ MaritalStatus)+
    geom_bar(colour="red") + labs(y='count',title = 'Product Usage with Gender and relationship status')

##Comtineous variables 
    ggplot(Cardio_Goods_tbl, aes(x=Income)) +
      geom_histogram(binwidth = 10000, colour='red' )+
      labs(y='Users count', title = 'INcome dist')
    
    ggplot(Cardio_Goods_tbl, aes(x=Age, fill=Product)) +
      facet_wrap(~ Usage ) +
      geom_histogram(binwidth = 10 )+
      labs(y=' count', title = 'Product distribution with usage and age')
    
    
    
    ggplot(Cardio_Goods_tbl, aes(x=Miles, fill=Usage)) +
      facet_wrap(~Fitness) +
      geom_histogram(binwidth = 50 )+ theme_bw() +
      labs(y=' count', title = 'Miles and Fitness')
    
    ggplot(Cardio_Goods_tbl, aes(x=Miles)) +
      facet_wrap(~Income) +
      geom_histogram(binwidth = 50 )+ theme_bw() +
      labs(y=' count', title = 'Miles and Fitness')
    
    
     
    #############variable transformation ##################
    
    Age_group = ifelse(Age < 26, "Early youth",
                       ifelse(Age > 35, "Middle Aged adult",
                              "late youth"))
    EducationLevel <- ifelse(Education<13, "Higher Secondary",
                             ifelse(Education>16, "Masters/ Post Graduation",
                                    "Graduation"))
    Usagelevel <- ifelse(Usage<4, "Fitness Amateurs",
                         ifelse(Usage>5, "Fitness Freaks",
                                "Fitness Regulars"))
    FitnessLevel <- ifelse(Fitness<3, "Low Intensity",
                           ifelse(Fitness>4, "High Intensity",
                                  "Medium Intensity"))
    HHIncome <- ifelse(Income<40001, "Low Income HH",
                       ifelse(Income>60000, "High Income HH",
                              "Medium Income HH"))
    TotalDistance <- ifelse(Miles<61, "Low Usage",
                            ifelse(Miles>120, "High Usage",
                                   "Medium Usage"))
    
Cardio_Goods_new = cbind(Cardio_Goods,Age_group,EducationLevel,Usagelevel,FitnessLevel,
                         HHIncome,TotalDistance)                       
                       
Cardio_Goods_new        

        