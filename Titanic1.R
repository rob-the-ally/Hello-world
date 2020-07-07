train1 = read.csv("train.csv")
test1 = read.csv("test.csv")

# test1 has 11 variables and train1 has 12 so before we combine data we need to create the Survived variable 
test1.Survived = data.frame(Survived = rep("None", nrow(test1)), test1[ , ])

#combine the data into data.combine
data.combined1 = rbind(train1, test1.Survived)

#check data for structure type change pclass and survived to factor
str(data.combined1)

data.combined1$Pclass = as.factor(data.combined1$Pclass)
data.combined1$Survived = as.factor(data.combined1$Survived)

#looking at Survived numbers in aggregate 
table(data.combined1$Survived)
#distribution across classes
table(data.combined1$Pclass)

library(ggplot2)

table(data.combined1$Survived, data.combined1$Pclass)

#hypothesis is rich people survive at a higher rate (we use train dataset to look at class survival rates)
train1$Pclass = as.factor(train1$Pclass)

train1$Survived = as.factor(train1$Survived)

ggplot(data = train1 , aes(x = Pclass, fill = Survived)) + 
                  geom_bar(width = 0.5) +
                  xlab("Pclass") +
                ylab("Total Count") + 
                labs(fill = "Survived")
#look at name variable 

head(as.character(train1$Name))

#find out if there are dupplicates
dup.names = data.combined1[which(duplicated(as.character(data.combined1$Name))), "Name"]
dup.names
#verify if the ovbservtions are actual duplicates (extract the entire observation )

data.combined1[which(data.combined1$Name %in% dup.names), ] 

library(stringr)
# could there be predicrive power with the use of title 'Mr.', 'Mrs.' , 'Master.'..
#hypothesis title correlated with age 

misses = data.combined1[which(str_detect(data.combined1$Name, "Miss.")), ]
misses

mres = data.combined1[which(str_detect(data.combined1$Name , "Mrs.")), ]

mister = data.combined1[ which(train1$Sex == "male") , ]

#There might be some predictive power with title, pclass, and survived
extractFunction = function(Name){
    Name = as.character(Name)
    if(length(grep("Miss.", Name)) > 0 ){
        return("Miss.")
    }else if(length(grep("Master.", Name)) > 0 ){
      return("Master.")
    }else if(length(grep("Mrs.", Name)) > 0 ){
      return("Mrs.")
    }else if(length(grep("Mr.", Name)) > 0 ){
      return("Mr.")
    }else {
      return("Other")
    }
  
}

titles = NULL
for (i in 1:nrow(data.combined1)){
  titles = c(titles, extractFunction(data.combined1[i, "Name"]))
}
data.combined1$Title = as.factor(titles)

ggplot(data.combined1[1:891, ], aes( x = Title, fill = Survived)) +
                geom_bar(width = 0.7) +
                facet_wrap(~Pclass) +
                xlab("Title") +
                ylab("Total Count") +
                labs(fill = "Survived")


#visualize the 3-way combination sex, pclass, survived 

ggplot(data.combined1[1:891, ], aes(x = Sex, fill = Survived)) +
              geom_bar(stat = "count") +
              facet_wrap(~Pclass)+
              xlab("Sex") +
              ylab("Total Count") +
              ggtitle("Pclass") +
              labs(fill = "Survived")
      
#title can be used as a proxy for age? 
#seems like sex and age are pretty important and indicative of whether someone survives in the titanic or not
summary(data.combined1$Age) # there are 263 missing values- how to fix that? get the data or replace it with 
# a median or mean value - Imputation (predictive model to predict the value of missing data) 
#or find a proxy to replace the variable - since we believe age and title are correlated we can use it 
#as a proxy 
summary(data.combined1[1:891, "Age"]) #a lot of missing values in the training dataset 

#formula for outliers is 3rd quarter + 1.5 (3rd quarter - 1st quarter)

#build a plot that takes a look at pclass, age, sex, survived
ggplot(data.combined1[1:891, ], aes(x = Age, fill = Survived)) +
              geom_histogram(binwidth = 10) +
              facet_wrap(~Sex + Pclass) +
              xlab("Age") +
              ylab("Total Count")
              
#is title "Master" a good proxy for boys 
boys = data.combined1[which(data.combined1$Title == "Master."), ]
summary(boys$Age)
#Master is a reasonable proxy for male children (min 0.33 max 14.5 only 8/263 NA's )
#probably not to worry about male children title Master 

misses = data.combined1[which(data.combined1$Title == "Miss."),]
summary(misses$Age)
#misses is more complicated it contains 50/263 NA's for Age 
#data squewed towards the right meaning there are more adult women than female children
#plot age of women, survival rate, and pclass

ggplot(misses[misses$Survived != "None", ], aes(x = Age, fill = Survived ))+
                    geom_histogram(binwidth = 5) +
                    facet_wrap(~Pclass) +
                    ggtitle("Age for 'Miss' by Pclass")+ 
                    xlab("Age")+
                    ylab("Total Count")
  
  #find misses that are not traveling with siblings and parents
misses.alone = misses[which(misses$SibSp== 0 & misses$Parch == 0  ), ]
summary(misses.alone$Age)
length(which(misses.alone$Age <= 14.5))
#only 4 with the title of 'Miss' treaveling alone under the age of 14.5                     


#look at sibsp
summary(data.combined1$SibSp)
str(data.combined1$SibSp)
#this is a number of siblings and we can make it a factor instead in order to find some interesting relationships
length(unique(data.combined1$SibSp))
#since there is only 7 unique numericals for sibsp we can change the str to factor
data.combined1$SibSp = as.factor(data.combined1$SibSp)
str(data.combined1$SibSp)

#Now lets take a look at the relationships between sibsp, survived, pclass, title
ggplot(data.combined1[1:891, ], aes(x = SibSp, fill = Survived)) +
                        geom_histogram(stat = "count") +
                        facet_wrap(~Pclass + Title) +
                        ggtitle("Pclass by Title") +
                        xlab("Number of Siblings") +
                        ylab("Total Count")+ 
                        ylim(0, 300)+
                        labs(fill = "Survived")
  
#lets take a look at parch (parent children variable) and change str to factor
data.combined1$Parch = as.factor(data.combined1$Parch)

ggplot(data.combined1[1:891, ], aes(x = Parch, fill = Survived)) +
  geom_histogram(stat = "count") +
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass by Title") +
  xlab("ParCh") +
  ylab("Total Count")+ 
  ylim(0, 300)+
  labs(fill = "Survived")

summary(data.combined1$Title)
summary(data.combined1[data.combined1$Title == "Miss.", "Age" ] ) 

#what about feature engineering a family size factor? 
temp.sibsp = c(train1$SibSp , test1$SibSp)#We grab train and test because data.combined1 are factors instead of integers
temp.parch = c(train1$Parch, test1$Parch)#same deal with Parch
#we want to create a total family feature 
data.combined1$Family.size = as.factor(temp.sibsp + temp.parch + 1 ) #total number siblings/spouses + parents/children + 1 (me) to know total family size

#then visualize it to see if it is predictive
ggplot(data.combined1[1:891, ], aes(x = Family.size, fill = Survived)) +
  geom_histogram(stat = "count") +
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass by Title") +
  xlab("Family Size") +
  ylab("Total Count")+ 
  ylim(0, 300)+
  labs(fill = "Survived")

#LOOKING at ticket wariable 
str(data.combined1$Ticket) #character structure 
data.combined1$Ticket[1:20]
 
#it is hard to see a consistency of the data for Ticket variable 
# lets take a look at the first char for each obs 
ticket.first.char = ifelse(data.combined1$Ticket == ""," ", substr(data.combined1$Ticket, 1, 1))#ifelse condition
#ifelse(test, yes, no ) allows me to have a test condition and if yes then or if not then... 
unique(ticket.first.char) #does this help us create a factor to then visualize or analyze? yes only 16 unique factors 
#this helps us use all these unique characters (16 ) and we can make a factor for analysis and visualize
data.combined1$ticket.first.char = as.factor(ticket.first.char)


#visualize data
ggplot(data.combined1[1:891, ], aes(x = ticket.first.char, fill = Survived)) +
                geom_bar() +
                ggtitle("Survivability by ticket.first.char")+
                ylab("Total Count")+
                xlab("Ticket.first.char")+ 
                ylim(0,350)+
                labs(fill = "Survived")
            
                
#lets do a more complex plot now having a facet_wrap on Pclass
ggplot(data.combined1[1:891, ], aes(x = ticket.first.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass)+
  ggtitle("Pclass")+
  ylab("Total Count")+
  xlab("Ticket.first.char")+ 
  ylim(0,150)+
  labs(fill = "Survived")
                

#lets do a more complex plot now having a facet_wrap on Pclass
ggplot(data.combined1[1:891, ], aes(x = ticket.first.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + Title)+
  ggtitle("Pclass, Title")+
  ylab("Total Count")+
  xlab("Ticket.first.char")+ 
  ylim(0,150)+
  labs(fill = "Survived")
  
  
  
#It seems like Ticket isnt really changing our intuition or it doesnt really help further from what we had
#so we might not use ticket, we already knew what we were able to visualize for the ticket.first.char variable

#now lets look at fair (amount paid for ticket)
#there is a strong correlation between pclass and price (first class costs more etc. but we are still gonna take a look)

summary(data.combined1$Fare)
length(unique(data.combined1$Fare))
str(data.combined1$Fare)
#distribution on the Fare is squewed to the right (high end) of 14.5 lbs, mean is double 33.295
#lets validate that our distribution is skewed to the right 
ggplot(data.combined1, aes(x = Fare)) +
  geom_histogram(binwidth = 5) +
  ggtitle("Distribution of combined fare") +
  ylab("Total count") +
  xlab("Fare")+
  ylim(0, 200)

#lets see if fare has predictive power
ggplot(data.combined1[1:891, ], aes(x = Fare, fill = Survived)) +
  geom_histogram(binwidth = 5) +
  facet_wrap(~Pclass + Title)+
  ggtitle("Pclass, Title")+
  ylab("Total Count")+
  xlab("Fare")+ 
  ylim(0,50)+
  labs(fill = "Survived")

# cabin variable see if it has some predictive power

summary(data.combined1$Cabin)
str(data.combined1$Cabin)
data.combined1$Cabin[1:100]

#replace all blank statements for a 'u'

data.combined1[which(data.combined1$Cabin == ""), "Cabin"] = "U"
data.combined1$Cabin[1:100]

#Take a look at the first letter of the word under the cabin variable for each observation

cabin.first.char = as.factor(substr(data.combined1$Cabin, 1, 1 ))

data.combined1$cabin.first.char = cabin.first.char #crammed into data.combined1 dataset to make it easier

str(cabin.first.char)
levels(cabin.first.char) #explicitly see the different factor levels

#High level plot 
ggplot(data.combined1[1:891, ], aes(x = cabin.first.char, fill = Survived)) +
  geom_bar() +
  ggtitle("Survivability by cabin") +
  ylab("total count") +
  xlab("cabin type")+
  ylim(0, 750)+
  labs(fill = "Survived")
  

#lets dive into a more complex examination using pclass this time
ggplot(data.combined1[1:891, ], aes(x = cabin.first.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass)+
  ggtitle("Pclass") +
  ylab("total count") +
  xlab("cabin type")+
  ylim(0, 750)+
  labs(fill = "Survived")

#Now lets look at pclass and title 
c#not really a lot of signaling that i am seeing 

#How about people with multiple cabins 


data.combined1$cabin.multiple = as.factor(ifelse(str_detect(data.combined1$Cabin, " "), "Y", "N" ))

str(data.combined1$cabin.multiple)
table(data.combined1$cabin.multiple)

#plot with multiple room factor and other to see predictive power 

ggplot(data.combined1[1:891, ], aes(x = cabin.multiple, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + Title)+
  ggtitle("Pclass + Title") +
  ylab("total count") +
  xlab("cabin multiple")+
  ylim(0, 400)+
  labs(fill = "Survived")   #shouldnt worry about anyone having too multiple rooms 

#look at embarked 
str(data.combined1$Embarked)
levels(as.factor(data.combined1$Embarked))

#utilize embarked to find out if it has relevant predictive power

ggplot(data.combined1[1:891, ], aes(x = Embarked , fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + Title)+
  ggtitle("Pclass + Title") +
  ylab("total count") +
  xlab("cabin multiple")+
  ylim(0, 300)+
  labs(fill = "Survived")   

#Title and Pclass are very important and predictive , family size has a good potential,

#Exploratory Modeling - things we have gleamed from the data (building models)  
#60-80% is low level work of cleaning, validating, massaging data, little of it is modeling. 
#Why do exploratory modeling? ML algorithms. RF- repeatedly training decision trees (chooses its own features/variables) 
#helps and validates the intuitions we have gained from our analysis #Exploratory modeling can help us 
#distinguish important features in large quantities (features that are more predictive) 

#Requirements of the Exploratory model
# 1 Simple (no need to worry about hyperparameters )
#2 Fast (we want our models to train as fast as possible )
#3 effective (exploratory model must be powerful enough )
# Apparently Random Forest is king of exploratory modeling  (meets all the criteria above)
#RF provides simple hyperparameter tuning (automatically)
# they can handle numeric, categorical, and correlated (between highly correlated variables) variables without pre-processing. 
#To fit the data into RF it cant have factor categorical variabel >32 levels 
#if continuous integer, it cant have any NULLs for either 

#---------------------------------------------------------
#
#
#             EXPLORATORY MODEL Random Forest
#
#
#--------------------------------------------------------

install.packages("randomForest")
library(randomForest)

#Pclass and Title were the most powerful predictive variables 
rf.train.1 = data.combined1[1:891, c("Pclass" , "Title")]
rf.label = as.factor(train1$Survived)

set.seed(1234)

rf.1 = randomForest(x = rf.train.1, y = rf.label, importance = TRUE, ntree = 1000 )  #keeps track of relative importance of the features and variables so i can report them later on
rf.1
#OOB stands for out of bag. On average ive trained 1000 trees and for each tree they are averaging an error rate of about 20.99% just using 2 variables (plcass and title) 
#basiccally nearing 80% accuracy basically saying based on these two if youre going to survive or not 


#the model predicted with 98% accuracy when someone would perish while 
#the model predicted with 50.8% accuracy when someone would survive... so not so great at that 

table(train1$Survived) #data is skewed towards not surviving 

varImpPlot(rf.1) #graphic representation of the RF results 
#we see that title is way more powerful when it comes to predicting if you survive or not

#train a RF using Pclass, Title, and SibSp
rf.train.2 = data.combined1[1:891, c("Pclass", "Title", "SibSp")]
set.seed(1234)

rf.2 = randomForest(x = rf.train.2, y = rf.label , importance = TRUE, ntree = 1000 )
rf.2
#adding SibSp decrease error rate by 1% and it helps the overall model
#How does it help? increased accuracy rate for those who survived from class.error (2 features = 50.3% to 3 features = 32.7 % )
#However it had a negative accuracy for those that failed to survive (2% to 11.3% class.error)

#now lets do one with Pclass, Title , and ParCh
rf.train.3 = data.combined1[1:891, c("Pclass", "Title", "Parch")]
set.seed(1234)
rf.3 = randomForest(x = rf.train.3, y = rf.label, importance = TRUE, ntree = 1000)
rf.3
#PLOT THE outcomes
varImpPlot(rf.3)
#they both have predictive power (sibsp and parch but it seems like sibs has more )

#what if they both together have better predictive power? (sibsp and parch )

rf.train.4 =  data.combined1[1:891, c("Pclass", "Title", "SibSp","Parch")]
set.seed(1234)
rf.4 = randomForest(x = rf.train.4, y = rf.label, importance = TRUE, ntree = 1000)
rf.4  
  
varImpPlot(rf.4)

#Now lets see if family size has a predictive power response on model
rf.train.5 =  data.combined1[1:891, c("Pclass", "Title", "Family.size")]
set.seed(1234)
rf.5 = randomForest(x = rf.train.5, y = rf.label, importance = TRUE, ntree = 1000)
rf.5  

varImpPlot(rf.5)


# RF might not be able to agreggate SibSp and Parch as well as our feature engineered variable Fam.Size
#error rate was lower for our engineered feature 

#now lets try SibSp with Fam.size 
rf.train.6 =  data.combined1[1:891, c("Pclass", "Title", "Family.size", "SibSp")]
set.seed(1234)
rf.6 = randomForest(x = rf.train.6, y = rf.label, importance = TRUE, ntree = 1000)
rf.6  

varImpPlot(rf.6)

#it seems like sibsp confused the model and increased the error rate from 18.18 to 19.87 

#lets try it with Parch + Fam.size instead 
rf.train.7 =  data.combined1[1:891, c("Pclass", "Title", "Family.size", "Parch")]
set.seed(1234)
rf.7 = randomForest(x = rf.train.7, y = rf.label, importance = TRUE, ntree = 1000)
rf.7  

varImpPlot(rf.7)

#parch isnt as predictive as sibsp so error rate goes up even more 

#Which model is the most accurate 
rf.5 #RF is gonna predict that our estimate of our accuracy is 81.82

#---------------------------------------------------------------------------------
#
#
#
#                     Data Science with Cross Validation
#
#
#
#---------------------------------------------------------------------------------
  
  