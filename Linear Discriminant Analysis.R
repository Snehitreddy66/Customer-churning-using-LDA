#----------Discriminant Analysis--------------

#Load the dataset
data =read.csv("churn.csv")

str(data)

#Converting the target variable to categorical variable
data$churn=factor(data$churn)


#Find the baseline churn rate for the dataset
prop.table((table(data$churn)))

#the baseline churn rate is 14.14%, which is an indicator that the 
#dataset is unbalanced(More #FALSE)

#Exploratory Data Analysis
#Determine the impact of the variables on churn, starting with account length
library(ggplot2)
ggplot(data, aes(x=churn, y=account_length, fill=churn)) + 
  geom_boxplot()

#Observation
#Account length does not seem to have an influence on customer churn. 

#Explore churn by voicemails
ggplot(data, aes(x=churn, y=number_vmail_messages, fill=churn)) + 
  geom_boxplot()

#Observation
# The number of voicemail messages seems to be lower for customers 
#who churn compared to those who don't

ggplot(data, aes(x=churn, y=total_day_charge, fill=churn)) + 
  geom_boxplot()

#Observation
#Generally, tariffs for day, evening, night, and international calls 
#are higher for customers who churn compared to those who don't. 
#This could indicate that customers who churn are not happy with the 
#amount of money they are paying for their plan.

ggplot(data, aes(x=churn, y=number_customer_service_calls, fill=churn)) + 
  geom_boxplot()

#Observation
#The number of customer service calls made to customers who churn is 
#relatively high. 
#This indicates that customers who have churned have tried contacting 
#customer service
#but might have not received a satisfactory resolution to their issue.

# Scale the variables so that the range of each variable does not have 
#an influence on the discriminant coefficients.
# Scale X variables
y <- subset(data,select=c(1:7))
scaled_y=scale(y)
data10=cbind(data[8],scaled_x)

#Split into training and test sets
library(caTools)

set.seed(123)
split = sample.split(data$churn, SplitRatio = 0.7)
traindata = subset(data, split == TRUE)
testdata = subset(data, split == FALSE)


#Check if distribution of partition data is correct Testing dataset
prop.table((table(traindata$churn)))

prop.table((table(testdata$churn)))

#Test the Significance of the Discriminant Function Using MANOVA

#Null Hypothesis:
#The null hypothesis of MANOVA is that all the means of the 
#independent variables are equal, 
#which implies that the independent variables are not differentiators 
#of the group.

#Alternate Hypothesis
#The alternative hypothesis is that at least one independent variable 
#has a different mean or, in other words, a significant differentiator.

head(traindata)
X1=cbind(as.matrix(traindata[,2:8]))
Y1=as.vector(traindata[,1])
Manova=manova(X1~Y1)
summary(Manova, test = "Wilks")

#Observations
#1.the Wilks' lambda for MANOVA is closer to 1, indicating that the 
#extent of discrimination in the model is relatively low.

#2. the p-value is highly significant, indicating that the null 
#hypothesis cannot be accepted.
#This implies that the discriminant model is highly significant.

#Develop the Fisher Discriminant Function 
#Identify a combination of features that separates our customers that 
#are likely to churn from those who are not
install.packages("DiscriMiner")
install.packages("MASS")

library(DiscriMiner)
library(MASS)

discPower(X1,Y1)

##Fischer discriminant functions
desDA(X1,Y1)

#Observations
#1.In terms of magnitude, the number of customer service calls has the 
#most impact,whereas the account length has the least impact.

#2.Number of voice mail messages has a negative sign indicating that 
#it has a negative impact on churn

#3.As the number of voice mail messages increases, the probability of 
#churn decreases.

# use the lda() function in R to classify records based on value of X 
#variables and predict the class and probability for the test set.
sublda=lda(churn~.,data = traindata)
sublda

#Observations
#The difference in group means is highest for number of customer 
#service calls and lowest for account length

#visualize what the non-churning customers look like compared to our 
#customers who will most likely churn
par(mar = c(1, 1, 1, 1))
plot(sublda, dimen = 1, type = "b")

#Observations
#Groups created by discriminant analysis can be seen in the graphs, 
#and are in sync with the Wilks lambda value of 0.89 that we got 
#from our MANOVA test. 

#These graphs are a good indicator that although the model is 
#significant, our two groups are not completely separated. 
#There is some overlap.

#Make Predictions on the Test Set 
lda.pred=predict(sublda, newdata = testdata)

install.packages("hmeasure")

library(hmeasure)

class.lda=lda.pred$class
true.class<-testdata[,1]
lda.counts <- misclassCounts(class.lda,true.class)
lda.counts$conf.matrix

print(lda.counts$metrics,digits=3)

#0bservations
#Sensitivity also called the true positive rate is defined as the 
#proportion of actual positives that are correctly identified by the 
#model. 
#The sensitivity of the model is 9.6% which is very low. 
#For a churn prediction model, it is important that the model picks up 
#positives as positives. 
#Evaluate Model Performance Measures 
#The accuracy of the model is 1-Error rate = 1-0.155 = 0.845 or 84.5%

#vary the threshold of the model from the default 50% to other values 
#to decide on a optimum balance for sensitivity and specificity.

#Classifying with a default threshold of 0.5
lda.pred$posterior[1:3,]



scores.lda <- lda.pred$posterior[,2]
all((scores.lda > 0.5)== (class.lda=="1"))

#The model, by default, uses a 50% threshold to classify records as 0 
#or 1.
# Use threshold of 30%
lda.counts.T03 <- misclassCounts(scores.lda>0.3,true.class)
lda.counts.T03$conf.matrix

# Use threshold of 20%
lda.counts.T02 <- misclassCounts(scores.lda>0.2,true.class)
lda.counts.T02$conf.matrix


#Use threshold of 17%
lda.counts.T017 <- misclassCounts(scores.lda>0.17,true.class)
lda.counts.T017$conf.matrix

#Use threshold of 16%
lda.counts.T016 <- misclassCounts(scores.lda>0.16,true.class)
lda.counts.T016$conf.matrix

# Usethreshold of 15%
lda.counts.T015 <- misclassCounts(scores.lda>0.15,true.class)
lda.counts.T015$conf.matrix

# Usethreshold of 10%
lda.counts.T01 <- misclassCounts(scores.lda>0.1,true.class)
lda.counts.T01$conf.matrix

#compare the values of sensitivity and specificity for three threshold 
#values.
lda.counts.T02$metrics[c('ER', 'Sens','Spec')]


lda.counts.T017$metrics[c('ER', 'Sens','Spec')]

lda.counts.T016$metrics[c('ER', 'Sens','Spec')]

lda.counts.T015$metrics[c('ER', 'Sens','Spec')]

lda.counts.T01$metrics[c('ER', 'Sens','Spec')]

