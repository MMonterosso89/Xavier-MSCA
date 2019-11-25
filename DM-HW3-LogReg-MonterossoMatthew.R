##HW 3 Logistic Regression Due 10/3/19

#packages you will need

install.packages("pacman")

library(pacman)

#Checks if package is installed, an installs and libraries if it isnt;
#else just libraries it. I knew this had to exist I felt like I was taking crazy pills

pacman::p_load(data.table)
pacman::p_load(tidyverse)
pacman::p_load(caTools)
#install.packages("ROCR")
#pacman::p_load(ROCR)
#install.packages("pROC")
pacman::p_load(pROC)
#install.packages("InformationValue")
pacman::p_load(InformationValue)
pacman::p_load(PerformanceAnalytics) #correlation chart
pacman::p_load(corrgram) #correlation plot
pacman::p_load(ROCR)
pacman::p_load(stats)
pacman::p_load(car)

pacman::p_load(DataExplorer)
## Read data

Bank_df <- fread("D:\\MSCA\\MMonterosso89.github.io\\UniversalBank.csv")

Bank_df <- fread("C:\\Users\\AES4355\\Desktop\\XUFall19\\UniversalBank.csv")


#Peep Correlations
Bank.cont <- Bank_df %>%
  select_if(is.numeric)

cor(Bank.cont)

corrgram::corrgram(Bank.cont) #wow that's garbage who wrote this
DataExplorer::plot_correlation(Bank.cont) # that's better


##Seed for repro, split data test and tain

set.seed(125)

sample.bank <- sample.split(Bank_df$Loan, SplitRatio = 0.70)

train.bank <- subset(Bank_df, sample.bank == TRUE)
test.bank <- subset(Bank_df, sample.bank == FALSE)

### Run full model

bank.log <- glm(Loan ~ ., train.bank, family = binomial)

summary(bank.log)


##test if factor give diff result...

train.bank$Education <- as.factor(train.bank$Education)

train.bank$CD <- as.factor(train.bank$CD)

train.bank$Securities <- as.factor(train.bank$Securities)

train.bank$Online <- as.factor(train.bank$Online)

train.bank$CreditCard <- as.factor(train.bank$CreditCard)

bank.log <- glm(Loan ~ ., train.bank, family = binomial)

summary(bank.log)


###same results as expected, but know we know for sure! Reverting to numeric


##try mixed stepwise

mixed.bank <- step(bank.log, direction = "both")

summary(mixed.bank)

car::vif(mixed.bank) ## confirm experience multicollinear with age


#Education of class professional is not significant, but we have to leave in as class Undergrad is very significant

predictTrain0 <- predict(mixed.bank) #just wanted to look at the log odds distribution

predictTrain <- predict(mixed.bank, type = "response")


stack(head(predictTrain))


tapply(predictTrain, train.bank$Loan, mean)

##Avg loan non accept .034, Avg loan accept 0.68

table(train.bank$Loan, predictTrain > .5)

confusionMatrix(train.bank$Loan, predictTrain > .5)

##Same

##Lets find optimall cutoff, although .5 seems decent?

optimalCutoff(train.bank$Loan, predictTrain, returnDiagnostics = TRUE)

## I was right, we were close already but .55 is optimal!

plotROC(train.bank$Loan, predictTrain)


#Prediction on Test Data 

predictTest <- predict(mixed.bank, type = "response", newdata = test.bank)
table(test.bank$Loan, predictTest > 0.55)

accTest <- 1 - misClassError(test.bank$Loan, predictTest, threshold = 0.55)


#Check overfit
aucTrain <- auc(train.bank$Loan, predictTrain)
aucTest <- auc(test.bank$Loan, predictTest)
accTrain <- 1 - misClassError(train.bank$Loan, predictTrain, threshold = 0.55)
accTest <- 1 - misClassError(test.bank$Loan, predictTest, threshold = 0.55)
compare <- matrix( c(aucTrain,aucTest,accTrain,accTest),nrow=2, ncol=2)
colnames(compare) <- c("AUC","Accuracy")
rownames(compare) <- c("Training","Test")

compare

##Not overfit!

####Interpret coeffs
options(scipen = 9999)

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

logit2prob(mixed.bank$coefficients) #dank


logit2oods <- function(logit){
  odds <- exp(logit)
  return(odds)
}

logit2oods(mixed.bank$coefficients)




Prdictprob <- test.bank[4]

Prdictprob$Age <- 62

Experience doesnt matter

Prdictprob$Income <- 92
Zipcode doesnt matter

Prdictprob$Family <- 4

Prdictprob$CCAvg <- 3.1

Prdictprob$Education <- "Graduate"

morgage doesnt matter


Prdictprob$Securities <- 1

Prdictprob$CD <- 1

Prdictprob$Online <- 0

Prdictprob$CreditCard <- 0


Predict(mixed.bank, type = "response", newdata = Prdictprob)




#################


Prdictprob <- test.bank[4]

Prdictprob$Age <- 33
Prdictprob$Income <- 88
Zipcode doesnt matter

Prdictprob$Family <- 3

Prdictprob$CCAvg <- 4.4

Prdictprob$Education <- "Undergraduate"

morgage doesnt matter


Prdictprob$Securities <- 0

Prdictprob$CD <- 1

Prdictprob$Online <- 1

Prdictprob$CreditCard <- 1


Predict(mixed.bank, type = "response", newdata = Prdictprob)
