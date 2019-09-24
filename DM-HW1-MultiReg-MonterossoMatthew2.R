#DataMining Homework 1 Multiple Regression 9/8/19

#Predict Sales on other continuous an cat features

##Read in Packages Required

library(data.table) #FAST read
library(tidyverse) #uhh, a lot
library(psych)  #Describe fuction, ect
library(stats) #Statistical functions
library(caret) #training and validation
#install.packages("PerformanceAnalytics")
library(PerformanceAnalytics) #correlation chart
library(corrplot) #corr plot
#install.packages("corrgram")
library(corrgram) #corr stuff
library(caTools) #to split data
library(leaps) #for reg
library(car) # VIF and reg
#install.packages("relaimpo")
library(relaimpo) #relative importance variable in model
#install.packages("forecast")
library(forecast) #forecast data
library(lmtest) #BPTest
library(caTools)
library(forecast)
################Read File##

Sales_df <- fread("D:\\MSCA\\FALL19\\Sales.csv")


psych::describe(Sales_df)

install.packages("DataExplorer")
library(DataExplorer)


DataExplorer::create_report(Sales_df) #EDA made easy - See report generated for insights!


#No missing

#Grab Continous

Sales_Cont_df <- Sales_df[,c(2:7,9)]

chart.Correlation(Sales_Cont_df, histogram = TRUE, pch=19)
corrplot(cor(Sales_Cont_df), method = 'color')
DataExplorer::plot_correlation(Sales_Cont_df)


mean(Sales_Cont_df$Price - Sales_Cont_df$CompPrice) #Speaker City is 8.78$ cheaper on average than competitors!


###Ensure Facors are encoded properly

Sales_df$Store <- as.factor(Sales_df$Store)


Sales_df$Top25Rank <- as.ordered(Sales_df$Top25Rank^-1 ) # is adding all 25 rank required?? We only know higher is better. Also inverse so that ordered is correct

Sales_df$`Product Placement` <- as.ordered(Sales_df$`Product Placement`)

Sales_df$Urban <- as.factor(Sales_df$Urban)

Sales_df$Packaging <- as.factor(Sales_df$Packaging)
###########


###Set Seed for Reproducibility and split dataset

set.seed(125)

sample.df <- sample.split(Sales_df$`Store Sales`, SplitRatio = 0.8)

train.Sales <- subset(Sales_df, sample.df == TRUE)
test.Sales <- subset(Sales_df, sample.df == FALSE)



###Exaustive for fun




options(scipen = 999)

leapsAll <- regsubsets(`Store Sales`~ . , data = train.Sales, nbest = 1, method = "exhaustive")

AllStats <- summary(leapsAll)

AllStats

subsets(leapsAll, statistic = "adjr2") #BIC, RSS, R2, CP etc,
subsets(leapsAll, statistic = "adjr2", legend = FALSE) #no derp click legend

AllStats$adjr2


subsets(leapsAll, statistic = "bic") #bic, aic, & mallow should decrease!



######Full Model Required for Stepwise Regressions
options(max.print=1000000)



full_model <- lm(`Store Sales` ~ ., train.Sales)

summary(full_model)

##Remove Store as it is unique per row and will not help us

train.Sales <- train.Sales[,2:12] 

############################################

full_model <- lm(`Store Sales` ~ ., train.Sales)

summary(full_model)


accuracy(full_model)

vif(full_model)

########Relative importance#########################################

rel.imp.fullmodel <- calc.relimp(full_model, type = "lmg", rela = TRUE)

rel.imp.full2 <- rel.imp.fullmodel$lmg


stack(sort(rel.imp.full2, decreasing = TRUE))


##########


backwards.step <- step(full_model, direction = "backward")

summary(backwards.step)


vif(backwards.step)

accuracy(backwards.step)


###Forwards

model1 <- lm(`Store Sales` ~ 1, train.Sales)

forward.step <- step(model1, scope = list(lower=model1, upper=full_model), direction = "forward")

summary(forward.step)

accuracy(forward.step)

##mixed

mixed.step <- step(full_model, direction = "both")

accuracy(mixed.step)

###All 3 models identical


mixed.step.predict <- predict(mixed.step, test.Sales)

mixed.step.predict


predfinal <- as.data.frame(cbind(test.Sales$`Store Sales`, mixed.step.predict))

head(predfinal)



accuracytest <- as.data.frame(accuracy(test.Sales$`Store Sales`, mixed.step.predict))


accuracytrain <- as.data.frame(accuracy(mixed.step))

compare <- rbind(accuracytest[,c(2,5)], accuracytrain[,c(2,5)])

colnames(compare) <- c("RMSE", "MAPE")

compare


###Check homoskedasticity

library(lmtest)

## H0 = constant variance

bptest(mixed.step)


## p = .11, we fail reject null. We're good!

#DALEX###
install.packages("DALEX")
library(DALEX)
install_dependencies()

explain_me <- DALEX::explain(mixed.step, label = "lm", data = train.Sales, y = train.Sales$`Store Sales`)


library(auditor)


explainer <- model_residual(explain_me)

plot(explainer, type = "residual_density")

plot(explainer, type = "prediction", abline = TRUE)

plot_prediction(explainer)


# Pred1 <- train.Sales
# 
# Pred1 <- as.data.frame(t(c("999",122,113,98,5,38,11^-1,245,1,2,1)))
# 
# 
# colnames(Pred1) <- colnames(train.Sales)
# 
# Pred <- rbind(train.Sales, Pred1)


# predict(mixed.step,Pred[257])

#### Predition questions, theres got to be a better way to do this...


Q1 <- train.Sales[42]


Q1$Price <- 122
Q1$CompPrice <- 113
Q1$Income <- 98  
Q1$Advertising <- 5
Q1$Age <- 38
Q1$Top25Rank <- 11^-1
Q1$Population <- 245
Q1$Urban <- as.factor(2)
Q1$Packaging <- as.factor(1)


predict(mixed.step, Q1)


Q2 <- train.Sales[43]

Q2$Price <- 134
Q2$CompPrice <- 127
Q2$Income <- 64  
Q2$Advertising <- 3
Q2$Age <- 64
Q2$Top25Rank <- 14^-1
Q2$Population <- 119
Q2$Urban <- as.factor(2)
Q2$Packaging <- as.factor(1)


predict(mixed.step, Q2)
