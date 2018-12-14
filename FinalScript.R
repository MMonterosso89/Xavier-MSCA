#Regression prediction final
library(tidyverse)
library(tibble)
library(tidytext)
library(car)
setwd("C:/Users/Matth/Desktop/MSCA FALL18/StatProg/Final")

df <- read_csv("Train_data.csv")
str(df)

df <- as.tibble(df)

##############################
### SCRUBBING & EDA PHASE ####
##############################
hist(df$Interest_Rate) #Relatively normal
hist(df$Loan_Amount_Requested) #also probably ok, modal at 5k increments which makes sense for a loan.

n_distinct(df$Borrower_ID) #no duplicate borrowers



n_distinct(df$Initial_Loan_Term_Months) # why as text?

df$Initial_Loan_Term_Months[df$Initial_Loan_Term_Months == "36 months"] <- (36) 
df$Initial_Loan_Term_Months[df$Initial_Loan_Term_Months == "60 months"] <- (60) 

n_distinct(df$Employer) #lots of dups
n_distinct(df$`Home_Owner?`) #only 6 classes!

unique(df$`Home_Owner?`)

hist(df$Annual_Income) # enourmous outlier, will chop off top and bottom 5%


topsal <- quantile(df$Annual_Income,.95, na.rm = T)
botsal <- quantile(df$Annual_Income,.05, na.rm = T)

dfFilter <- df %>%
  filter(topsal > Annual_Income) %>%
  filter(Annual_Income > botsal)

hist(dfFilter$Annual_Income) #Still left skewed, but so much better


n_distinct(dfFilter$Income_Verified)
unique(dfFilter$Income_Verified)   # Coerce the two verifieds to one

dfFilter$Income_Verified[dfFilter$Income_Verified == "VERIFIED - income source"] <- ("VERIFIED - income")

unique(dfFilter$Income_Verified)

n_distinct(dfFilter$Purpose_Of_Loan) 
unique(dfFilter$Purpose_Of_Loan) #only 14 categories will leave for now.



n_distinct(dfFilter$Loan_Title) 
unique(dfFilter$Loan_Title) #big oofs more NLP I guess

dfFilter %>%
  ggplot(aes(x=Month_Issued,y=Interest_Rate,color=Income_Verified)) +
  geom_boxplot()  #This seems counter to logic?


substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
} #ty based stack overflow



substrRight(dfFilter$Date_Earliest_Credit,2) # rips only the year which we can work with

#since lubridate and base r have failed me

dfFilter$Date_Earliest_Credit <- substrRight(dfFilter$Date_Earliest_Credit,2) # rips only the year which we can work with

str(dfFilter$Date_Earliest_Credit)

dfFilter$Date_Earliest_Credit <- as.numeric(dfFilter$Date_Earliest_Credit)

dfFilter$Years_Credit <- 118 - dfFilter$Date_Earliest_Credit

dfFilter$Years_Credit[dfFilter$Years_Credit == 118] <- (18) # Very sloppy way to do this



TokenizedLoanDesc <- unnest_tokens(dfFilter,word,Loan_Description) %>%
  inner_join(get_sentiments("afinn")) %>% 
  group_by(index = Loan_id) %>% 
  summarise(sentiment = sum(score)) %>% 
  mutate(method = "AFINN") 
  
colnames(TokenizedLoanDesc)[1] <- "Loan_id"

FinalDfScrubbed <- full_join(dfFilter,TokenizedLoanDesc,by = "Loan_id")

FinalDfScrubbed$sentiment <- replace(FinalDfScrubbed$sentiment, is.na(FinalDfScrubbed$sentiment),0)


hist(FinalDfScrubbed$sentiment)

FinalDfScrubbed %>%
  ggplot(aes(x=sentiment,y=Interest_Rate, color=Income_Verified))+
  geom_jitter()+
  geom_smooth(method = "lm")


FinalDfScrubbed$Loan_id <- NULL
FinalDfScrubbed$Borrower_ID <- NULL
FinalDfScrubbed$Loan_Description <- NULL
FinalDfScrubbed$method <- NULL

hist(FinalDfScrubbed$Months_Since_Last_Derogatory)
hist(FinalDfScrubbed$sentiment)
#simpler df with cray factors removed for initial modeling
DfSimple <- FinalDfScrubbed

DfSimple$Employer <- NULL

DfSimple$`Home_Owner?` <- NULL

DfSimple$Purpose_Of_Loan <- NULL

DfSimple$Loan_Title <- NULL
DfSimple$Zip_3 <- NULL
DfSimple$State <- NULL

DfSimple$Months_Since_Record <- NULL
DfSimple$Months_Since_Last_Derogatory <- NULL
DfSimple$Months_Since_Deliquency <- NULL
DfSimple$Date_Earliest_Credit <- NULL

DfSimple$Length_Employed[is.na(DfSimple$Length_Employed)] <- 0
############################
####MODELING PHASE##########
############################

lm1 <- lm(FinalDfScrubbed$Interest_Rate ~ FinalDfScrubbed$sentiment)

summary(lm1)

crPlots(lm1)





#Stepwise regression cause idk what to do

lm_null <- lm(DfSimple$Interest_Rate~1, data=DfSimple)

summary(lm_null)

lm_full <- lm(DfSimple$Interest_Rate~.,data=DfSimple)

summary(lm_full)


lm_forward <- step(lm_null, scope=list(lower=lm_null,upper=lm_full),direction = "forward")


summary(lm_forward)

# crPlots(lm_forward) # commented out for recompiling - takes forever

#First simple and naive stepwise results in R^2 .48 and no overfitting, not bad

RecordFix <- DfSimple %>%
  filter(Public_Record_Count < 7)



lm_null2 <- lm(RecordFix$Interest_Rate~1, data=RecordFix)

summary(lm_null)

lm_full2 <- lm(RecordFix$Interest_Rate~.,data=RecordFix)

summary(lm_full)


lm_forward2 <- step(lm_null2, scope=list(lower=lm_null2,upper=lm_full2),direction = "forward")


summary(lm_forward2)

anova(lm_forward,lm_forward2)

#.4896

DfSimple <- DfSimple %>%
  filter(Public_Record_Count < 7) %>%
  filter(Number_Delinqueny_2yrs < 8) %>%
  filter(Inquiries_Last_6Mo < 7) %>%
  filter(Collections_12Mo_Exclude_Med < 3)


lm_null3 <- lm(DfSimple$Interest_Rate~1, data=DfSimple)

summary(lm_null)

lm_full3 <- lm(DfSimple$Interest_Rate~.,data=DfSimple)

summary(lm_full)


lm_forward3 <- step(lm_null3, scope=list(lower=lm_null3,upper=lm_full3),direction = "forward")


summary(lm_forward3)

#.4907
# Can probably add back in factors now and see what we get
# Test Loan term as factor

lmtest <- lm(DfSimple$Interest_Rate ~ as.factor(Initial_Loan_Term_Months) + Revolving_Utilization + 
               Inquiries_Last_6Mo + Purpose_Of_Loan + Public_Record_Count + 
               Number_Delinqueny_2yrs + Annual_Income + Loan_Amount_Requested + 
               Number_Open_Accounts + Revolving_Balance + Income_Verified + 
               Total_Accounts + Month_Issued + Debt_To_Income + Years_Credit + 
               sentiment + Collections_12Mo_Exclude_Med + Length_Employed, data = DfSimple)

summary(lmtest)

vif(lmtest) #No Collinearity issues yet

anova(lm_forward3,lmtest)

#Oh, no difference. Know we know!

#Add back in paramters and test

FinalDfScrubbed <- FinalDfScrubbed %>%
  filter(Public_Record_Count < 7) %>%
  filter(Number_Delinqueny_2yrs < 8) %>%
  filter(Inquiries_Last_6Mo < 7) %>%
  filter(Collections_12Mo_Exclude_Med < 3)


lmAddBack <- lm(FinalDfScrubbed$Interest_Rate ~ Initial_Loan_Term_Months + Revolving_Utilization + 
               Inquiries_Last_6Mo + Purpose_Of_Loan + Public_Record_Count + 
               Number_Delinqueny_2yrs + Annual_Income + Loan_Amount_Requested + 
               Number_Open_Accounts + Revolving_Balance + Income_Verified + 
               Total_Accounts + Month_Issued + Debt_To_Income + Years_Credit + 
               sentiment + Collections_12Mo_Exclude_Med + Length_Employed + as.factor(`Home_Owner?`), data = FinalDfScrubbed)

summary(lmAddBack)

#.4975!!

lmAddBack2 <- lm(FinalDfScrubbed$Interest_Rate ~ Initial_Loan_Term_Months + Revolving_Utilization + 
                  Inquiries_Last_6Mo + Purpose_Of_Loan + Public_Record_Count + 
                  Number_Delinqueny_2yrs + Annual_Income + Loan_Amount_Requested + 
                  Number_Open_Accounts + Revolving_Balance + Income_Verified + 
                  Total_Accounts + Month_Issued + Debt_To_Income + Years_Credit + 
                  sentiment + Collections_12Mo_Exclude_Med + Length_Employed + as.factor(`Home_Owner?`) +as.factor(Zip_3), data = FinalDfScrubbed)
summary(lmAddBack2)


#.5176

anova(lmAddBack,lmAddBack2) #Massive P>F due to so many insignificant zips?

#I want to check correlation with state for funsies


lmAddBack3 <- lm(FinalDfScrubbed$Interest_Rate ~ Initial_Loan_Term_Months + Revolving_Utilization + 
                   Inquiries_Last_6Mo + Purpose_Of_Loan + Public_Record_Count + 
                   Number_Delinqueny_2yrs + Annual_Income + Loan_Amount_Requested + 
                   Number_Open_Accounts + Revolving_Balance + Income_Verified + 
                   Total_Accounts + Month_Issued + Debt_To_Income + Years_Credit + 
                   sentiment + Collections_12Mo_Exclude_Med + Length_Employed + as.factor(`Home_Owner?`) +as.factor(Zip_3) +as.factor(State), data = FinalDfScrubbed)
summary(lmAddBack3)

vif(lmAddBack3)


#Yup, perfectly collinear as expected



lmAddBack4 <- lm(FinalDfScrubbed$Interest_Rate ~ Initial_Loan_Term_Months + Revolving_Utilization + 
                   Inquiries_Last_6Mo + Purpose_Of_Loan + Public_Record_Count + 
                   Number_Delinqueny_2yrs + Annual_Income + Loan_Amount_Requested + 
                   Number_Open_Accounts + Revolving_Balance + Income_Verified + 
                   Total_Accounts + Month_Issued + Debt_To_Income + Years_Credit + 
                   sentiment + Collections_12Mo_Exclude_Med + Length_Employed + as.factor(`Home_Owner?`) +as.factor(Zip_3), data = FinalDfScrubbed)
summary(lmAddBack4)

plot(lmAddBack4)

###maybe now is good time to check test set

##############################
#####TESTING PHASE############
##############################


df_test <- read_csv("Test_data.csv") # Will have to preprocess in the same way

for (i in 1:ncol(df_test)){
  print(c(names(df_test[i]),sum(is.na(df_test[,i]))))
}


df_test$Initial_Loan_Term_Months[df_test$Initial_Loan_Term_Months == "36 months"] <- (36) 
df_test$Initial_Loan_Term_Months[df_test$Initial_Loan_Term_Months == "60 months"] <- (60)

df_test$Initial_Loan_Term_Months <- as.numeric(df_test$Initial_Loan_Term_Months)

df_test$Income_Verified[df_test$Income_Verified == "VERIFIED - income source"] <- ("VERIFIED - income")

substrRight(df_test$Date_Earliest_Credit,2) # rips only the year which we can work with

df_test$Years_Credit <- substrRight(df_test$Date_Earliest_Credit,2) # rips only the year which we can work with

str(dfFilter$Date_Earliest_Credit)

df_test$Years_Credit <- as.numeric(df_test$Years_Credit)

df_test$Years_Credit <- 118 - df_test$Date_Earliest_Credit

df_test$Years_Credit[df_test$Years_Credit == 118] <- (18) # Very sloppy way to do this

df_test$Date_Earliest_Credit -> NULL

TokenizedLoanDesc_test <- unnest_tokens(df_test,word,Loan_Description) %>%
  inner_join(get_sentiments("afinn")) %>% 
  group_by(index = Loan_id) %>% 
  summarise(sentiment = sum(score)) %>% 
  mutate(method = "AFINN") 

colnames(TokenizedLoanDesc_test)[1] <- "Loan_id"

TokenizedLoanDesc_test <- full_join(df_test,TokenizedLoanDesc_test,by = "Loan_id")

df_test <- TokenizedLoanDesc_test

df_test$sentiment <- replace(df_test$sentiment, is.na(df_test$sentiment),0)

hist(df_test$sentiment)

df_test$Loan_id <- NULL
df_test$Borrower_ID <- NULL
df_test$Loan_Description <- NULL
df_test$method <- NULL


df_test$PK <- 1

df_test$PK <- seq.int(nrow(df_test))

TokenTest <- unnest_tokens(df_test,word,Loan_Title) %>%
  inner_join(get_sentiments("afinn")) %>% 
  group_by(index = PK) %>% 
  summarise(sentiment = sum(score)) %>% 
  mutate(method = "AFINN") 

colnames(TokenTest)[1] <- "PK"

df_test <- full_join(df_test,TokenTest,by = "PK")

df_test$sentiment.y <- replace(df_test$sentiment.y, is.na(df_test$sentiment.y),0)

df_test$PK <- NULL
df_test$method <- NULL





predict(lmAddBack4,tryboi) #Some nominal levels to scrub


tryboi <- df_test[!(df_test$Zip_3=="040xx"),]
tryboi <- tryboi[!(tryboi$Zip_3=="090xx"),]        #Remove zips that arent in training
tryboi <- tryboi[!(tryboi$Zip_3=="823xx"),]
tryboi <- tryboi[!(tryboi$Zip_3=="838xx"),]


RMSE1 <- predict(lmAddBack4,tryboi) 

WholeSetTest <- FinalDfScrubbed
WholeSetTest$Interest_Rate <- NULL

summary(lmAddBack4)

Test1 <- predict(lmAddBack4,WholeSetTest)

WholeSetTest <- WholeSetTest[!(WholeSetTest$Zip_3=="169xx"),]
WholeSetTest <- WholeSetTest[!(WholeSetTest$Zip_3=="332xx"),]
WholeSetTest <- WholeSetTest[!(WholeSetTest$Zip_3=="408xx"),]
WholeSetTest <- WholeSetTest[!(WholeSetTest$Zip_3=="680xx"),]


FinalDfScrubbed <- FinalDfScrubbed[!(FinalDfScrubbed$Zip_3=="169xx"),]
FinalDfScrubbed <- FinalDfScrubbed[!(FinalDfScrubbed$Zip_3=="332xx"),]
FinalDfScrubbed <- FinalDfScrubbed[!(FinalDfScrubbed$Zip_3=="408xx"),]
FinalDfScrubbed <- FinalDfScrubbed[!(FinalDfScrubbed$Zip_3=="680xx"),]


predict.lm(lmAddBack4,WholeSetTest)

library(Metrics)

str(FinalDfScrubbed$Interest_Rate)
str(Test1)

Test1 <- as.numeric(Test1)

str(Test1)
rmse(FinalDfScrubbed$Interest_Rate,Test1)

hmmm <- FinalDfScrubbed$Interest_Rate - Test1

hmm <- hmmm^2

why <- sum(as.numeric(hmm), na.rm = TRUE)

why <- why / 22168


RMSE <- why
# Guess I'll try no NLP on loan title as well

#Have to add back a pseudo pk

FinalDfScrubbed$PK <- 1

FinalDfScrubbed$PK <- seq.int(nrow(FinalDfScrubbed))

TokenizedLoanTitle <- unnest_tokens(FinalDfScrubbed,word,Loan_Title) %>%
  inner_join(get_sentiments("afinn")) %>% 
  group_by(index = PK) %>% 
  summarise(sentiment = sum(score)) %>% 
  mutate(method = "AFINN") 

colnames(TokenizedLoanTitle)[1] <- "PK"

FinalDfScrubbed <- full_join(FinalDfScrubbed,TokenizedLoanTitle,by = "PK")

FinalDfScrubbed$sentiment.y <- replace(FinalDfScrubbed$sentiment.y, is.na(FinalDfScrubbed$sentiment.y),0)

FinalDfScrubbed$PK <- NULL
FinalDfScrubbed$method <- NULL


summary(lmAddBack4)


#Baller_lm <- lm(FinalDfScrubbed$Interest_Rate ~ Initial_Loan_Term_Months + 
 #                 Revolving_Utilization + Inquiries_Last_6Mo + Purpose_Of_Loan + 
  #                Public_Record_Count + Number_Delinqueny_2yrs + Annual_Income + 
   #               Loan_Amount_Requested + Number_Open_Accounts + Revolving_Balance + 
    #              Income_Verified + Total_Accounts + Month_Issued + Debt_To_Income + 
     #             Years_Credit + sentiment.x + sentiment.y + Collections_12Mo_Exclude_Med + 
      #            Length_Employed + as.factor(`Home_Owner?`) + as.factor(Zip_3), 
       #         data = FinalDfScrubbed)

#summary(Baller_lm) # 0.5179

#Lets add poly and log fits from here 

# crPlots(Baller_lm)

FinalDfScrubbed$Initial_Loan_Term_Months <- as.numeric(FinalDfScrubbed$Initial_Loan_Term_Months)

str(FinalDfScrubbed)



Baller_lm2 <- lm(FinalDfScrubbed$Interest_Rate ~ Initial_Loan_Term_Months + 
                  Revolving_Utilization + poly(Inquiries_Last_6Mo,3,raw=T) + Purpose_Of_Loan + 
                  poly(Public_Record_Count,3,raw=T) + poly(Number_Delinqueny_2yrs,3,raw=T) + Annual_Income + 
                  poly(Loan_Amount_Requested,2,raw=T) + Number_Open_Accounts + Revolving_Balance + 
                  Income_Verified + Total_Accounts + Month_Issued + Debt_To_Income + 
                  Years_Credit + sentiment.x + sentiment.y + Collections_12Mo_Exclude_Med + 
                  Length_Employed + as.factor(`Home_Owner?`) + as.factor(Zip_3), 
                data = FinalDfScrubbed)

 summary(Baller_lm2) #0.5559!

# crPlots(Baller_lm2)

#Revolving Balance could use normalizing

topbal <- quantile(FinalDfScrubbed$Revolving_Balance,.95, na.rm = T)
botbal <- quantile(FinalDfScrubbed$Revolving_Balance,.05, na.rm = T)

FinalDfScrubbed <- FinalDfScrubbed %>%
  filter(topbal > Revolving_Balance) %>%
  filter(Revolving_Balance > botbal)

hist(FinalDfScrubbed$Revolving_Balance)
hist(FinalDfScrubbed$Annual_Income)
range(FinalDfScrubbed$Annual_Income)


Baller_lm3 <- lm(FinalDfScrubbed$Interest_Rate ~ Initial_Loan_Term_Months + 
                   Revolving_Utilization + poly(Inquiries_Last_6Mo,3,raw=T) + Purpose_Of_Loan + 
                   poly(Public_Record_Count,3,raw=T) + poly(Number_Delinqueny_2yrs,3,raw=T) + Annual_Income + 
                   poly(Loan_Amount_Requested,2,raw=T) + Number_Open_Accounts + Revolving_Balance + 
                   Income_Verified + Total_Accounts + Month_Issued + Debt_To_Income + 
                   Years_Credit + sentiment.x + sentiment.y + Collections_12Mo_Exclude_Med + 
                   Length_Employed + as.factor(`Home_Owner?`) + as.factor(Zip_3), 
                 data = FinalDfScrubbed)

summary(Baller_lm3) #.5651 !!

df_test <- df_test[!(df_test$Zip_3=="040xx"),]
df_test <- df_test[!(df_test$Zip_3=="090xx"),]
df_test <- df_test[!(df_test$Zip_3=="250xx"),]
df_test <- df_test[!(df_test$Zip_3=="407xx"),]
df_test <- df_test[!(df_test$Zip_3=="491xx"),]
df_test <- df_test[!(df_test$Zip_3=="595xx"),]
df_test <- df_test[!(df_test$Zip_3=="736xx"),]
df_test <- df_test[!(df_test$Zip_3=="823xx"),]
df_test <- df_test[!(df_test$Zip_3=="838xx"),]

df_test$Years_Credit <- as.numeric(df_test$Years_Credit)

df_test$Years_Credit <- 118 - df_test$Years_Credit

df_test$Years_Credit[df_test$Years_Credit == 118] <- (18) # Very sloppy way to do this


crPlots(Baller_lm3$coefficients)

Predict1 <- predict(Baller_lm3,df_test)

PredictDeliverable <- as.data.frame(Predict1)

PredictDeliverable$PK <- 1

PredictDeliverable$PK <- seq.int(nrow(PredictDeliverable))


write_csv(PredictDeliverable, file.path("C:\\Users\\Matth\\Desktop\\MSCA FALL18\\StatProg", "PredictDeliverable.csv"))


crPlots(Baller_lm3)


unique(df_test$Income_Verified)



df_test$Months_Since_Deliquency[is.na(df_test$Months_Since_Deliquency)] <- (median(df_test$Months_Since_Deliquency, na.rm = T)) 
df_test$Months_Since_Record[is.na(df_test$Months_Since_Record)] <- (median(df_test$Months_Since_Record, na.rm = T)) 
df_test$Months_Since_Last_Derogatory[is.na(df_test$Months_Since_Last_Derogatory)] <- (median(df_test$Months_Since_Last_Derogatory, na.rm = T)) 


Predict2 <- predict.lm(Baller_lm3,df_test)

sum(is.na(Predict2))



for (i in 1:ncol(df_test)){
  print(c(names(df_test[i]),n_distinct(df_test[,i])))
  
}


for (i in 1:ncol(FinalDfScrubbed)){
  print(c(names(FinalDfScrubbed[i]),n_distinct(FinalDfScrubbed[,i])))
  
}




Baller_lm_HELP <- lm(FinalDfScrubbed$Interest_Rate ~ Initial_Loan_Term_Months + 
                   Revolving_Utilization + poly(Inquiries_Last_6Mo,3,raw=T) + Purpose_Of_Loan + 
                   poly(Public_Record_Count,3,raw=T) + poly(Number_Delinqueny_2yrs,3,raw=T) + Annual_Income + 
                   poly(Loan_Amount_Requested,2,raw=T) + Number_Open_Accounts + Revolving_Balance + 
                   Income_Verified + Total_Accounts + Month_Issued + Debt_To_Income + 
                   Years_Credit + sentiment.x + sentiment.y + Collections_12Mo_Exclude_Med + 
                   Length_Employed + as.factor(`Home_Owner?`), 
                 data = FinalDfScrubbed)

summary(Baller_lm_HELP)



Predict2 <- predict.lm(Baller_lm_HELP,df_test,na.action = na.keep)

sum(is.na(Predict2))


vif(Baller_lm3)


