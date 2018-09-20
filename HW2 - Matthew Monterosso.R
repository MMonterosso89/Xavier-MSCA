Math_Perf_df <- read.csv("https://asayanalytics.com/Monterosso-data",sep=";", header = TRUE)
# Load df found at https://asayanalytics.com/Monterosso-data
# Attribute info at https://archive.ics.uci.edu/ml/datasets/Student+Performance

dim(Math_Perf_df) # get dimms

sum(is.na(Math_Perf_df)) # Check for NA values - There are none!

library(dplyr) # Load dplyr

##FILTERING##

Math_Perf_Male_df <- filter(Math_Perf_df, sex == 'M')
Math_Perf_Female_df <- filter(Math_Perf_df, sex == 'F')  # Check for difference in Male and Female scores

Math_Perf_GP_df <- filter(Math_Perf_df, school == 'GP')
Math_Perf_MS_df <- filter(Math_Perf_df, school == 'MS') # Check for differences in school's performances

Good_Boi_df <- filter(Math_Perf_df, sex =='M', goout >= 3, Dalc >= 1, Walc >= 1 ,studytime <= 3)
#Filters only the bestest of bois - those that study quite a bit and do not go out or drink excessively

##SELECTING##

Rowdy_df <- select(Math_Perf_df,romantic,goout,Dalc,Walc,G3) # Checks if individual is in a relationship how often they drink

Fam_Life_df <- select(Math_Perf_df, famsize,Pstatus, Medu, Fedu, Mjob, Fjob,guardian, famsup, G3) #Checks a student's homelife

##ARRANGING##

Score_Arrange_df <- arrange(Math_Perf_df,desc(G3)) # Order by target variable of final score in Math

Truancy_Health_df <- arrange(Math_Perf_df,desc(absences),desc(health)) # arranges by absences and health code so we can quickly determine which students are missing due to medical issues

##ALL##

Bad_boi_df <- select(Math_Perf_df,romantic,goout,Dalc,Walc,sex,G3)
Bad_boi_df <- filter(Bad_boi_df, sex == 'M')
Bad_boi_df <- arrange(Bad_boi_df,desc(Dalc), desc(Walc), romantic)
# This selected, filtered, and arranged table looks at male students who drink often on weekdays and weekends

##TRANSMUTES and SUMMARY##

Improvement_score_df <- transmute(Math_Perf_df,
                                  (-1*G1) + G2) #Checks if student is improving or declining

Study_ratio_df <- transmute(Math_Perf_df, G3/studytime) #explores student studytime to final score. Higher scores means more efficient learning

Educated_Parents_Score_df <- transmute(Math_Perf_df, Medu + Fedu, G3) # Looks at education of both parents

Leg_up_df <- transmute(Math_Perf_df, (Medu + Fedu + ifelse(schoolsup =='yes',1,2) + ifelse(famsup =='yes',1,2) + ifelse(paid=='yes',1,2) + ifelse(activities=='yes',1,2) + ifelse(nursery=='yes',1,2) + famrel + freetime + health),G3)
# Naive Summation of all factors which would help a student succeed

Score_Spread_df <- summarise(Math_Perf_df, 
          G3_mean = mean(G3, na.rm = TRUE), 
          G3_sd = sd(G3, na.rm = TRUE), 
          n = n()
) #Checks Mean and sd for dataset

BestBoiz_Score_Spread_df <- summarise(Good_Boi_df, 
                             G3_mean = mean(G3, na.rm = TRUE), 
                             G3_sd = sd(G3, na.rm = TRUE), 
                             n = n()
) # Compares with good boy subset - They don't score any better!
