Math_Perf_df <- read.csv("https://asayanalytics.com/Monterosso-data",sep=";", header = TRUE)
# Load df found at https://asayanalytics.com/Monterosso-data
# Attribute info at https://archive.ics.uci.edu/ml/datasets/Student+Performance

dim(Math_Perf_df) # get dimms
sum(is.na(Math_Perf_df)) # Check for NA values - There are none!

library(tidyverse) # Load dplyr and magrittr

##START HW3 OPTIMIZATION AND PIPING##

Bad_boi_df <- Math_Perf_df %>%
select(romantic,goout,Dalc,Walc,sex,G3) %>%
filter(sex == 'M') %>%
arrange(desc(Dalc), desc(Walc), romantic)
# This selected, filtered, and arranged table piped with magrittr looks
# at male students who drink often on weekdays and weekends

Truancy_Health_df <- Math_Perf_df %>%
select(G3, health, absences) %>%
filter(absences > 2) %>%
arrange(desc(absences),desc(health))
# This dataframe looks at how truancy and health code of students along with final score
# i.e student with healthcode 5 (perfectly healthy) and many absences should be looked in to

Improve_and_Efficiency <- Math_Perf_df %>%
select(G3, G1, G2, studytime) %>% 
mutate(Improve_score = -1 * G1 + G2 , Efficiency = G3/studytime) %>%
filter(Improve_score > 1) %>%
arrange(desc(Efficiency)) %>%
select(G3,Efficiency,Improve_score)
##Explores a student's final score to study effiency along with improvement from 1st to second semester

Leg_Up_df <- Math_Perf_df %>%
transmute(Leg_Up = Medu + Fedu + ifelse(schoolsup =='yes',1,2) + ifelse(famsup =='yes',1,2) 
+ ifelse(paid=='yes',1,2) + ifelse(activities=='yes',1,2) + ifelse(nursery=='yes',1,2) 
+ famrel + freetime + health,G3) %>%
arrange(desc(Leg_Up))
# Naive Summation of all factors which would help a student succeed


##Vizualizations##
library(ggplot2)

ggplot(data = Math_Perf_df, aes(x = G3)) + 
  geom_histogram(binwidth = 1) + ggtitle("Final Score Histogram")

#Checking for normality in final scores

ggplot(data = Leg_Up_df, aes(x = Leg_Up)) + 
  geom_histogram(binwidth = 1) + ggtitle("Leg Up Score Histogram")

#Checking for normality in Leg_Up summation

ggplot(data = Leg_Up_df, aes(Leg_Up,G3)) + geom_jitter(size = 2 , color = "red") +
ggtitle("Final Scores vs Summation of Helpful Metrics")
# Jitter Scatter of G3 vs Leg Up score - jitter used as many values overlap each other
# due to integer nature. Jitter gives better idea of point density.

ggplot(data = Truancy_Health_df, aes(absences,G3)) + geom_jitter(size = 1 , color = "blue") +
ggtitle("Truancy vs Final Score")
#Plot of Truancy vs Final Score, note negative trend

ggplot(data = Truancy_Health_df, aes(health,G3)) + geom_jitter(size = 3 , color = "black") +
ggtitle("Health vs Final Score")
#Plot of Health vs Final Score, no strong trend really

ggplot(data = Improve_and_Efficiency, aes(Efficiency,G3)) + geom_violin(size = .5 , color = "purple") +
  ggtitle("Study Efficiency vs Final Score")
#Violin chart of study efficiency vs final score
# Very interesting to see efficiencies converge to 7-9 for the highest scorers!
# Stated another way the students must work hard regardless of natural talent to score well
# Midling scores are much much wider as gifted but lazy students can score average with
# little study
#It also looks like a pretty stringray and that's real neato.
