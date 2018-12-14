#GLM 3

library(tidyverse)
library(car)
library(readr)

df3 <- read.csv("http://asayanalytics.com/ceos_csv")

df_tibb <- as.tibble(df3)

# Check what empty values look like in excel cause im scrub
write_csv(df3, file.path("C:\\Users\\Matth\\Desktop\\MSCA FALL18\\StatProg", "wutdis.csv"))

df3$totalcomp <- df3$Salary+df3$Bonus

hist(df3$totalcomp) # not remomtely normal due to bonus!

# 1.	Does it seem appropriate to combine salary and bonuses together as a single
# variable, "compensation", or should these two be estimated separately?

# After exploring the histograms and simple scatter of salary and bonus,
# we see that the data is very non-normal and a bit hereroskedastic to boot.
# From the scatter, we can see that there a quite a few CEOs with roughly median
# Salary but huge bonuses, and those with outlier salary but much less overall bonus.
# Although we are predicting for them combined (initially), we drown out a lot
# of predictive power in these differences which are likely due to industry
# and other factors.


### Check NAs

is.na(df3[3,4])



sum(is.na(df3$CityofBirth)) # y u no work

for (i in ncol(df_tibb)){
  if (str(df_tibb[,i]) == 'int'){
    hist(df_tibb[,i])
  }
}

hist(df_tibb$Age)

hist(df3$Salary)
hist(df3$Bonus)
scatter.smooth(df3$Salary,df3$Bonus)
scatter.smooth(df3$totalcomp)
scatter.smooth(df3$Salary)

scatter.smooth(df3yhatfilter$Salary)
scatter.smooth(df3yhatfilter$Bonus)


plot(df3$Salary,df3$Bonus, asp = 1)


# if we plot bonus x salary on same aspect ratio, we get a better sense
# of how much more predictive bonuses seems to be from the variation in values.
# Max salary in dataset is 2.7M but max bonus is over 11M (before filter)!


sum(is.na(df3$Bonus)) #98 empty - large chunk of dataset
sum(is.na(df3$Salary)) #10 empty - less detrimental both of these look they are attached to very small salaries 


topsal <- quantile(df3$Salary,.95, na.rm = T)
botsal <- quantile(df3$Salary,.05, na.rm = T)

topbon <- quantile(df3$Bonus,.95, na.rm = T)
botbon <- quantile(df3$Bonus,.05, na.rm = T)

#Perhaps we can get closer to normalcy by chopping anything beyond 2sd off?

df3yhatfilter <- df3 %>%
  filter(topsal > Salary) %>%
  filter(Salary > botsal) %>%
  filter(topbon > Bonus) %>%
  filter(Bonus > botbon)      # we lose 190 rows - although almost half of these were empty bonuses!

plot(df3yhatfilter$Salary,df3yhatfilter$Bonus, asp = 1)

hist(df3yhatfilter$Salary)

hist(df3yhatfilter$Bonus) # Vast improvements, especially for bonus although still fairly left skewed.

hist(df3yhatfilter$totalcomp)











InitialLM <- lm(df3$totalcomp ~ df3$Age + df3$YearsCEO + as.factor(df3$MBA.)
  + as.factor(df3$MasterPhd.)+ as.factor(df3$WideIndustry) + df3$Profits
  +df3$Other+df3$StGains +df3$StockOwned)

summary(InitialLM)

# Bad predictive power, although fairly generalized (mult r^2 and adj. r^2 similar)

crPlots(InitialLM)
plot(InitialLM)

InitialLM2 <- lm(df3$totalcomp ~
                +  poly(df3$Profits,4,raw=T)
                +df3$Other+df3$StGains)

summary(InitialLM2)
summary(InitialLM2)$r.squared
anova(InitialLM,InitialLM2)
crPlots(InitialLM2)

vif(InitialLM2)

# neato loop for finding most predictive single parameter!
# a decent quick technique to get some bearings.

rsqdf <- data.frame(matrix(vector(mode = 'numeric',length = 10), nrow = 30, ncol = 2))

for (i in 1:ncol(df3)) {
  lmloop <- lm(df3$totalcomp ~ df3[,i])
  
  print(c(names(df3[i]), summary(lmloop)$r.squared))
  
  rsqdf[i,1] <- names(df3[i])              #put in df as well for maths
  rsqdf[i,2] <- summary(lmloop)$r.squared
  }


scatter.smooth(df3$totalcomp,df3$Bonus)
scatter.smooth(df3$totalcomp,df3$Salary)

boxplot(df3$totalcomp~df3$CityofBirth)

for (i in 1:ncol(df3)) {
  lmloop <- lm(df3$totalcomp ~ as.factor(df3[,i]))
  
  print(c(names(df3[i]), summary(lmloop)$r.squared))
}


Educatedlm <- lm(df3$totalcomp ~ as.factor(df3$CityofBirth)+ as.factor(df3$Undergrad) + as.factor(df3$Industry))

summary(Educatedlm)



Bonuslm <- lm(df3$totalcomp ~ df3$Bonus)

summary(Bonuslm)

Sallm <- lm(df3$totalcomp ~ df3$Salary)

summary(Sallm)


Educatedlm2 <- lm(df3$totalcomp ~ as.factor(df3$CityofBirth) +
as.factor(df3$Undergrad) + as.factor(df3$Industry) + df3$Compfor5Yrs + df3$Other)



summary(Educatedlm2)


alias(lm(df3$totalcomp ~ as.factor(df3$CityofBirth) +
                      as.factor(df3$Undergrad) + as.factor(df3$Industry) + df3$Compfor5Yrs + df3$Other))

# Dummy variable trap?
crPlots(Educatedlm2)

vif(Educatedlm2)

Educatedlm3 <- lm(df3$totalcomp ~ as.factor(df3$Undergrad) + as.factor(df3$Industry.Code) + df3$Compfor5Yrs + df3$Other)

alias(Educatedlm3)

crPlots(Educatedlm3)

summary(Educatedlm3)

#Negative adj r^2, insignificance of variables. Back to drawing board...

#Other is pulled down by single outlier (Viacom, gg), some issues with compfor5years as well


Educatedlm4 <- lm(df3$totalcomp ~ df3$Other + as.factor(df3$Industry.Code))

summary(Educatedlm4)


NewLm <- lm(df3$totalcomp ~ as.factor(df3$CityofBirth))
NewLm2 <- lm(df3$totalcomp ~ as.factor(df3$CityofBirth)+ as.factor(df3$Industry))

anova(NewLm,NewLm2)

summary(NewLm2)





# 2.	Properly define and carefully use your variables 
# All variables for assessed for data type and NAs

# 3.	Check your primary regression assumptions of linearity and multicollinearity. There is no source of autocorrelation in this cross-sectional dataset.

#Linearity was tested via CrPLots and multicollinearity with VIF

# Some issues with linearity due to outliers, which we can fix. Also finding perfect
# multicollinearity due to I believe the variable dummmy trap, so removing catergorical
# Variables until this is no longer the case.

# The major flaw in our assumptions of linearity is predicting on total compensation
# in which bonus is extremely non-normal (see initial plot). Bonus is extremely,
# left skewed in addition to having enormous outliers

# 4.	Check your secondary regression assumptions of homoskedasticity and normality-are there other possible concerns you might have regarding these and how do your findings influence your conclusion?
# All parameters for checked for heteroskedacity and normality at beginning of script.
# When we work with non-normal data is alters the basline accuracy of our model and we have
# flawwed initial assumptions.

# 5.	Be sure to answer/explain the following at minimum:
# a.	What variable explains most of the variation in CEO compensation?

# A first glance, bonus seems most predictive of CEO compensation,
# although bonus is a function of total compensation so this is not really fair.
# City of birth also seems extremely predictive although this is a quite a fit overfit
# Due to small number of CEO from specific hometowns making very high total
# compensations (Ex: Evanston, a very affluent suburb of Chicago - also my ex's
# hometown, gross).

#Other than these Industry at 29% of variation explained seems to be best single predictor.

# b.	What is the overall fit of your best fitting model(s)?

summary(Educatedlm2)
#.96 ,but extremely overfit?

# c.	How does R handle missing data when using it in a regression model? Does this seem appropriate for the situation?

# R handles missing data by simply ignoring it - this is logically correct
# as you cannot find predictive power in data which does not exist. You must make
# logical assumptions (which can be dangerous) and impute a value of choice
# (mean, median, distinct factor, etc) if you want these rows to be used.

# d.	What other unaccounted for 'Z' variables do you believe exist that influence CEO salaries?

# There an untold amount of additional variable which we cannot hope to account for.
# Off the top of my head, a person's social network, financial status (Did the rise to CEO through
# competency or are they "old money"), personal health, contributions to the company, etc.  
  