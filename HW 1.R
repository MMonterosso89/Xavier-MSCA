# R Assignment 1

# PROBLEM STATEMENT

# The "Titanic" was an ocean liner that sunk on its first attempt at crossing the Atlantic Ocean in 1912.
# This assessment uses a sample of data from the Titanic passenger manifest. Variables include:


# - Name of the passenger
# - Ticket class of the passenger (First, Second or Third)
# - Age of the passenger, in years
# - Sex of the passenger (male or female)
# - Did the passenger survive? (1="Yes", 2="No")


# Complete the following data manipulation and analysis tasks in order using data available from the following website:
# http://asayanalytics.com/titanic


#################
## BEGIN TASKS ##
#################


# Task 1 (5 Points):
# Import the data into a data frame named: "titanic"

titanic.df <- read.csv("http://asayanalytics.com/titanic") # Import the dataset


# Task 2 (5 Points):
# Report the maximum, minimum and range for the age variable. 
# Set each of these to be a new values object of the same name. 
# For example, the 'max' object should be stored as a value that always returns the max age.

summary(titanic.df)

Min.Age.List <- titanic.df[,3]
typeof(Min.Age.List)               # Me finding a sneaky NA value, other solution more efficient
Min.Age <- min(Min.Age.List, na.rm = TRUE)
Min.Age

Min.Age <- min(titanic.df$age, na.rm = TRUE) # Min Age on Ship 

Max.Age <- max(titanic.df$age, na.rm = TRUE) # Max Age on Ship

Range.Age <- Max.Age - Min.Age # Age Range on Ship, let's also use range fxn for funsies
Range.Age <- range(titanic.df$age, na.rm = TRUE)[2] - range(titanic.df$age, na.rm = TRUE)[1]

# Task 3 (5 Points):
# Age was originally unknown for the passenger Mr. Michael Connaghton.
# It has since been determined that Mr. Connaghton was 31 years old when the Titanic sunk.
# Update the titanic data frame to reflect this new information. 

titanic.df[216,3] <- 31 # Replaces NA with 31 in 216th Row and 3rd Column - Mr. Michael Connaghton's Age


# Task 4 (5 Points):
# Create a histogram from the age variable.
# Properly label the chart with a main title, X label, Y label and set the color to "cyan".  

hist(titanic.df$age,
     main = "Histogram of Ages on The Titanic",  # Histogram with desired properties
     xlab = "Passenger Ages",
     ylab = "Frequency",
     col = "cyan"
     )


# Task 5 (10 Points):
# Create a new column vector named "child" that indicates whether the passenger was a child using 
# TRUE or FALSE. All passengers under 18 are considered children.

titanic.df$Child <- c(titanic.df$age < 18) # Sets new columm to TRUE for all age values under 18, else FALSE


# Task 6 (10 Points):
# Find the percent of Titanic passengers that survived
# Store this value as an object named: "percent.survived"
# This object should return the percent in percent form--not decimal form.

length(titanic.df$name)
length(titanic.df$survived)  # 757 passengers in dataset 
sum(titanic.df$survived) #314 lived

library(formattble) #package to add % and still be numeric

Percent.Survived <-   formattable::percent(sum(titanic.df$survived) / length(titanic.df$survived))  
typeof(Percent.Survived)
Percent.Survived*2 # Check that math works on it, it does!

Percent.Survived2 <-   100*(sum(titanic.df$survived) / length(titanic.df$survived)) #Simpler version with no %

# Task 7 (15 points):
# Using your results from task 5, calculate the number of passengers that survived.
# Store this value as an object named "number.survived"

Number.Survived <- sum(titanic.df$survived)

Number.Survived2 <- (Percent.Survived2 / 100) * length(titanic.df$survived) # Version using calculation, outputs numeric instead of int

# Task 8 (10 Points):
# Sort the titanic data frame by passenger age in ascending order. 
# When completed correctly, infants of age 0 should be at the top.  No 0 yrs in dataset

sort(titanic.df$age)


# Task 9 (15 Points):
# Use simple math operations and the other objects you have already created to calculate 
# the average age of survivors.

Survive.Mean <- sum(titanic.df$age*titanic.df$survived) / Number.Survived #Average Age of Survivors

# HT: Think conceptually what this calculation requires.
#       You need to sum up the total age of all passengers who survived and divide it by the 
#       the total number of survivors. To calculate the numerator, consider using the "survived"
#       column where everyone who did not survive is a zero and anything multipled by zero is zero...



# Task 10 (10 Points):
# Make a boxplot showing age by survival. Properly label the chart with a main title,
# x axis and y axis label. 
# Does there appear to be any difference in age between those who survived and those who did not?

boxplot(titanic.df$age~titanic.df$survived,
        main = "Age vs Survival on Titanic Boxplot", # Code for Boxplot
        xlab = "Survived",
        ylab = "Passenger Age") # We see a few extreme age outliers who did not survive, and a larger IQR for survivors


# Task 11 (10 Points):
# Make a boxplot showing age by class. Properly label the chart with a main title,
# x axis and y axis label. 
# Does there appear to be a relationship between the age of an individual and the passenger class
# he or she purchased? Why do you think this is?

boxplot(titanic.df$age~titanic.df$class,
        main = "Age vs Class on Titanic Boxplot", # Code for Boxplot
        xlab = "Passenger Class",
        ylab = "Passenger Age") # There is definite negative trend with age vs class (best to worst),
                                # and is likely due to older people typically
                                # having healthier finances due to more years worked!

