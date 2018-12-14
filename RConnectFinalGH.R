# Impoting Data from MySQL to R
# Install the RMySQL package
install.packages("RMySQL")
library(RMySQL)

# Connect to your DB.  
# This is not required, but simplifies calling the DB in the future without copying this entire command multiple times
# General Example
mydb <- dbConnect(MySQL(), user='monterossom', password='REDACTED', dbname='REDACTED')

# My inclass example:
instacart <- dbConnect(MySQL(), user='students', password='REDACTED', dbname='instacart', host='REDACTED')

# Listing tables:
# Use the 'dbListTables()' and dbListFields('YOUR_DB', 'YOUR_TABLE') to identify the tables and fields in the DB
# Example:
dbListTables(instacart)
dbListFields(instacart, 'products')

# Running queries:
# Use the dbSendQuery command to send queries to your MySQL server.
# You still need root / admin / write access to send changes or non-queries
dbSendQuery(instacart, 'DROP table IF EXISTS table2, table3')

# Retrieving Data:
# To import data into R, you can either specify your data immediately,
# or I recommend you create a new query object that can be recalled easily.

product_dept_aisle <- dbSendQuery(instacart, 
                                  "SELECT product_name, aisle, department FROM products 
                                    JOIN aisles ON products.aisle_id = aisles.aisle_id
                                    JOIN departments ON departments.department_id = products.department_id")

# Use fetch(query, n=rows) to capture a query results as data in R.  Use n=-1 to specify all query records,
# 0 to specify the MySQL default number of row or some other value for 'n' rows in default order
product_data <- fetch(product_dept_aisle, n=-1)

# You will need to recall the query each time you wish to store data from it. 
# You may also need to close the connection if pending rows exist, especially if
# you choose an N other than -1.  In this case, clear your db connection with: 
dbClearResult(dbListResults(instacart)[[1]])

# Example 2
housing  <- dbConnect(MySQL(), user='students', password='powerofx', dbname='housing', host='bais674-student.czfrpgaxozsi.us-east-1.rds.amazonaws.com')
housing_query <- dbSendQuery(housing, "SELECT * FROM pierce_housing_pool") 
housing_data <- fetch(housing_query, n=-1)
# We can now perform basic analysis on the data:
summary(housing_data)

# Simple scatterplot"
plot(housing_data$sqft,housing_data$trans_value)

# or simple regression:
fit_price_on_bath_bed_sqft <- lm(trans_value ~ sqft + bedrms + bath, data=housing_data)


# Fit the regression line to our scatterplot:
abline(fit_price_on_bath_bed_sqft, col="red")

# Summarize the regression output:
summary(fit_price_on_bath_bed_sqft)


Select1 <- dbSendQuery(mydb,"SELECT * FROM campaign_desc")
fetch1 <- fetch(Select1, n=-1)


Duration <- dbSendQuery(mydb,"SELECT *, end_day - start_day AS DURATION FROM campaign_desc")
DurFetch <- fetch(Duration, n=-1)

hist(DurFetch$DURATION)

mean(DurFetch$DURATION)
median(DurFetch$DURATION)





CandyQ <- dbSendQuery(mydb,"SELECT * FROM coupon_redempt
JOIN transaction_data ON coupon_redempt.household_key = transaction_data.household_key
JOIN product ON transaction_data.product_id = product.product_id
WHERE sub_commodity_desc = 'CANDY'")

CandyFetch <- fetch(CandyQ,n=-1)


HistCandy <- hist(as.numeric(CandyFetch$week_no))

str(CandyFetch$week_no)


NogQ <- dbSendQuery(mydb,"SELECT week_no,sub_commodity_desc FROM transaction_data
JOIN product on transaction_data.product_id = product.product_id
WHERE sub_commodity_desc = 'EGG NOG/BOILED CUSTARD';")

NogFetch <- fetch(NogQ,n=-1)


HistNog <- hist(as.numeric(NogFetch$week_no))

hist(as.numeric(CandyFetch$week_no), col=rgb(1,0.1,0.1,0.5),xlim=c(0,125), ylim=c(0,350))
hist(as.numeric(NogFetch$week_no), col=rgb(0.8,1,0.8,0.5), add=T)
box()



NationalQ <- dbSendQuery(mydb,"SELECT coupon.coupon_upc, brand, week_no  FROM coupon_redempt
JOIN transaction_data ON coupon_redempt.household_key = transaction_data.household_key
JOIN product ON transaction_data.product_id = product.product_id
JOIN coupon ON product.product_id = coupon.product_id;")

NationalFetch <- fetch(NationalQ,n=-1)

library(ggplot2)

ggplot(data = NationalFetch, aes(x=as.numeric(NationalFetch$week_no),y=as.factor(NationalFetch$brand),color=as.factor(NationalFetch$brand)))+
  geom_point()

library(rgl)



BeerQ <- dbSendQuery(mydb,"SELECT week_no FROM transaction_data
JOIN product on transaction_data.product_id = product.product_id
                     WHERE commodity_desc = 'BEERS/ALES';")

BeerFetch <- fetch(BeerQ,n=-1)

hist(as.numeric(CandyFetch$week_no), col=rgb(1,0.1,0.1,0.5),xlim=c(0,125), ylim=c(0,350))
hist(as.numeric(NogFetch$week_no), col=rgb(0.8,1,0.8,0.5), add=T)

hist(as.numeric(BeerFetch$week_no), col=rgb(1,1,0.3,.1), add=T)
box()


BeerQ <- dbSendQuery(mydb,"SELECT week_no FROM transaction_data
JOIN product on transaction_data.product_id = product.product_id
                     WHERE commodity_desc = 'BEERS/ALES';")

BeerFetch <- fetch(BeerQ,n=-1)


AgeCompFacet <- dbSendQuery(mydb,"SELECT quanitity,trans_time,week_no,income_desc,age_desc,brand FROM hh_demographic
JOIN transaction_data ON transaction_data.household_key = hh_demographic.household_key
JOIN product on transaction_data.product_id = product.product_id;")


AgeCompFetch <- fetch(AgeCompFacet,n=-1)

library(tidyverse)

AgeCompFetch %>%
  ggplot(aes(x=as.numeric(week_no),fill=age_desc))+
  geom_histogram()


AgeCompFetch %>%
  ggplot(aes(x=as.numeric(week_no),fill=income_desc))+
  geom_histogram(position = position_dodge())


AgeCompFetch %>%
  ggplot(aes(x=as.numeric(week_no),y=as.numeric(quanitity),color=age_desc))+
  geom_point()


AgeCompFetch %>%
  ggplot(aes(x=as.numeric(week_no),y=as.numeric(quanitity),color=income_desc))+
  geom_point()


couponQ <- dbSendQuery(mydb,"SELECT  age_desc,coupon_upc,campaign_desc.campaign,end_day - start_day AS Duration from campaign_desc
JOIN coupon_redempt ON coupon_redempt.campaign = campaign_desc.campaign
JOIN hh_demographic ON hh_demographic.household_key = coupon_redempt.household_key;")


CouponFetch <- fetch(couponQ,n=-1)


CouponFetchGrp <- CouponFetch %>%
  group_by(campaign)



CouponFetch %>%
  ggplot(aes(x=campaign,y=Duration), fill=age_desc))+
  geom_col()+
  ylim(0,2500)


CouponFetch %>%
  ggplot(aes(x=campaign,y=Duration)))+
  geom_point()


hist(CouponFetch$campaign)
hist(CouponFetch$Duration)


plot(CouponFetch$campaign,CouponFetch$Duration)


UPCUseQ <- dbSendQuery(mydb,"SELECT product.product_id, department, commodity_desc, week_no FROM coupon 

                       JOIN MostUsedUPC ON MostUsedUPC.coupon_upc = coupon.coupon_upc 
                       
                       JOIN product ON product.product_id = coupon.product_id 
                       
                       JOIN transaction_data ON transaction_data.product_id = product.product_id 
                       
                       WHERE coupon.coupon_upc = MostUsedUPC.coupon_upc;")

UPCUseFetch <- fetch(UPCUseQ,n=-1)


 UPCUseFetch %>%
   ggplot(aes(x=as.numeric(week_no),fill=department))+
   geom_histogram()

 
 hist(UPCUseFetch$week_no)
 
 
 
 xmasQ <- dbSendQuery(mydb,"SELECT week_no, COMMODITY_DESC FROM transaction_data
JOIN product on transaction_data.product_id = product.product_id
                      WHERE commodity_desc = 'CHRISTMAS  SEASONAL';")
 
 xmasFetch <- fetch(xmasQ,n=-1)
 
 ,xlim=c(0,125), ylim=c(0,350))

 hist(as.numeric(xmasFetch$week_no), col=rgb(0.8,1,0.8,0.5), add=T)
 hist(as.numeric(BeerFetch$week_no), col=rgb(1,1,0.3,.1), add=T)
 box()
 
 
 hist(as.numeric(xmasFetch$week_no), col=rgb(1,.2,0.4,0.5))
 hist(as.numeric(NogFetch$week_no), col=rgb(0.8,1,0.8,0.5), add=T)
 hist(as.numeric(CandyFetch$week_no), col=rgb(1,0.1,0.1,0.5), add=T)
 
 TGQ <- dbSendQuery(mydb,"SELECT week_no,COMMODITY_DESC FROM transaction_data
JOIN product on transaction_data.product_id = product.product_id
                    WHERE commodity_desc = 'TURKEY';")

 TGFetch <- fetch(TGQ,n=-1) 
 
hist(as.numeric(TGFetch$week_no), col=rgb(.5,.5,0.4,0.5),add=T)
hist(as.numeric(xmasFetch$week_no), col=rgb(1,.2,0.4,0.5))
hist(as.numeric(BeerFetch$week_no), col=rgb(1,1,0.3,.1), add=T)


ggplot(aes(x=week_no,color=COMMODITY_DESC))+
  geom_histogram(data=xmasFetch)




fuelQ <- dbSendQuery(mydb,"SELECT week_no,COMMODITY_DESC FROM transaction_data
JOIN product on transaction_data.product_id = product.product_id
                   WHERE commodity_desc = 'FUEL';")
fuelFetch <- fetch(fuelQ,n=-1) 


hist(as.numeric(fuelFetch$week_no))
