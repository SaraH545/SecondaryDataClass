#Date Formats

#Convert character variables to dates (https://www.statmethods.net/input/dates.html)
strDates <- c("01/05/1965", "08/16/1975")
dates <- as.Date(strDates, "%m/%d/%Y")

#INDEPENDENT ACTIVITY: Convert the string dates to abbreviated month, two digit year 
#(HINT: Use link above for help)

#Chi square test from summary data (using in class smoking by gender example)
table1<-matrix(nrow=2,ncol=2,c(68,70,230,876))
chisq.test(table1)

#Chi square test from individual level data (see frequency and correlations activity)
#INDEPENDENT ACTIVITY: Create breaks for the percent below poverty variable at 10, 20, and 30 inclusive at upper range
#Is the distribution of percent below poverty categories by county independent in Indiana and Michigan?

#INDEPENDENT ACTIVITY: Is the overall distribution of black and white individuals different by state?

#ttest example (long format): Test if the mean county area is different in metro countries vs. nonmetro counties. 
#Use a two-sample, unpaired, one-sided t-test with alpha=0.05 https://www.statmethods.net/stats/ttest.html
t.test(midwest$area~midwest$inmetro)

#Paired ttest example (Wide format): Test if the asian population is larger than the American Indian population, by county
t.test(midwest$popasian,midwest$popamerindian,paired=TRUE,alternative ="greater")

#INDEPENDENT ACTIVITY: Test if, by county, the mean percent below poverty level is significantly different from 15% 

#Regression analysis http://www.cyclismo.org/tutorial/R/linearLeastSquares.html
#Example: Is the percent below poverty level related to the percent of the population that is black, whether or not the county is in a metro area, state, and the area of county?
midwest$percblack<-midwest$popblack/midwest$poptotal
reg<-lm(data=midwest, formula=percbelowpoverty~percblack+inmetro+area+state)
summary(reg)

