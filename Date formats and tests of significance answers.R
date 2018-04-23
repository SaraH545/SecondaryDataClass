#Date Formats

#Convert character variables to dates (https://www.statmethods.net/input/dates.html)
strDates <- c("01/05/1965", "08/16/1975")
dates <- as.Date(strDates, "%m/%d/%Y")

#INDEPENDENT ACTIVITY: Convert the string dates to abbreviated month, two digit year 
#(HINT: Use link above for help)
dates2<- format(dates, format="%b %y")

#Chi square test from summary data (using in class smoking by gender example)
table1<-matrix(nrow=2,ncol=2,c(68,70,230,876))
chisq.test(table1)

#Chi square test from individual level data (see frequency and correlations activity)
#INDEPENDENT ACTIVITY: Create breaks for the percent below poverty variable at 10, 20, and 30 inclusive at upper range
#Is the distribution of percent below poverty categories by county independent in Indiana and Michigan?
MI_IN<-subset(midwest,state=="IN"|state=="MI")
range(MI_IN$percbelowpoverty)
MI_IN$pov_cat<-cut(MI_IN$percbelowpoverty, c(0,10,20,30), right=TRUE)
pov_table<-table(MI_IN$pov_cat,factor(MI_IN$state)) #Note: The factor command will allow you to exclude the levels of the original factor that aren't present in this data frame
summary(pov_table) #The distribution is not independent p<0.001

#INDEPENDENT ACTIVITY: Is the overall distribution of black and white individuals different by state?
blacksum<-aggregate(midwest$popblack, by=list(Category=midwest$state), FUN=sum)
colnames(blacksum) <- c("state","popblack")
whitesum<-aggregate(midwest$popwhite, by=list(Category=midwest$state), FUN=sum)
colnames(whitesum) <- c("state","popwhite")
table<-merge(blacksum,whitesum,id=state)
chisq.test(table[2:3])

#ttest example (long format): Test if the mean county area is different in metro countries vs. nonmetro counties. 
#Use a two-sample, unpaired, one-sided t-test with alpha=0.05 https://www.statmethods.net/stats/ttest.html
t.test(midwest$area~midwest$inmetro)

#Paired ttest example (Wide format): Test if the asian population is larger than the American Indian population, by county
t.test(midwest$popasian,midwest$popamerindian,paired=TRUE,alternative ="greater")

#INDEPENDENT ACTIVITY: Test if, by county, the mean percent below poverty level is significantly different from 15% 
t.test(midwest$percbelowpoverty,mu=15)

#Regression analysis http://www.cyclismo.org/tutorial/R/linearLeastSquares.html
#Example: Is the percent below poverty level related to the percent of the population that is black, whether or not the county is in a metro area, state, and the area of county?
midwest$percblack<-midwest$popblack/midwest$poptotal
reg<-lm(data=midwest, formula=percbelowpoverty~percblack+inmetro+area+state)
summary(reg)

