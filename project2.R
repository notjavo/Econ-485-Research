setwd("/Users/notjavo/Desktop/Econ 485/Project 2 Econ 485")
library(stargazer)
library(ggplot2)
library(plm)
library(vtable)
remove(list=ls())
mydf=read.csv("project2data.csv")
str(mydf)

#create new varible for fast food... # of fast food resteraunts per 10,000 people
mydf$newfastfood <- (mydf$fastfood / mydf$pop) *10
#creating descriptive statistics for all variables 
stargazer(mydf, type = "text",
          title = "Descriptive Statistics",
          align = TRUE, digits = 2,
          iqr = TRUE, median = TRUE, omit = "pcspend","fastfood",out = "Descriptive Stats All.html")
#descriptive stats for just fast food and oberate
vars=c('oberate','fastfood')
stargazer(mydf[vars],type='text')
#try experimenting with some plots
ggplot(mydf,aes(x = fastfood, y = oberate)) +
  geom_point(aes(color = factor(state)))

#examine ordering the data
head(mydf)

## Re Order the data
mydf = mydf[order(mydf$state),]

#create descriptive stats for each year in the data set
sumtable(mydf,vars = c('oberate','fastfood','unerate','bachelors','income'),group="year",
         group.long=TRUE)

regpool = lm(oberate~newfastfood, data=mydf)
regfe = lm(oberate~newfastfood + factor(state),data=mydf)
regboth = lm(oberate~newfastfood+factor(year)+factor(state),data=mydf)

#telling R that we have a panel data set
mypdata = pdata.frame(mydf,index = c("state","year"))

#making sure data is balanced 
pdim(mypdata)$balanced

#first differences regression 
regfd = plm(oberate~newfastfood + 0,data=mypdata,model="fd")
summary(regfd)

#FD with an intercept
regfd1=plm(oberate~newfastfood,data=mypdata,model="fd")
summary(regfd1)

stargazer(regpool,regfe,regboth,regfd,regfd1,omit.stat=c("f"),type="text",digets=5)

#regressions with the other independent variables 
regpoolall= lm(oberate~newfastfood+unerate+bachelors+income,data=mydf)
regfeall= lm(oberate~newfastfood+unerate+bachelors+income+factor(state),data=mydf)
regbothall=lm(oberate~newfastfood+unerate+bachelors+income+factor(state)+factor(year),data=mydf)
regfdall=plm(oberate~newfastfood+unerate+bachelors+income+0,data=mypdata,mode="fd")
regfd1all=plm(oberate~newfastfood+unerate+bachelors+income,data=mypdata,mode="fd")
stargazer(regpoolall,regfeall,regbothall,regfdall,regfd1all,omit.stat=c("f"),type="text",digets=5,out = "RegressionResults.html" )
summary(regbothall)
anova(regpoolall)
anova(regfeall)
anova(regbothall)
