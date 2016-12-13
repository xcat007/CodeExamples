library(googleVis)
setwd("xxx")
NLNdata <- read.csv("xxx")
NLNdata$Weekending<- as.Date(NLNdata$Weekending, format = "%m/%d/%y")

Motion <- gvisMotionChart(NLNdata,
                          idvar = "Origin",
                          timevar = "Weekending")
plot(Motion)


#---------------------------------------------------------
TMSdate2 <- function(tdate){
  temp <- as.Date(tdate,format = "%m/%d/%Y")
  return(temp)
}

library(lubridate)
library(googleVis)
NLN_OB <- read.csv("C:/Users/steven.phillips01/Documents/R Working Directory/NLN OB/Dynamic.csv")
NLN_OB$Clean.Ship.Date <- TMSdate2(NLN_OB$Actual.Ship)
NLN_OB$Week <- ceiling_date(NLN_OB$Clean.Ship.Date,"week")

NLN_OB$DestConCat <- paste(NLN_OB$Dest.Name,NLN_OB$Dest.City,NLN_OB$Dest.State)


OB_DF <- sqldf("select DestConCat, Week, 
           avg(weight) 'mean.lbs',
           sum(weight) 'total.lbs' 
           from 'NLN_OB' 
           group by DestConCat, Week")

Motion_OB <- gvisMotionChart(OB_DF,
                          idvar = "DestConCat",
                          timevar = "Week")
plot(Motion_OB)
