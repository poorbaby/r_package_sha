## ----loaddata, echo=TRUE, message=FALSE, warning=FALSE-------------------
library("googlesheets")
googlekey <- gs_url("https://docs.google.com/spreadsheets/d/1KdOw0FIxY2Kt8UgY16F3UMQSC7KJaBgtmewNZUGuLY8/edit#gid=0",lookup = FALSE,visibility = "public")
mydata <- gs_read(googlekey, skip = 3)

## ----data_view, echo=TRUE, results='asis'--------------------------------
knitr::kable(head(mydata), format = "markdown", align = 'c')

## ----str_data, echo= FALSE, include= FALSE-------------------------------
no_of_col <- dim(mydata)[2]
var_names <- colnames(mydata)

## ----dataclean01, echo=TRUE, message=FALSE-------------------------------
library(tidyverse)
mydata <- mydata %>% 
  mutate(DayType = ifelse(DayofWeek %in% c("Sat", "Sun"), "Weekend", "Weekday")) %>% 
  select(-DayofWeek) 

## ----schema, echo=TRUE, results='asis'-----------------------------------
#assign the variable name with units of measurement
sch <- c("Date", "Weight(kg)", "SleepDuration(hr)", "Tempreture(°C)", "Activity(steps)", 
         "HeartRate(bpm)","HavingDog", "DayType")
knitr::kable(head(mydata, 10), col.names = sch, align = "c")


## ----count, echo=TRUE----------------------------------------------------
n <- mydata %>%
  summarize(n = n())
firstday <- mydata$Date[1]
lastday <- mydata$Date[length(mydata$Date)]  
avg_st <- round(mean(mydata$SleepDuration),2)
percent_avg_st <- round(avg_st/24,4)*100

## ----count2, echo=TRUE---------------------------------------------------

count2 <- mydata %>% 
  group_by(DayType) %>%
  tally()


## ----sleep_by_day, echo=TRUE, results= 'asis'----------------------------
sleep_by_day <- mydata %>% 
  group_by(DayType) %>% 
  summarise(avg_sleep = mean (SleepDuration)) %>% 
  mutate(avg_sleep = round (avg_sleep, 2)) %>% 
  arrange(desc(avg_sleep))

knitr::kable(sleep_by_day, format = "markdown", align = 'l')

weekend_sd <- sleep_by_day$avg_sleep[1]
weekday_sd <- sleep_by_day$avg_sleep[0]

## ----sha, echo=TRUE, message=FALSE---------------------------------------
# Import the package *sha*
library(sha)

## ----boxplot, echo=TRUE--------------------------------------------------
plot_box(mydata, x = "DayType", y= "SleepDuration")

## ----as.factor, echo=TRUE------------------------------------------------
mydata$DayType <- as.factor(mydata$DayType)
mydata$HavingDog <- as.factor(mydata$HavingDog)


## ----scatterplot, echo= TRUE---------------------------------------------
plot_scatter(data = mydata,x = "Weight",y = "SleepDuration", color = "DayType")


## ----scatterplot2, echo=TRUE---------------------------------------------
plot_scatter(data = mydata,x = "Activity",y = "SleepDuration", color = "DayType")

## ----anova_test, echo=TRUE-----------------------------------------------
summary(aov(SleepDuration ~ DayType, data = mydata ))


## ---- echo=TRUE----------------------------------------------------------
summary(aov(SleepDuration ~ HavingDog, data = mydata))

## ----anova_test2, echo=TRUE----------------------------------------------
summary(aov(Activity~ HavingDog, data = mydata))

## ----corrplot, echo=TRUE-------------------------------------------------
cor_plot(mydata)

## ---- echo=TRUE,results='asis'-------------------------------------------
cor.test(mydata$Weight,mydata$Activity)

## ----corrleration2, echo=TRUE, results='asis'----------------------------
ans <- mydata %>% 
  filter(HavingDog == "Yes") %>% 
  group_by(DayType) %>% 
  summarise(r = cor(SleepDuration, Tempreture)) %>% 
  mutate(r = round(r, 2)) %>% 
  arrange(desc(r))

knitr::kable(ans, format = "markdown", align = "l")  

## ----slm1, echo=TRUE-----------------------------------------------------
slm_model <- slm_s(mydata, x = "Tempreture", y="HeartRate")
slm_model
modelplot(mydata, x = "Tempreture", y="HeartRate")


## ----normalitycheck, echo=TRUE-------------------------------------------
qqnorm(slm_model$residuals)
qqline(slm_model$residuals)

## ----scattermatrix, echo=TRUE--------------------------------------------
par(ps = 12, cex = 1, cex.main = 1)
plot(mydata %>% select(SleepDuration, Weight, Tempreture, Activity, HeartRate), pch=16, col="blue", main="Matrix Scatterplot of SleepDuration, Weight, Tempreture, Activity and HeartRate")

## ----ml01, echo=TRUE-----------------------------------------------------
ml.saturated = lm(SleepDuration ~ Weight + Tempreture + Activity + HeartRate +  DayType + HavingDog, 
                  data= mydata)
mlr_sum <- summary(ml.saturated)
mlr_sum
knitr::kable(mlr_sum $coef, digits = c(4, 4, 3, 4), format = 'markdown')

## ----assumption_check, echo= TRUE----------------------------------------
plot(ml.saturated, which = c(1,2))

## ----info, echo= T-------------------------------------------------------
sessionInfo()

