## ----loaddata, echo=TRUE, message=FALSE----------------------------------
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
sch <- c("Date", "Weight(kg)", "SleepDuration(hr)", "Weatehr", "Tempreture(c)", "Activity(steps)", 
         "HeartRate(bpm)", "DayType")
knitr::kable(head(mydata, 10), col.names = sch, align = "c")


## ----count, echo=TRUE----------------------------------------------------
n <- mydata %>%
  summarize(n = n())

## ----count2, echo=TRUE---------------------------------------------------
count2 <- mydata %>% 
  filter(SleepDuration > 8 & Tempreture > 20) %>% 
  summarize(n = n())


## ----sleep_by_day, echo=TRUE, results= 'asis'----------------------------
sleep_by_day <- mydata %>% 
  group_by(DayType) %>% 
  summarise(avg_sleep = mean (SleepDuration)) %>% 
  mutate(avg_sleep = round (avg_sleep, 2)) %>% 
  arrange(desc(avg_sleep))

knitr::kable(sleep_by_day, format = "markdown", align = 'l')
  

## ----avg_sleep2, echo=TRUE, results= 'asis'------------------------------
avg_sleep_weather <- mydata %>% 
  group_by(Weather) %>% 
  summarise(avg_sleep = mean(SleepDuration)) %>% 
  mutate(avg_sleep = round(avg_sleep, 2)) %>% 
  arrange(desc(avg_sleep))

knitr::kable(avg_sleep_weather, format = "markdown", align = 'l')


## ----sha, echo=TRUE, message=FALSE---------------------------------------
library(sha)

## ----as.factor, echo=TRUE------------------------------------------------
mydata$DayType <- as.factor(mydata$DayType)
mydata$Weather <- as.factor(mydata$Weather)

## ----scatterplot, echo= TRUE---------------------------------------------
plot_scatter(mydata,"Weight","SleepDuration", "DayType")


## ----scatterplot2, echo=TRUE---------------------------------------------
plot_scatter(mydata,"Activity","SleepDuration", "DayType")

## ----boxplot, echo=TRUE--------------------------------------------------
plot_box(mydata, x = "DayType", y= "SleepDuration")

## ----anova_test, echo=TRUE-----------------------------------------------
summary(aov(SleepDuration ~ DayType, data = mydata ))


## ----anova_test2, echo=TRUE----------------------------------------------
summary(aov(SleepDuration ~ Weather, data = mydata))

## ----corrplot, echo=TRUE, message=FALSE----------------------------------
library(corrplot)
corrplot(paircorr(mydata), method = "number",tl.srt = 45)

## ----corrleration2, echo=TRUE, results='asis'----------------------------
ans <- mydata %>% 
  filter(Weather == "Sunny") %>% 
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
ml.saturated = lm(SleepDuration ~ Weight + Tempreture + Activity + HeartRate +  DayType, 
                  data= mydata)
mlr_sum <- summary(ml.saturated)

knitr::kable(mlr_sum $coef, digits = c(4, 4, 3, 4), format = 'markdown')

## ----assumption_check, echo= TRUE----------------------------------------
plot(ml.saturated, which = c(1,2))

## ----info, echo= T-------------------------------------------------------
sessionInfo()

