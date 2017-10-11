## ----loaddata, echo=TRUE, message=FALSE----------------------------------
library("googlesheets")
googlekey <- gs_url("https://docs.google.com/spreadsheets/d/1KdOw0FIxY2Kt8UgY16F3UMQSC7KJaBgtmewNZUGuLY8/edit#gid=0",lookup = FALSE,visibility = "public")
mydata <- gs_read(googlekey, skip = 3)

## ----data_view, echo=TRUE, results='asis'--------------------------------
knitr::kable(head(mydata), format = "markdown", align = 'c')

## ----sha, echo=TRUE------------------------------------------------------
library(sha)

## ----classifyday, echo=TRUE, results='asis'------------------------------
c <- classifyday(df = mydata, x = "DayofWeek")
mydata$DayType <- as.factor(c)
knitr::kable(head(mydata), format = "markdown", align = 'c')

## ----scatterplot, echo= TRUE---------------------------------------------
plot_scatter(mydata,"Weight","SleepDuration", "DayType")


## ----scatterplot2, echo=TRUE---------------------------------------------
plot_scatter(mydata,"Activity","SleepDuration", "DayType")

## ----boxplot, echo=TRUE--------------------------------------------------
plot_box(mydata, x = "DayType", y= "SleepDuration")

## ----lode_tidyverse, echo=TRUE, message = FALSE--------------------------
library(tidyverse)

## ----count, echo=TRUE, results='asis'------------------------------------
n <- mydata %>%
  summarize(n = n())
knitr::kable(n, format = "markdown", align = 'l')

## ----count2, echo=TRUE, results='asis'-----------------------------------
count2 <- mydata %>% 
  filter(SleepDuration > 8 & Tempreture > 20) %>% 
  summarize(n = n())
knitr::kable(count2, format = "markdown", align = 'l')

## ----rank, echo=TRUE-----------------------------------------------------
rank <- mydata %>% 
  group_by(DayofWeek) %>% 
  summarise(avg_activity = mean(Activity)) %>% 
  mutate(avg_activity = round(avg_activity, 2)) %>% 
  arrange(desc(avg_activity))
knitr::kable(rank, format = "markdown", align = "l")  

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


## ----anova_test, echo=TRUE-----------------------------------------------
summary(aov(SleepDuration ~ DayType, data = mydata ))


## ----anova_test2, echo=TRUE----------------------------------------------
summary(aov(SleepDuration ~ Weather, data = mydata))

## ----paircorr1, echo=TRUE, results='asis'--------------------------------
knitr::kable(paircorr(mydata), format = "markdown", align = "l", padding = 2 )

## ----corrleration2, echo=TRUE, results='asis'----------------------------
ans <- mydata %>% 
  filter(Weather == "Sunny") %>% 
  group_by(DayType) %>% 
  summarise(r = cor(SleepDuration, Tempreture)) %>% 
  mutate(r = round(r, 2)) %>% 
  arrange(desc(r))

knitr::kable(ans, format = "markdown", align = "l")  

## ----slm1, echo=TRUE-----------------------------------------------------
slm_s(mydata, x = "Tempreture", y="Heart Rate")
modelplot(mydata, x = "Tempreture", y="Heart Rate")

## ----slm2, echo=TRUE-----------------------------------------------------
slm_s(mydata, x = "Activity", y="SleepDuration")
modelplot(mydata, x = "Activity", y="SleepDuration")

## ----info, echo= T-------------------------------------------------------
sessionInfo()

