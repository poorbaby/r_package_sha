## ----loaddata, echo=TRUE-------------------------------------------------
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
knitr::kable(head(mydata), format = "markdown", align = 'c', caption = "Sleep Health Data Set")

## ----scatterplot, echo= TRUE---------------------------------------------
plot_scatter(mydata,"Weight","SleepDuration", "DayType")


## ----scatterplot2, echo=TRUE---------------------------------------------
plot_scatter(mydata,"Activity","SleepDuration", "DayType")

## ----boxplot, echo=TRUE--------------------------------------------------
plot_box(mydata, x = "DayType", y= "SleepDuration")

## ----meandiff, echo=TRUE-------------------------------------------------
basic_stats(mydata, "SleepDuration", "DayType", mean)


## ----anova_test, echo=TRUE-----------------------------------------------
aov_t(mydata,"SleepDuration", "DayType")

## ----paircorr1, echo=TRUE, results='asis'--------------------------------
knitr::kable(paircorr(mydata), format = "markdown", align = "l", padding = 2 )

## ----slm1, echo=TRUE-----------------------------------------------------
slm_s(mydata, x = "Tempreture", y="Heart Rate")
modelplot(mydata, x = "Tempreture", y="Heart Rate")

## ----slm2, echo=TRUE-----------------------------------------------------
slm_s(mydata, x = "Activity", y="SleepDuration")
modelplot(mydata, x = "Activity", y="SleepDuration")

## ----info, echo= T-------------------------------------------------------
sessionInfo()

