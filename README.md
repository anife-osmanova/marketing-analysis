# marketing-analysis
# Introduction

## About the Company

**Bellabeat** is a leading high-tech company specializing in smart wellness products for women, established in 2013 by Urška Sršen and Sando Mur. Their range includes the Bellabeat App, Leaf wellness tracker, Time wellness watch, Spring smart water bottle, and a subscription-based Bellabeat Membership program.
By collecting and analyzing data on various aspects of women's health, Bellabeat aims to empower women with knowledge about their well-being.


## Business Task

To conduct a comprehensive analysis of non-Bellabeat smart device usage data, uncovering trends that can be applied to Bellabeat's customer base. The goal is to strategically leverage these insights to influence and enhance Bellabeat's marketing strategy for a selected product, ultimately optimizing consumer engagement and identifying potential growth opportunities.

## Key Stakeholders

Urška Sršen (Co-founder, CCO)
Sando Mur (Co-founder, Mathematician)
Bellabeat Marketing Analytics Team
Bellabeat Executive Team


# Data Preparation

## Data Source

The dataset utilized in this case study is sourced from Fitbit Fitness Tracker Data, an openly available dataset on Kaggle. It comprises personal fitness tracker details from 30 FitBit users, with the explicit consent of these users for the inclusion of their personal tracker data in the dataset.


## Data Integrity and Credibility

Analysis of the dataset using the ROCCC criteria yields the following observations:

**Reliable**: The dataset lacks reliability due to its limited scope, showcasing data from only 30 FitBit users without providing essential demographic or geographical information. The small sample size raises concerns about the generalizability of findings.
**Original**: The dataset is not entirely original, as the provenance section indicates prior preprocessing. This introduces potential complications in understanding the complete data lifecycle.
**Comprehensive**: Despite its limitations, the dataset is deemed comprehensive as the included metrics align with the specific requirements of Bellabeat's analysis.
**Current**: The dataset exhibits a lack of currency, with data from 2016. This temporal gap may impact the relevance of insights derived from the analysis.
**Cited**: The dataset lacks proper citation details, as it was collected by a third party, and crucial information such as the author's name is unavailable. This absence of citation affects the transparency and credibility of the dataset.


## Loading Packages

First, we will install the packages. Packages in R are sets of tools and functions that will help us in our tasks.

install.packages("tidyverse")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("dplyr")
install.packages("janitor")
install.packages("lubridate")


Next, we will load the packages into R:

library(tidyverse)
library(ggplot2)
library(tidyr)
library(dplyr)
library(janitor)
library(lubridate)


## Importing datasets

Loading CSV files with our data:

daily_activity <- read.csv("dailyActivity_merged.csv")
daily_sleep <- read.csv(«sleepDay_merged.csv")
hourly_steps <- read.csv("hourlySteps_merged.csv")
weight <- read.csv("weightLogInfo_merged.csv")
heart_rate <- read.csv("heartrate_seconds_merged.csv")

Determining the sample size by identifying unique values in each data frame using the distinct() function:

n_distinct(daily_activity$Id)
n_distinct(daily_sleep$Id)
n_distinct(hourly_steps$Id)
n_distinct(weight$Id)
n_distinct(heart_rate$Id)


Due to the limited number of participants who provided weight and heart rate data, we will exclude them from our analysis.



## View and clean the datasets:

head(daily_activity)
str(daily_activity)

head(daily_sleep)
str(daily_sleep)

head(hourly_steps)
str(hourly_steps)


Check for duplicates:

sum(duplicated(daily_activity))
sum(duplicated(daily_sleep))
sum(duplicated(hourly_steps))


Remove duplicates and n/a:

daily_activity <- daily_activity %>%
  distinct() %>%
  drop_na()

daily_sleep <- daily_sleep %>%
  distinct() %>%
  drop_na()

hourly_steps <- hourly_steps %>%
  distinct() %>%
  drop_na()


Check that duplicates have been removed:

sum(duplicated(daily_sleep))


To further merge datasets, we will make sure that all column names are consistent and in the same format.
clean_names() is used first to clean the column names, and then rename_with(tolower) is used to convert all remaining column names to lowercase using the tolower() function:

clean_names(daily_activity)
daily_activity<- rename_with(daily_activity, tolower)
clean_names(daily_sleep)
daily_sleep <- rename_with(daily_sleep, tolower)
clean_names(daily_steps)
daily_steps <- rename_with(daily_steps, tolower)


Frequently, ID numbers serve solely as unique identifiers without intrinsic numerical significance, often prompting to be treated as nominal (categorical) variables.


Change the data type of activitydate and sleepday from character to date:

daily_activity$activitydate <- as.Date(daily_activity$activitydate, format = «%m/%d/%Y")
daily_activity$date <- format(daily_activity$activitydate, format = "%m/%d/%y")


daily_sleep$sleepday <- as.Date(daily_sleep$sleepday, format = "%m/%d/%Y %I:%M:%S %p»)
daily_sleep$date <- format(daily_sleep$sleepday, format = "%m/%d/%y")


Choose between as.POSIXct and as.Date based on whether you want to include the time information in your resulting object. If you only need the date, use as.Date; if you need both date and time, use as.POSIXct. As we need to have both date and time, we will use as.POSIXct:

hourly_steps$activityhour <- as.POSIXct(hourly_steps$activityhour, format = "%m/%d/%Y %I:%M:%S %p")


We will separate the activityhour column into two columns - date and time, thus it will be easier for us to analyze the most active and less active times throughout the day:

hourly_steps <- hourly_steps %>% 
  separate(activityhour, c("date", "time"), sep = "^\\S*\\K") %>%
  mutate(date = mdy(date))


Check the formatting:

str(daily_activity)
str(daily_sleep)
str(hourly_steps)


hourly_steps$date <- as.Date(hourly_steps$date, format = "%m/%d/%Y")
str(hourly_steps)



# Data Exploration

For the daily activity data frame:
daily_activity %>%  
  select(TotalSteps,
         TotalDistance,
         SedentaryMinutes) %>%
  summary()


daily_activity %>%
  select(veryactiveminutes, fairlyactiveminutes, lightlyactiveminutes) %>%
  summary()


daily_activity %>% select(Calories) %>% summary()


daily_sleep %>%
  select(totalsleeprecords, totalminutesasleep, totaltimeinbed) %>%
  summary()


merged_data <- merge(daily_sleep, daily_activity, by=c('id', 'date'))
head(merged_data)


Now, we want to find out the average amount of sleep for each participant:

mean_sleep <- daily_sleep %>%
    group_by(id) %>%
    summarize(mean_sleep = mean(totalminutesasleep)) %>%
    select(id, mean_sleep) %>%
    arrange(desc(mean_sleep)) %>%
    as.data.frame()

head(mean_sleep)


The percentage of actual sleep duration while laying in bed:

daily_sleep %>%
	group_by(id) %>%
	mutate(percent_sleep = (totalminutesasleep/totaltimeinbed)*100) %>%
	select(id, percent_sleep) %>%
	summarize(avg_persleep = mean(percent_sleep)) %>%
	arrange(desc(avg_persleep)) %>%
	mutate_if(is.numeric, round, 2)

The majority of participants slept for at least 90% of the time they were in bed. However, only four participants spent a smaller percentage of time sleeping, with the lowest being 63.37%.


Activity levels by participants:

activity_id <- daily_activity %>%
    group_by(id) %>%
    summarize(sum_very = sum(veryactiveminutes),
              sum_fairly = sum(fairlyactiveminutes),
              sum_lightly = sum(lightlyactiveminutes),
              sum_sed = sum(sedentaryminutes)) %>%
    select(id, sum_very, sum_fairly, sum_lightly, sum_sed) %>%
    as.data.frame()


Let’s see which hour of the day saw the highest average number of steps taken:

hourly_steps %>%
    group_by(time) %>%
    summarize(mean_steps = mean(steptotal)) %>%
    select(time, mean_steps) %>%
    arrange(desc(mean_steps)) %>%
    head(1)

The highest average number of steps was taken around 6:00 PM with an average of about 600 steps. 


Creating a data frame with average hourly steps for later visualization:

mean_steps <- hourly_steps %>%
    group_by(time) %>%
    summarize(mean_steps = mean(steptotal)) %>%
    select(time, mean_steps) %>%
    arrange(desc(time)) %>%
    as.data.frame()


Calculating the mean and standard deviation for total steps taken by each participant:

steps_byid <- hourly_steps %>%
    group_by(id) %>%
    summarize(mean_steps_id = mean(steptotal), sd_steps_id = sd(steptotal)) %>%
    mutate_if(is.numeric, round, 2) %>%
    as.data.frame()
head(steps_byid)



# Visualization
ggplot(data = daily_activity, aes(x=totalsteps, y=calories)) + 
  geom_point() + geom_smooth() + labs(title="Total Steps vs. Calories")
![image](https://github.com/anife-osmanova/marketing-analysis/assets/77293628/75849f84-0af3-41fc-a587-0495e5bfd689)

I've noticed a clear positive correlation between Total Steps and Calories, which intuitively aligns with the notion that increased activity leads to higher calorie expenditure.


ggplot(data = daily_sleep, aes(x=totalminutesasleep, y=totaltimeinbed)) + 
  geom_point()+ geom_smooth() + labs(title="Total Minutes Asleep vs. Total Time in Bed»)
![image](https://github.com/anife-osmanova/marketing-analysis/assets/77293628/2b2cbf26-3b83-4593-ba32-f262bdfecc22)


The relationship between Total Minutes Asleep and Total Time in Bed looks linear. So if Bellabeat users want to improve their sleep, we should consider using notification to go to sleep.


ggplot(data=merged_data, aes(x=totalminutesasleep, y=sedentaryminutes)) + 
+     geom_point(color='darkblue') + geom_smooth() +
+     labs(title="Minutes Asleep vs. Sedentary Minutes")
![image](https://github.com/anife-osmanova/marketing-analysis/assets/77293628/8e8768f9-ccf3-4ec6-bc9f-a61ae70e3d61)


Here, the negative relationship between Sedentary Minutes and Sleep time is evident. As a suggestion, if Bellabeat users seek to enhance their sleep quality, the Bellabeat app could recommend reducing sedentary time.


The relationship between daily steps taken and sedentary minutes.

ggplot(data=daily_activity, aes(x=totalsteps, y=sedentaryminutes)) + 
    geom_point() + 
    geom_smooth() + 
    labs(title="Total Steps vs. Sedentary Minutes",
         x = "Steps", y = «Minutes")
![image](https://github.com/anife-osmanova/marketing-analysis/assets/77293628/b6a4f844-8fca-4def-82a7-0aae5afe6c20)

It seems that there is no correlation between the total daily steps taken and sedentary minutes.


The average duration of nightly sleep among participants throughout the study period:

ggplot(mean_sleep, aes(x = id, y = mean_sleep)) +
	geom_col(aes(reorder(id, +mean_sleep), y = mean_sleep)) +
	labs(title = "Average Minutes of Sleep", x = "Participant Id", y = "Minutes") +
	theme(axis.text.x = element_text(angle = 90)) +
	geom_hline(yintercept = mean(mean_sleep$mean_sleep), color = "red")

![image](https://github.com/anife-osmanova/marketing-analysis/assets/77293628/a9a1dc6e-b89b-4af7-a4dd-17a61f4b4e3c)

The graph displays the individual average sleep duration of each participant, along with a comparison of their sleep patterns to the overall average across all participants.


The average number of steps taken per hour:

ggplot(mean_steps, aes(x = time, y = mean_steps)) +
    geom_col(aes(reorder(time, +mean_steps), mean_steps)) +
    theme(axis.text.x = element_text(angle = 90)) +
    labs(title = "Average Steps Taken per Hour of Day",
         x = "Hour", y = "Average Steps»)
![image](https://github.com/anife-osmanova/marketing-analysis/assets/77293628/57523b51-b54a-427b-8719-bf7b99085b28)

The highest number of steps were taken in the evening, specifically from 5-7 pm, while the lowest number of steps were recorded in the middle of the night, between 12-4 am.



# Summary


In this analysis, we were able to see how users use their smart devices in order to help Bellabeat with decision-making. By analyzing FitBit Fitness Tracker data*, we came up with some recommendations that could enhance Bellabeat’s strategy.


Bellabeat app can send push notifications that would encourage users to stand up more, get more steps, be active, exercise when low activity/continued inactivity occurs.

Points for steps taken, high activity, and challenges completed could be transferred to discounts for subscriptions, or purchases within the company to motivate users to be more active and increase the retention rate.

Mindfulness is a way to reduce stress and get more focused. «Mindful minute» notifications are for stress relief, self-reflection and deep breathing for 1 minute. It can be either a routine reminder and/or based on the heart rate measurements.

Increase retention and sales rate by making it more fun to use - implement inviting friends to the app to challenge each other and achieve goals together. 

Personalized content for different types of goals, targeted notifications and rewards. We all love attention and to be heard, thus it might be a good idea to note users’ dietary goals, physical activity and sleep schedule in order to send personalized notifications, exercise recommendations and food ideas/recipes developed for their type of diet/preferences.  


*Please, be aware that the current data does not fully meet the ROCCC criteria.
