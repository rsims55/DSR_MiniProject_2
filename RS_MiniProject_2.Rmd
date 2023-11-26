---
title: "RS_MiniProject_2"
author: "Randi Sims"
date: "`r Sys.Date()`"
output: html_document
---
# Introduction

For my analysis, I selected the ChronicAbsenteeismRate (Chronic Absenteeism Rate) variable as my numeric response variable.

My research question for this project is: To what extent does poverty category influence chronic absenteeism in South Carolina high school students over time between the 2017-18 and the 2021-22 school years?

Between 2017 and 2022 the world changed in unimaginable ways. The 2020 COVID-19 pandemic led to unprecedented levels of absenteeism in public schools. Importantly, the pandemic disporporationatly impacted impoverished and low-income communities, leading to substaintial impacts within these educational communities as well. 

***

# Load Packages
```{r setup, message=FALSE, warning=FALSE}
library(readxl)
library(tidyverse)
library(nlme)
library(emmeans)
library(dataedu)
```

***

# Import Data
```{r import financial data, message = FALSE, warning = FALSE}
# Get filenames from the data folder 
filenames <-
  list.files(path = here::here("data"),
             full.names = TRUE)

# Pass filenames to map and read_excel
all_files <-
  filenames %>%
  # Apply the function read_csv to each element of filenames
  map(., ~ read_excel(., 
                      sheet = "8.FinancialDataPage",
                      na = c("*", "N/AV")))

# Check for file continuity
all_files %>%
  map(ncol)

# Create function to select the correct columns
pick_vars <-
  function(data) {
    data %>%
      select_at(vars(1:6))
  }

# Use function on all files
all_files <-
  all_files %>%
  map(pick_vars)

# Combine data
financial_data <-
  all_files %>% 
  # Change all columns to double
  lapply(\(x) mutate(x, 
                     across (c("ReportCardYear", 
                               "SCHOOLID"), 
                             as.double))) %>%
  bind_rows()

# Check data
glimpse(financial_data)
```


```{r import absence data, message = FALSE, warning = FALSE}
# 2018 Data
data_18 <- read_excel("data/ReportCardData_AdditionalInfo2018.xlsx",
                      sheet = "5.SchoolQualityPage",
                      na = c("*", "N/AV"))

# 2019 Data
data_19 <- read_excel("data/ReportCardData_AdditionalInfo2019.xlsx",
                      sheet = "5.StudentEngagementPage",
                      na = c("*", "N/AV"))

# 2020 Data
data_20 <- read_excel("data/ReportCardData_AdditionalInfo2020.xlsx",
                      sheet = "5.StudentEngagementPage",
                      na = c("*", "N/AV"))

# 2021 Data
data_21 <- read_excel("data/ReportCardData_AdditionalInfo2021.xlsx",
                      sheet = "5b.StudentEngagement_Chronic",
                      na = c("*", "N/AV"))

# 2022 Data
data_22 <- read_excel("data/ReportCardData_AdditionalInfo2022.xlsx",
                      sheet = "5b.SchoolClimate_ChronicAbs",
                      na = c("*", "N/AV"))

# Make list of data
all_files <- list(data_18, data_19, data_20, data_21, data_22)

# Check for file continuity
all_files %>%
  map(ncol)

# Create function to select the correct columns
pick_vars <-
  function(data) {
    data %>%
      select(any_of(c(
        "ReportCardYear",
        "DistrictNm",
        "SchoolNm",
        "SCHOOLID",
        "SCHOOLTYPECD",
        "ChronicAbsenteeismRate",
        "PctChronic_ALL"
        )))
  }

# Use function on all files
all_files <-
  all_files %>%
  map(pick_vars)

# Function to change absentee rate column name
absentee_name <-
  function(data){
    names(data)[length(names(data))] <- "PctAbsentee"
    return(data)
  }

# Change name function on all files
all_files <-
  all_files %>%
  map(absentee_name)

# Combine data
absentee_data <-
  all_files %>%
  # Change all columns to double
  lapply(\(x) mutate(x,
                     across (c("ReportCardYear",
                               "SCHOOLID"),
                             as.double))) %>%
  bind_rows()

glimpse(absentee_data)
```

```{r join financial and absentee, warning = FALSE, message = FALSE}
# Join absentee and financial data together
data <- left_join(financial_data, absentee_data,
          by = c("ReportCardYear", 
                 "SCHOOLID", 
                 "SCHOOLTYPECD", 
                 "SchoolNm",
                 "DistrictNm"),
          keep = FALSE,
          unmatched = "drop")
```

***

# Process Data
```{r filtering data, warning = FALSE, message = FALSE}
data <- data %>%
  # Filter to only high schools
  filter(SCHOOLTYPECD == "H") %>%
  # Filtering out non-traditional high schools
  filter(str_detect(SchoolNm, 
                    "Academy|Charter|College|Magnet|School Of|School For|Juvenile",
                    negate = TRUE)) %>%
  # Filtering out non-traditional districts
  filter(str_detect(DistrictNm,
                    "Charter|Unified|Governor's|Djj|Juvenile",
                    negate = TRUE)) %>%
  # Remove duplicates
  unique()
```

```{r check for typos, warning = FALSE, message = FALSE}
# Checking typos in school id
data %>%
  # Grouping by school id
  group_by(SCHOOLID, .drop = FALSE) %>%
  # Summarizing counts and adding school names
  summarize(Name = SchoolNm, count = n()) %>%
  # Finding any counts less than five
  filter(count < 5)

# Rename variables that were incorrect in data set
data['SCHOOLID'][data['SCHOOLID'] == 3805010] <- 3809010
data['SCHOOLID'][data['SCHOOLID'] == 3804049] <- 3809049
data['SCHOOLID'][data['SCHOOLID'] == 1404002] <- 1403016
data['SCHOOLID'][data['SCHOOLID'] == 3804024] <- 3809024
data['SCHOOLID'][data['SCHOOLID'] == 2503001] <- 2502011
data['SCHOOLID'][data['SCHOOLID'] == 3804054] <- 3809054
data['SCHOOLID'][data['SCHOOLID'] == 3805042] <- 3809042
data['SCHOOLID'][data['SCHOOLID'] == 3805028] <- 3809028
data['SCHOOLID'][data['SCHOOLID'] == 1404001] <- 1401001
data['SCHOOLID'][data['SCHOOLID'] == 2503002] <- 2501001
    
# Checking typos again - looks good
data %>%
  # Grouping by school id
  group_by(SCHOOLID, .drop = FALSE) %>%
  # Summarizing counts and adding school names
  summarize(Name = SchoolNm, count = n()) %>%
  # Finding any counts less than five
  filter(count < 5)

# Creating new variable for school classification based on poverty
data <- data %>%
  # Grouping by schools
  group_by(SCHOOLID) %>% 
  # Transforming two columns to numeric
  mutate(StudentsinPoverty_PctCurrYr = as.numeric(StudentsinPoverty_PctCurrYr)) %>%
  mutate(PctAbsentee = as.numeric(PctAbsentee)) %>%
  mutate(mean_poverty = mean(StudentsinPoverty_PctCurrYr)) %>%
  mutate(school_classification = case_when(
    mean_poverty <= 25 ~ "low",
    mean_poverty <= 50 ~ "mid-low",
    mean_poverty <= 75 ~ "mid-high",
    mean_poverty > 75 ~ "high"
  ))
```

***

# Visualize Data
```{r absentee data over time, warning=FALSE, message = FALSE, fig.cap="Average Percent Student Absentees Over Time per School Poverty Classification"}
# Table 
poverty_absentee_time <- 
  data %>%
  # Grouping by poverty classification and year
  group_by(school_classification, ReportCardYear) %>%
  # Calculates average absentee
  summarize(mean_absentee = mean(PctAbsentee, na.rm = TRUE))

# Line Plot
poverty_absentee_time %>%
  ggplot(aes(x = ReportCardYear, y = mean_absentee, color = school_classification)) + 
  geom_freqpoly(stat = "identity", size = 1) +
  labs(title = "Average Percent Absentee Rates Over Time",
       subtitle = "Per School Poverty Classification",
       x = "Year",
       y = "Average Percent Student Absence",
       fill = "Poverty Classification") +
  scale_color_dataedu(breaks = c("high", "mid-high", "mid-low", "low")) +
  theme_dataedu()
```

Based on Figure 1 - it appears that school poverty classification is somewhat related to absentee rates. We should do more testing to check for this though.

***

# Model Data
```{r mixed effects model 1, warning = FALSE, message = FALSE}
# Base model
pad_lme <- lme(StudentsinPoverty_PctCurrYr ~
                  school_classification*ReportCardYear,
               data = data,
               random = ~school_classification | SCHOOLID,
               na.action = na.omit)

anova(pad_lme)
```

Interaction of school classification and year is not significant at the 0.05 level, removing.

```{r mixed effects model 2, warning = FALSE, message = FALSE}
# Base model
pad_lme.me <- lme(StudentsinPoverty_PctCurrYr ~
                  school_classification+ReportCardYear,
               data = data,
               random = ~school_classification | SCHOOLID,
               na.action = na.omit)

anova(pad_lme.me)

# Check levels for school
contrast(emmeans(pad_lme.me, "school_classification"), "tukey")
```

This model shows that each level of school classification is significant for the model.

Both school classification (at various levels) and year impact absentee rates.

***

# Conclusion
As a reminder, my research question for this project was: To what extent does poverty category influence chronic absenteeism in South Carolina high school students over time between the 2017-18 and the 2021-22 school years?

There were relationships between both school classification and time for rates of absence. These relationships were very interesting and showed that poverty rates are closely tied to absenteeism. Based on Figure 1, it is also easy to see the effect of time on absentee rates. These are both sensible conclusions based on previous studies on poverty and absence rates in schools. 

One of the most interesting things to note, though, is the effect of the dip in absenteeism (therefore more attendance) in 2021. This may speak to the sociological implications as a result of the pandemic. Perhaps students were more likely to attend school in 2021 due to a flexibility of modalities (students had more options for hybrid/remote classes) or because of a desire to re-engage socially. These would be very interesting future studies. 