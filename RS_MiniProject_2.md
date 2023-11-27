RS_MiniProject_2
================
Randi Sims
2023-11-26

# Introduction

For my analysis, I selected the ChronicAbsenteeismRate (Chronic
Absenteeism Rate) variable as my numeric response variable.

My research question for this project is: To what extent does poverty
category influence chronic absenteeism in South Carolina high school
students over time between the 2017-18 and the 2021-22 school years?

Between 2017 and 2022 the world changed in unimaginable ways. The 2020
COVID-19 pandemic led to unprecedented levels of absenteeism in public
schools. Importantly, the pandemic disporporationatly impacted
impoverished and low-income communities, leading to substaintial impacts
within these educational communities as well.

------------------------------------------------------------------------

# Load Packages

``` r
library(readxl)
library(tidyverse)
library(nlme)
library(emmeans)
library(dataedu)
```

------------------------------------------------------------------------

# Import Data

``` r
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
```

    ## [[1]]
    ## [1] 17
    ## 
    ## [[2]]
    ## [1] 29
    ## 
    ## [[3]]
    ## [1] 29
    ## 
    ## [[4]]
    ## [1] 29
    ## 
    ## [[5]]
    ## [1] 29

``` r
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

    ## Rows: 6,979
    ## Columns: 6
    ## $ ReportCardYear              <dbl> 2018, 2018, 2018, 2018, 2018, 2018, 2018, …
    ## $ DistrictNm                  <chr> "Abbeville County School District", "Abbev…
    ## $ SchoolNm                    <chr> "Abbeville High School", "Dixie High Schoo…
    ## $ SCHOOLID                    <dbl> 160001, 160003, 160007, 160016, 160017, 16…
    ## $ SCHOOLTYPECD                <chr> "H", "H", "E", "M", "E", "E", "M", "E", "M…
    ## $ StudentsinPoverty_PctCurrYr <dbl> 67.36, 52.49, 86.56, 70.44, 74.86, 57.24, …

``` r
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
```

    ## [[1]]
    ## [1] 31
    ## 
    ## [[2]]
    ## [1] 31
    ## 
    ## [[3]]
    ## [1] 21
    ## 
    ## [[4]]
    ## [1] 57
    ## 
    ## [[5]]
    ## [1] 57

``` r
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

    ## Rows: 6,926
    ## Columns: 6
    ## $ ReportCardYear <dbl> 2018, 2018, 2018, 2018, 2018, 2018, 2018, 2018, 2018, 2…
    ## $ DistrictNm     <chr> "Abbeville County School District", "Abbeville County S…
    ## $ SchoolNm       <chr> "Abbeville High School", "Dixie High School", "John C. …
    ## $ SCHOOLID       <dbl> 160001, 160003, 160007, 160016, 160017, 160018, 160018,…
    ## $ SCHOOLTYPECD   <chr> "H", "H", "E", "M", "E", "E", "M", "E", "M", "P", "D", …
    ## $ PctAbsentee    <dbl> 1.2, 9.8, 1.1, 9.4, 1.7, 11.5, 11.5, 19.9, 19.9, 9.9, 7…

``` r
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

------------------------------------------------------------------------

# Process Data

``` r
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

``` r
# Checking typos in school id
data %>%
  # Grouping by school id
  group_by(SCHOOLID, .drop = FALSE) %>%
  # Summarizing counts and adding school names
  summarize(Name = SchoolNm, count = n()) %>%
  # Finding any counts less than five
  filter(count < 5)
```

    ## # A tibble: 82 × 3
    ## # Groups:   SCHOOLID [207]
    ##    SCHOOLID Name                                     count
    ##       <dbl> <chr>                                    <int>
    ##  1  1001121 "Lucy Garrett Beckham High"                  2
    ##  2  1001121 "Lucy Garrett Beckham High"                  2
    ##  3  1401001 "Scott's Branch High School"                 4
    ##  4  1401001 "Scott's Branch High School"                 4
    ##  5  1401001 "Scott's Branch Middle High School"          4
    ##  6  1401001 "Scott's Branch High"                        4
    ##  7  1403016 "East Clarendon Middle / High School"        4
    ##  8  1403016 "East Clarendon Middle / High School"        4
    ##  9  1403016 "East Clarendon Middle / High\r\nSchool"     4
    ## 10  1403016 "East Clarendon Middle / High\r\nSchool"     4
    ## # ℹ 72 more rows

``` r
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
```

    ## # A tibble: 32 × 3
    ## # Groups:   SCHOOLID [197]
    ##    SCHOOLID Name                      count
    ##       <dbl> <chr>                     <int>
    ##  1  1001121 Lucy Garrett Beckham High     2
    ##  2  1001121 Lucy Garrett Beckham High     2
    ##  3  2103029 Lake City High School         4
    ##  4  2103029 Lake City High School         4
    ##  5  2103029 Lake City High School         4
    ##  6  2103029 Lake City High School         4
    ##  7  2301005 Carolina High School          3
    ##  8  2301005 Carolina High School          3
    ##  9  2301005 Carolina High School          3
    ## 10  2301122 Fountain Inn High School      1
    ## # ℹ 22 more rows

``` r
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

------------------------------------------------------------------------

# Visualize Data

``` r
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

<figure>
<img
src="RS_MiniProject_2_files/figure-gfm/absentee%20data%20over%20time-1.png"
alt="Average Percent Student Absentees Over Time per School Poverty Classification" />
<figcaption aria-hidden="true">Average Percent Student Absentees Over
Time per School Poverty Classification</figcaption>
</figure>

Based on Figure 1 - it appears that school poverty classification is
somewhat related to absentee rates. We should do more testing to check
for this though.

------------------------------------------------------------------------

# Model Data

``` r
# Base model
pad_lme <- lme(StudentsinPoverty_PctCurrYr ~
                  school_classification*ReportCardYear,
               data = data,
               random = ~school_classification | SCHOOLID,
               na.action = na.omit)

anova(pad_lme)
```

    ##                                      numDF denDF   F-value p-value
    ## (Intercept)                              1   756 18020.542  <.0001
    ## school_classification                    3   193   793.567  <.0001
    ## ReportCardYear                           1   756   116.199  <.0001
    ## school_classification:ReportCardYear     3   756     2.283  0.0778

Interaction of school classification and year is not significant at the
0.05 level, removing.

``` r
# Base model
pad_lme.me <- lme(StudentsinPoverty_PctCurrYr ~
                  school_classification+ReportCardYear,
               data = data,
               random = ~school_classification | SCHOOLID,
               na.action = na.omit)

anova(pad_lme.me)
```

    ##                       numDF denDF   F-value p-value
    ## (Intercept)               1   759 18113.328  <.0001
    ## school_classification     3   193   754.190  <.0001
    ## ReportCardYear            1   759   115.741  <.0001

``` r
# Check levels for school
contrast(emmeans(pad_lme.me, "school_classification"), "tukey")
```

    ##  contrast               estimate   SE  df t.ratio p.value
    ##  high - low                 65.6 1.51 193  43.452  <.0001
    ##  high - (mid-high)          22.5 1.07 193  20.997  <.0001
    ##  high - (mid-low)           44.2 1.40 193  31.587  <.0001
    ##  low - (mid-high)          -43.0 1.47 193 -29.246  <.0001
    ##  low - (mid-low)           -21.3 1.72 193 -12.375  <.0001
    ##  (mid-high) - (mid-low)     21.7 1.36 193  15.954  <.0001
    ## 
    ## Degrees-of-freedom method: containment 
    ## P value adjustment: tukey method for comparing a family of 4 estimates

This model shows that each level of school classification is significant
for the model.

Both school classification (at various levels) and year impact absentee
rates.

------------------------------------------------------------------------

# Conclusion

As a reminder, my research question for this project was: To what extent
does poverty category influence chronic absenteeism in South Carolina
high school students over time between the 2017-18 and the 2021-22
school years?

There were relationships between both school classification and time for
rates of absence. These relationships were very interesting and showed
that poverty rates are closely tied to absenteeism. Based on Figure 1,
it is also easy to see the effect of time on absentee rates. These are
both sensible conclusions based on previous studies on poverty and
absence rates in schools.

One of the most interesting things to note, though, is the effect of the
dip in absenteeism (therefore more attendance) in 2021. This may speak
to the sociological implications as a result of the pandemic. Perhaps
students were more likely to attend school in 2021 due to a flexibility
of modalities (students had more options for hybrid/remote classes) or
because of a desire to re-engage socially. These would be very
interesting future studies.
