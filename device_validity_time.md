---
title: "Device validity over time"
author: "Daniel Fuller"
date: "20/01/2021"
output:
  html_document:
    keep_md: true
---



### Packages


```r
library(tidyverse)
```

```
## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
## ✔ ggplot2 3.4.0      ✔ purrr   0.3.5 
## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
## ✔ tidyr   1.2.1      ✔ stringr 1.4.1 
## ✔ readr   2.1.3      ✔ forcats 0.5.2 
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
```

```r
library(readxl)
library(broom)
library(broom.mixed)
library(lme4)
```

```
## Loading required package: Matrix
## 
## Attaching package: 'Matrix'
## 
## The following objects are masked from 'package:tidyr':
## 
##     expand, pack, unpack
```

```r
library(rstatix)
```

```
## 
## Attaching package: 'rstatix'
## 
## The following object is masked from 'package:stats':
## 
##     filter
```

```r
library(gtsummary)
library(knitr)
```

### Reading in data


```r
validity1 <- read_csv("wearable_review_data_validity.csv")
```

```
## Rows: 1672 Columns: 107
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (62): Author, Substudy, Setting, Measured, Measure_Unit, Brand, Device, ...
## dbl (44): X1, Year, device_year, age_SD, weight_SD, height_SD, BMI_SD, actua...
## lgl  (1): n_15pctofcrit
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
validity <- select(validity1, 1:37, MPE, MAPE)

write_csv(validity, "validity_over_time.csv")

validity <- validity %>% 
  mutate(MAPE = ifelse(is.na(MAPE),
                            abs(MPE*100),
                            MAPE))
```

### Reading in data


```r
glimpse(validity)
```

```
## Rows: 1,672
## Columns: 39
## $ X1                 <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, …
## $ Author             <chr> "Dooley", "Dooley", "Dooley", "Boudreaux", "Boudrea…
## $ Year               <dbl> 2017, 2017, 2017, 2018, 2018, 2018, 2017, 2017, 201…
## $ Substudy           <chr> "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "…
## $ Setting            <chr> "Controlled", "Controlled", "Controlled", "Controll…
## $ Measured           <chr> "HR", "HR", "HR", "HR", "HR", "HR", "SC", "SC", "SC…
## $ Measure_Unit       <chr> "bpm", "bpm", "bpm", "bpm", "bpm", "bpm", "steps/10…
## $ Brand              <chr> "Apple", "Apple", "Apple", "Apple", "Apple", "Apple…
## $ Device             <chr> "Watch", "Watch", "Watch", "Watch Series 2", "Watch…
## $ device_name        <chr> "Apple Watch", "Apple Watch", "Apple Watch", "Apple…
## $ device_year        <dbl> 2015, 2015, 2015, 2016, 2016, 2016, 2015, 2015, 201…
## $ Wear_Location      <chr> "Wrist", "Wrist", "Wrist", "Wrist", "Wrist", "Wrist…
## $ Wear_Info          <chr> "wrist, random", "wrist, random", "wrist, random", …
## $ Type               <chr> "full-text", "full-text", "full-text", "full-text",…
## $ `Good?`            <chr> "y", "y", "y", "y", "y", "y", "y", "y", "y", "y", "…
## $ Criterion_Measure  <chr> "Heart rate sensor chest strap (Polar T31)", "Heart…
## $ Criterion_Type     <chr> "chest strap", "chest strap", "chest strap", "ECG",…
## $ Wear_Info_crit     <chr> "chest", "chest", "chest", "upper torso", "upper to…
## $ Wear_Location_crit <chr> "Torso", "Torso", "Torso", "Torso", "Torso", "Torso…
## $ population_n       <chr> "62", "62", "62", "50", "50", "50", "31", "31", "31…
## $ population_m       <chr> "26", "26", "26", "22", "22", "22", "16", "16", "16…
## $ population_f       <chr> "36", "36", "36", "28", "28", "28", "15", "15", "15…
## $ population         <chr> "healthy adults", "healthy adults", "healthy adults…
## $ age_code           <chr> "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "…
## $ health_code        <chr> "H", "H", "H", "H", "H", "H", "H", "H", "H", "H", "…
## $ age                <chr> "22.55", "22.55", "22.55", "22.71", "22.71", "22.71…
## $ age_SD             <dbl> 4.34, 4.34, 4.34, 2.99, 2.99, 2.99, 12.00, 12.00, 1…
## $ weight             <chr> "72.02", "72.02", "72.02", "67.79", "67.79", "67.79…
## $ weight_SD          <dbl> 18.99, 18.99, 18.99, 14.01, 14.01, 14.01, NA, NA, N…
## $ height             <chr> "170", "170", "170", "162.71", "162.71", "162.71", …
## $ height_SD          <dbl> 11.00, 11.00, 11.00, 5.79, 5.79, 5.79, NA, NA, NA, …
## $ BMI                <chr> "24.6", "24.6", "24.6", "25.83", "25.83", "25.83", …
## $ BMI_SD             <dbl> 4.77, 4.77, 4.77, 4.83, 4.83, 4.83, 2.40, 2.40, 2.4…
## $ location           <chr> "TX, USA", "TX, USA", "TX, USA", "LA, USA", "LA, US…
## $ activity_type      <chr> "Rest: Seated", "Rest: Seated", "Rest: Seated", "Re…
## $ test_type          <chr> "Rest", "Rest", "Rest", "Rest", "Rest", "Activity",…
## $ activity_type_code <chr> "Se", "Se", "Se", "Se", "Se", "Mi", "At", "At", "At…
## $ MPE                <dbl> 0.007190265, 0.001096041, -0.005327335, NA, NA, NA,…
## $ MAPE               <dbl> 0.0276, 0.0163, 0.0114, 0.0121, 0.0144, 0.0299, 0.0…
```

### Filtering the data for SC


```r
validity <- subset(validity, Measured != "EE" & Measured != "HR")
```

### Removing Xiaomi and Mio (only one year of devices)


```r
validity <- subset(validity, Brand != "Xiaomi")

validity <- subset(validity, Brand != "Mio")
```

### Removing years prior to 2011


```r
validity <- subset(validity, device_year != "2008" & device_year != "2009")
```


```r
clean_data <- validity %>% drop_na(MAPE)

### Create study_year variable

clean_data$study_year <- str_c(clean_data$Author, clean_data$Year, sep = "_")
length(clean_data$study_year)
```

```
## [1] 944
```


### Number of SC validity tests conducted within each brand


```r
table(clean_data$Brand)
```

```
## 
##    Apple   Fitbit   Garmin   Misfit    Polar  Samsung Withings 
##       22      601      148       41       41       15       76
```

There are 7 different brands of commercial wearable devices included in this study. They are: Apple (31 validity tests), Fitbit (673 validity tests), Garmin (169 validity tests), Misfit (42 validity tests), Polar (42 validity tests), Samsung (15 validity tests) and Withings (84 validity tests).

### Number of SC validity tests conducted within each device type per Brand


```r
table(clean_data$device_name)
```

```
## 
##             Apple Watch    Apple Watch Series 2           Fitbit Charge 
##                      21                       1                      20 
##         Fitbit Charge 2        Fitbit Charge HR             Fitbit Flex 
##                      25                      66                      98 
##            Fitbit Force              Fitbit One            Fitbit Surge 
##                       5                     145                      18 
##            Fitbit Ultra              Fitbit Zip   Garmin Forerunner 235 
##                      37                     187                       2 
## Garmin Forerunner 735XT Garmin Forerunner 920XT       Garmin Vivoactive 
##                       3                       6                       6 
##          Garmin Vivofit        Garmin Vivofit 2        Garmin Vivofit 3 
##                      77                       3                       6 
##        Garmin Vivosmart     Garmin Vivosmart HR    Garmin Vivosmart HR+ 
##                      14                       8                      23 
##            Misfit Flash            Misfit Shine              Polar A360 
##                       6                      35                       6 
##            Polar Active              Polar Loop              Polar M600 
##                       6                      27                       1 
##              Polar V800          Samsung Gear 2          Samsung Gear S 
##                       1                       4                       7 
##         Samsung Gear S2         Samsung Gear S3       Withings Pulse O2 
##                       3                       1                      47 
##       Withings Pulse Ox 
##                      29
```

There are multiple of device types within each brand that were tested for step count validity. Apple has two device types tested: Apple Watch (30 validity tests) and Apple Watch Series 2 (1 validity test). Fitbit has 11 device types: Fitbit (3), Fitbit Charge (21), Fitbit Charge 2 (30), Fitbit Charge HR (76), Fitbit Classic (17), Fitbit Flex (109), Fitbit Force (6), Fitbit One (160), Fitbit Surge (18), Fitbit Ultra (39), Fitbit Zip (194). Garmin has 11 device types: Garmin Forerunner 235 (2), Garmin Forerunner 405CX (1), Garmin Forerunner 735XT (3), Garmin Forerunner 920XT (6), Garmin Vivoactive (6), Garmin Vivofit (81), Garmin Vivofit 2 (11), Garmin Vivofit 3 (6), Garmin Vivosmart (14), Garmin Vivosmart HR (13) and Garmin Vivosmart HR+ (26). Misfit has two device types: Misfit Flash (6) and Misfit Shine (36). Polar has 6 device types: Polar A300 (1), Polar A360 (6), Polar Active (6), Polar Loop (27), Polar M600 (1) and Polar V800 (1). Samsung has 4 device types: Samsung Gear 2 (4), Samsung Gear S (7), Samsung Gear S2 (3) and Samsung Gear S3 (1). Withings has 2 device types: Withings Pulse O2 (50) and Withings Pulse Ox (34).


### Number of devices tested for step count per brand and year of release


```r
brand_time <- table(clean_data$Brand, clean_data$device_year)
kable(brand_time)
```



|         | 2011| 2012| 2013| 2014| 2015| 2016|
|:--------|----:|----:|----:|----:|----:|----:|
|Apple    |    0|    0|    0|    0|   21|    1|
|Fitbit   |   37|  145|  290|   20|   84|   25|
|Garmin   |    0|    0|    0|   97|   19|   32|
|Misfit   |    0|   35|    0|    0|    6|    0|
|Polar    |    6|    0|   27|    1|    6|    1|
|Samsung  |    0|    0|    0|   11|    3|    1|
|Withings |    0|    0|   47|   29|    0|    0|

```r
histo_device_year_Brand <- ggplot(data = clean_data, aes(device_year)) +
        geom_histogram() +
        facet_wrap(~ Brand)

plot(histo_device_year_Brand)
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](device_validity_time_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

### Summary of Mean Absolute Percentage Error for SC of all brands


```r
summary(clean_data$MAPE)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
##   0.00000   0.06628   0.57450   8.98760   6.50774 100.00000
```

```r
mape_time <- clean_data %>%
    group_by(device_year) %>%
    get_summary_stats(MAPE, type = "mean_sd") %>%
    arrange(device_year)
kable(mape_time)
```



| device_year|variable |   n|   mean|     sd|
|-----------:|:--------|---:|------:|------:|
|        2011|MAPE     |  43|  3.861|  8.776|
|        2012|MAPE     | 180|  7.290| 15.312|
|        2013|MAPE     | 364| 14.495| 25.513|
|        2014|MAPE     | 158|  7.304| 17.519|
|        2015|MAPE     | 139|  2.595|  5.231|
|        2016|MAPE     |  60|  3.587|  9.665|

```r
mape_time_brand <- clean_data %>%
    group_by(Brand, device_year) %>%
    get_summary_stats(MAPE, type = "mean_sd") %>%
    arrange(Brand, device_year)
kable(mape_time_brand)
```



|Brand    | device_year|variable |   n|   mean|     sd|
|:--------|-----------:|:--------|---:|------:|------:|
|Apple    |        2015|MAPE     |  21|  1.678|  2.129|
|Apple    |        2016|MAPE     |   1|  0.420|     NA|
|Fitbit   |        2011|MAPE     |  37|  1.958|  5.901|
|Fitbit   |        2012|MAPE     | 145|  5.020| 11.668|
|Fitbit   |        2013|MAPE     | 290| 15.816| 25.714|
|Fitbit   |        2014|MAPE     |  20| 22.754| 29.263|
|Fitbit   |        2015|MAPE     |  84|  3.378|  6.407|
|Fitbit   |        2016|MAPE     |  25|  3.592|  9.753|
|Garmin   |        2014|MAPE     |  97|  6.290| 16.190|
|Garmin   |        2015|MAPE     |  19|  0.620|  1.317|
|Garmin   |        2016|MAPE     |  32|  2.332|  5.639|
|Misfit   |        2012|MAPE     |  35| 16.691| 23.339|
|Misfit   |        2015|MAPE     |   6|  0.101|  0.053|
|Polar    |        2011|MAPE     |   6| 15.594| 14.294|
|Polar    |        2013|MAPE     |  27|  2.166|  4.085|
|Polar    |        2014|MAPE     |   1| 22.959|     NA|
|Polar    |        2015|MAPE     |   6|  3.490|  3.396|
|Polar    |        2016|MAPE     |   1| 50.180|     NA|
|Samsung  |        2014|MAPE     |  11|  2.412|  2.945|
|Samsung  |        2015|MAPE     |   3|  2.783|  1.454|
|Samsung  |        2016|MAPE     |   1|  0.210|     NA|
|Withings |        2013|MAPE     |  47| 13.424| 29.523|
|Withings |        2014|MAPE     |  29|  1.357|  3.047|

```r
histo_MAPE_SC <- ggplot(data = clean_data, aes(MAPE)) +
        geom_histogram(bins = 45) + 
        theme_bw()

plot(histo_MAPE_SC)
```

![](device_validity_time_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

The histogram indicates that the MAPE values are not normally distributed and that there is an extreme positive skewness. Majority of the MAPE values lie to the left of the distribution. The mean is likely not be a good measure of central tendency for this data because of the degree of skewness seen in the histogram. Therefore, we will want to use the median value instead.


### Scatter plot for SC MAPE for all brands compared to device years of release


```r
scatter_MAPE_year_SC <- ggplot(data = clean_data, aes(x = device_year, y = MAPE)) +
      geom_point(alpha = 0.2) +
      stat_smooth(method = "lm", colour = "gray") + 
      stat_smooth() + 
      theme_classic()

plot(scatter_MAPE_year_SC)
```

```
## `geom_smooth()` using formula = 'y ~ x'
## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
```

![](device_validity_time_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

As seen in the above scatter plot, as time (device year of release) increases, the Mean Absolute Percentage Error of Step Count measurement increases. MAPE is representative of accuracy, therefore the accuracy for these commercial wearable devices to measure step count decreases over time. 


### Scatter plot for SC MAPE per brand compared to device years of release


```r
scatter_MAPE_year_Brand <- ggplot(data = clean_data, aes(x = device_year, y = MAPE)) +
      geom_point(alpha = 0.2) +
      stat_smooth(method = "lm", colour = "gray") +
      facet_wrap(~ Brand) +
      theme_bw()

plot(scatter_MAPE_year_Brand)
```

```
## `geom_smooth()` using formula = 'y ~ x'
```

![](device_validity_time_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

```r
ggsave("scatter_MAPE_year_Brand.pdf", scatter_MAPE_year_Brand, dpi = 300)
```

```
## Saving 7 x 5 in image
## `geom_smooth()` using formula = 'y ~ x'
```

As seen in the above figure, the accuracy of wearable devices to measure step count varies by brand. Of the validity tests conducted using Apple devices, there was a decrease in MAPE values between devices released in 2015 and 2016. This indicates that the accuracy of Apple devices increased with the release of a newer version of wearable technology. When analyzing the data for the brand Fitbit, it can be seen that there was an increased in MAPE values between devices released from 2011-2016. This indicates that the accuracy of Fitbit devices decreased with the release of newer versions of wearable technology. A similar trend was seen in Garmin devices released between the years of 2009-2016. The step count accuracy of these devices decreased as well. The brands Misfit and Polar show increases in accuracy in devices released from 2012-2015 and 2013-2015, respectively. Samsung and Withings show decreases in accuracy in devices released from 2014-2016 ans 2013-2014, respectively. 

## Recoding device year as continuous 


```r
clean_data <- clean_data %>%
	mutate(device_year_c = case_when(
		device_year == 2011 ~ 1,
		device_year == 2012 ~ 2,
		device_year == 2013 ~ 3,
		device_year == 2014 ~ 4,
		device_year == 2015 ~ 5,
		device_year == 2016 ~ 6,
	))
summary(clean_data$device_year_c)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.000   3.000   3.000   3.371   4.000   6.000
```

## Model 1: Linear Regression Device year as a predictor of Step Count MAPE


```r
lm_year_MAPE <- lm(MAPE ~ device_year_c, data = clean_data)

summary(lm_year_MAPE)
```

```
## 
## Call:
## lm(formula = MAPE ~ device_year_c, data = clean_data)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -12.631  -9.482  -6.465  -2.363  90.443 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)     14.168      1.819   7.788 1.79e-14 ***
## device_year_c   -1.537      0.506  -3.037  0.00245 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 19.44 on 942 degrees of freedom
## Multiple R-squared:  0.009698,	Adjusted R-squared:  0.008647 
## F-statistic: 9.225 on 1 and 942 DF,  p-value: 0.002453
```

```r
tidy(lm_year_MAPE, conf.int = TRUE)
```

```
## # A tibble: 2 × 7
##   term          estimate std.error statistic  p.value conf.low conf.high
##   <chr>            <dbl>     <dbl>     <dbl>    <dbl>    <dbl>     <dbl>
## 1 (Intercept)      14.2      1.82       7.79 1.79e-14    10.6     17.7  
## 2 device_year_c    -1.54     0.506     -3.04 2.45e- 3    -2.53    -0.544
```

```r
clean_data_m1 <- augment(lm_year_MAPE, newdata = clean_data, interval = "prediction")

scatter_fitted_year <- ggplot(data = clean_data_m1, aes(x = device_year, y = MAPE)) +
      geom_point(alpha = 0.2) +
      stat_smooth(method = "lm", colour = "gray") +
      theme_bw()

plot(scatter_fitted_year)
```

```
## `geom_smooth()` using formula = 'y ~ x'
```

![](device_validity_time_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

## Model 2: Linear Regression - Device year as a predictor of Step Count MAPE + Wear location control


```r
lm_year_MAPE_loc <- lm(MAPE ~ device_year_c + Wear_Location, data = clean_data, na.action = na.exclude)

summary(lm_year_MAPE_loc)
```

```
## 
## Call:
## lm(formula = MAPE ~ device_year_c + Wear_Location, data = clean_data, 
##     na.action = na.exclude)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -20.579  -7.895  -6.047  -1.148  92.035 
## 
## Coefficients:
##                        Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             21.8751     2.9557   7.401 3.01e-13 ***
## device_year_c           -0.6454     0.6376  -1.012   0.3117    
## Wear_LocationThigh      10.8787    19.2475   0.565   0.5721    
## Wear_LocationTorso      -2.7079     3.2039  -0.845   0.3982    
## Wear_LocationUpper Arm -21.2097     8.2318  -2.577   0.0101 *  
## Wear_LocationWaist/Hip -11.9736     2.6994  -4.436 1.03e-05 ***
## Wear_LocationWrist     -12.5477     2.7508  -4.561 5.76e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 19.09 on 937 degrees of freedom
## Multiple R-squared:  0.05022,	Adjusted R-squared:  0.04414 
## F-statistic: 8.258 on 6 and 937 DF,  p-value: 9.892e-09
```

```r
tidy(lm_year_MAPE_loc, conf.int = TRUE)
```

```
## # A tibble: 7 × 7
##   term                   estimate std.error statistic  p.value conf.low conf.h…¹
##   <chr>                     <dbl>     <dbl>     <dbl>    <dbl>    <dbl>    <dbl>
## 1 (Intercept)              21.9       2.96      7.40  3.01e-13    16.1    27.7  
## 2 device_year_c            -0.645     0.638    -1.01  3.12e- 1    -1.90    0.606
## 3 Wear_LocationThigh       10.9      19.2       0.565 5.72e- 1   -26.9    48.7  
## 4 Wear_LocationTorso       -2.71      3.20     -0.845 3.98e- 1    -9.00    3.58 
## 5 Wear_LocationUpper Arm  -21.2       8.23     -2.58  1.01e- 2   -37.4    -5.05 
## 6 Wear_LocationWaist/Hip  -12.0       2.70     -4.44  1.03e- 5   -17.3    -6.68 
## 7 Wear_LocationWrist      -12.5       2.75     -4.56  5.76e- 6   -17.9    -7.15 
## # … with abbreviated variable name ¹​conf.high
```

```r
clean_data_m2 <- augment(lm_year_MAPE_loc, newdata = clean_data, interval = "prediction")

scatter_fitted_year_loc <- ggplot(data = clean_data_m2, aes(x = device_year, y = MAPE)) +
      geom_point(alpha = 0.2) +
      stat_smooth(method = "lm", colour = "gray") +
      theme_bw()

plot(scatter_fitted_year_loc)
```

```
## `geom_smooth()` using formula = 'y ~ x'
```

![](device_validity_time_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

## Model 3:  Linear Regression - Analyzing differences in SC MAPE with device year and brand as factors


```r
lmer_year_by_brand_MAPE <- lm(MAPE ~ device_year_c*Brand + Wear_Location, data = clean_data)

summary(lmer_year_by_brand_MAPE)
```

```
## 
## Call:
## lm(formula = MAPE ~ device_year_c * Brand + Wear_Location, data = clean_data)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -20.413  -8.126  -5.810  -0.009  91.867 
## 
## Coefficients:
##                             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                  21.0830    98.2397   0.215   0.8301    
## device_year_c                -1.2577    19.4456  -0.065   0.9484    
## BrandFitbit                  -3.2801    98.2594  -0.033   0.9734    
## BrandGarmin                   7.1651    98.5879   0.073   0.9421    
## BrandMisfit                  18.0624    98.4785   0.183   0.8545    
## BrandPolar                   -2.1909    98.5460  -0.022   0.9823    
## BrandSamsung                 -3.2497   104.5815  -0.031   0.9752    
## BrandWithings                36.4653    99.5623   0.366   0.7143    
## Wear_LocationThigh           10.2640    19.1565   0.536   0.5922    
## Wear_LocationTorso           -3.2014     3.1954  -1.002   0.3167    
## Wear_LocationUpper Arm      -18.6998     8.2401  -2.269   0.0235 *  
## Wear_LocationWaist/Hip      -12.4205     2.7302  -4.549 6.10e-06 ***
## Wear_LocationWrist          -13.1171     2.9387  -4.464 9.05e-06 ***
## device_year_c:BrandFitbit     2.1746    19.4638   0.112   0.9111    
## device_year_c:BrandGarmin    -1.0281    19.5378  -0.053   0.9580    
## device_year_c:BrandMisfit    -4.0671    19.6516  -0.207   0.8361    
## device_year_c:BrandPolar      1.3313    19.6061   0.068   0.9459    
## device_year_c:BrandSamsung    0.7091    21.1142   0.034   0.9732    
## device_year_c:BrandWithings  -9.5470    20.0041  -0.477   0.6333    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 19 on 925 degrees of freedom
## Multiple R-squared:  0.07136,	Adjusted R-squared:  0.05328 
## F-statistic: 3.949 on 18 and 925 DF,  p-value: 6.502e-08
```

```r
tidy(lmer_year_by_brand_MAPE, conf.int = TRUE)
```

```
## # A tibble: 19 × 7
##    term                        estimate std.er…¹ stati…² p.value conf.…³ conf.…⁴
##    <chr>                          <dbl>    <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
##  1 (Intercept)                   21.1      98.2   0.215  8.30e-1 -172.    214.  
##  2 device_year_c                 -1.26     19.4  -0.0647 9.48e-1  -39.4    36.9 
##  3 BrandFitbit                   -3.28     98.3  -0.0334 9.73e-1 -196.    190.  
##  4 BrandGarmin                    7.17     98.6   0.0727 9.42e-1 -186.    201.  
##  5 BrandMisfit                   18.1      98.5   0.183  8.55e-1 -175.    211.  
##  6 BrandPolar                    -2.19     98.5  -0.0222 9.82e-1 -196.    191.  
##  7 BrandSamsung                  -3.25    105.   -0.0311 9.75e-1 -208.    202.  
##  8 BrandWithings                 36.5      99.6   0.366  7.14e-1 -159.    232.  
##  9 Wear_LocationThigh            10.3      19.2   0.536  5.92e-1  -27.3    47.9 
## 10 Wear_LocationTorso            -3.20      3.20 -1.00   3.17e-1   -9.47    3.07
## 11 Wear_LocationUpper Arm       -18.7       8.24 -2.27   2.35e-2  -34.9    -2.53
## 12 Wear_LocationWaist/Hip       -12.4       2.73 -4.55   6.10e-6  -17.8    -7.06
## 13 Wear_LocationWrist           -13.1       2.94 -4.46   9.05e-6  -18.9    -7.35
## 14 device_year_c:BrandFitbit      2.17     19.5   0.112  9.11e-1  -36.0    40.4 
## 15 device_year_c:BrandGarmin     -1.03     19.5  -0.0526 9.58e-1  -39.4    37.3 
## 16 device_year_c:BrandMisfit     -4.07     19.7  -0.207  8.36e-1  -42.6    34.5 
## 17 device_year_c:BrandPolar       1.33     19.6   0.0679 9.46e-1  -37.1    39.8 
## 18 device_year_c:BrandSamsung     0.709    21.1   0.0336 9.73e-1  -40.7    42.1 
## 19 device_year_c:BrandWithings   -9.55     20.0  -0.477  6.33e-1  -48.8    29.7 
## # … with abbreviated variable names ¹​std.error, ²​statistic, ³​conf.low,
## #   ⁴​conf.high
```

```r
clean_data_m3 <- augment(lmer_year_by_brand_MAPE, newdata = clean_data, interval = "prediction")

scatter_fitted_year_loc_brand <- ggplot(data = clean_data_m3, aes(x = device_year, y = MAPE)) +
      geom_point(alpha = 0.2) +
      stat_smooth(method = "lm", colour = "gray") +
      theme_bw()

plot(scatter_fitted_year_loc_brand)
```

```
## `geom_smooth()` using formula = 'y ~ x'
```

![](device_validity_time_files/figure-html/unnamed-chunk-17-1.png)<!-- -->

## Subset of Fitbit data


```r
clean_data_fitbit <- subset(clean_data, Brand == "Fitbit")
```

### Model 4: Linear Regression - Device year as a predictor of Step Count MAPE


```r
lm_year_MAPE_fb <- lm(MAPE ~ device_year_c + Wear_Location, data = clean_data_fitbit)

summary(lm_year_MAPE_fb)
```

```
## 
## Call:
## lm(formula = MAPE ~ device_year_c + Wear_Location, data = clean_data_fitbit)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -20.112  -9.207  -7.137   0.303  90.784 
## 
## Coefficients:
##                        Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             17.4187     3.5858   4.858 1.52e-06 ***
## device_year_c            1.0643     0.9426   1.129 0.259316    
## Wear_LocationThigh      10.2060    20.5674   0.496 0.619921    
## Wear_LocationTorso      -5.6854     3.4972  -1.626 0.104551    
## Wear_LocationUpper Arm -18.4630     8.8576  -2.084 0.037549 *  
## Wear_LocationWaist/Hip -11.3955     2.9608  -3.849 0.000132 ***
## Wear_LocationWrist     -13.4375     3.2362  -4.152 3.77e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 20.4 on 594 degrees of freedom
## Multiple R-squared:  0.04062,	Adjusted R-squared:  0.03093 
## F-statistic: 4.191 on 6 and 594 DF,  p-value: 0.0003859
```

```r
tidy(lm_year_MAPE_fb, conf.int = TRUE)
```

```
## # A tibble: 7 × 7
##   term                   estimate std.error statistic    p.value conf.…¹ conf.…²
##   <chr>                     <dbl>     <dbl>     <dbl>      <dbl>   <dbl>   <dbl>
## 1 (Intercept)               17.4      3.59      4.86  0.00000152  10.4     24.5 
## 2 device_year_c              1.06     0.943     1.13  0.259       -0.787    2.92
## 3 Wear_LocationThigh        10.2     20.6       0.496 0.620      -30.2     50.6 
## 4 Wear_LocationTorso        -5.69     3.50     -1.63  0.105      -12.6      1.18
## 5 Wear_LocationUpper Arm   -18.5      8.86     -2.08  0.0375     -35.9     -1.07
## 6 Wear_LocationWaist/Hip   -11.4      2.96     -3.85  0.000132   -17.2     -5.58
## 7 Wear_LocationWrist       -13.4      3.24     -4.15  0.0000377  -19.8     -7.08
## # … with abbreviated variable names ¹​conf.low, ²​conf.high
```

```r
clean_data_m4 <- augment(lm_year_MAPE_fb, newdata = clean_data_fitbit, interval = "prediction")

scatter_fitted_fb <- ggplot(data = clean_data_m4, aes(x = device_year, y = MAPE)) +
      geom_point(alpha = 0.2) +
      stat_smooth(method = "lm", colour = "gray") +
      theme_bw()

plot(scatter_fitted_fb)
```

```
## `geom_smooth()` using formula = 'y ~ x'
```

![](device_validity_time_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

## Table 2: Creating model table


```r
m1 <- tbl_regression(lm_year_MAPE) 
m2 <- tbl_regression(lm_year_MAPE_loc) 
m3 <- tbl_regression(lmer_year_by_brand_MAPE) 
m4 <- tbl_regression(lm_year_MAPE_fb) 


tbl_2_multi <- tbl_merge(list(m1, m2, m3, m4))

tbl_2_multi %>% as_kable()
```



|**Characteristic**       | **Beta** | **95% CI**  | **p-value** | **Beta** | **95% CI** | **p-value** | **Beta** | **95% CI** | **p-value** | **Beta** | **95% CI** | **p-value** |
|:------------------------|:--------:|:-----------:|:-----------:|:--------:|:----------:|:-----------:|:--------:|:----------:|:-----------:|:--------:|:----------:|:-----------:|
|device_year_c            |   -1.5   | -2.5, -0.54 |    0.002    |  -0.65   | -1.9, 0.61 |     0.3     |   -1.3   |  -39, 37   |    >0.9     |   1.1    | -0.79, 2.9 |     0.3     |
|Wear_Location            |          |             |             |          |            |             |          |            |             |          |            |             |
|LAF                      |          |             |             |    —     |     —      |             |    —     |     —      |             |    —     |     —      |             |
|Thigh                    |          |             |             |    11    |  -27, 49   |     0.6     |    10    |  -27, 48   |     0.6     |    10    |  -30, 51   |     0.6     |
|Torso                    |          |             |             |   -2.7   | -9.0, 3.6  |     0.4     |   -3.2   | -9.5, 3.1  |     0.3     |   -5.7   |  -13, 1.2  |    0.10     |
|Upper Arm                |          |             |             |   -21    | -37, -5.1  |    0.010    |   -19    | -35, -2.5  |    0.023    |   -18    | -36, -1.1  |    0.038    |
|Waist/Hip                |          |             |             |   -12    | -17, -6.7  |   <0.001    |   -12    | -18, -7.1  |   <0.001    |   -11    | -17, -5.6  |   <0.001    |
|Wrist                    |          |             |             |   -13    | -18, -7.1  |   <0.001    |   -13    | -19, -7.3  |   <0.001    |   -13    | -20, -7.1  |   <0.001    |
|Brand                    |          |             |             |          |            |             |          |            |             |          |            |             |
|Apple                    |          |             |             |          |            |             |    —     |     —      |             |          |            |             |
|Fitbit                   |          |             |             |          |            |             |   -3.3   | -196, 190  |    >0.9     |          |            |             |
|Garmin                   |          |             |             |          |            |             |   7.2    | -186, 201  |    >0.9     |          |            |             |
|Misfit                   |          |             |             |          |            |             |    18    | -175, 211  |     0.9     |          |            |             |
|Polar                    |          |             |             |          |            |             |   -2.2   | -196, 191  |    >0.9     |          |            |             |
|Samsung                  |          |             |             |          |            |             |   -3.2   | -208, 202  |    >0.9     |          |            |             |
|Withings                 |          |             |             |          |            |             |    36    | -159, 232  |     0.7     |          |            |             |
|device_year_c * Brand    |          |             |             |          |            |             |          |            |             |          |            |             |
|device_year_c * Fitbit   |          |             |             |          |            |             |   2.2    |  -36, 40   |    >0.9     |          |            |             |
|device_year_c * Garmin   |          |             |             |          |            |             |   -1.0   |  -39, 37   |    >0.9     |          |            |             |
|device_year_c * Misfit   |          |             |             |          |            |             |   -4.1   |  -43, 34   |     0.8     |          |            |             |
|device_year_c * Polar    |          |             |             |          |            |             |   1.3    |  -37, 40   |    >0.9     |          |            |             |
|device_year_c * Samsung  |          |             |             |          |            |             |   0.71   |  -41, 42   |    >0.9     |          |            |             |
|device_year_c * Withings |          |             |             |          |            |             |   -9.5   |  -49, 30   |     0.6     |          |            |             |

