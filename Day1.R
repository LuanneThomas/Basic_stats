#Day1.R
#The first day of the stats class
#Purpose: to practice some of the concepts that we will encounter 
#Luanne Thomas
#12 April 2018

# Load Library ------------------------------------------------------------

library(dplyr) #tidyverse did not want to successfully load
library(ggplot2)

# Types of Data: ----------------------------------------------------------

# Nominal (discrete) data: [integer] ----------------------------------------------------------

integer_r <- as.integer(seq(5, 24, by = 2)) #generate some integer data 

integer_r #view/ list values in object 

summary(integer_r) #Look at a brief sumary 


# Continuous Data  --------------------------------------------------------

numeric_r <- seq(23, 43, length.out = 10) #Generate a seq of numeric data 

#length.out = desired length of a seq 


# Dates -------------------------------------------------------------------

as.Date("2005-12-31") - as.Date("2005-12-12") #arithmetic with dates
# above gives difference of days between dates 

#or 

date_r <- seq(as.Date("2005-12-16"), as.Date("2005-12-25"), by = "day")
#above seq of days between two dates 

date_r #view dates

summary(date_r)


# DataFrame ---------------------------------------------------------------

#if all the same length, can be created into a data frame

df_r <- data.frame(integers = integer_r,
                  numeric = numeric_r,
                  dates = date_r)

#ensure the objects have the same amount of values for the dataframe [1:10]

#then upgrade it to a tibble 

df_r <- as_tibble(df_r)

summary(df_r)


# Categories --------------------------------------------------------------

#Electronics
elec_r <- as.factor((c("laptops", "desktops", "cell phones")))

#People
people_r <- as.factor(c("funny", "beautiful", "beanies"))

#Colours 

colour_r <- as.factor(c("red", "blue"))

#summary tells you the amount of categories and levels 


# Ordinal data (ranks)------------------------------------------------------------

#qualitative data wih order

colour_qual <- ordered(c("blue", "green","yellow", "orange", "red"), 
                          levels = c("blue", "green","yellow",
                                     "orange", "red"))


# Binary data -------------------------------------------------------------

#generally represented as: True of False

binary_r <- c(TRUE, FALSE, TRUE, TRUE) #count number of true and false

summary(binary_r)


# Characters --------------------------------------------------------------

sites_r <- c("Yztervarkpunt", "Bettys Bay", 
             "Gansbaai", "Seapoint")


# Missing values ----------------------------------------------------------

#seen as NA

chicks_nest <- c(3, 2, 0, 10, 5, 6, 8, 2, 4, NA) #NA = nest not sampled 

summary(chicks_nest)

#NA affects mean and standard deviation 
mean(chicks_nest) 

sd(chicks_nest)


# View data ---------------------------------------------------------------

#simply type name of dataframe
ChickWeight

#summary view

summary(ChickWeight)

#appear in Env. pane

chicks <- ChickWeight

#view first few rows

head(chicks)

#view bottom values of data

tail(chicks)

#Look at certain rows and columns 

chicks[1:3]

chicks[c(1, 54, 61, 12), 2]


# Descriptive statistics --------------------------------------------------

#first create dataframe

chicks <- as_tibble(ChickWeight)

#count the data 
chicks %>% 
  summarise(chicken_count= n())

#number of rows

nrow(chicks)


# Measures of central tendency  -------------------------------------------

#Calculate mean weight
chicks %>% 
  summarise(mean_wt = mean(weight))

#be more specific 

central_chicks <- chicks %>% 
  filter(Time == 21) %>%
  group_by(Diet) %>% 
  summarise(mean_wt = mean(weight), 
            median_wt = median(weight))

#== equivalent to 

#Visualise the density of the data

ggplot(data = filter(chicks, Time == 21),
       aes(x = weight, fill = Diet)) + #y = count
  geom_density(alpha = 0.2)
  

# Skewness ----------------------------------------------------------------

#Calculate the numeric value 
#Load library 

library(e1071)

#comparing diff in mean and median against skewness 

chicks %>% 
  filter(Time == 21) %>%
  group_by(Diet) %>% 
  summarise(mean_wt = mean(weight), 
            median_wt = median(weight), 
            skew_wt = skewness(weight))

#median is middle of our data
#mean core of data 

#negative skew to the left
#positive skew to the right 


# Kurtosis ----------------------------------------------------------------

#normal distribution = bell shaped curve

#Calulate the kurtosis of the tails of a distribution

chicks %>% 
  filter(Time == 21) %>%
  group_by(Diet) %>% 
  summarise(mean_wt = mean(weight), 
            median_wt = median(weight), 
            skew_wt = skewness(weight), 
            kurtosis_wt = kurtosis(weight))


# Measure of Variability  ----------------------------------------------------------------

#summary of many different statistical properties

wt_summary <- chicks %>% 
  filter(Time == 21) %>% 
  group_by(Diet) %>% 
  summarise(wt_mean = mean(weight),
            wt_median = median(weight),
            wt_quart2 = quantile(weight, p = 0.5),
            wt_var = var(weight),
            wt_sd = sd(weight),
            wt_min = min(weight), 
            wt_quart1 = quantile(weight, p = 0.25), 
            wt_quart3 = quantile(weight, p = 0.75))

#sd = var^2





