#Day2.R
#Data visualisation and distribution 
#Luanne Thomas
#13 April 2018

#voice notes = Day2 part 1, 2 & 3  

# Load libraries  ---------------------------------------------------------

library(tidyverse)


# Manual Calculations  ----------------------------------------------------

#generate dummy data:
#random data used 

r_dat <- data.frame(dat = rnorm(n = 600, mean = 372, sd = 50), #random normal distribution data, 3 arguments needed 
 
                    sample = "A") 
#data.frame makes a spreedsheet 

#Quick visualisation 

#good to visual data before test 

ggplot(data = r_dat, aes(x = dat)) +
  geom_density()

#geom_density = Computes and draws kernel density estimate,
  #which is a smoothed version of the histogram.
    #This is a useful alternative to the histogram if for continuous data that 
      #comes from an underlying smooth distribution.

#mean 
#sum of all data (points) / number of data (points)

r_dat %>% 
  summarise(r_sum = sum(dat), 
            r_n = n(), 
            r_mean = r_sum/r_n, 
            r_mean_func = mean(dat))

#summarise = pulls out what you want 

#median 

#brute force with base R
r_dat$dat[(length(r_dat$dat)+1)/2]

r_dat$dat[length(r_dat$dat)/2]

#Or tidy automagic way

r_dat %>% 
  summarise(r_median = median(dat))

#when you calculate median, data needs to be ordered

#use tidy 
r_dat %>% 
  arrange(dat) %>% 
  slice(n()/2)

#variance

#sum of 
  #each value minus 
    #the mean 
      #squared 
#Divided by 
  #count of samples minus 1

r_dat %>% 
  mutate(r_error = dat - mean(dat), 
         r_error_square = (r_error* r_error)) %>% 
  summarise(r_square_sum = sum(r_error_square), 
            r_var = r_square_sum/(n()-1), 
            r_var_func = var(dat)) #built in function 

#mutate = new column 
#r-error = anomaly column 

#standard deviation

r_dat %>% 
  summarise(r_var = var(dat),
            r_sd = sqrt(r_var), 
            r_sd_func = sd(dat))

#random data won't match your instructions/ arguments 100%

# Exercise 1 --------------------------------------------------------------

#1st want to summary return 

round(summary(ChickWeight$weight), digits = 1)

#reproduce in tidy data

#do not need to assign an object

ChickWeight %>% 
  summarise(wt_min = min(weight), 
            wt_quart1 = quantile(weight, 0.25), 
            wt_median = median(weight), 
            wt_mean = round(mean(weight), digits = 1), 
            wt_quart3 = round(quantile(weight, 0.75), digits = 1),
            wt_max = max(weight))
                  

# Visualisations ----------------------------------------------------------

#load libraries
#few packages contain most functios necessary

library(tidyverse)
library(ggpubr)
library(RColorBrewer)
library(viridis) 
#suitable for colourblind people

#load sa_time 
sa_time <- read.csv("sa_time.csv") 
#when saving text file, explicitly save file name with .csv

#need to convert from wide to long format

#edit data

#human column 

sa_time <- sa_time %>% 
  mutate(human = seq(1, n(), 1), 
         geo = c(rep(c("Cape Town", "George", "PE"), times = 6),
                 rep("Joburg", 2))) #seq(1 = starting at row 1, up to n rows)

#long data 
sa_long <- sa_time %>% 
  gather(key = "time_type", value = "minutes", -human, -geo)

#minus function excludes coloumn 

#key = name of the coloumn that contains the variables 

# Qualitative -------------------------------------------------------------

sa_count <- sa_long %>%
  count(time_type) %>% 
  mutate(prop = n / sum(n)) #****

#what is the num of count for each type, 

#Stacked bar graph

ggplot(data = sa_count, aes(x = "", y = n, fill = time_type)) +
  geom_bar(width = 1, stat = "identity") +
  labs(title = "Stacked bar graph", subtitle = "cumulative sum",
       x = NULL, y = "Count") +
  theme_minimal()

# If you want the heights of the bars to represent 
  # values in the data, use stat="identity" and map
    #a value to the y aesthetic.

#stacked proportion bar graph 

ggplot(data = sa_count, aes(x = "", y = prop, fill = time_type)) +
  geom_bar(width = 1, stat = "identity") +
  scale_y_continuous(breaks = c(0.00, 0.33, 0.66, 1.00)) +
  labs(title = "Stacked bar graph", subtitle = "relative proportions",
       x = NULL, y = "Proportion") +
  theme_minimal()

#a pie chart 

ggplot(data = sa_count, aes(x = "", y = n, fill = time_type)) +
  geom_bar(width = 1, stat = "identity") +
  labs(title = "pie Chart", subtitle = "but why though", 
       x = NULL, y= NULL) +
  coord_polar("y", start = 0) +
  theme_minimal()


# Continuous data ------------------------------------------------------------

#Histogram 

#only has an x axis, y = count 

ggplot(data = sa_long, aes(x = minutes)) +
  geom_histogram()

#omit data (should not do this unless explained why)

sa_clean <- sa_long %>% 
  filter(minutes < 300)

#Try again (facet histogram)
ggplot(data = sa_clean, aes(x = minutes)) +
  geom_histogram(aes(fill = time_type), position = "dodge") +
  facet_wrap(~time_type, ncol = 1, scales = "free_x")

#relative proportion histogram 

ggplot(data = sa_clean, aes(x = minutes)) +
  geom_histogram(aes(y = ..density.., fill = time_type),
                 position = "dodge", bindwidth = 1) +
  facet_wrap(~time_type, ncol = 1, scales = "free_x")


#boxplots
ggplot(data = sa_clean, aes(x = time_type, y = minutes)) +
  geom_boxplot(aes(fill = time_type))

#anatomy of boxplot 
#middle line = median 
#end and start of rectangle = quantile 
#interquantile range inside rectangle =measures of central tendency 
#interquantile range *1.5 = tails 
#tail shows data within 1.5 if more than 1.5 considered an outliner
#longer tail = data more variable

#notched boxplots

ggplot(data = sa_clean, aes(x = time_type, y = minutes)) +
  geom_boxplot(aes(fill = time_type), notch = TRUE)

#Notch is symmetrical to top and bottom 
#areas that overlap within the notch is not signficant 
#boxplot to test data

#calculte summary stats for plotting ovr boxplots

sa_summary_stats <- sa_clean %>%
  group_by(time_type) %>% 
  summarise(time_type_mean = mean(minutes))

ggplot(data = sa_clean, aes(x = time_type, y = minutes)) +
  geom_boxplot(aes(fill = time_type), notch = TRUE) +
  geom_point(data = sa_summary_stats, size = 6, shape = 18,
             aes(y = time_type_mean), colour = "goldenrod") +
  labs(x = "Time Type", y = "Minutes")


# Relationships -----------------------------------------------------------

#A basic scatterplot

ggplot(data = sa_time,aes(y = now_now, x = just_now)) +
  geom_point() +
  coord_equal(xlim = c(0, 60), ylim = c(0, 60)) +
  labs(x = "Just Now (minutes)", y = "Now Now (minutes)")

#each dots represents one human

#adding trend lines 

ggplot(data = sa_time,aes(y = now_now, x = just_now)) +
  geom_point(aes(colour = geo)) +
  geom_smooth(aes(colour = geo), method = "lm") +
  coord_equal(xlim = c(0, 60), ylim = c(0, 60)) +
  labs(x = "Just Now (minutes)", y = "Now Now (minutes)")

#lm = linear model
#grey shade in graph shows significant relationships 

