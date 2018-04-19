#Day3.R
#Distributions and T-tests 
#Luanne Thomas 
#17 April 2018 

#Voicenote = Day3. part 1 & 2

# Discrete distribution ---------------------------------------------------

#Count integer kind of data

#Bernoulli distribution

  #either one or the other, absent/present, yes/no, true/false 

  #only a pair of options available 

  #example coin toss

  #above has 50% probability , if not biased and large enough sample is taken

  #true approxiamation = increase sample size. 

#Binomial distribution 

  #large scale of Bernoulli distribution

#Negative binomial distribution

  #calculate probability of a failure occuring after a positive 

#Geometric distribution

  #probability of a tagged foot of a starfish , other feet move in relation to the tagged foot

#Poisson distribution 

  #fixed interval of time that you count a no of occurence of an event 

# Continuous distriution --------------------------------------------------

#Things that can take on any number of infinite steps of real data between 2 points 

#mostly used by Biologists

#Normal distribution

  #Gaussian data/ bell-shaped data

  #measurement of the average height 

  #follows a bell shaped curve 

  #increase in sample size = more bell shaped 

  #independent of each other

  #Mean and sd generally make sense

  #normal data= mean will be informative, same as median 

  #data will be clustered around the mean (central limit theorem)

#Uniform distribution

  #take 2 value with aeven probability of any vaule occuring in between these values 

  #Random process

  #experimental ecology and quantitative ecology

#Student T distribution 

  #sample that is comprised of many means 

#Chi-squared distribution

  #useful for stats properties

#Exponential distribution

  #time between events (Poisson)

  #rate = rate of decay

#F distribution 

  #compare a bunch of variance 

#Gamma dstribution 

  #skewed to right 

#beta distribution

  #dont often come across

#Paranormal distribution 

#finding your data distribution 

library(fitdistrplus)
library(logspline)

#Example from book 
y <- c(37.50,46.79,48.30,46.04,43.40,39.25,38.49,49.51,40.38,36.98,40.00,
       38.49,37.74,47.92,44.53,44.91,44.91,40.00,41.51,47.92,36.98,43.40,
       42.26,41.89,38.87,43.02,39.25,40.38,42.64,36.98,44.15,44.91,43.40,
       49.81,38.87,40.00,52.45,53.13,47.92,52.45,44.91,29.54,27.13,35.60,
       45.34,43.37,54.15,42.77,42.88,44.26,27.14,39.31,24.80,16.62,30.30,
       36.39,28.60,28.53,35.84,31.10,34.55,52.65,48.81,43.42,52.49,38.00,
       38.65,34.54,37.70,38.11,43.05,29.95,32.48,24.63,35.33,41.34)

par(mfrow = c(2, 2))
plot(x = c(1:length(y)), y = y)
hist(y)
descdist(y, discrete = FALSE, boot = 100) 

#Cullen and Frey graph = plots you data relative to skewness of theoretical disribution 

# Do it yourself (Cullen & Frey graph) ------------------------------------

#load libraries 

library(fitdistrplus)
library(logspline)

#Generate data 

r_norm <- rnorm(n = 1000, mean = 13, sd = 1)

hist(r_norm) #generates histogram of all the data

descdist(r_norm, discrete = FALSE, boot = 100)

#Uniform data  

y <- runif(100)
par(mfrow = c(2, 2))
plot(x = c(1:100), y = y)
hist(y)
descdist(y, discrete = FALSE)


# T-Test ------------------------------------------------------------------

#load libraries

library(tidyverse)
library(plotly)

#t-test vs ANOVA is the no. of variables you compare

#t-test = two things 
#ANOVA = more than two things

#can compare yoursamples against a known data set

#Generate a random data set

r_dat <- data.frame(dat = c(rnorm(n = 1000, mean = 10, sd = 3),
                            rnorm(n = 1000, mean = 8, sd = 2)),
                    sample = c(rep("A", 1000), rep("B", 1000)))

# Check assumptions -------------------------------------------------------

#Normality
#For this we may use the Sharpiro-Wilk test

shapiro.test(r_dat$dat)
shapiro.test(r_dat$dat)[1] #Only output the W value
shapiro.test(r_dat$dat)[2] #only output the p-value

#hypothesis associated is that the data 
#distribution is not significantly diff from norm

#But that is testing all of the data together 
#we must be a bit more clever about how we make this test 

r_dat %>% 
  group_by(sample) %>% #test for group A & B 
  summarise(r_norm_dist = as.numeric(shapiro.test(dat)[2]))

#shapiro-wilk test above = 2 is for p- value to be returned 

#Remember, the data are normal when p > 0.05
#non-normal when p <= 0.05


# Check Homoscedasticity --------------------------------------------------

#there are many ways to check for Homoscedasticity
#which is the similarity of variance between sample sets
#for now we will simply say that this assumptions is met when  
#the variance of the samples are not more than 2 - 4 times greater 
#than one another

#check everything at once
#WRONG

var(r_dat$dat)

#Or do it the tidy 

r_dat %>% 
  group_by(sample) %>% 
  summarise(r_norm_dist = as.numeric(shapiro.test(dat)[2]),
            r_norm_var = var(dat))


# One sample T-test -------------------------------------------------------

r_one <- data.frame(dat = rnorm(n = 20, mean = 20, sd = 5),
                    sample = "A")

#test normality of distribution 

shapiro.test(r_one$dat)

#Perhaps a visual?

library(ggpubr)
ggboxplot(r_one$dat,
          ylab = FALSE, xlab = FALSE,
          ggtheme = theme_minimal())

#Run the test

t.test(r_one$dat, mu = 20)

#df = sample size - 1

#Run a test we know will produce a significan result 

t.test(r_one$dat, mu = 30)

#visual of what we done above 

ggplot(data = r_one, aes(y = dat, x = sample, y)) +
  geom_boxplot(fill = "lightsalmon") +
  geom_hline(yintercept = 20, colour = "blue", 
             size = 3, linetype = "dashed") +
  geom_hline(yintercept = 30, colour = "red", 
             size = 3, linetype = "dashed") +
    labs(y = "Value", x = NULL) +
  coord_flip()


# Pick a side -------------------------------------------------------------

#are these data SMALLER/LESS tha the population mean 

t.test(r_one$dat, mu = 20, alternative = "less")

#Or greater

t.test(r_one$dat, mu = 20, alternative = "greater")

#But what about for the larger population mean?
#are the samples less than he population of 30?

t.test(r_one$dat, mu = 30, alternative = "less")

#what about greater than?

t.test(r_one$dat, mu = 30, alternative = "greater")


# Two-sample T-test -------------------------------------------------------

r_two <- data.frame(dat = c(rnorm(n = 20, mean = 4, sd = 1),
                            rnorm(n = 20, mean = 5, sd = 1)),
                    sample = c(rep("A", 20), rep("B", 20)))

#Run a default/basic test

t.test(dat ~ sample, data = r_two, var.equal = TRUE)


# Pick a side -------------------------------------------------------------

#is A less than B?
t.test(dat ~ sample, data = r_two, var.equal = TRUE, alternative = "less")

#is A greater than B
t.test(dat ~ sample, data = r_two, var.equal = TRUE, alternative = "greater")


# A t-test workflow -------------------------------------------------------

#Load data
ecklonia <- read.csv("data/ecklonia.csv") %>% 
  gather(key = "variable", value = "value", -species, -site, -ID)

#Visual data

ggplot(data = ecklonia, aes(x = variable, y = value, fill = site)) +
  geom_boxplot() +
  coord_flip()

#filter data
ecklonia_sub <- ecklonia %>% 
  filter(variable == "stipe_mass")

#visualise data 
ggplot(data = ecklonia_sub, aes(x = variable, y = value, fill = site)) +
  geom_boxplot() +
  coord_flip() +
  labs(y = "stipe mass (kg)", x = "") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

#Check assumptions

ecklonia_sub %>% 
  group_by(site) %>% 
  summarise(stipe_mass_var = var(value)[1],
            stipe_mass_norm = as.numeric(shapiro.test(value)[2]))

#running analysis

t.test(value ~ site, data = ecklonia_sub, var.equal = TRUE, alternative = "greater")

# dataframe output

compare_means(value ~ site, data = ecklonia_sub, method = "t.test", var.equal = TRUE, alternative = "greater")


