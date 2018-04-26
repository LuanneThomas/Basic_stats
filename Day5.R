#Day5.R
#ANOVA examples
#Luanne Thomas
#20 April 2018

# Load Libraries ----------------------------------------------------------

library(tidyverse)
# library(Rmisc) # Unfortunately this overrides many dplyr functions
library(ggpubr)

# Load data  --------------------------------------------------------------

snakes <- read_csv("snakes.csv") %>% 
  mutate(day = as.factor(day))

#mutate = change day into a factor of day

#or  

snakes$day <- as.factor(snakes$day) 

# Summarise the data -----------------------------------------------------

snakes_sum <- snakes %>% 
  group_by(day, snake) %>% 
  summarise(mean_opening = mean(openings), 
            sd_openings = sd(openings))

#NaN for sd (only have one measurement of a snake per day)
#therefore

snakes.summary <- snakes %>% 
  group_by(day) %>% 
  summarise(mean_openings = mean(openings),
            sd_openings = sd(openings))

#another way

snakes.summary2 <- Rmisc::summarySE(data = snakes,
                             measurevar = "openings",
                             groupvars = c("day"))

#Visual data

ggplot(data = snakes, aes(x = day, y = openings)) +
  geom_segment(data = snakes.summary2, 
               aes(x = day, xend = day, y = openings - ci,
                   yend = openings + ci, colour = day),
               size = 2.0, linetype = "solid", show.legend = F) +
  geom_boxplot(aes(fill = day), alpha = 0.6, show.legend = F) + 
  geom_jitter(width = 0.05)

#The jitter geom is a convenient shortcut for 
  #geom_point(position = "jitter"). It adds a small amount of
    #random variation to the location of each point, and 
      #is a useful way of handling overplotting caused by 
        #discreteness in smaller datasets.

# Formulate hypothesis ----------------------------------------------------

#H0: There is no difference in the number of openings from day to day
#H1: There is a difference in the number of openings from day to day 

#But wait, we have two factors, so we need another null hypothesis

#H0: There is no difference between snakes with
  #respect to the number of openings at which they habituate.
#H0: There is no difference between days in terms of the
  #number of openings at which the snakes habituate.


# Test the hypothesis  ----------------------------------------------------

#Test just the days hypothesis
snakes.day.aov <- aov(openings ~ day, data = snakes)

summary(snakes.day.aov)

#Test both hypothesis

snakes.all.aov <- aov(openings ~ day + snake, data = snakes)

summary(snakes.all.aov)


# Test assumptions --------------------------------------------------------

#Visualise normality of results

#residuals = unexplained variation

snakes.res <- residuals(snakes.all.aov)

hist(snakes.res)

#Visualise homoscedaticity of results 

plot(fitted(snakes.all.aov), residuals(snakes.all.aov))

#Check Tukey results

snakes.tukey <- TukeyHSD(snakes.all.aov, which = "day")

#visualise tukey
plot(snakes.tukey)

ggplot(data = snakes, aes(x = as.numeric(day), 
                          y = openings, 
                          colour = snake)) +
         geom_line(size = 3) +
         geom_point(size = 4)


# Exercise ----------------------------------------------------------------

#get moth data from Github
#Run a two-way ANOVA 

#Load data

moths <- read_csv("moth_traps.csv.txt") %>% 
  gather(key = "trap", value = "count", -Location)

#Formulate hypothesis

#H0: There is no difference between counts in relation to the location

#ANOVA Test
moth.aov <- aov(count ~ trap*Location, data = moths)
summary(moth.aov)

#Test assumptions
moth.res <- residuals(moth.aov)

hist(moth.res)

plot(fitted(moth.aov), residuals(moth.aov))

#Tukey test

moth.tukey <- TukeyHSD(moth.aov, which = "trap")

plot(moth.tukey)

#visualisation

moth.summary <- Rmisc::summarySE(data = moths,
                             measurevar = "count",
                             groupvars = c("trap"))

ggplot(data = moths, aes(x = trap, y = count)) +
  geom_segment(data = moth.summary, 
               aes(x = trap, xend = trap, y = count - ci,
                   yend = count + ci, colour = trap),
               size = 2.0, linetype = "solid", show.legend = F) +
  geom_boxplot(aes(fill = trap), alpha = 0.6, show.legend = F) + 
  geom_jitter(width = 0.05)

#playing around

ggplot(data = moths, aes(x = Location, 
                          y = count, 
                          colour = trap)) +
  geom_bar(aes(fill = trap), stat = "identity")

plot1 <- ggplot(moths, aes(x = Location, y = count)) +
  geom_boxplot() +
  geom_jitter(width = 0.05, shape = 21)

plot2 <- ggplot(moths, aes(x = trap, y = count)) +
  geom_boxplot() +
  geom_jitter(width = 0.05, shape = 21 )

plot3 <- ggplot(moths, aes(x = Location, y = count)) +
  geom_boxplot(aes(fill = trap))

ggarrange(plot1, plot2, plot3)


# Linear Regression -------------------------------------------------------

#For the explanation of this stats analysis 
#we are going to use eruption data from Ol' Faithful

#Look at the top half of data
head(faithful)

#plot a quick scatterplot

ggplot(data = faithful, aes(x = waiting, y = eruptions)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, colour = "Hotpink")

# formulate hypothesis ----------------------------------------------------

#H0: wiing time does no influence the duration of an eruption
#H1: waiting time does influence the duration of an eruption

# Test hypothesis ---------------------------------------------------------

faithful.lm <- lm(eruptions~ waiting, data = faithful)

summary(faithful.lm)

#Visualisation

slope <- round(faithful.lm$coef[2], 3)

p.val = 0.001

r2 <- round(summary(faithful.lm)$r.squared, 3)

ggplot(data = faithful, aes(x = waiting, y = eruptions)) +
  geom_point() +
  annotate("text", x = 45, y = 5, label = paste0("slope == ", slope, "~(min/min)"), parse = TRUE, hjust = 0) +
  annotate("text", x = 45, y = 4.75, label = paste0("italic(p) < ", p.val), parse = TRUE, hjust = 0) +
  annotate("text", x = 45, y = 4.5, label = paste0("italic(r)^2 == ", r2), parse = TRUE, hjust = 0) +
  stat_smooth(method = "lm", colour = "salmon") +
  labs(title = "Old Faithful eruption data",
       subtitle = "Linear regression",
       x = "Waiting time (minutes)",
       y = "Eruption duration (minutes)")

# Correlations ------------------------------------------------------------

#load libraries
library(corrplot)
library(ggplot2)

ecklonia <- read.csv("data/ecklonia.csv")

# Formulate the hypothesis ------------------------------------------------

#H0: There is no relationship between fond length and frond mass
  #for the kelp Ecklonia maxima
#H1: There is relationship between fond length and frond mass
  #for the kelp Ecklonia maxima

# Test hypothesis ---------------------------------------------------------

cor.test(ecklonia$frond_length, ecklonia$frond_mass)

#Visualise the data 
ggplot(data = ecklonia, aes(x = frond_length, y = frond_mass)) +
  geom_point()

#run hecka tests at once
ecklonia_sub <- ecklonia %>% 
  select(stipe_length:epiphyte_length)

ecklonia_cor <- cor(ecklonia_sub)

ecklonia_cor

#just cor = all possible correlations

# Spearman rank test ------------------------------------------------------

ecklonia$length <- as.numeric(cut((ecklonia$stipe_length+ecklonia$frond_length), 3))

#Then run a Spearman test

cor.test(ecklonia$length, ecklonia$primary_blade_length, method = "spearman")

# Kendall rank ------------------------------------------------------------

cor.test(ecklonia$primary_blade_length, ecklonia$primary_blade_width, method = "kendall")

# Visualise all the things ------------------------------------------------

ecklonia_pearson <- cor(ecklonia_sub)

corrplot(ecklonia_pearson, method = "circle")

# ggplot heatmap ----------------------------------------------------------

library(reshape2)

melted_ecklonia <- melt(ecklonia_pearson)

ggplot(data = melted_ecklonia, aes(x= Var1, y=Var2, fill=value)) + 
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab")



