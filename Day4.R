#Day4.R
#ANOVA 
#Luanne Thomas 
#19 April 2018


# Remember t-test ------------------------------------------------------------------------

# Load libraries ----------------------------------------------------------

library(tidyverse)

chicks <- as_tibble(ChickWeight)

chicks_sub <- chicks %>% 
  filter(Diet %in% c(1, 2), Time == 21)

#t-test

t.test(weight ~ Diet, data = chicks_sub)

#we do not reject null hypothesis 


# 1-way ANOVA -------------------------------------------------------------------

#Research Question: is there a difference in chicken mass attained after
#21 days after the chickens having been fed four different diets?

#Null hypothesis: No difference in chicken final mass
#based on different diet after 21 days. 

chicks_21 <- chicks %>% 
  filter(Time == 21)

chicks.aov1 <- aov(weight ~ Diet, data = chicks_21)

summary(chicks.aov1)

#visualisatiion

ggplot(chicks_21, aes(x = Diet, y = weight)) +
  geom_boxplot(aes(fill = Diet), notch = TRUE)

#Output
#we reject the null hyothesis, and 
#therefore accept the alternative hypothesis (different diets haS an effect on weight)


# Tukey HSD test --------------------------------------------------------------

TukeyHSD(chicks.aov1)

#lower confidence interval positive = significant difference

#visualisation

#Boxplot 

ggplot(chicks_21, aes(x = Diet, y = weight, fill = Diet)) +
  geom_boxplot(notch = TRUE, colour = "grey50") +
  geom_segment(aes(x = Diet, xend = Diet, y = weight, yend = weight+2))

#segments showing confidence intervals

chicks_tukey <- as.data.frame(TukeyHSD(aov(weight ~ Diet, data = chicks_21))$Diet)

chicks_tukey$pairs <- as.factor(row.names(chicks_tukey))

?TukeyHSD

plot(TukeyHSD(chicks.aov1, "Diet"))

#or

plot(TukeyHSD(aov(weight ~ Diet, data = chicks_21)))


# Multiple factor ANOVA --------------------------------------------------------

#H0: There is no change in chicken mass (kg) from day 0 to day 21

#Create a dataframe with just those days
chicks_0_21 <- ChickWeight %>% 
  filter(Time %in% c(0, 2, 21))

#visualise the data
ggplot(data = chicks_0_21, aes(x = as.factor(Time), y = weight)) +
  geom_boxplot(notch = TRUE, aes(fill = as.factor(Time)))

#Run ANOVA

summary(aov(weight ~ as.factor(Time), data = chicks_0_21))

#Perform a tukey post-hoc test

TukeyHSD(aov(weight ~ as.factor(Time), data = chicks_0_21))

plot(TukeyHSD(aov(weight ~ as.factor(Time), data = chicks_0_21)))

#as.facor = convert the data type of a variable to 
  #a factor/categorical variable. Typically when you want 
    #to convert a numeric/integer/character variable into a
      #categorical variable we use as.factor.

#Look only at day 0 and 21 for both time and diet

summary(aov(weight ~ Diet + as.factor(Time), data = filter(chicks, Time %in% c(0, 21))))

#simply look at all of the time
#..which is not the hypothesis

summary(aov(weight~Diet + as.factor(Time), data = chicks))

#note the increase in degrees of freedom for the time factor
#but no increase for d.f for diet

#Now to look at interactions BETWEEN factors

summary(aov(weight~Diet * as.factor(Time), data = filter(chicks, Time %in% c(0, 21))))

#Let'slook at the tukey results
TukeyHSD(aov(weight~Diet * as.factor(Time), data = filter(chicks, Time %in% c(0, 21))))

#Create a line graph to help explain this concept
#first create a mean value by Time and diet

chicks_mean <- chicks %>% 
  group_by(Diet, Time) %>% 
  summarise(wt.mean = mean(weight, na.rm = TRUE))

#visualisation
ggplot(data = chicks_mean, aes(x = Time , y = wt.mean, colour = Diet)) +
  geom_line(size = 2) +
  geom_point(shape = 15, size = 4)


# Non-parametric tests ----------------------------------------------------

#but what if
#we don't have normal data?

#for a t-tes we rather use Wilcox rank sum test

wilcox.test() #and then one fills this in the same as for a t.test()

#and now for the Kruskall-Wallis test

kruskal.test(weight~Diet, data = chicks_0_21) #same as Anova

#Load this for a non-parametric post-hoc test

library(pgirmess)

kruskalmc(weight~Diet, data = chicks_0_21)


