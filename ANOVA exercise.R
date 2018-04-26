#ANOVA exercise.R
#attempt the ANOVA exercises 
#Luanne Thomas
#26 April 2018

# Load libraries ----------------------------------------------------------

library(tidyverse)
library(ggpubr)

# Exercise 1 --------------------------------------------------------------

#Null hypothesis: feed type has no effect on the mass of the pigs 

# enter the mass at the end of the experiment
feed_1 <- c(60.8, 57.0, 65.0, 58.6, 61.7)
feed_2 <- c(68.7, 67.7, 74.0, 66.3, 69.8)
feed_3 <- c(102.6, 102.1, 100.2, 96.5)
feed_4 <- c(87.9, 84.2, 83.1, 85.7, 90.3)

# make a dataframe
bacon <- as.tibble(data.frame(feed = c(rep("Feed 1", length(feed_1)),
    rep("Feed 2", length(feed_2)),
    rep("Feed 3", length(feed_3)),
    rep("Feed 4", length(feed_4))),
  mass = c(feed_1, feed_2, feed_3, feed_4)))

#One-way 

bacon.aov1 <- aov(mass ~ feed, data = bacon)

summary(bacon.aov1)

#Visualisation
ggplot(bacon, aes(x = feed, y = mass)) +
  geom_boxplot(aes(fill = feed), notch = TRUE)

#Tukey
TukeyHSD(bacon.aov1)

plot(TukeyHSD(bacon.aov1, "feed"))

#Feed type has an effect on mass (P < 0.05)
#null hypothesis refuted

# Exercise 2 --------------------------------------------------------------

#H0: There is no difference in the length of teeth in relation to the supplement
#H0: There is no difference in supplement in relation to the dose
#H1: There is a difference in the length of teeth in relation to the dose and supplement 

#load data
teeth <- datasets::ToothGrowth

#Visualisation
ggboxplot(teeth, x = "dose", y = "len", color = "supp",
          palette = c("#00AFBB", "#E7B800"))

#Two-way test
teeth.aov <- aov(len ~ supp + dose, data = teeth)

summary(teeth.aov)

#can conclude that both supp and dose are significant. 
#dose is the most significant factor variable.
#Thus we reject null hypothesis and accept alternative hypothesis

# Two-way ANOVA with interaction effect (for fun:))
teeth.aov2 <- aov(len ~ supp + dose + supp:dose, data = teeth)

summary(teeth.aov2)

#results show that the two main effects (supp and dose) are significant
#as well as their interaction.



