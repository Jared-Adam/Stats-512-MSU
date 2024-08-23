# This is for the review from chapter 1
# data and packages ####
library(readr)
install.packages('remotes')
library(remotes)
remotes::install_github('greenwood-stat/catstats2')
library(catstats2)
library(mosaic)
library(tidyverse)



# data 
treadmill <- read_csv('http://www.math.montana.edu/courses/s217/documents/treadmill.csv')
treadmill

data(treadmill)

# explore with this package ####

data <- treadmill

# summary stats
favstats(data$RunTime)

#visuals
hist(data$RunTime)
hist(data$RunTime, labels = T)

# outlier?
# the standard for base R boxplot is that an outlier is 1.5 times greater than the IQR (inter-quartile range)
# IQR = Q3 - Q1
IQR <- 11.27 - 9.78
IQR

11.27 + 1.5*IQR
# it appears that the max of 14.03 is an outlier

boxplot(data$RunTime,
        ylab  = '1.5 Mile Run Time (minutes)',
        main = 'Boxplot of run times of n=31')

# doing some ggplot stuff ####
library(ggplot2)
theme_set(theme_bw())
# this guy likes bw, I am not setting that theme and will add it as I see fit

ggplot(data, aes(x = RunTime))+
  geom_histogram(fill = 'grey', bins = 8)+
  geom_boxplot(color = 'tomato')+
  geom_rug(color = 'skyblue', sides = 'b', aes(y = 0), position = 'jitter')+ # does jitter do anything helpful here? 
  labs(title = 'hist w boxplot and /n rug of run times',
       x = '1.5 mile run time (minutes)',
       y = 'frequency')+
  theme_light()

# working example ####

data(full100results)
f100results <- full100results %>% 
  filter(Division == 'Women') %>% 
  mutate(
    lane = factor(lane), 
    country = factor(country),
    athlete = factor(athlete),
    meet = factor(meet),
    event = factor(event),
    Division = factor(Division)
  )

dim(f100results)
f100resultsR <- f100results %>% 
  drop_na(mark1, reactiontime)
dim(f100resultsR)

plot_fs <- f100resultsR %>% 
  ggplot(aes(reactiontime, mark1))+
  geom_point()+
  geom_smooth(method = 'lm')+
  geom_smooth(se = F, color = 'red', lty = 2)+
  theme_bw()

plot_fv <- f100resultsR %>% 
  ggplot(aes(lane, mark1))+
  geom_violin()+
  geom_point(alpha = 0.2)+
  theme_bw()
plot_fv


library(yarrr)
pirateplot(mark1 ~ lane, data = f100resultsR,
           inf.method = 'ci', #frequentist 95% CI
           inf.disp = 'line', #error bars
           main = 'plot of 100 m times for female competitors by lane',
           xlab = 'lane',
           ylab = '100 m time (sec)')
# this is fine, but idk how much I like it

p_es <- enhanced_stripchart(mark1 ~ lane, data = f100resultsR) +
  labs( title = 'plot of 1-- m times \n female competitors by lane',
        x = 'lane',
        y = '100 m time (sec)')
p_es

f100resultsR %>% 
  ggplot(aes(reactiontime, mark1))+
  geom_point(alpha = 0.3)+
  geom_smooth(method = 'lm', aes(color = lane))+
  geom_smooth(se = F, color = 'red', lty = 2)+
  theme_bw()+
  facet_wrap(~lane, nrow = 3)+
  scale_color_viridis_d(end = 0.8, option ="A")


# a nice way to combine plots into one window

library(patchwork)

plot_fs + p_es +
  plot_annotation(tag_levels = 'a')


# section 1.8 on reproducibility ####

data(cardist_2014)

cardist_2014 <- cardist_2014 %>% 
  mutate(Condition = factor(Condition)) 

plot_all <- enhanced_stripchart(Distance ~ Condition, data = cardist_2014) + 
  labs(title = "(a) Plot of all groups and observations")
set.seed(50311)

card_red <- cardist_2014 %>% 
  dplyr::select(Distance, Condition) %>% 
  filter(Condition %in% c('commute', 'casual')) %>% 
  group_by(Condition) %>% 
  slice_sample(n = 15) %>%  # random sample of 15 from each level of the group_by()
  ungroup()

plot_sample <- enhanced_stripchart(Distance ~ Condition, data = card_red) +
  labs(title = '(b) Plot of random sample of two groups')
plot_all/plot_sample

 # lm time 
lm_red <- lm(Distance ~ Condition, data = card_red)
summary(lm_red)

# looking at common mean
lm_commonmean <- lm(Distance ~ 1, data = card_red)
summary(lm_commonmean)
favstats(Distance ~ 1, data = card_red)

# 
# the citation paper ####
data(cites)
set.seed(406)

cites_larger <- cites %>% 
  group_by(journal_title) %>% 
  mutate(totalcountsbyjournal = n()) %>% # adding count column
  dplyr::filter(totalcountsbyjournal > 10)

cites_strat <- cites_larger %>% 
  slice_sample(prop = 0.5) %>%  #take 50% of the articles
  ungroup() %>% 
  dplyr::select(-totalcountsbyjournal)

cites_strat <- cites_strat %>% 
  mutate(onacad = factor(onacad),
         onacad = fct_recode(onacad,
                             'NotOnAcad' = '0', # was binary, making it categorical
                             'OnAcad' = '1'),
         online = factor(online),
         online = fct_recode(online,
                             'NotOnline' = '0',
                             'Online' = '1'))

unique(cites_strat$title)

cites_strat <- cites_strat %>% mutate(
  journal_title = factor(journal_title),
  divisions = factor(divisions),
  yearssincepub = 2014 - year) %>%
  rename( ChemSci = 'Div. Chemical Sciences',
          EnvSci = 'Div. Environmental Sciences',
          BioSci = 'Div. Biological Sciences',
          Eng = 'Div. Engineering',
          MedHealthSci = 'Div. Medical and Health Sciences') %>%
  mutate( ChemSci = factor(ChemSci),
          EnvSci = factor(EnvSci),
          BioSci = factor(BioSci),
          Eng = factor(Eng),
          MedHealthSci = factor(MedHealthSci),) %>%
  dplyr::select(-starts_with("Div.")) #Removing unused division variables (remove code to explore your favorite discipline, but note that these are more rare in data set)

# log1p() log plus one 

