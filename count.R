library(dplyr)
library(ggplot2)
library(lme4)
library(emmeans)
library(ggeffects)
library(performance)

setwd("/Users/yufeihui/Desktop")
rm(list = ls())

count <- read.csv('labrecord_for_EPGmodel.csv')
?unique
length(unique(count$id))
sum(count$eggcounts.strongyle.)

#remove all well2s
#remove week 5
countwell1 <- count %>% 
  filter(well == 1) %>% 
  filter(week !=5) %>%
  mutate(epg = epg.2wellscombined..strongyle.) %>%
  select(c(farmlet, slurry, replicate, epg, week))

#convert date to date format
countwell1$collectdate <- as.Date(countwell1$collectdate, 
                                  format = '%d/%m/%Y')

#plot the data
ggplot(countwell1,
       aes(x = week, y = epg, color = farmlet)) +
  geom_point()

#week as factor
countwell1$week <- factor(countwell1$week)

#descriptive stats
countwell1 %>% count(farmlet)
countwell1 %>% count(week)
summary(countwell1)
sd(countwell1$epg)
range(countwell1$epg)

#build model
model <- glmer.nb(epg ~ week*farmlet + (1|farmlet/slurry/replicate),
              data = countwell1)

#check assumptions
#fitted vs residual
plot(model)
#scale-location
plot(model,
     sqrt(abs(resid(.)))~fitted(.),
     type=c("p","smooth"), col.line=1)
# Q-Q Plot
lattice::qqmath(model)
#residual vs leverage
plot(model, rstudent(.) ~ hatvalues(.))

#interpret model
summary(model)
#pairwise comparison
emmeans(model, pairwise~farmlet|week)

#mean epg
exp(3.16757)

#graph
figure3 <- 
  plot(ggpredict(model, terms=c("week", "farmlet")), show_data = T, jitter=T)+
  ylab("Faecal Egg Count (FEC)") +
  xlab("Week") +
  labs(color = "Cattle Herd") +
  scale_color_discrete(labels = c('Blue', 'Green'), 
                       type = c("dodgerblue3", "forestgreen")) +
  theme(title = element_blank())
  
figure3
