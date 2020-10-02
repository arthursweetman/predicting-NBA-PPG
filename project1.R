library(tidyverse)
library(readxl)
library(ggfortify)

shots <- read_excel("shot_logs.xlsx")

# Average touch time per game vs points per game
touchpoints <- shots %>% 
  group_by(player_name, GAME_ID1) %>% 
  summarise(ttime=sum(TOUCH_TIME), points=sum(PTS)) %>% 
  group_by(player_name) %>% 
  summarise(TTPG=mean(ttime), PPG=mean(points))

model <- lm(PPG~TTPG, data=touchpoints)
summary(model)
autoplot(model)

ggplot(aes(x=TTPG, y=PPG),data=touchpoints)+
  geom_point()+
  geom_smooth(method="lm")+
  theme_bw()+
  labs(x="Touch Time per Game (seconds)",
       y="Points Per Game",
       title="Touch Time per Game vs Points per Game",
       subtitle="2014-2015 NBA Regular Season")

ggplot(data=touchpoints)+
  geom_point(aes(x=TTPG, y=model$residuals))+
  geom_hline(yintercept = 0)+
  geom_hline(yintercept=3*2.582)+
  geom_hline(yintercept = -3*2.581)+
  theme_bw()+
  labs(x="Touch Time per Game (seconds)",
       y="Model Residuals",
       title="Residual Plot")

#######################################################

library(MASS)
bc <- boxcox(model)
powertransformation <- bc$x[bc$y==max(bc$y)]

touchpoints1 <- touchpoints %>% 
  mutate(sqrt.PPG = sqrt(PPG),
         sqrt.TTPG=sqrt(TTPG),
         bc.PPG = PPG^(powertransformation),
         e.PPG=PPG^2)

model <- lm(PPG~sqrt.TTPG, data=touchpoints1)
summary(model)

ggplot(aes(x=sqrt.TTPG, y=e.PPG),data=touchpoints1)+
  geom_point()+
  geom_smooth(method="lm")

ggplot(data=touchpoints1)+
  geom_point(aes(x=sqrt.TTPG, y=model$residuals))+
  geom_hline(yintercept = 0)
