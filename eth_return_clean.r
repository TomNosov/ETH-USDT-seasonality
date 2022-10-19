# install and import packages
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("scales")
install.packages("stringr")
install.packages("Hmisc") 
install.packages("forcats")
install.packages("ggthemes") 
library(tidyverse)
library(ggplot2)
library(scales) 
library(stringr) 
library(Hmisc) 
library(forcats) 
library(ggthemes) 

#Clean data daily bars
# create dataframe clean_binace_d with DAILY strings/bars from Binance_ETHUSDT_daily.csv
clean_binance_d<-Binance_ETHUSDT_daily %>% 
#remove unused columns for better vizualisation
  select(-unix, -symbol,-tradecount, -`Volume ETH`,-`Volume USDT`) %>% 
## Separated year, month, day and time into new columns
  separate( date, c("Month", "Day", "Year_time") ,sep = "/")%>% 
  separate( Year_time, c("Year", "Time") ,sep = " ") %>% 
## Calculate and add variable "delta" as an average price between close and open of the bar. Measured in %
  mutate(delta = ((close - open)/ open *100)) %>% 
  mutate_at('Day', as.numeric) %>% 
  mutate_at('Month', as.numeric)
tibble(clean_binance_d)

# clean data HOURLY bars
clean_binance_h<-ETH_finam_Dirty_18.22 %>% 
## Separated year, month, day and time into new columns
  separate( X.DATE., c("Year", "MonthDay") ,sep = 4)%>%
  separate( MonthDay, c("Month", "Day") ,sep = 2)%>%
  mutate(Hour = X.TIME./10000) %>% 
## Calculate and add variable "delta" as an average price between close and open of the bar. Measured in %
  mutate(delta = ((X.CLOSE. - X.OPEN.)/ X.OPEN. *100)) %>% 
  mutate_at('Day', as.numeric) %>% 
  mutate_at('Year', as.numeric) %>% 
  mutate_at('Month', as.numeric)
tibble(clean_binance_h)

  
## export sample for formilas validation in excel
check_eth<-clean_binance %>% 
  filter(Month == 11)
print(check_eth)
write.csv(clean_binance,"Hour.csv", row.names = TRUE) 

################################################################## Code to create charts
## plot example using https://murraylax.org/rtutorials/barplots.html 
## DAILY return breakdown by DAYS for whole period Aug 2017-Oct 2022
ggplot(data=clean_binance_d, mapping=aes(x=Day, y=delta))+
  stat_summary(fun.data=mean_sdl, geom="bar",fill="steelblue")+
  stat_summary(fun.data=mean_cl_boot, geom="errorbar", width=0.2,color="gray40")+
  labs(title="Average daily ETH return(%) with confidence inte", 
       subtitle = "From Feb.2018 to Oct.2022")+
  ylab("Mean Return")+
  scale_x_continuous(breaks = seq(from = 0, to = 31, by = 2))+
  xlab("Day of Month")+
  theme(axis.title.y = element_text(face = "italic"),
        axis.title.x = element_text(face = "italic"))


# DAILY return breakdown by DAYS Aug 2017-Oct 2022 for every year
## data NOT validated. meandaily != mean hourly summarized
to_string <- as_labeller(c(`17` = "2017", `18` = "2018", `19` = "2019", 
                           `20` = "2020", `21` = "2021", `22` = "2022"))
ggplot(data=clean_binance_d, mapping=aes(x=Day, y=delta))+
  stat_summary(fun.data=mean_sdl, geom="bar",fill="steelblue")+
  stat_summary(fun.data=mean_cl_boot, geom="errorbar", width=0.01, colour="cadetblue")+
  coord_cartesian(ylim = c(-5, 5))+
  ylab("Average Return,%")+
  scale_x_continuous(breaks = seq(from = 0, to = 31, by = 2))+
  facet_wrap(~Year,labeller = to_string,scales = "free")+
  theme(axis.title.y = element_text(face = "italic"),
        axis.title.x = element_text(face = "italic"))+
  labs(title="Average daily ETH return(%) with confidence interval")

##scale_x_discrete(limits=31,breaks=Day[seq(1,length(Day),by=2)])

## HOURLY return breakdown by HOURS for WHOLE PERIOD of Feb 2018-Oct 2022
## data NOT validated manually with excel. Mean is the same
ggplot(data=clean_binance_h, mapping=aes(x=Hour, y=delta))+
  stat_summary(fun.data=mean_sdl, geom="bar", fill="steelblue")+
  stat_summary(fun.data=mean_cl_boot, geom="errorbar", width=0.2,)+  
  coord_cartesian(ylim = c(-0.15, 0.15))+
  scale_x_continuous(breaks = seq(from = 0, to = 24, by = 2))+
  labs(title="Average ETH return(%) by hour with confidence interval", 
       subtitle = "Data from Feb.2018 to Oct.2022")+
  ylab("Average Return,%")+
  xlab("Hour, UTC")+
  theme(axis.title.y = element_text(face = "italic"),
        axis.title.x = element_text(face = "italic"))+
  geom_curve(aes(x = 6, y = 0.1, xend = 11, yend = 0.06), 
             colour = "black", 
             size=0.3, 
             curvature = 0,
             arrow = arrow(length = unit(0.03, "npc"))) +
  annotate("text", x = 4.5, y = 0.1, label = "Confidence intervals",
           size = 3)+
  geom_curve(aes(x = 1, y = -0.13, xend = 7, yend = -0.13), 
             colour = "black", 
             size=0.3, 
             curvature = 0,
             arrow = arrow(length = unit(0.03, "npc"))) +
  annotate("text", x = 4, y = -0.11, label = "Asia hours",
           size = 3)+
  geom_curve(aes(x = 13, y = -0.13, xend = 20, yend = -0.13), 
             colour = "black", 
             size=0.3, 
             curvature = 0,
             arrow = arrow(length = unit(0.03, "npc"))) +
  annotate("text", x = 16, y = -0.11, label = "New-York hours",
           size = 3)

## HOURLY return breakdown by HOURS for EVERY YEAR of Feb 2018-Oct 2022
## data validated manually with excel. Mean is the same
ggplot(data=clean_binance_h, mapping=aes(x=Hour, y=delta))+
  stat_summary(fun.data=mean_sdl, geom="bar", fill="steelblue")+
  stat_summary(fun.data=mean_cl_boot, geom="errorbar", width=0.2,colour="gray60")+
  facet_wrap(~Year)+
  scale_x_continuous(breaks = seq(from = 0, to = 23, by = 2))+
  labs(title="Average ETH return(%) by hour", 
       subtitle = "Data from Feb.2018 to Oct.2022")+
  coord_cartesian(ylim = c(-0.3, 0.3))+
  ylab("Average Return,%")+
  xlab("Hour, UTC")+
  theme(axis.title.y = element_text(face = "italic"),
        axis.title.x = element_text(face = "italic"))



## example from https://ggplot2tutor.com/tutorials/summary_statistics
#eth_clean %>% 
#  ggplot(aes(x = Hour, y = delta)) +
#  geom_col()



