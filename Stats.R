rm(list = ls())
#### load libraries ----
library(readxl)
library(jsonlite)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(timetk)
library(gghighlight)
library(anomalize)
library(imputeTS)
library(AnomalyDetection)

#### https://datahub.io/core/global-temp#r ----
json_file <- 'https://datahub.io/core/global-temp/datapackage.json'
json_data <- fromJSON(paste(readLines(json_file), collapse=""))

# get list of all resources:
print(json_data$resources$name)
# print all tabular data(if exists any)
for(i in 1:length(json_data$resources$datahub$type)){
  if(json_data$resources$datahub$type[i]=='derived/csv'){
    path_to_file = json_data$resources$path[i]
    data <- read.csv(url(path_to_file))
    print(data)
  }
}
## Hockeystick graph 
data%>%
  mutate(Date = lubridate::ymd(Date))%>%
  ggplot()+
  geom_line(aes(x=Date, y=Mean, col=Source))+
  theme_bw()+
  labs(x="Date", 
       y="Temperature anomaly",
       title="Global Temperature Time Series",
       subtitle="Mean temperature anomalies in degrees Celcius relative to a base period",
       caption="Source: https://datahub.io/core/global-temp#r")


#### https://www.knmi.nl/nederland-nu/klimatologie/daggegevens----
KNMI_deBilt <- read_excel("Data/KNMI_deBilt.xlsx")%>%
  mutate(Date = lubridate::ymd(YYYYMMDD))%>%
  select(-c(STN, YYYYMMDD))

KNMI_deBilt_long<-KNMI_deBilt%>%
  mutate(Date = lubridate::ymd(YYYYMMDD))%>%
  select(-c(STN, YYYYMMDD))%>%
  pivot_longer(-Date,
               names_to = "Metric", 
               values_to = "Value")%>%
  mutate(Value = Value/10)

ggplot(KNMI_deBilt_long)+
  geom_line(aes(x=Date, y=Value))+
  facet_grid(~Metric, scales="free")+
  theme_bw()+
  labs(x="Date", 
       y="Value", 
       title="Values from KNMI climate data",
       subtitle= "Daily records from De Bilt weather station",
       caption="Source: https://www.knmi.nl/nederland-nu/klimatologie/daggegevens")


KNMI_deBilt%>%
  summarise_by_time(
    .date_var = Date,
    .by       = "year", 
    TG_mean  = mean(TG), 
    TN_mean  = mean(TN),
    TX_mean  = mean(TX))%>%
  ggplot()+
  geom_line(aes(x=Date, y=TG_mean))+
  geom_ribbon(aes(x=Date, ymin=TN_mean, ymax=TX_mean), alpha=0.4)+
  theme_bw()+
  labs(x="Date", 
       y="Average temperature", 
       title="Average temperature over time for the Netherlands", 
       subtitle = "Yearly values coming from De Bilt", 
       caption="Source: https://www.knmi.nl/nederland-nu/klimatologie/daggegevens")


KNMI_deBilt%>%
  pivot_longer(-Date,
               names_to = "Metric", 
               values_to = "Value")%>%
  mutate(Value = Value/10)%>%
  filter(Metric%in%(c("TG","TN","TX")))%>%
  group_by(Metric)%>%
  summarise_by_time(
    .date_var = Date,
    .by       = "month", 
    mean_value  = mean(Value))%>%
  plot_time_series(Date, mean_value, .facet_ncol = 3, .interactive = FALSE)

roll_avg_10 <- slidify(.f = mean, .period = 10, .align = "center", .partial = TRUE)
KNMI_deBilt%>%
  pivot_longer(-Date,
               names_to = "Metric", 
               values_to = "Value")%>%
  mutate(Value = Value/10)%>%
  filter(Metric%in%(c("TG","TN","TX")))%>%
  group_by(Metric)%>%
  summarise_by_time(
    .date_var = Date,
    .by       = "year", 
    mean_value  = mean(Value))%>%
  mutate(rolling_avg_10 = roll_avg_10(mean_value))%>%
  tidyr::pivot_longer(cols = c(mean_value, rolling_avg_10)) %>%
  plot_time_series(Date, value, .color_var = name,
                   .facet_ncol = 1, .smooth = FALSE, 
                   .interactive = FALSE)


KNMI_deBilt%>%
  summarise_by_time(
    .date_var = Date,
    .by       = "month", 
    TG_mean  = mean(TG/10), 
    TN_mean  = mean(TN/10),
    TX_mean  = mean(TX/10))%>%
  filter(Date>"1979-12-01")%>%
  ggplot()+
  geom_line(aes(x=Date, y=TG_mean))+
  geom_ribbon(aes(x=Date, ymin=TN_mean, ymax=TX_mean), alpha=0.4)+
  theme_bw()+
  labs(x="Date", 
       y="Average temperature (month)", 
       title="Average temperature over time for the Netherlands", 
       subtitle = "Monthly values coming from De Bilt", 
       caption="Source: https://www.knmi.nl/nederland-nu/klimatologie/daggegevens")

month_names<-month.abb
month_labeller <- function(variable,value){
  return(month_names[value])
}
KNMI_deBilt%>%
  summarise_by_time(
    .date_var = Date,
    .by       = "month", 
    TG_mean  = mean(TG/10), 
    TN_mean  = mean(TN/10),
    TX_mean  = mean(TX/10))%>%
  filter(Date>"1979-12-01")%>%
  mutate(month_num =  lubridate::month(Date))%>%
  ggplot()+
  geom_line(aes(x=Date, y=TG_mean))+
  geom_ribbon(aes(x=Date, ymin=TN_mean, ymax=TX_mean), alpha=0.4)+
  facet_grid(~month_num, 
             labeller=month_labeller)+
  theme_bw()+
  labs(x="Date", 
       y="Average temperature (month)", 
       title="Average temperature over time for the Netherlands", 
       subtitle = "Monthly values coming from De Bilt", 
       caption="Source: https://www.knmi.nl/nederland-nu/klimatologie/daggegevens")

KNMI_deBilt%>%
  mutate(TG=(TG/10),
         TN=(TN/10),
         TX=(TX/10),
         month_num  = lubridate::month(Date),
         Year       = lubridate::year(Date),
         Day_month  = lubridate::mday(Date),
         Day        = lubridate::yday(Date),
         month_lab  = month.abb[month_num])%>%
  ggplot()+
  geom_line(aes(x=Day, y=TG, group=Year, col=factor(Year)))+
  gghighlight(Year>2018)+
  theme_bw()+
  labs(x="Day of the year", 
       y="Average temperature", 
       title="Average temperature over time for the Netherlands", 
       subtitle = "Values coming from De Bilt - we highlighted the last five years", 
       caption="Source: https://www.knmi.nl/nederland-nu/klimatologie/daggegevens")

KNMI_deBilt%>%
  mutate(TG=(TG/10),
         TN=(TN/10),
         TX=(TX/10),
         month_num  = lubridate::month(Date),
         Year       = lubridate::year(Date),
         Day_month  = lubridate::mday(Date),
         Day        = lubridate::yday(Date),
         month_lab  = month.abb[month_num])%>%
  ggplot()+
  geom_line(aes(x=Day, y=TG, group=Year, col=factor(Year)))+
  gghighlight(Year>2018,use_direct_label = FALSE)+
  facet_grid(~Year)+
  theme_bw()+
  theme(legend.position = "none")+  # remove legend
  labs(x="Day of the year", 
       y="Average temperature",
       title="Average temperature over time for the Netherlands", 
       subtitle = "Values coming from De Bilt - we highlighted the last five years", 
       caption="Source: https://www.knmi.nl/nederland-nu/klimatologie/daggegevens")


KNMI_deBilt%>%
  mutate(TG=(TG/10),
         TN=(TN/10),
         TX=(TX/10),
         month_num  = lubridate::month(Date),
         Year       = lubridate::year(Date),
         Day_month  = lubridate::mday(Date),
         Day        = lubridate::yday(Date),
         month_lab  = month.abb[month_num])%>%
  ggplot()+
  geom_tile(aes(y=Day_month, x=Year, fill=TG),col="black")+
  scale_fill_viridis_c(option="turbo")+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(y="Day of the month", 
       x="Year",
       fill="Celcius",
       title="Average temperature over time for the Netherlands", 
       subtitle = "Values coming from De Bilt", 
       caption="Source: https://www.knmi.nl/nederland-nu/klimatologie/daggegevens")


g1<-KNMI_deBilt%>%
  mutate(TG=(TG/10),
         TN=(TN/10),
         TX=(TX/10),
         month_num  = lubridate::month(Date),
         Year       = lubridate::year(Date),
         Day_month  = lubridate::mday(Date),
         Day        = lubridate::yday(Date),
         month_lab  = month.abb[month_num])%>%
  mutate(Hot = if_else(TG>24, 1, 0))%>%
  group_by(Year)%>%
  summarise(Hot_sum=sum(Hot))%>%
  ggplot()+
  geom_bar(aes(x=factor(Year), y=Hot_sum),stat="identity")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
  labs(y="Frequency", 
       x="",
       title="Number of days temperature >=25 for the Netherlands", 
       subtitle = "Based on average daily temperatures")
g2<-KNMI_deBilt%>%
  mutate(TG=(TG/10),
         TN=(TN/10),
         TX=(TX/10),
         month_num  = lubridate::month(Date),
         Year       = lubridate::year(Date),
         Day_month  = lubridate::mday(Date),
         Day        = lubridate::yday(Date),
         month_lab  = month.abb[month_num])%>%
  mutate(Hot = if_else(TX>24, 1, 0))%>%
  group_by(Year)%>%
  summarise(Hot_sum=sum(Hot))%>%
  ggplot()+
  geom_bar(aes(x=factor(Year), y=Hot_sum),stat="identity")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
  labs(y="Frequency",
       x="",
       subtitle = "Based on maximum daily temperatures")
g3<-KNMI_deBilt%>%
  mutate(TG=(TG/10),
         TN=(TN/10),
         TX=(TX/10),
         month_num  = lubridate::month(Date),
         Year       = lubridate::year(Date),
         Day_month  = lubridate::mday(Date),
         Day        = lubridate::yday(Date),
         month_lab  = month.abb[month_num])%>%
  mutate(Hot = if_else(TN>24, 1, 0))%>%
  group_by(Year)%>%
  summarise(Hot_sum=sum(Hot))%>%
  ggplot()+
  geom_bar(aes(x=factor(Year), y=Hot_sum),stat="identity")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
  labs(y="Frequency", 
       x="Year",
       subtitle = "Based on minimum daily temperatures", 
       caption="Source: https://www.knmi.nl/nederland-nu/klimatologie/daggegevens")
gridExtra::grid.arrange(g1,g2,g3,nrow=3)



#### https://www.knmi.nl/kennis-en-datacentrum/achtergrond/centraal-nederland-temperatuur-cnt ----
tg_CNT <- readr::read_table("Data/tg_CNT.txt")
colnames(tg_CNT)
tg_CNT%>%
  select(-X14)%>%
  pivot_longer(-YEAR, 
               values_to = "Temp", 
               names_to = "Month")%>%
  ggplot()+
  geom_line(aes(x=YEAR, y=Temp, col=Month))+
  theme_bw()



# https://www.knmi.nl/over-het-knmi/nieuws/warme-juni
tg_CNT%>%
  select(-X14)%>%
  pivot_longer(-YEAR, 
               values_to = "Temp", 
               names_to = "Month")%>%
  mutate(Month_num = recode(Month,Jan = 1,Feb = 2,Mar = 3,
                        Apr = 4,May = 5,Jun = 6,Jul = 7,Aug = 8,
                        Sep = 9,Oct = 10,Nov = 11,Dec = 12))%>%
  mutate(Date = zoo::as.yearmon(paste(.$YEAR, .$Month_num), "%Y %m"))%>%
  ggplot()+
  geom_line(aes(x=Date, y=Temp))+
  theme_bw()  

tg_CNT%>%
  select(-X14)%>%
  pivot_longer(-YEAR, 
               values_to = "Temp", 
               names_to = "Month")%>%
  mutate(month_num = recode(Month,Jan = 1,Feb = 2,Mar = 3,
                            Apr = 4,May = 5,Jun = 6,Jul = 7,Aug = 8,
                            Sep = 9,Oct = 10,Nov = 11,Dec = 12))%>%
  mutate(Date = zoo::as.yearmon(paste(.$YEAR, .$month_num), "%Y %m"))%>%
  mutate(Day_month  = lubridate::mday(Date),
         Day        = lubridate::yday(Date),
         month_lab  = month.abb[month_num])%>%
  ggplot()+
  geom_line(aes(x=Day, y=Temp, group=YEAR, col=factor(YEAR)))+
  gghighlight(YEAR>2018, use_direct_label = FALSE)+
  facet_grid(~YEAR)+
  theme_bw()+
  theme(legend.position = "none")+  # remove legend
  labs(x="Day of the year", 
       y="Average temperature (month)",
       title="Average temperature over time for the Netherlands", 
       subtitle = "Monthly values coming from De Bilt - we highlighted the last five years", 
       caption="Source: https://www.knmi.nl/kennis-en-datacentrum/achtergrond/centraal-nederland-temperatuur-cnt")
  


#### https://www.knmi.nl/klimaatdashboard ----
KNMI_deBilt_jaargemiddelde <- read_excel("Data/KNMI_deBilt_jaargemiddelde.xlsx")
str(KNMI_deBilt_jaargemiddelde)

KNMI_deBilt_jaargemiddelde%>%
  rename(Trend_gem = `Trendlijn metingen`)%>%
  mutate_if(is.character, as.numeric)%>%
  mutate(Trend_gem = if_else(Trend_gem>100, Trend_gem/1000, Trend_gem),
         lower = if_else(lower>100, lower/1000, lower),
         higher = if_else(higher>100, higher/1000, higher))%>%
  ggplot()+
  geom_line(aes(x=Jaar, y=Jaargemiddelde))+
  geom_point(aes(x=Jaar, y=Jaargemiddelde))+
  geom_line(aes(x=Jaar, y=Trend_gem), col="blue")+
  geom_ribbon(aes(x=Jaar, ymin=lower, ymax=higher), fill="blue", alpha=0.4)+
  theme_bw()+
  ylim(7, 16)+
  labs(x="Year", 
       y="Yearly average", 
       title="Yearly average for the Netherlands", 
       subtitle="Blue line and band show trend average and 90% confidence interval - limits for Y-axis chosen to mimic dashboard", 
       caption="Source: https://www.knmi.nl/klimaatdashboard")



#### Compare daily observations to trend ----
df_KNMI_gem<-read_excel("Data/KNMI_deBilt_jaargemiddelde.xlsx")%>%
  rename(Trend_gem = `Trendlijn metingen`)%>%
  mutate_if(is.character, as.numeric)%>%
  mutate(Trend_gem = if_else(Trend_gem>100, Trend_gem/1000, Trend_gem),
         lower = if_else(lower>100, lower/1000, lower),
         higher = if_else(higher>100, higher/1000, higher))
df_KNMI_dag<-read_excel("Data/KNMI_deBilt.xlsx")%>%
  mutate(Date = lubridate::ymd(YYYYMMDD))%>%
  select(-c(STN, YYYYMMDD))%>%
  summarise_by_time(
    .date_var = Date,
    .by       = "year", 
    Jaargemiddelde  = mean(TG/10), 
    TN_mean  = mean(TN/10),
    TX_mean  = mean(TX/10))%>%
  mutate(Jaar=lubridate::year(Date))

cols <- c("KNMI (yearly average)" = "black", 
          "KNMI (trend line)" = "blue", 
          "KNMI (daily records)"= "black")
fills <- c("KNMI - trend (90% band)" = "blue", 
           "KNMI - records (min - max)"= "gray")
ggplot()+
  geom_ribbon(data=df_KNMI_gem,
              aes(x=Jaar, ymin=lower, ymax=higher, fill="KNMI - trend (90% band)"),
              alpha=0.4)+
   geom_ribbon(data=df_KNMI_dag, 
               aes(x=Jaar, ymin=TN_mean, ymax=TX_mean, fill="KNMI - records (min - max)"), 
               alpha=0.4)+
  #geom_line(data=df_KNMI_gem, 
            #aes(x=Jaar, y=Jaargemiddelde, col="KNMI (yearly average)"))+
  geom_line(data=df_KNMI_gem, 
            aes(x=Jaar, y=Trend_gem, col="KNMI (trend line)"))+
  geom_line(data=df_KNMI_dag, 
            aes(x=Jaar, y=Jaargemiddelde, col="KNMI (daily records)"))+
  geom_point(data=df_KNMI_dag, 
            aes(x=Jaar, y=Jaargemiddelde, col="KNMI (daily records)"))+
  theme_bw()+
  theme(legend.position = "bottom")+
  scale_colour_manual(values = cols)+
  scale_fill_manual(values = fills)+
  labs(x="Year", 
       y="Temperature (year)",
       title="Average temperature in the Netherlands from de Bilt",
       subtitle="KNMI daily observations averaged per year combined with trend data", 
       caption="Sources: https://www.knmi.nl/nederland-nu/klimatologie/daggegevens & https://www.knmi.nl/klimaatdashboard",
       col="", 
       fill="")
  

#### Anomaly detection ----
read_excel("Data/KNMI_deBilt.xlsx")%>%
  mutate(Date = lubridate::ymd(YYYYMMDD))%>%
  select(-c(STN, YYYYMMDD))%>%
  mutate(TG=(TG/10))%>%
  summarise_by_time(
    .date_var = Date,
    .by       = "month", 
    TG_mean  = mean(TG))%>%
  select(Date, TG_mean)%>%
  time_decompose(TG_mean, method = "stl", frequency = "auto", trend = "auto") %>%
  anomalize(remainder, method = "iqr", alpha = 0.05, max_anoms = 0.2) %>%
  plot_anomaly_decomposition()

read_excel("Data/KNMI_deBilt.xlsx")%>%
  mutate(Date = lubridate::ymd(YYYYMMDD))%>%
  select(-c(STN, YYYYMMDD))%>%
  mutate(TG=(TG/10))%>%
  summarise_by_time(
    .date_var = Date,
    .by       = "month", 
    TG_mean  = mean(TG))%>%
  select(Date, TG_mean)%>%  
  time_decompose(TG_mean) %>%
  anomalize(remainder) %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE, ncol = 3, alpha_dots = 0.5)
  
read_excel("Data/KNMI_deBilt.xlsx")%>%
  mutate(Date = lubridate::ymd(YYYYMMDD))%>%
  select(-c(STN, YYYYMMDD))%>%
  mutate(TG=(TG/10))%>%
  summarise_by_time(
    .date_var = Date,
    .by       = "month", 
    TG_mean  = mean(TG))%>%
  select(Date, TG_mean)%>%  
  time_decompose(TG_mean) %>%
  anomalize(remainder) %>%
  time_recompose() %>%
  filter(anomaly == 'Yes') 


df<-readr::read_table("Data/tg_CNT.txt")%>%
  select(-X14)%>%
  as.data.frame()%>%
  pivot_longer(-YEAR, 
               values_to = "Temp", 
               names_to = "Month")%>%
  mutate(Month_num = recode(Month,Jan = 1,Feb = 2,Mar = 3,
                            Apr = 4,May = 5,Jun = 6,Jul = 7,Aug = 8,
                            Sep = 9,Oct = 10,Nov = 11,Dec = 12))%>%
  mutate(Date = lubridate::as_date(zoo::as.yearmon(paste(.$YEAR, .$Month_num), "%Y %m")))%>%
  ungroup()%>%
  select(Date, Temp)


df%>%
  zoo::read.zoo()%>%
  ggplot_na_distribution()
imp<-df%>%
  zoo::read.zoo()%>%
  na_kalman()
df%>%
  zoo::read.zoo()%>%
  ggplot_na_imputations(., imp)

df%>%
  tidyr::drop_na()%>%
  time_decompose(Temp, method = "stl", frequency = "auto", trend = "auto") %>%
  anomalize(remainder, method = "iqr", alpha = 0.05, max_anoms = 0.2) %>%
  plot_anomaly_decomposition()

df%>%
  tidyr::drop_na()%>%
  as.data.frame()%>%
  AnomalyDetectionTs(., max_anoms=0.1, direction="both")



#### Distribution analysis ---- 
KNMI_deBilt <- read_excel("Data/KNMI_deBilt.xlsx")%>%
  mutate(Date = lubridate::ymd(YYYYMMDD))%>%
  select(-c(STN, YYYYMMDD))

KNMI_deBilt%>%
  mutate(TG=(TG/10),
         TN=(TN/10),
         TX=(TX/10),
         month_num  = lubridate::month(Date),
         Year       = lubridate::year(Date),
         Day_month  = lubridate::mday(Date),
         Day        = lubridate::yday(Date),
         month_lab  = month.abb[month_num])%>%
  group_by(Year)%>%
  ggplot()+
  geom_histogram(aes(x=TG), bins = 50)+
  theme_bw()+ 
  labs(x="Average temperature", 
       y="Count",
       title="Average temperature over time for the Netherlands", 
       subtitle = "Frequency across daily observations from 1901 - 2023" , 
       caption="Source: https://www.knmi.nl/nederland-nu/klimatologie/daggegevens")

floor_decade    = function(value){ return(value - value %% 10) }
ceiling_decade  = function(value){ return(floor_decade(value)+10) }
round_to_decade = function(value){ return(round(value / 10) * 10) }
KNMI_deBilt%>%
  mutate(TG=(TG/10),
         TN=(TN/10),
         TX=(TX/10),
         month_num  = lubridate::month(Date),
         Year       = lubridate::year(Date),
         Day_month  = lubridate::mday(Date),
         Day        = lubridate::yday(Date),
         month_lab  = month.abb[month_num])%>%
  mutate(decade = round_to_decade(Year))%>%
  ggplot()+
  geom_line(aes(x=Date, y=TG, col=factor(decade)), show.legend = FALSE)+
  facet_grid(~decade, scales = "free")+
  theme_bw()

KNMI_deBilt%>%
  summarise_by_time(
    .date_var = Date,
    .by       = "month", 
    TG_mean  = mean(TG/10), 
    TN_mean  = mean(TN/10),
    TX_mean  = mean(TX/10))%>%
  mutate(month_num =  lubridate::month(Date),
         Year       = lubridate::year(Date),
         Day_month  = lubridate::mday(Date),
         Day        = lubridate::yday(Date),
         month_lab  = month.abb[month_num])%>%
  mutate(decade = floor_decade(Year))%>%
  ggplot()+
  geom_line(aes(x=Date, y=TG_mean))+
  geom_ribbon(aes(x=Date, ymin=TN_mean, ymax=TX_mean, fill="Minimum & maximum temperature"), alpha=0.4)+
  facet_grid(~decade, scales = "free")+
  theme_bw()+ 
  theme(legend.position = "bottom")+
  labs(x="Date", 
       y="Average temperature (month)",
       fill="",
       title="Average temperature over time for the Netherlands", 
       subtitle = "Monthly Values coming from De Bilt", 
       caption="Source: https://www.knmi.nl/nederland-nu/klimatologie/daggegevens")

KNMI_deBilt%>%
  mutate(TG=(TG/10),
         TN=(TN/10),
         TX=(TX/10),
         month_num  = lubridate::month(Date),
         Year       = lubridate::year(Date),
         Day_month  = lubridate::mday(Date),
         Day        = lubridate::yday(Date),
         month_lab  = month.abb[month_num])%>%
  mutate(decade = floor_decade(Year))%>%
  ggplot()+
  geom_histogram(aes(x=TG), bins = 50)+
  facet_grid(~decade)+
  theme_bw()+ 
  labs(x="Average temperature", 
       y="Count",
       title="Average temperature over time for the Netherlands", 
       subtitle = "Frequency across daily observations from 1901 - 2023" , 
       caption="Source: https://www.knmi.nl/nederland-nu/klimatologie/daggegevens")




KNMI_deBilt%>%
  mutate(TG=(TG/10),
         TN=(TN/10),
         TX=(TX/10),
         month_num  = lubridate::month(Date),
         Year       = lubridate::year(Date),
         Day_month  = lubridate::mday(Date),
         Day        = lubridate::yday(Date),
         month_lab  = month.abb[month_num])%>%
  mutate(decade = floor_decade(Year))%>%
  ggplot()+
  geom_density(aes(x=TG, group=Year))+
  facet_grid(~decade)+
  theme_bw()+ 
  labs(x="Average temperature", 
       y="Count",
       title="Average temperature over time for the Netherlands", 
       subtitle = "Frequency across daily observations from 1901 - 2023" , 
       caption="Source: https://www.knmi.nl/nederland-nu/klimatologie/daggegevens")

KNMI_deBilt%>%
  mutate(TG=(TG/10),
         TN=(TN/10),
         TX=(TX/10),
         month_num  = lubridate::month(Date),
         Year       = lubridate::year(Date),
         Day_month  = lubridate::mday(Date),
         Day        = lubridate::yday(Date),
         month_lab  = month.abb[month_num])%>%
  mutate(decade = floor_decade(Year))%>%
  ggplot()+
  geom_boxplot(aes(x=Year, y=TG, group=Year))+
  facet_grid(~decade, scales="free")+
  theme_bw()+ 
  labs(x="Average temperature", 
       y="Count",
       title="Average temperature over time for the Netherlands", 
       subtitle = "Frequency across daily observations from 1901 - 2023" , 
       caption="Source: https://www.knmi.nl/nederland-nu/klimatologie/daggegevens")








#### 2023 Heetste September op 1 na -----
# https://www.knmi.nl/over-het-knmi/nieuws/september-2023
tg_CNT%>%
  filter(YEAR<2024)%>%
  ggplot()+
  geom_bar(aes(x=YEAR, y=Sep, fill=factor(YEAR)), stat="identity")+
  gghighlight(Sep>17)+
  theme_bw()+
  labs(x="Year", 
       y="Temperature", 
       fill="", 
       title="Average temperature in the Netherlands", 
       subtitle= "Focus on September - https://www.knmi.nl/over-het-knmi/nieuws/september-2023", 
       caption="Source: https://www.knmi.nl/kennis-en-datacentrum/achtergrond/centraal-nederland-temperatuur-cnt")

tg_CNT%>%
  filter(YEAR==2023)%>%
  select(Sep) #17.9

KNMI_deBilt%>%
  summarise_by_time(
    .date_var = Date,
    .by       = "month", 
    TG_mean  = mean(TG/10), 
    TN_mean  = mean(TN/10),
    TX_mean  = mean(TX/10))%>%
  filter(Date=="2023-09-01") #17.5

tg_CNT%>%
  filter(YEAR<2024)%>%
  ggplot(aes(x=Sep))+
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white", bins=30)+
  geom_density(aes(x=Sep), col="black")+
  geom_vline(xintercept = 17.5, lty=2, col="black")+
  ggpubr::stat_overlay_normal_density(color = "red", linetype = "dashed")+
  labs(x="Temperature", 
       y="Density", 
       fill="", 
       title="Yearly temperature in the Netherlands for Sep", 
       subtitle= "How extreme is the 2023 temperature? - https://www.knmi.nl/over-het-knmi/nieuws/september-2023", 
       caption="Source: https://www.knmi.nl/kennis-en-datacentrum/achtergrond/centraal-nederland-temperatuur-cnt")

prob_test<-tg_CNT%>%
  filter(YEAR<2023)%>%
  select(Sep)%>%
  summarise(mean = mean(Sep), 
            sd = sd(Sep))
1-pnorm(17.5, prob_test$mean, prob_test$sd)  # 0.00563




#### Zo'n warme juni was begin vorige eeuw bijna onmogelijk ----
# https://www.knmi.nl/over-het-knmi/nieuws/warme-juni
tg_CNT%>%
  filter(YEAR<2024)%>%
  ggplot()+
  geom_bar(aes(x=YEAR, y=Jun, fill=factor(YEAR)), stat="identity")+
  gghighlight(Jun>19)+
  theme_bw()+
  labs(x="Year", 
       y="Temperature", 
       fill="", 
       title="Average temperature in the Netherlands", 
       subtitle= "Focus on June - https://www.knmi.nl/over-het-knmi/nieuws/september-2023", 
       caption="Source: https://www.knmi.nl/kennis-en-datacentrum/achtergrond/centraal-nederland-temperatuur-cnt")

tg_CNT%>%
  filter(YEAR==2023)%>%
  select(Jun) #19.9

KNMI_deBilt%>%
  summarise_by_time(
    .date_var = Date,
    .by       = "month", 
    TG_mean  = mean(TG/10), 
    TN_mean  = mean(TN/10),
    TX_mean  = mean(TX/10))%>%
  filter(Date=="2023-06-01") #19.4

tg_CNT%>%
  filter(YEAR<2024)%>%
  ggplot(aes(x=Jun))+
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white", bins=30)+
  geom_density(aes(x=Jun), col="black")+
  geom_vline(xintercept = 19.4, lty=2, col="black")+
  ggpubr::stat_overlay_normal_density(color = "red", linetype = "dashed")+
  labs(x="Temperature", 
       y="Density", 
       fill="", 
       title="Yearly temperature in the Netherlands for June", 
       subtitle= "How extreme is the 2023 temperature? - https://www.knmi.nl/over-het-knmi/nieuws/warme-juni ", 
       caption="Source: https://www.knmi.nl/kennis-en-datacentrum/achtergrond/centraal-nederland-temperatuur-cnt")

prob_test<-tg_CNT%>%
  filter(YEAR<2023)%>%
  select(Jun)%>%
  summarise(mean = mean(Jun), 
            sd = sd(Jun))
1-pnorm(19.4, prob_test$mean, prob_test$sd)  # 0.002926055
  

# Rond het jaar 1900 was zo’n warme junimaand vrijwel onmogelijk. 
KNMI_deBilt%>%
  mutate(TG=(TG/10),
         TN=(TN/10),
         TX=(TX/10),
         month_num  = lubridate::month(Date),
         Year       = lubridate::year(Date),
         Day_month  = lubridate::mday(Date),
         Day        = lubridate::yday(Date),
         month_lab  = month.abb[month_num])%>%
  ggplot()+
  geom_line(aes(x=Day, y=TG, group=Year, col=factor(Year)))+
  gghighlight(Year%in%(c(1990,2023)))+
  geom_vline(xintercept = 151.5, lty=2, col="black")+
  geom_vline(xintercept = 181.5, lty=2, col="black")+
  geom_text(aes(x=151.5, label="start June", y=0), 
            colour="darkgreen", angle=90, vjust = -1.2, 
            text=element_text(size=11), check_overlap = TRUE)+
  geom_text(aes(x=181.5, label="end June", y=0), 
            colour="darkred", angle=90, vjust = 1.2, 
            text=element_text(size=11), check_overlap = TRUE)+
  theme_bw()+
  labs(x="Day of the year", 
       y="Average temperature", 
       title="Average temperature over time for the Netherlands", 
       subtitle = "Values coming from De Bilt - we highlighted 1990 and 2023", 
       caption="Source: https://www.knmi.nl/nederland-nu/klimatologie/daggegevens")

KNMI_deBilt%>%
  mutate(TG=(TG/10),
         TN=(TN/10),
         TX=(TX/10),
         month_num  = lubridate::month(Date),
         Year       = lubridate::year(Date),
         Day_month  = lubridate::mday(Date),
         Day        = lubridate::yday(Date),
         month_lab  = month.abb[month_num])%>%
  filter(Day%in%(151:182))%>%
  ggplot()+
  geom_line(aes(x=Day, y=TG, group=Year, col=factor(Year)))+
  gghighlight(Year%in%(c(1990,2023)))+
  theme_bw()+
  labs(x="Day of the year", 
       y="Average temperature", 
       title="Average temperature over time for the Netherlands", 
       subtitle = "Values coming from De Bilt - we highlighted June", 
       caption="Source: https://www.knmi.nl/nederland-nu/klimatologie/daggegevens")


KNMI_deBilt%>%
  mutate(TG=(TG/10),
         TN=(TN/10),
         TX=(TX/10),
         month_num  = lubridate::month(Date),
         Year       = lubridate::year(Date),
         Day_month  = lubridate::mday(Date),
         Day        = lubridate::yday(Date),
         month_lab  = month.abb[month_num])%>%
  filter(Day%in%(181:212))%>%
  ggplot()+
  geom_line(aes(x=Day, y=TG, group=Year, col=factor(Year)))+
  gghighlight(Year%in%(c(1990,2023)))+
  theme_bw()+
  labs(x="Day of the year", 
       y="Average temperature", 
       title="Average temperature over time for the Netherlands", 
       subtitle = "Values coming from De Bilt - we highlighted July", 
       caption="Source: https://www.knmi.nl/nederland-nu/klimatologie/daggegevens")

tg_CNT%>%
  filter(YEAR==2023)%>%
  select(Jul) #18.3

KNMI_deBilt%>%
  summarise_by_time(
    .date_var = Date,
    .by       = "month", 
    TG_mean  = mean(TG/10), 
    TN_mean  = mean(TN/10),
    TX_mean  = mean(TX/10))%>%
  filter(Date=="2023-07-01") #18.1

tg_CNT%>%
  filter(YEAR<2024)%>%
  ggplot(aes(x=Jul))+
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white", bins=30)+
  geom_density(aes(x=Jul), col="black")+
  geom_vline(xintercept = 18.1, lty=2, col="black")+
  ggpubr::stat_overlay_normal_density(color = "red", linetype = "dashed")+
  labs(x="Temperature", 
       y="Density", 
       fill="", 
       title="Yearly temperature in the Netherlands for July", 
       subtitle= "How extreme is the 2023 temperature? - https://www.knmi.nl/over-het-knmi/nieuws/warme-juni ", 
       caption="Source: https://www.knmi.nl/kennis-en-datacentrum/achtergrond/centraal-nederland-temperatuur-cnt")

prob_test<-tg_CNT%>%
  filter(YEAR<2023)%>%
  select(Jul)%>%
  summarise(mean = mean(Jul), 
            sd = sd(Jul))
1-pnorm(18.1, prob_test$mean, prob_test$sd)  # 0.284124









