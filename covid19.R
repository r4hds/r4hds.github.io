## load required libararies

library(tidyverse)
library(plotly)
library(lubridate)

# file_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"

file_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"

download.file(url = file_url, destfile = "./covid19.csv")


covid19 <- read_csv("./covid19.csv") 

names(covid19) <- stringr::str_to_lower(names(covid19))

covid19_2 <- covid19 %>% pivot_longer(- c(`province/state`, `country/region`, `lat`, `long`), names_to = "date", values_to = "cases")

covid19_2$date <- lubridate::mdy(covid19_2$date)



## number of countries affected at end of February 2020

n_count <- covid19_2 %>%
  filter(date == as.Date("2020-02-29")) %>%
  group_by(`country/region`, `date`) %>%
  summarise(cases = sum(cases)) %>%
  filter(cases != 0)




## no of cases as at 22nd March 2020

n_count2 <- covid19_2 %>%
  filter(date == as.Date("2020-03-22")) %>%
  group_by(`country/region`, `date`) %>%
  summarise(cases = sum(cases)) %>%
  filter(cases != 0)



global_trend <- covid19_2 %>%
  group_by(date) %>%
  summarise(cases = sum(cases)) %>%
  ggplot(aes(x = date, y = cases)) + 
  geom_line(size = 1, colour = "darkred") + 
  theme_minimal() +
  scale_y_continuous(labels = scales::comma, minor_breaks = NULL) + 
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d", minor_breaks = NULL) + 
  labs(x = "Date", 
       y = "No of cases",
       title = "Global reported number of confirmed COVID19 cases")



## no of countries affected

n_countries <- covid19_2 %>%
  group_by(`country/region`, date) %>%
  summarise(cases = sum(cases)) %>%
  filter(cases != 0) %>%
  group_by(date) %>%
  count() %>%
  ggplot(aes(x = date, y = n)) + 
  geom_line(colour = "powderblue", size = 1.5) +
  theme_minimal() +
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d", minor_breaks = NULL) + 
  labs(y = "No of countries affected", 
       title = "No of countries affected by covid-19")




world <- covid19_2 %>%
  group_by(`country/region`, `date`) %>%
  summarise(cases = sum(cases)) %>%
  ggplot(aes(x = date, y = cases, color = `country/region`)) + 
  geom_line(size = 1) + 
  theme_minimal() +
  theme(legend.position = "none") + 
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d", minor_breaks = NULL) +
  scale_y_continuous(breaks = seq(from = 0, to = 100000, by = 10000), labels = scales::comma, minor_breaks = NULL) +
  labs(x = "Date", 
       y = "No of Cases",
       title = "COVID19 trend around the World")







# world2 <- covid19_2 %>%
#   group_by(`country/region`, `date`) %>%
#   summarise(cases = sum(cases)) %>%
#   filter(`country/region` != "China") %>% 
#   filter(`country/region` != "Italy") %>%
#   ggplot(aes(x = date, y = cases, color = `country/region`)) + 
#   geom_line(size = 1) + 
#   theme_minimal() +
#   theme(legend.position = "none") + 
#   scale_x_date(date_breaks = "1 week", date_labels = "%b %d", minor_breaks = NULL) +
#   scale_y_continuous(breaks = seq(from = 0, to = 50000, by = 5000), labels = scales::comma, minor_breaks = NULL) + 
#   labs(x = "Date", 
#        y = "No of Cases",
#        title = "COVID19 trend around the World (Excluding China and Italy")
# 
# 
# 
# ggplotly(world2)




## Top 10 countries in the world excluding China and Italy

world2 <- covid19_2 %>%  # select the data
  group_by(`country/region`, `date`) %>%  # one entry per country per day
  summarise(cases = sum(cases)) %>%  # add all the cases for the country
  pivot_wider(names_from = date, values_from = cases) %>%
  arrange(desc(`2020-03-22`))

world2 <- world2[c(3:10), ]  %>% ## filtering 10 worst hit countries apart                                   from China and Italy
  
  pivot_longer(-`country/region`, names_to = "date", values_to = "cases")  #converting back to long                                                format
  

world2$date <- lubridate::ymd(world2$date)

world2_plot <- world2 %>%
  ggplot(aes(x = date, y = cases, color = `country/region`)) + 
  geom_line() + 
  theme_minimal() +
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d", minor_breaks = NULL) +
  scale_y_continuous(breaks = seq(from = 0, to = 50000, by = 5000), labels = scales::comma, minor_breaks = NULL) + 
  labs(x = "Date", 
       y = "No of Cases",
       title = "COVID19 trend for top 10 hit countries (Excluding China and Italy")





## africa data analysis


africa_countries <- c("Algeria", "Angola", "Benin", "Burkina Faso", "Cabo Verde", "Cameroon", "Central African Republic", "Chad", "Congo (Brazzaville)", "Congo (Kinshasa)", "Cote d'Ivoire", "Djibouti", "Egypt", "Equatorial Guinea", "Eritrea", "Eswatini", "Ethiopia", "Gabon", "Gambia", "Ghana", "Guinea", "Kenya", "Liberia", "Madagascar", "Mauritania", "Mauritius", "Morocco", "Mozambique", "Namibia", "Niger", "Nigeria", "Papua New Guinea", "Rwanda", "Saint Vincent and the Grenadines	", "Senegal", "Seychelles", "Somalia", "South Africa", "Sudan", "Tanzania", "Togo", "Tunisia", "Uganda", "Zambia", "Zimbabwe")

africa <- subset(covid19, `country/region` %in% africa_countries)

africa2 <- africa %>% 
  arrange(desc(`3/22/20`)) %>%
  pivot_longer(- c(`province/state`, `country/region`, `lat`, `long`), names_to = "date", values_to = "cases") %>%
  group_by(`country/region`, `date`) %>%
  summarise(cases = sum(cases)) 

africa2$date <- lubridate::mdy(africa2$date)  


africa_trend <- africa2 %>%
  group_by(date) %>%
  summarise(cases = sum(cases)) %>%
  ggplot(aes(x = date, y = cases)) + 
  geom_line(size = 1, colour = "darkred") + 
  theme_minimal() +
  scale_y_continuous(breaks = seq(from = 0, to = 2000, by = 200), labels = scales::comma, minor_breaks = NULL) + 
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d", minor_breaks = NULL) + 
  labs(x = "Date", 
       y = "No of Cases",
       title = "COVID19 trend in Africa")




## affected countries in Africa
africa2_plot <- africa2 %>%
  ggplot(aes(x = date, y = cases, colour = `country/region`)) +
  geom_line() +
  theme_light() + 
  theme(legend.position = "none") + 
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d", minor_breaks = NULL) +
  scale_y_continuous(breaks = seq(from = 0, to = 1000, by = 100), labels = scales::comma, minor_breaks = NULL) + 
  labs(x = "Date", 
       y = "No of Cases",
       title = "COVID19 trend in Africa")



ngr <- covid19_2 %>%
  group_by(`country/region`, `date`) %>%
  summarise(cases = sum(cases)) %>%
  filter(`country/region` == "Nigeria") %>%
  ggplot(aes(x = date, y = cases)) + 
  geom_line(size = 1.5, color = "lightblue") + 
  theme_minimal() +
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d", minor_breaks = NULL) +
  scale_y_continuous(breaks = seq(from = 0, to = 100, by = 5), minor_breaks = NULL) + 
  labs(x = "Date", 
       y = "No of Cases",
       title = "COVID19 trend in Nigeria as at 22nd March 2020")






