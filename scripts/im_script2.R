##############################################################################
#                            
#                            SCRIPT II
#               --------------------------------------         
#                   Infant Mortality Data Analysis
#             Data source: WorldBank Development Indicator API
#             Date:       March 28, 2017
#             
#############################################################################



###############################################################################
#                              PART I
#               loading required packages, functions and data
###############################################################################

source("./scripts/my_functions.R")  # loads required packages and functions

# read-in the downloaded data ---------------------------------------------

mr_infant <- read_csv("./scripts/infant_mortality")
mr_u5 <- read_csv("./scripts/under_five_mortality")
mr_neonatal <- read_csv("./scripts/neonatal_mortality")

### ---------------------  end of part I  -------------------------- ###



###############################################################################
#                              PART II
#                   Data cleaning and wrangling
###############################################################################

# clean, convert categorical variables to factors and add two variables -------

# clean the data
nm <- mr_neonatal %>% 
  rename(nmr = SH.DYN.NMRT) %>%
  clean_data("nmr") %>% 
  factorise() %>%
  add_decline("nmr")

im <- mr_infant %>%  
  rename(imr = SP.DYN.IMRT.IN) %>% 
  clean_data("imr") %>%
  factorise() %>%
  add_decline("imr")

u5m <- mr_u5 %>% 
  rename(u5mr = SH.DYN.MORT) %>%
  clean_data("u5mr") %>% 
  factorise() %>%
  add_decline("u5mr")


# population of under-five children ---------------------------------------

data("popF")  ## total population of females
data("popM")  ## total population of males

# filter population of under-five from "popF" and "popM"

u5_female_pop <- u5_pop(popF) %>%
rename(u5_female = under_five) # filters that of females

u5_male_pop <- u5_pop(popM) %>%
  rename(u5_male = under_five)   # filters that of males

under5_pop <- u5_female_pop %>%
  left_join(u5_male_pop, by = c("year", "continent", "country", "iso3c")) %>%
  mutate(u5_pop = u5_female + u5_male) %>%
  select(-c(u5_female, u5_male)) 
under5_pop$year <- parse_number(under5_pop$year)


u5data <- u5m %>%
  left_join(under5_pop, by = c("year", "continent", "country", "iso3c")) %>% 
  mutate(u5_death = u5mr * u5_pop)

### ---------------------  end of part II  -------------------------- ###



###############################################################################
#                              PART III
#                           Data analysis
###############################################################################


# plot of children deaths pre-mdgs ----------------------------------------

nm_plot <- nm %>%
  filter(year == 1990) %>%
  highest_mortality("nmr") +
  labs(title = "Countries with the Highest Neonatal Mortality Rate", 
       subtitle = "Year 1990",
       x = "Countries",
       y = "Neonatal Mortality Rate",
       caption = "Data from World Bank through the `WDI` package")

im_plot <- im %>%
  filter(year == 1990) %>%
  highest_mortality("imr") +
  labs(title = "Countries with the Highest Infant Mortality Rate", 
       subtitle = "Year 1990",
       x = "Countries",
       y = "Infant Mortality Rate",
       caption = "Data from World Bank through the `WDI` package")

u5m_plot <- u5m %>%
  filter(year == 1990) %>%
  highest_mortality("u5mr") +
  labs(title = "Countries with the Highest Under-five Mortality Rate", 
       subtitle = "Year 1990",
       x = "Countries",
       y = "Under-five Mortality Rate",
       caption = "Data from World Bank through the `WDI` package")


nm_plot2 <- nm %>%
  filter(year == 2000) %>%
  highest_mortality("nmr") +
  labs(title = "Countries with the Highest Neonatal Mortality Rate", 
       subtitle = "Year 2000",
       x = "Countries",
       y = "Neonatal Mortality Rate",
       caption = "Data from World Bank through the `WDI` package")

im_plot2 <- im %>%
  filter(year == 2000) %>%
  highest_mortality("imr") +
  labs(title = "Countries with the Highest Infant Mortality Rate", 
       subtitle = "Year 2000",
       x = "Countries",
       y = "Infant Mortality Rate",
       caption = "Data from World Bank through the `WDI` package")

u5m_plot2 <- u5m %>%
  filter(year == 2000) %>%
  highest_mortality("u5mr") +
  labs(title = "Countries with the Highest Under-five Mortality Rate", 
       subtitle = "Year 2000",
       x = "Countries",
       y = "Under-five Mortality Rate",
       caption = "Data from World Bank through the `WDI` package")


# plot of children deaths after mdg ---------------------------------------


nm_plot3 <- nm %>%
  filter(year == 2015) %>%
  highest_mortality("nmr") +
  labs(title = "Countries with the Highest Neonatal Mortality Rate", 
       subtitle = "Year 2015",
       x = "Countries",
       y = "Neonatal Mortality Rate",
       caption = "Data from World Bank through the `WDI` package")

im_plot3 <- im %>%
  filter(year == 2015) %>%
  highest_mortality("imr") +
  labs(title = "Countries with the Highest Infant Mortality Rate", 
       subtitle = "Year 2015",
       x = "Countries",
       y = "Infant Mortality Rate",
       caption = "Data from World Bank through the `WDI` package")

u5m_plot3 <- u5m %>%
  filter(year == 2015) %>%
  highest_mortality("u5mr") +
  labs(title = "Countries with the Highest Under-five Mortality Rate", 
       subtitle = "Year 2015",
       x = "Countries",
       y = "Under-five Mortality Rate",
       caption = "Data from World Bank through the `WDI` package")



# does the MDG have impact childhood death rates? ---------------

# a) globally
# 
annual_u5death_decline <- u5data %>% 
  group_by(year) %>%
  summarise(annual_death = signif((sum(u5_death, na.rm = TRUE))/10^6, 4)) %>%
  filter(annual_death != 0)

global_stat <- map(list(nm, im, u5m), tidy_stat, alt = "less", paired = TRUE) %>%
  reduce(dplyr::full_join) %>% 
  mutate(mortality_rate = c("neonatal", "infant", "under-five")) %>% 
  select(mortality_rate, statistic, p.value)

# b) Nigeria

child_mr <- list(nm %>% filter(country == "Nigeria"),
                 im %>% filter(country == "Nigeria"),
                 u5m %>% filter(country == "Nigeria"))

nig_stat <- map(child_mr, test_stat, alt = "less") %>%
  reduce(dplyr::full_join) %>%  
  mutate(mortality_rate = c("neonatal", "infant", "under-five")) %>% 
  select(mortality_rate, statistic, p.value)

# despite the significant improvement, childhood mortality rates in Nigeria is still significantly high compared to some other countries in Africa
# 
# in 2015, a total of ----- under-five deaths occured in Nigeria

nig_u5death_2015 <- u5data %>%
  filter(country == "Nigeria" & year == 2015) %>%
  select(year, country, u5mr, u5_death, u5_pop) %>%
  mutate(u5_death = signif((u5_death / 10^6), 3))


# plot trend in mortality rate in Nigeria  ----------------------------------

nm_trend <- nm %>%
  filter(country == "Nigeria") %>%
  trend_plot("nmr") + 
  labs(title = "Neonatal Mortality Trend in Nigeria", 
       subtitle = "1990 - 2015",
       y = "neonatal mortality rate")

im_trend <- im %>% 
  filter(country == "Nigeria") %>%
  trend_plot("imr") + 
  labs(title = "Infant Mortality Trend in Nigeria", 
       subtitle = "1990 - 2015",
       y = "infant mortality rate")

u5m_trend <- u5m %>% 
  filter(country == "Nigeria") %>%
  trend_plot("u5mr") + 
  labs(title = "Under-five Mortality Trend in Nigeria", 
       subtitle = "1990 - 2015",
       y = "under-five mortality rate")




# finally, i will be parting with this animated chart. Two countries (apart from Nigieria) are of particular interest: Niger (highest under-five mortality rate in 1990), and Haiti (highest under-five mortality rate in 2010)
# 
# 
animate <- u5data %>% 
  filter(year == 1990 | year == 1995 | year == 2000 | year == 2005 | year == 2010 | year == 2015) %>%
  mutate(u5_pop = signif((u5_pop / 10^3), 4)) %>% # convert to per million
  mutate(u5_death = signif((u5_death / 10^6), 4)) %>% # convert also to per million
  ggplot(aes(x = u5_death, y = u5mr, color = continent)) + 
  geom_point(aes(size = u5_pop, frame = year, ids = country, na.rm = TRUE), alpha = .3, show.legend = FALSE) +
   scale_x_continuous(limits = c(0, 16), breaks = seq(0, 16, 2)) +
  scale_y_continuous(limits = c(0, 350), breaks = seq(0, 350, 50)) +
  scale_size(range = c(1, 10)) +
  labs(x = "no of under-five deaths (in millions)",
       y = "mortality rate ('000)", 
       title = "Trends in under-five mortality") +
  theme_minimal()


ggplotly(animate, tooltip = c("country", "u5_pop", "u5_death", "u5mr")) %>%
  animation_opts(2000, easing = "linear") %>%
  animation_button(
    x = 1, xanchor = "right", y = 0, yanchor = "bottom"
  ) 
