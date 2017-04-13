##############################################################################
#                            
#                            SCRIPT II
#                   Maternal Mortality Data Analysis
#             Data source: WorldBank Development Indicator API
#                         World Population Prospects 2015 API
#             Date: March 24, 2017
#             
############################################################################



###############################################################################
#                             PART I
#             loading required packages, functions and data
###############################################################################

source("./scripts/my_functions.R")

# read-in the downloaded data ---------------------------------------------

mr_maternal <- read_csv("./scripts/maternal_mortality")
deaths_maternal <- read_csv("./scripts/deaths_maternal")

### ---------------------  end of part I  -------------------------- ###



###############################################################################
#                              PART II
#                   Data cleaning and wrangling
##############################################################################

# clean and convert categorical variables to factors ----------------------

# clean the data
mm <- mr_maternal %>%  
  rename(mmr = SH.STA.MMRT) %>% 
  clean_data("mmr") %>%
  factorise() %>%
  add_decline("mmr")

mdeaths <- deaths_maternal %>% 
  rename(m_deaths = SH.MMR.DTHS) %>%
  clean_data("m_deaths") %>% 
  factorise()


## joining maternal mortality ratio (mm) data with that of maternal deaths (mdeaths)
mdata <- mm %>% 
  left_join(mdeaths, 
            by = c("year", "continent", "iso3c", "country", "mdg_years")) %>%
  select(-mdg_years, mdg_years) ## moves mdg_years to become the last variable


data("popF") # loads-in women population data from the wpp2015 package
# calculate the population of females within the reproductive years

women_pop <- popF %>%
  as_tibble() %>%   # converts the dataframe into a tibble (easier for working                          with tidyverse packages)
  select(- country_code) %>% # drops the country_code variable
  gather(year, pop, -c(country, age)) %>% # pulls all the "years" variable into a                                             single column
  spread(age, pop) %>%  # spread the "age" column into multiple columns
  mutate(women_pop = `15-19` + `20-24` + `25-29` + `30-34` + `35-39` + `40-44` +`45-49`) %>% 
  mutate(women_pop = signif((women_pop * 1000), 3)) %>%
  select(country, year, women_pop)

#change the class of year from character to numeric vector
women_pop$year <- parse_number(women_pop$year)

# data for maternal death and reproductive age group size
mdata_sub <- mdata %>%
  filter(year == 1990 | 
           year == 1995 | 
           year == 2000 | 
           year == 2005 | 
           year == 2010 |
           year == 2015) %>%
  left_join(women_pop, by = c("country", "year"))

### ---------------------  end of part II  -------------------------- ###



###############################################################################
#                         PART III
#               Analysing Maternal moratlity ratio
###############################################################################


# countries with highest mmr pre and during MDG ---------------------------

highest_mmr2000 <- mdata %>% 
  filter(year == 2000) %>% 
  ungroup() %>% 
  arrange(desc(mmr)) %>%
  slice(1)

highest_mmr2015 <- mdata %>% 
  filter(year == 2015) %>% 
  ungroup() %>% 
  arrange(desc(mmr)) %>%
  slice(1)

mr1 <- mdata %>%
  filter(year == 2000) %>%
  highest_mortality("mmr") +
  labs(title = "Countries with the Highest Maternal Mortality Ratio", 
       subtitle = "Year 2000",
       x = "Countries",
       y = "Maternal Mortality Ratio") +
  scale_y_continuous(limits = c(0, 3000), breaks = seq(0, 3000, by = 500))
#ggplotly(mr1)

mr2 <- mdata %>%
  filter(year == 2015) %>%
  highest_mortality("mmr") +
  labs(title = "Countries with the Highest Maternal Mortality Ratio", 
       subtitle = "Year 2015",
       x = "Countries",
       y = "Maternal Mortality Ratio") +
  scale_y_continuous(limits = c(0, 1500), breaks = seq(0, 1500, by = 500))
#ggplotly(mr2)


# check for significant reduction in mmr during MDG -----------------------
# start by saying 'despite a reduction in mmr from 1990 to 2015, how significant 
# has the MDG contribute to this decline statistically


# a) globally 

stat_global <- mdata %>% 
  tidy_stat(alt = "less", paired = TRUE)

# b) Nigeria

stat_nig <- mdata %>%
  filter(country == "Nigeria") %>%
  test_stat(alt = "less")
stat_nig


# trend in mmr in Nigeria ----------------------------------
# (remember to add the slope value to each of the charts later)

nig1 <- mdata %>%
  filter(country == "Nigeria" & mdg_years == "no") %>%
  trend_plot("mmr") + 
  labs(title = "Maternal Mortality Trend in Nigeria", 
       subtitle = "Before MDGs",
       y = "maternal mortality ratio")

nig2 <- mdata %>% 
  filter (country == "Nigeria" & mdg_years == "yes") %>% 
  trend_plot("mmr") + 
  labs(title = "Maternal Mortality Trend in Nigeria", 
       subtitle = "During MDGs",
       y = "maternal mortality ratio")

new("ggmultiplot", plots = list(nig1, nig2))

##  ------------------------- end of part III ----------------------------  ##



###############################################################################
#                         PART IV
#                 Analysing Maternal Deaths
###############################################################################

# top 20 countries with the highest number of maternal deaths
# a) at start of MDGs i.e year 2000
md1 <- mdata_sub %>%
  filter(year == 2000) %>%
  highest_mortality("m_deaths") +
  labs(title = "Countries with the Highest Maternal Deaths (year 2000)", 
       subtitle = "Year 2000",
       x = "Countries",
       y = "Maternal Mortality Ratio") +
  scale_y_continuous(limits = c(0, 120000), 
                     breaks = seq(0, 120000, by = 20000),
                     labels = scales::comma_format())
# ggplotly(md1)


# b) at the end of the MDGs
md2 <- mdata_sub %>%
  ungroup() %>%
  filter(year == 2015) %>%
  highest_mortality("m_deaths") +
  labs(title = "Countries with the Highest Maternal Deaths (year 2015)", 
       subtitle = "Year 2015",
       x = "Countries",
       y = "Maternal Mortality Ratio") +
  scale_y_continuous(limits = c(0, 60000), 
                     breaks = seq(0, 60000, by = 10000),
                     labels = scales::comma_format())
ggplotly(md2)


# global decrease in maternal deaths --------------------------------------

global_decline <- mdata %>% 
  group_by(year) %>%
  summarise(annual_maternal_deaths = signif(sum(m_deaths, na.rm = TRUE), 3))

## this is despite an increase in the  population of women of reproductive age-group by:

women_pop_growth <- mdata_sub %>% 
  group_by(year) %>%
  summarise(women_pop = signif(sum(women_pop, na.rm = TRUE), 3))

# accounting for the increase in the population of women, maternal deaths decreased by "this" percent:

standardised_mdeath <- women_pop_growth %>% 
  inner_join(global_decline, by = "year") %>%
  mutate(mdg_years = ifelse(year > 2000, "yes", "no")) %>%
  mutate("standardised(%)" = signif((annual_maternal_deaths / women_pop * 100), 2))

stat_death_decline <- tidy(t.test(`standardised(%)` ~ mdg_years, 
                                  data = standardised_mdeath, 
                                  alternative = "greater")) %>%
  select(estimate2, estimate1, statistic, p.value) %>% 
  rename(estimate_during_mdg = estimate1) %>%
  rename(estimate_before_mdg = estimate2)

# contribution of Nigeria to Maternal deaths ------------------------------

mdata2 <- mdata_sub %>%
  mutate(region = ifelse(country == "Nigeria", "Nigeria", "Globe")) %>%
  filter(year == 1990 | year == 2000 | year == 2015)

nig_globe <- mdata2 %>%
  group_by(year, region) %>%
  summarise(maternal_deaths = sum(m_deaths, na.rm = TRUE)) %>%
  spread(region, maternal_deaths) %>% 
  mutate(Globe = signif((Nigeria + Globe), 3)) %>%
  mutate("contribution(%)" = round((Nigeria / Globe * 100), 2))

nig_globe

mdata2$region <- str_replace(mdata2$region, "Globe", "Africa")

nig_africa <- mdata2 %>% 
  filter(continent == "Africa") %>% 
  group_by(year, region) %>%
  summarise(maternal_deaths = sum(m_deaths, na.rm = TRUE)) %>% 
  spread(region, maternal_deaths) %>%
  mutate(Africa = signif((Nigeria + Africa), 3)) %>%
  mutate(contribution = round((Nigeria / Africa * 100), 2))

nig_africa

# maternal deaths and reprod_age trend in nigeria ------------------------------
# (figure out how to make the label more appropriate with ggplotly)

nig_mdeath <- mdata %>%
  filter(country == "Nigeria") %>%
  mutate(year = make_date(year)) %>%
  ggplot(aes(x = year, y = m_deaths)) + 
  geom_point(alpha = .3) + 
  geom_point(data = mdata_sub %>%
               filter(country == "Nigeria") %>%
               mutate(year = make_date(year)),
             aes(x = year, label = women_pop, size = 2), 
             alpha = .5, color = "red", show.legend = FALSE) +
  geom_line(color = "lightblue") +
  labs(title = "Trend in maternal deaths in Nigeria", 
       y = "maternal deaths ('000)", 
       subtitle = "1990 to 2015") +
  scale_x_date("year", date_breaks = "5 years", date_labels = "%Y", minor_breaks = NULL) +
  scale_y_continuous(labels = scales::comma_format()) +
  theme_minimal()
nig_mdeath <- ggplotly(nig_mdeath, tooltip = c("year","m_deaths", "women_pop"))

ind_mdeath <- mdata %>%
  filter(country == "India") %>%
  mutate(year = make_date(year)) %>%
  ggplot(aes(x = year, y = m_deaths)) + 
  geom_point(alpha = .3) + 
  geom_point(data = mdata_sub %>% 
               mutate(year = make_date(year)) %>%
               filter(country == "India"), 
             aes(x = year, label = women_pop, size = 2), 
             color = "red", alpha = .5, show.legend = FALSE) +
  geom_line(color = "lightblue") +
  labs(title = "Trend in maternal deaths in India", 
       y = "no of maternal deaths (per thousands)", 
       caption = "from 1990 to 2015") +
  scale_x_date("year", date_breaks = "5 years", date_labels = "%Y", minor_breaks = NULL) +
  scale_y_continuous(labels = scales::comma_format()) +
  theme_minimal()
ind_mdeath <- ggplotly(ind_mdeath, tooltip = c("year","m_deaths", "women_pop"))


layout(
    subplot(nig_mdeath, ind_mdeath, nrows = 1, margin = 0.08), 
    xaxis = list(title = "years"), 
   yaxis = list(title = "maternal deaths"),
   xaxis2 = list(title = "years"),
  #  title = "Top 20 Countries with the highest maternal deaths", 
   titlefont = list(size = 17)
  )


animate <- mdata_sub %>% 
  ggplot(aes(x = m_deaths, y = mmr, color = continent)) + 
  geom_point(aes(size = women_pop, frame = year, ids = country, na.rm = TRUE), alpha = .3, show.legend = FALSE) +
  labs(x = "maternal deaths",
       y = "maternal mortality ratio", 
       title = "Trends in maternal mortality") +
  scale_x_continuous(limits = c(0, 160000), breaks = seq(0, 160000, 25000), 
                     labels = scales::comma_format()) +
  scale_size(range = c(1, 10)) +
  theme_minimal()

ggplotly(animate, 
         tooltip = c("country", "women_pop", "m_deaths", "mmr")) %>%
  animation_opts(2000, easing = "linear") %>%
  animation_button(
    x = 1, xanchor = "right", y = 0, yanchor = "bottom"
  ) 


lowest_4 <- mdata_sub %>% 
  filter(continent == "Africa" & mmr < 100 & year == 2015) %>% 
  bind_rows(filter(.data = mdata_sub, country == "Nigeria" & year == 2015)) %>%
  select(country, mmr) %>% 
  arrange(mmr)
### ---------------------  end of part IV  -------------------------- ###
