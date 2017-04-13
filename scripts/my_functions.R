# load required libraries -------------------------------------------------

suppressMessages(
  c(
    library(magrittr),
    library(tidyverse),
    library(WDI),
    library(wpp2015),
    library(countrycode),
    library(stringr),
    library(lubridate),
    library(car),
    library(forcats),
    library(ggfortify),
    library(ggExtra),
    library(GGally),
    library(ggpmisc),
    library(ggrepel),
    library(plotly),
    library(leaps),
    library(psych),
    library(broom),
    library(modelr)
    
  )
)


# data cleaning and preprocessing -----------------------------------------

clean_data <- function(data, var) {
  wrapr::let(
    alias = list(value = var),
    expr = {
      ## use the countrycode package to get the full names of countries and their continent
      cc <- countrycode_data %>% select(iso3c, continent) 
      
      names(data) <- str_to_lower(names(data))  ## change the name title to lower letters
      data <- data %>% 
        filter(region != "Aggregates") %>%
        select(year, country, iso3c, value) %>% ## select important variables
        left_join(cc, by = c("iso3c")) %>% ## join the "cc" dataframe to the "data" dataframe
        select(-value, value) %>% 
        drop_na(continent) %>%
        mutate(mdg_years = ifelse(.$year <= 2000, "no", "yes"))
      return(data)
    })
}



factorise <- function(df) {
  df %>%
    mutate(continent = as_factor(.$continent))%>%
    mutate(iso3c = as_factor(.$iso3c)) %>% 
    mutate(country = as_factor(.$country)) %>% 
    mutate(mdg_years = as_factor(.$mdg_years)) %>%
    group_by(country) %>% 
    arrange(country,desc(year))
}


difference <- function(d) {
  output <- vector("double", length(d))
  for(i in seq_along(d)) {
    output[i] <- d[i+1] - d[i]
  }
  output
}


add_decline <- function(df, var, na.rm = TRUE) {
  wrapr::let(
    alias = list(value = var),
    expr = {
      df %>%
        arrange(desc(year)) %>%
        mutate(annual_decline = difference(value)) %>%
        mutate(annual_decline_rate = round((annual_decline / value * 100), 2))
    }
  )
  
}


highest_mortality <- function(df, var) { 
  wrapr::let(
    alias = list(value = var),
    expr = {
      df %>%
        ungroup() %>%
        arrange(desc(value)) %>%
        slice(1:20) %>%
        mutate(position = 1:n()) %>%
        mutate(country = fct_reorder(country, value)) %>%
        ggplot(aes(x = country, y = value, label = position)) + 
        geom_bar(stat = "identity", width = .5, fill = "lightblue") + 
        theme_minimal() +
        coord_flip()
    }
  )
  
}



test_stat <- function(df, alt = "two.sided", paired = FALSE) {
  t.test(annual_decline_rate ~ mdg_years, alternative = alt, paired = paired, data = df) %>%
    tidy() %>%
    select(estimate1, estimate2, statistic, p.value) %>%
    rename(before_mdg = estimate1) %>%
    rename(after_mdg = estimate2)
}


# tidy_stat <- function(df, alt = "two.sided", paired = FALSE) {
#  df %>%
#     group_by(country, mdg_years) %>%
#     # arrange(desc(year)) %>%
#     summarise(rate = mean(annual_decline_rate, na.rm = TRUE)) %>%
#      test_stat(alt = alt, paired = paired) %>% 
#      tidy() %>%  
#      select(estimate, statistic, p.value)
# }

tidy_stat <- function(df, alt = "two.sided", paired = FALSE) {
  df <- df %>%
    group_by(country, mdg_years) %>%
    summarise(rate = mean(annual_decline_rate, na.rm = TRUE))
    t.test(rate ~ mdg_years, alternative = alt, paired = paired, data = df) %>% 
    tidy() %>%  
    select(estimate, statistic, p.value)
}


trend_plot <- function(df, var) {
  wrapr::let(
    alias = list(value = var),
    expr = {
      df %>%
        mutate(year = make_date(year)) %>%
        ggplot(aes(x = year, y = value)) +
        geom_point(alpha = .5, size = 2) + 
        geom_line(color = "blue", alpha= .5) + 
        geom_smooth(method = "lm", se = F, color = "green", size = .5, alpha = .5) +
        theme_minimal() + 
        scale_x_date("years", date_breaks = "2 year", date_labels = "%Y", minor_breaks = NULL)
    }
  )
}


country_model <- function(df) {
  loess(mmr ~ year, data = df)
}


u5_pop <- function(data) {
  cc <- countrycode::countrycode_data %>%
    select(continent, iso3c, country.name.en) %>%
    rename(country = country.name.en)
  
  data %>%
    as_tibble() %>%
    gather(year, pop, -c(country, age)) %>%
    spread(age, pop) %>%
    select(country, year, `0-4`) %>% 
    rename(under_five = `0-4`) %>%
    left_join(cc, by = c("country")) %>%
    drop_na() %>%
    filter(year != "country_code") %>%
    select(year, continent, country, iso3c, everything())
}



model_nig <- function(df) {
  df %>% 
  filter(country == "Nigeria" & mdg_years == "no") %>%
    lm(mortality_rate ~ year, data = .)
  # loess(mortality_rate ~ year, data = ., control = loess.control(surface = "direct"))
}


model_fit <- function(df) {
  df %>%
  filter(country == "Nigeria") %>%
  add_predictions(model(df), "mortality_r") %>%
  add_residuals(model(df), "residual") %>%
  select(-c(mortality_rate, predicted_mr, residual), predicted_mr, mortality_rate, residual) %>%
  rename(actual_mr = mortality_rate) %>% 
  mutate(predicted_mr = round(.$predicted_mr, 2)) %>%
  mutate(residual = round(.$residual, 2))
}



lowest_mortality <- function(df, var) {
  wrapr::let(
    alias = list(value = var),
    expr = {
      df %>%
        filter(continent == "Africa") %>% 
        arrange(desc(year), value) %>%
        ungroup() %>% 
        slice(1:5) %>% 
        select(year, country, value)
    }
  )
  
}
