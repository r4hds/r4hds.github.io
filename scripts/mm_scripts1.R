##############################################################################
#                            
#                            SCRIPT I
#                   Maternal Mortality Data Analysis
#             Data source: WorldBank Development Indicator API
#             Date:       March 24, 2017
#             
#############################################################################



###############################################################################
#                           PART I
#                    Downloading the data
###############################################################################


# loading the required libraries  ------------------------------------------

suppressMessages(
  c(
    library("WDI"),
    library("readr")
    )
)


# downloading the data ----------------------------------------------------

mr_maternal <- WDI(country = "all", 
                   indicator = "SH.STA.MMRT", 
                   start = 1990, 
                   end = 2015, 
                   extra = TRUE, 
                   cache = NULL)


## b) maternal deaths

deaths_maternal <- WDI(country = "all", 
                       indicator = "SH.MMR.DTHS", 
                       start = 1990, end = 2015, 
                       extra = TRUE, 
                       cache = NULL)   
  ### ---------------------  end of part I  -------------------------- ###



###############################################################################
#                           PART II
#                       Saving the data
###############################################################################

write_csv(mr_maternal, "./scripts/maternal_mortality")
write_csv(deaths_maternal, "./scripts/maternal_mortality")
write_csv(mr_maternal_cre, "./scripts/maternal_mortality_cre")

  ### ---------------------  end of part I I -------------------------- ###




c <- deaths_maternal %>%
  filter(region != "Aggregates") %>%
  group_by(year) %>%
  summarise(sum(SH.MMR.DTHS, na.rm = TRUE))
