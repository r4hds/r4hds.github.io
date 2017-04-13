##############################################################################
#                            
#                            SCRIPT I
#             -----------------------------------------
#                   Infant Mortality Data Analysis
#             Data source: WorldBank Development Indicator API
#             Date:       March, 28, 2017
#             
#############################################################################



###############################################################################
#                           PART I
#                    Downloading the data
###############################################################################


# loading the required libraries  ------------------------------------------

suppressMessages(c(library("WDI"),
                   library("readr")
                   ))

# download the data -------------------------------------------------------

# (a) Dowload the infant mortality rate data
mr_infant <- WDI(country = "all", 
                 indicator = "SP.DYN.IMRT.IN", 
                 start = 1990, end = 2015, 
                 extra = TRUE, 
                 cache = NULL)  

# (b) Download the under-five mortality rate data
mr_u5 <- WDI(country = "all", 
             indicator = "SH.DYN.MORT", 
             start = 1990, end = 2015, 
             extra = TRUE, 
             cache = NULL)  

# (c) Download the neonatal mortality rate data
mr_neonatal <- WDI(country = "all", 
                   indicator = "SH.DYN.NMRT", 
                   start = 1990, end = 2015, 
                   extra = TRUE, 
                   cache = NULL)  

  ### ---------------------  end of part I  -------------------------- ###



###############################################################################
#                           PART II
#                     saving the data
###############################################################################

write_csv(mr_infant, "./scripts/infant_mortality")
write_csv(mr_u5, "./scripts/under_five_mortality")
write_csv(mr_neonatal, "./scripts/neonatal_mortality")

  ### ---------------------  end of part II  -------------------------- ###
