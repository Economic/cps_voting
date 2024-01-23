library(tidyverse)

source("misc_functions.R")

# get one of year of data
one_year_data <- merge_epi_supplement(2022)

# get multiple years of data
supplement_years <- seq(1996, 2022, 2)

multiple_years_data <- map(supplement_years, merge_epi_supplement) %>% 
  list_rbind()

# unweighted counts of pes1 by year
# to confirm matches with attachment 13 in CPS documentation
multiple_years_data %>% 
  count(year, pes1) %>% 
  pivot_wider(id_cols = pes1, names_from = year, values_from = n)
