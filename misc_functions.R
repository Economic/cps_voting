# functions below also require packages
# censusapi
# epiextractr

# character vector of voting variables, by year
get_voting_variables <- function(year) {
  # voting variables 2014-2020, see attachment 7 of 
  # https://www2.census.gov/programs-surveys/cps/techdocs/cpsnov14.pdf
  # https://www2.census.gov/programs-surveys/cps/techdocs/cpsnov16.pdf
  # https://www2.census.gov/programs-surveys/cps/techdocs/cpsnov18.pdf
  # https://www2.census.gov/programs-surveys/cps/techdocs/cpsnov20.pdf
  if (year %in% seq(2014, 2020, 2)) {
    voting_variables <- c(
      "PES1",
      "PES2",
      "PES3",
      "PES4",
      "PES5",
      "PES6",
      "PES7",
      "PRS8",
      "PUSCK4"
    )
  }
  # voting variables 2022, see attachment 7 of 
  # https://www2.census.gov/programs-surveys/cps/techdocs/cpsnov22.pdf
  if (year == 2022) {
    voting_variables <- c(
      "PRSUPINT",
      "PES1",
      "PES2",
      "PES3",
      "PES4",
      "PES5",
      "PES6",
      "PES7",
      "PRS8",
      "PESCK4"
    )
  }
  
  voting_variables
}

download_voting_supplement <- function(year) {
  # name the variables to download
  
  # raw variables used to merge EPI CPS extracts
  id_variables <- c("GESTFIPS", "HRHHID", "HRHHID2", "PULINENO")
  
  # voting supplement variables
  voting_variables <- get_voting_variables(year)
  
  # use Census API to download variables for specific year of supplement
  censusapi::getCensus(
    name = "cps/voting/nov",
    vintage = year,
    vars = c(id_variables, voting_variables)
  ) %>% 
    janitor::clean_names() %>% 
    mutate(
      pulineno = as.numeric(pulineno),
      gestfips = as.numeric(gestfips)
    ) 
}

merge_epi_supplement <- function(year) {
  
  # voting supplement data
  voting_supplement <- download_voting_supplement(year)
  
  # EPI CPS Basic data, via epiextractr
  epi_basic <- epiextractr::load_basic(year) %>% 
    filter(month == 11) %>% 
    mutate(
      hrhhid2 = as.numeric(hrhhid2),
      gestfips = as.numeric(statefips)
    )
  
  epi_basic %>% 
    inner_join(
      voting_supplement, 
      relationship = "one-to-one", 
      unmatched = "error",
      by = join_by(hrhhid, hrhhid2, pulineno, gestfips)
    )
}