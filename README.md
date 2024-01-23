# merge CPS voting supplements to EPI CPS extracts
Currently works for 1996-2022 CPS Voting Supplements

## file descriptions

-   misc_functions.R: contains several functions to download CPS voting supplement data and merge it to EPI CPS Basic extracts

-   main.R: shows how to use the functions

## package requirements

-   tidyverse
-   epiextractr (also download CPS Basic)
-   censusapi (also set up Census API key as variable in .Renviron)
