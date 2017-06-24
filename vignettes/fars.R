## ----eval = FALSE--------------------------------------------------------
#  devtools::install_github("https://github.com/friveramariani/fars")
#  library(fars)

## ----eval = FALSE--------------------------------------------------------
#  data <- fars_read(filename = "./accident_2013.csv.bz2")

## ----eval = FALSE--------------------------------------------------------
#  summarize_years<-fars_summarize_years(years=2013:2015)

## ----eval = FALSE--------------------------------------------------------
#  fars_map_state(40, 2014)
