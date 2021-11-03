# Load/install packages 
if (!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "rio", "countrycode")

# Load data: Eurobarometer 89.3 (2018)
source.df <- import("./data/ZA7483_v1-0-0.dta")

# Remove variables from other modules (QB and QC)
eb.df <- source.df %>%
  select(!matches("^q(b|c)")) %>%
  mutate(across(c(contains("qa12_"), "qa20", "qa21"), ~ifelse(.x == 5, NA_integer_, .x)))

# Look at attitudes
eb.df %>%
  mutate(across(contains("qa12_"), ~ifelse(.x == 5, NA_integer_, .x))) %>%
  select(contains("qa12_"), isocntry) %>%
  group_by(isocntry) %>%
  skim() %>% 
  ungroup() %>%
  mutate(isocntry = countrycode(isocntry, "iso2c", "country.name.en", 
                                custom_match = c("DE-E" = "East Germany",
                                                 "DE-W" = "West Germany")))
# Correlations
eb.df %>%
  select(c(contains("qa12_"), "qa20", "qa21")) %>%
  na.omit() %>%
  GGally::ggcorr(label = TRUE)
