# Load/install packages 
if (!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "rio", "countrycode", "skimr", "wbstats")

# Load data: Eurobarometer 89.3 (2018)
# from: https://search.gesis.org/research_data/ZA7483
source.df <- import("./data/ZA7483_v1-0-0.dta")

# Remove variables from other modules (QB and QC)
eb.df <- source.df %>%
  select(!matches("^q(b|c)")) %>%
  mutate(across(c(contains("qa12_"), qa20, qa21), ~ifelse(.x == 5, 
                                                          NA_integer_, 
                                                          .x)),
         iso3cntry = countrycode(isocntry, "iso2c", "iso3c", 
                                 custom_match = c("DE-E" = "East Germany",
                                                  "DE-W" = "DEU"))) # West Germany

# Look at attitudes
eb.df %>%
  mutate(across(contains("qa12_"), ~ifelse(.x == 5, NA_integer_, .x))) %>%
  select(contains("qa12_"), qa20, qa21, schengen_att, isocntry) %>%
  group_by(isocntry) %>%
  skim() %>% 
  ungroup() %>%
  mutate(isocntry = countrycode(isocntry, "iso2c", "country.name.en", 
                                custom_match = c("DE-E" = "East Germany",
                                                 "DE-W" = "West Germany"))) %>%
  arrange(skim_variable, numeric.mean)

# Correlations
eb.df %>%
  select(c(contains("qa12_"), "qa20", "qa21")) %>%
  na.omit() %>%
  GGally::ggcorr(label = TRUE)

# Composite measure of attitudes toward Schengen
eb.df <- eb.df %>%
  rowwise() %>%
  mutate(schengen_att = mean(c_across(contains("qa12_"))))

# Socio-economic background: 
# - occupation (d15a)
# - education (d8)

# Controls:
# - age (d11)
# - marital status (d7)
# - household composition (d40a)
# - left-right placement (d1)
# - gender (d10)

# Correlation between schengen_att and age
eb.df %>%
  select(d11, schengen_att, isocntry) %>%
  na.omit() %>%
  group_by(isocntry) %>%
  summarise(correlation = cor(schengen_att, d11)) %>%
  view()

# 
ggplot(eb.df, aes(x = d11, y = schengen_att)) +
  geom_smooth(method = "lm") +
  geom_point() +
  facet_wrap(~isocntry)

# Macro indicators
# KOF indicator
# Load
kof.df <- import("https://ethz.ch/content/dam/ethz/special-interest/dual/kof-dam/documents/Medienmitteilungen/Globalisierungsindex/KOFGI_2020_public.dta")

# Limit to year == 2018 and Eurobarometer countries
kof.df <- kof.df %>%
  filter(year == 2018,
         code %in% eb.df$iso3cntry)

# World Bank Indicators
wb.info <- wb_data(country = unique(eb.df$iso3cntry),
                   indicator = c("NY.GDP.PCAP.CD", "SP.POP.TOTL", "SI.POV.GINI"), 
                   start_date = 2018, end_date = 2018, return_wide = TRUE)

# Join to eb.df
eb.df <- eb.df %>%
  left_join(y = kof.df, by = c("iso3cntry" = "code")) %>%
  left_join(y = wb.info, by = c("iso3cntry" = "iso3c"))
