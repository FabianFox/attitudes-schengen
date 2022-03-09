# Load/install packages 
### ------------------------------------------------------------------------ ###
if (!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "rio", "countrycode", "skimr", "wbstats", "eurostat")

# Load data: Eurobarometer 89.3 (2018)
### ------------------------------------------------------------------------ ###
# from: https://search.gesis.org/research_data/ZA7483
source.df <- import("./data/ZA7483_v1-0-0.dta")

# Create weights for unified Germany (West + East)
### ------------------------------------------------------------------------ ###
# See: https://www.gesis.org/en/eurobarometer-data-service/survey-series/standard-special-eb/weighting-overview
# Create combined weight
eb.df <- source.df %>%
  mutate(w3a = w1,
         w3a = if_else(cntry_de == 1, 0, w3a),
         w3a = w3a + w3) 

# New country variable
eb.df <- eb.df %>%
  mutate(cntr_de = country,
         cntr_de = if_else(cntr_de == 14, 4, cntr_de))

# Adjust Stata labels
attributes(eb.df$w3a)$label <- "WEIGHT RESULT FROM TARGET - CNTR_DE"
attributes(eb.df$cntr_de)$label <- "NATION (GERMANY EAST+WEST)" 
attributes(eb.df$cntr_de)$format.stata <- "%8.0g"

attributes(eb.df$cntr_de)$labels <- attributes(eb.df$country)$labels

# Copy then adjust from original country variable
cntr_de.names <- names(attributes(eb.df$country)$labels)
cntr_de.names[[4]] <- "DE - GERMANY"
cntr_de.names[[9]] <- "GB - Great Britain"    
cntr_de.names[[12]] <- "ES - Spain"
cntr_de.names[[14]] <- "-"
names(attributes(eb.df$cntr_de)$labels) <- cntr_de.names

# Transform into R format
### ------------------------------------------------------------------------ ###
eb.df <- eb.df %>%
  inner_join(y = enframe(attributes(eb.df$cntr_de)$labels), by = c("cntr_de" = "value")) %>%
  mutate(cntr_de = name) %>%
  select(-name) %>%
  separate(cntr_de, into = c("nation", "nation_name"), sep = "-") %>%
  mutate(across(c("nation", "nation_name"), ~str_trim(.x)))

# Deal with NA
eb.df <- eb.df %>%
  select(!matches("^q(b|c)")) %>%
  mutate(across(c(contains("qa12_"), qa20, qa21), ~ifelse(.x == 5, 
                                                          NA_real_, 
                                                          .x)),
         iso3cntry = countrycode(nation, "iso2c", "iso3c"))

# Composite measure of attitudes toward Schengen
### ------------------------------------------------------------------------ ###
# Potential variables are:
tibble(
  variable = 
    source.df %>%
    select(c(contains("qa12_"), qa20, qa21)) %>%
    colnames(),
  question = source.df %>%
    select(c(contains("qa12_"), qa20, qa21)) %>%
    map_chr(., ~sjlabelled::get_label(.x)) %>%
    str_to_sentence(),
  values = source.df %>%
    select(c(contains("qa12_"), qa20, qa21)) %>%
    map(., ~sjlabelled::get_labels(.x, values = "p"))) %>%
  gt::gt()
  
# Create composite measure
eb.df <- eb.df %>%
  rowwise() %>%
  mutate(schengen_att = mean(c_across(contains("qa12_"))))

# Correlations
### ------------------------------------------------------------------------ ###
eb.df %>%
  select(c(contains("qa12_"), "qa20", "qa21")) %>%
  na.omit() %>%
  GGally::ggcorr(label = TRUE)

# Look at attitudes
eb.df %>%
  mutate(across(contains("qa12_"), ~ifelse(.x == 5, NA_integer_, .x))) %>%
  select(contains("qa12_"), qa20, qa21, schengen_att, iso3cntry) %>%
  group_by(iso3cntry) %>%
  skim() %>% 
  ungroup() %>%
  arrange(skim_variable, numeric.mean)

# Select variables
### ------------------------------------------------------------------------ ###
# Socio-economic background: 
# - occupation (d15a)
# - education (d8)
# - economic issues (d60)

# Controls:
# - age (d11)
# - marital status (d7)
# - household composition (d40a-c)
# - left-right placement (d1)
# - gender (d10)
# - rural/urban (d25)

# Macrolevel:
# - KOF globalization index
# - Gini coefficient

# Correlation between schengen_att and age
eb.df %>%
  select(d11, schengen_att, isocntry) %>%
  na.omit() %>%
  group_by(iso3cntry) %>%
  summarise(correlation = cor(schengen_att, d11)) %>%
  view()

# Left-right scale
eb.df %>%
  mutate(d1 = if_else(d1 %in% c(97, 98), NA_real_, d1)) %>%
  ggplot(., aes(x = d1, y = schengen_att)) +
  geom_smooth(method = "lm") +
  geom_point() +
  facet_wrap(~iso3cntry)

# Macro indicators
### ------------------------------------------------------------------------ ###
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

# Eurostat
# Eurostat country codes
eu_code <- countrycode(unique(eb.df$iso3cntry), origin = "iso3c", "eurostat")

# Variables: tessi190: Gini coefficient, demo_urespop: Population size,
#            tec00114: GDP pc, in PPS; tps00191: asylum applications (yearly)
vars <- c("tessi190", "demo_urespop", "tec00114", "tps00191")

eurostat.df <- vars %>%
  map(., ~get_eurostat(.x,  filters = list(geo = eu_code, time = 2018), time_format = "num"))

# Join to eb.df
eb.df <- eb.df %>%
  left_join(y = kof.df, by = c("iso3cntry" = "code")) %>%
  left_join(y = wb.info, by = c("iso3cntry" = "iso3c"))
