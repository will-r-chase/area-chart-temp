library(outbreakinfo)
library(tidyverse)
library(lubridate)
#devtools::install_github("outbreak-info/R-outbreak-info", ref="dev")

#GISAID reg
authenticateUser()
#(s9X4KdY

#area chart
us_prevalence <- getAllLineagesByLocation(location = "United States", other_threshold = 0.01, nday_threshold = 10, ndays = 100)
percentize <- function(x)(x * 100)

data_df <- us_prevalence %>% 
  as.data.frame() %>% 
  filter(ymd(date) >= ymd("2021-01-01")) %>%
  filter(ymd(date) <= ymd(today() - 13)) %>% 
  select(lineage, date, prevalence_rolling) %>%
  pivot_wider(names_from = lineage, values_from = prevalence_rolling) %>%
  mutate_if(is.numeric, percentize) %>%
  rowwise() %>%
  mutate(ba1 = sum(across(starts_with("ba.1")), na.rm = TRUE),
         ba2 = sum(across(starts_with("ba.2")), na.rm = TRUE),
         ) %>%
  select(date, other, ba1, ba2)

write_csv(data_df, "variants_area.csv")
