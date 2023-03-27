## Clean data merge
library(tidyverse)
library(here)
library(haven)

# Read in the data
covid <- read_dta(here("data", "covid.dta")) |>
  arrange(state, level, year, month, age0)
noncovid <- read_dta(here("data", "noncovid.dta")) |>
  arrange(country, state, age0, age)

# Join to the  table by age0
# left_join() will have the same effect
alldata <- full_join(covid, noncovid,
                     by = c("state", "age0"),
                     relationship = "many-to-many") |>
  relocate(age, .before = covid) |>
  arrange(year, state, month, age0, age) |>
  # fill pop19 and uspop downwards for ages 85>
  # These will be NA otherwise
  fill(pop19, uspop)

alldata

save(alldata, file = here("out", "alldata.Rdata"))
write_csv(alldata, here("out", "merged_table.csv"))

