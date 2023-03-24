## Covid Life Table merge example
## Kieran Healy
## 2023-03-24

## If necessary install these packages:
# install.packages("tidyverse")
# install.packages("here")
# install.packages("janitor")
# install.packages("readxl")
# install.packages("curl")

library(tidyverse)
library(here)
library(janitor)

## CDC Deaths table has full name of state; CDC Wonder has state abbrevs
## State names/abbreviation crosswalk
state_names <- read_csv("https://raw.githubusercontent.com/kjhealy/fips-codes/master/state_fips_master.csv") |>
  select(state_name:state_abbr)

## CDC Wonder Monthly COVID data as of 2023-03-24
## Queried from WONDER and saved locally
deaths <- read_tsv(here("data", "cdc_covid_provisional.txt"),
                   na = "Not Applicable") |>
  clean_names() |>
  select(-population,crude_rate) |>
  mutate(month_ym = ym(month_code), .after = month_code) |>
  left_join(state_names, by = join_by("residence_state" == "state_name")) |>
  relocate(state_abbr)


## CDC 2020 Life Tables by State
## These are available as individual Excel files in an FTP directory.
## We'll get them all at once remotely.

## First, open a connection to get the file names
ftp_url <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Publications/NVSR/71-02/"
list_files <- curl::new_handle()
curl::handle_setopt(list_files, ftp_use_epsv = TRUE, dirlistonly = TRUE)
con <- curl::curl(url = url, "r", handle = list_files)
files <- readLines(con)
close(con)

## The files ending in "1" are the overall life tables
## The others are for M/F etc. We'll just use the 1s here.
fname_stubs <- files[str_detect(files, "1")]
fname_labs <- substr(fname_stubs, start = 1, stop = 2)

## Construct the filenames and give them a name attribute of the 2-letter state
## abbreviation, so they are trackable in the data frame we're about to make.
http_base <- "https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Publications/NVSR/71-02/"
fnames <- paste0(http_base, fname_stubs)
fnames <- set_names(fnames, fname_labs)

## Get the Excel files.
## Unfortunately read_xlsx() can't take a URL as a path so we
## write a function do it ourselves, and clean what comes down.
get_lifetable <- function(x) {
  httr::GET(x, httr::write_disk(tf <- tempfile(fileext = ".xlsx")))
  readxl::read_xlsx(tf, skip = 2) |>
    rename("age" = `...1`) |>
    filter(!str_detect(age, "SOURCE")) # remove trailing source line
}

## Now get all the states in one step
life_tabs <- fnames |>
  map_dfr(~ get_lifetable(.x),
          .id = "state")

## Neat
life_tabs

## Next we need a bridging table.
## Unix sort order messes up our age categories, but
## `rep` is a surprisingly useful function.
agecat_xwalk <- tibble(age = c(0, 15:100, 1:4, 10:14, 5:9, NA),
                       five_year_age_groups_code = rep(unique(deaths$five_year_age_groups_code),
                                                       c(1, rep(5, 17), 1, rep(4, 1), rep(5, 2), 1)))
agecat_xwalk

## Now we clean the life tables and join the bridging table to them.
## NB: the "–" in these columns is a proper en-dash character not a "-" minus
lt_clean <- life_tabs |>
  rename(state_abbr = state) |>
  mutate(age = str_replace(age, " and over", "–over")) |>
  separate(age, into = c("age", "age_upper"), sep = "–") |>
  mutate(age = as.numeric(age)) |>
  left_join(agecat_xwalk, by = "age")

lt_clean

## Finally, we can join this to the deaths table
merged_table <- left_join(deaths, lt_clean,
                          by = c("state_abbr", "five_year_age_groups_code"),
                          relationship = "many-to-many")

## I think this is what we want?
merged_table |>
  select(state_abbr, month_code, five_year_age_groups, age, deaths, qx:ex)
