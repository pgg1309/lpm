
# Loads and prepare fed funds future --------------------------------------------

require(fpp3)
require(tidyverse)

fedfunds <- readxl::read_excel(
  "E:/RData/DATASTREAM/FedfundsRequestTable.xlsm",
  sheet = "database",
  col_types = c("date", rep("text", 410)),
  na = "NA",
  skip = 1)

fedfunds <- fedfunds %>%
  mutate(Code = as_date(Code)) %>%
  pivot_longer(-Code) %>%
  mutate(name = str_remove(name, "\\(PS\\)"),
         value = parse_number(value)) %>%
  drop_na() %>%
  rename(day = Code)

# read metadata
meta <- readxl::read_excel(
  "E:/RData/DATASTREAM/FedfundsMetadata.xlsm",
  sheet = "metadata",
#  col_types = c("date", "text", "text", "date", "date", "text", "text"),
  na = "NA")

meta <- meta %>%
  transmute(
    Data = as_date(Data),
    name = Ticker,
    last_trade = as_date(`LAST TRADING DATE`),
    first_trade = as_date(`FUT.1ST PRICE DATE`),
    full_name = NAME) %>%
  drop_na()

fedfunds <- left_join(fedfunds, select(meta, -Data), by = 'name')
write_rds(fedfunds, "data/fedfunds.rds")
rm(fedfunds, meta)
