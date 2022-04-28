
# Loads and prepare eurodollar --------------------------------------------

require(fpp3)
require(tidyverse)

eurodollar <- readxl::read_excel(
  "E:/RData/DATASTREAM/EurodollarRequestTable.xlsm",
  sheet = "database",
  col_types = c("date", rep("text", 367)),
  na = "NA",
  skip = 1)

eurodollar <- eurodollar %>%
  mutate(Code = as_date(Code)) %>%
  pivot_longer(-Code) %>%
  mutate(name = str_remove(name, "\\(PS\\)"),
         value = parse_number(value)) %>%
  drop_na() %>%
  rename(day = Code)

# read metadata
meta <- readxl::read_excel(
  "E:/RData/DATASTREAM/EurodollarMetadata.xlsm",
  sheet = "metadata",
  col_types = c("date", "text", "text", "date", "date", "text", "text"),
  na = "NA")

meta <- meta %>%
  transmute(
    Data = as_date(Data),
    name = Ticker,
    last_trade = as_date(`LAST TRADING DATE`),
    first_trade = as_date(`FUT.1ST PRICE DATE`),
    full_name = NAME) %>%
  drop_na()

eurodollar <- left_join(eurodollar, select(meta, -Data), by = 'name')
write_rds(eurodollar, "data/eurodollar.rds")
rm(eurodollar, meta)
