
# download Public Debt Transactions table ---------------------------------
require(tidyverse)
require(tsibble)

#baseurl <- "https://www.transparency.treasury.gov/services/api/fiscal_service"
baseurl <- "https://api.fiscaldata.treasury.gov/services/api/fiscal_service"


# Debt subject to limit TABLE 3c

# There is a max limit of 10000 per query
nmax <- 10000
start_date <- "2005-03-10"

table <- "/v1/accounting/dts/dts_table_3c?"
fields <- "fields=record_date,debt_catg,debt_catg_desc,close_today_bal"
format <- "&format=json"
pagination <- paste0("&page[number]=1&page[size]=", nmax)

ncount <- nmax
res <- vector("list")
i <- 1

while (ncount >= nmax) {
  filter <- paste0("&filter=record_date:gte:",start_date)
  furl <- paste0(baseurl, table, fields, filter, format, pagination)
  
  request <- httr::GET(furl)
  tmp <- jsonlite::fromJSON(httr::content(request, "text", encoding = "UTF-8"))
  
  res[[i]] <- as_tibble(tmp$data) 
  
  start_date <- tail(unique(tmp$data$record_date),2)[1]
  ncount <- tmp$meta$count
  i <- i + 1
}

res <- reduce(res, bind_rows)
res <- distinct(res)

res <- res %>% 
  mutate(
    record_date = as.Date(record_date),
    close_today_bal = parse_integer(close_today_bal, na = c("", "NA", "null"))
    )

debt_public <- res %>% 
  filter(debt_catg == "Debt Held by the Public") %>% 
  select(-debt_catg_desc) %>% 
  transmute(index = record_date, key = debt_catg, value = close_today_bal)

debt_intra  <- res %>% 
  filter(debt_catg == "Intragovernmental Holdings") %>% 
  select(-debt_catg_desc) %>% 
  transmute(index = record_date, key = debt_catg, value = close_today_bal)

debt_total <- res %>% 
  filter(debt_catg %in% c("Debt Held by the Public", "Intragovernmental Holdings")) %>% 
  select(-debt_catg_desc) %>% 
  pivot_wider(names_from = debt_catg, values_from = close_today_bal) %>% 
  mutate(close_today_bal = `Debt Held by the Public` + `Intragovernmental Holdings`) %>% 
  transmute(index = record_date, key = "Total Public Debt", value = close_today_bal)

debt_exc_limit <- res %>% 
  filter(debt_catg == "Less Debt Not Subject to Limit") %>% 
  group_by(record_date) %>% 
  summarize(close_today_bal = -sum(close_today_bal), .groups = "drop_last") %>% 
  transmute(index = record_date, key = "Debt Not Subject to Limit", value = close_today_bal)

debt_limit <- res %>% 
filter(debt_catg == "Statutory Debt Limit") %>% 
  select(-debt_catg_desc) %>% 
  transmute(index = record_date, key = debt_catg, value = close_today_bal)


rm(request, res, tmp, baseurl, fields, filter, format, furl, i, ncount, nmax, pagination, start_date, table)
