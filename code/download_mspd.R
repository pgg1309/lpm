
# download mspd table III ---------------------------------
# TABLE III - DETAIL OF TREASURY SECURITIES OUTSTANDING

require(tidyverse)
require(tsibble)

baseurl <- "https://api.fiscaldata.treasury.gov/services/api/fiscal_service"

# There is a max limit of 10000 per query
nmax <- 10000
start_date <- "2020-01-01"

table <- "/v1/debt/mspd/mspd_table_3?"
# fields <- "fields=record_date,debt_catg,debt_catg_desc,close_today_bal"
# fields <- "fields=security_class1_desc,security_class2_desc,issue_date,maturity_date,interest_pay_date_1,interest_pay_date_2,interest_pay_date_3,interest_pay_date_4,"
fields <- ""
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

# Adjust column types
# replace 'null' by 'NA', '*' by NA and '(*)' by NA
res <- res %>%
  mutate(across(everything(), ~ stringr::str_replace(.x, '^null$', 'NA')),
         across(everything(), ~ stringr::str_replace(.x, '^\\(\\*\\)$', 'NA')),
         across(everything(), ~ stringr::str_replace(.x, '^\\*$', 'NA')),
         ) %>%
  mutate(
    record_date                   = as.Date(record_date),
    interest_rate_pct             = parse_number(interest_rate_pct),
    yield_pct                     = parse_number(yield_pct),
    issue_date                    = as.Date(issue_date),
    maturity_date                 = as.Date(maturity_date),
    issued_amt                    = parse_number(issued_amt),
    inflation_adj_amt             = parse_number(inflation_adj_amt),
    redeemed_amt                  = parse_number(redeemed_amt),
    outstanding_amt               = parse_number(outstanding_amt),
    prior_month_outstanding_amt   = parse_number(prior_month_outstanding_amt),
    current_month_issued_amt      = parse_number(current_month_issued_amt),
    current_month_redeemed_amt    = parse_number(current_month_redeemed_amt),
    current_month_outstanding_amt = parse_number(current_month_outstanding_amt),
    src_line_nbr                  = parse_integer(src_line_nbr),
    record_fiscal_year            = parse_integer(record_fiscal_year),
    record_fiscal_quarter         = parse_integer(record_fiscal_quarter),
    record_calendar_year          = parse_integer(record_calendar_year),
    record_calendar_quarter       = parse_integer(record_calendar_quarter),
    record_calendar_month         = parse_integer(record_calendar_month),
    record_calendar_day           = parse_integer(record_calendar_day)
  )



# debt_public <- res %>%
#   filter(security_class1_desc == "Debt Held by the Public") %>%
#   select(-debt_catg_desc) %>%
#   transmute(index = record_date, key = debt_catg, value = close_today_bal)
#
# debt_intra  <- res %>%
#   filter(debt_catg == "Intragovernmental Holdings") %>%
#   select(-debt_catg_desc) %>%
#   transmute(index = record_date, key = debt_catg, value = close_today_bal)
#
# debt_total <- res %>%
#   filter(debt_catg %in% c("Debt Held by the Public", "Intragovernmental Holdings")) %>%
#   select(-debt_catg_desc) %>%
#   pivot_wider(names_from = debt_catg, values_from = close_today_bal) %>%
#   mutate(close_today_bal = `Debt Held by the Public` + `Intragovernmental Holdings`) %>%
#   transmute(index = record_date, key = "Total Public Debt", value = close_today_bal)
#
# debt_exc_limit <- res %>%
#   filter(debt_catg == "Less Debt Not Subject to Limit") %>%
#   group_by(record_date) %>%
#   summarize(close_today_bal = -sum(close_today_bal), .groups = "drop_last") %>%
#   transmute(index = record_date, key = "Debt Not Subject to Limit", value = close_today_bal)
#
# debt_limit <- res %>%
# filter(debt_catg == "Statutory Debt Limit") %>%
#   select(-debt_catg_desc) %>%
#   transmute(index = record_date, key = debt_catg, value = close_today_bal)
#

rm(request, tmp, baseurl, fields, filter, format, furl, i, ncount, nmax, pagination, start_date, table)
