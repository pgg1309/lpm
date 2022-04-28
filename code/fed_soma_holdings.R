
# Download NY FED data ----------------------------------------------------
require(tidyverse)
baseurl <- "https://markets.newyorkfed.org/api/soma/"  # for SOMA



# Treasury Weighted Average Maturity (wam) ------------------------------------

# get all possible dates
asofdates <- jsonlite::fromJSON(paste0(baseurl, "asofdates/list.json"))
asofdates <- asofdates$soma$asOfDates

# treasury (bills + notesbonds + frn + tips)
wam_treasury <- vector("numeric", length(asofdates))
i <- 1 # contador
for (d in asofdates) {
  x <- jsonlite::fromJSON(paste0(baseurl, "tsy/wam/all/asof/", d, ".json"))$soma$wam
  wam_treasury[i] <- ifelse(is.null(x), NA_real_, x)
  i <- i + 1
}

# treasury notes and bonds only
wam_notesbonds <- vector("numeric", length(asofdates))
i <- 1 # contador
for (d in asofdates) {
  x <- jsonlite::fromJSON(paste0(baseurl, "tsy/wam/notesbonds/asof/", d, ".json"))$soma$wam
  wam_notesbonds[i] <- ifelse(is.null(x), NA_real_, x)
  i <- i + 1
}

# agency
wam_agency <- vector("numeric", length(asofdates))
i <- 1
for (d in asofdates) {
  x <- jsonlite::fromJSON(paste0(baseurl, "agency/wam/agency%20debts/asof/", d, ".json"))$soma$wam
  wam_agency[i] <- ifelse(is.null(x), NA_real_, x)
  i <- i + 1
}

# Average maturity chart
bind_rows(
  tibble(date = asofdates, wam = wam_treasury) %>%
    mutate(date = as_date(date), key = 'all treasury') %>%
    as_tsibble(index = date, key = 'key')
  ,
  tibble(date = asofdates, wam = wam_notesbonds) %>%
    mutate(date = as_date(date), key = 'notes and bonds only') %>%
    as_tsibble(index = date, key = 'key')
) %>%
  ggplot(aes(x = date, y = wam, color = key)) +
  geom_line(size = 1) +
  scale_y_continuous(
    breaks = scales::breaks_extended(n = 20),
    sec.axis = dup_axis(name = NULL)
  ) +
  scale_x_date(
    labels = scales::label_date_short(),
    breaks = scales::breaks_pretty(n = 12)
  ) +
  labs(
    title = "Weighted average maturity of Fed Treasury holdings",
    x = NULL,
    y = "years",
    color = NULL
  ) +
  theme(legend.position = 'bottom')

# Fed holdings and maturities -------------------------------------------------
summary <- as_tibble(jsonlite::fromJSON(paste0(baseurl, "summary.json"))$soma$summary) %>%
  mutate(asOfDate = as_date(asOfDate),
         across(mbs:total, parse_number))

# holdings over time
select(summary, asOfDate, total, notesbonds) %>%
  pivot_longer(-asOfDate) %>%
  ggplot(aes(x = asOfDate, y = value, color = name)) +
  geom_line() +
  scale_y_continuous(
    labels = scales::label_dollar(scale = 1e-9, accuracy = 1),
    breaks = scales::breaks_extended(n = 15),
    sec.axis = dup_axis(name = NULL)
  ) +
  labs(
    title = "Fed's holdings of Treasury securities",
    y = "billion",
    x = NULL,
    color = NULL
  ) +
  theme(legend.position = "bottom")


# Treasury maturities - soma holdings -----------------------------------------

# most recent release
most_recent <- jsonlite::fromJSON(paste0(baseurl, "asofdates/latest.json"))$soma$asOfDates

tsy_details <- jsonlite::fromJSON(
  paste0(baseurl, "tsy/get/all/asof/", most_recent, ".json")
  )$soma$holdings

tsy_details <- as_tibble(tsy_details) %>%
  mutate(asOfDate = as_date(asOfDate),
         across(coupon:changeFromPriorYear, parse_number)
  )

tsy_maturities <-
tsy_details %>%
  group_by(month = yearmonth(maturityDate), securityType) %>%
  summarize(vencimentos = sum(parValue, inflationCompensation, na.rm = TRUE)) %>%
  ungroup()


# annual maturities
tsy_maturities %>%
  group_by(year = year(month), securityType) %>%
  summarize(vencimentos = sum(vencimentos)) %>%
  ggplot(aes(x = year, y = vencimentos, fill = securityType)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(
    labels = scales::label_dollar(scale = 1e-9, accuracy = 1),
    breaks = scales::breaks_extended(n = 20)
  ) +
   scale_x_continuous(
     breaks = scales::breaks_width(2)
   ) +
  labs(
    title = "Fed's holdings: maturing Treasury securities",
    x = NULL,
    y = "billion"
  )

# monthly maturities
tsy_maturities %>%
  filter(month < yearmonth("2025 Jan")) %>%
  ggplot(aes(x = month, y = vencimentos, fill = securityType)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(
    labels = scales::label_dollar(scale = 1e-9, accuracy = 1),
    breaks = scales::breaks_extended(n = 20)
  ) +
  scale_x_yearmonth(
    breaks = scales::breaks_width('1 month'),
    labels = scales::label_date_short()
  ) +
  labs(
    title = "Fed's holdings: maturing Treasury securities",
    x = NULL,
    y = "billion"
  )





soma <- "soma/summary.json"
holding <- "soma/"

furl <- paste0(baseurl,soma)
request <- jsonlite::fromJSON(furl)

furl <- paste0(baseurl,holding)
request_holding <- jsonlite::fromJSON(furl)



res <- as_tibble(request$soma$summary)
res <- res %>%
  mutate(asOfDate = as.Date(asOfDate)) %>%
  mutate(across(-asOfDate, as.numeric))

treasury_holdings <-
  res %>%
  mutate(treasury_total = tips + if_else(is.na(frn), 0, frn) + notesbonds + bills,
         # convert to US$ millions
         treasury_total = treasury_total * 1e-6) %>%
  select(asOfDate, treasury_total) %>%
  transmute(index = asOfDate, key = "Fed Treasury Holdings", value = treasury_total)

treasury_tips <-
  res %>%
  select(asOfDate, tips) %>%
  transmute(index = asOfDate, key = "Fed Tips Holdings", value = tips * 1e-6)

treasury_bonds <-
  res %>%
  select(asOfDate, notesbonds) %>%
  transmute(index = asOfDate, key = "Fed Notes and Bonds Holdings", value = notesbonds * 1e-6)

treasury_bills <-
  res %>%
  select(asOfDate, bills) %>%
  transmute(index = asOfDate, key = "Fed Bills Holdings", value = bills * 1e-6)

rm(request, res, baseurl, furl, soma)
