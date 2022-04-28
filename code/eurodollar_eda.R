
require(fpp3)
require(tidyverse)

# Aux functions -----------------------------------------------------------

# Function to fit an yield curve
curve_fit <- function(refday, tenor, fftable) {
  sel_ff <- filter(fftable, day == refday)
  splinefun(x = sel_ff$duration,
            y = sel_ff$spread,
            method = "natural")(tenor)
}

# function to fit a spread curve
spread_fit <- function(tenor, stype, spread_table) {
  splinefun(x = filter(spread_table, type == stype)$q,
            y = filter(spread_table, type == stype)$value,
            method = "natural")(tenor)
}

# consolidate spread table
consolidate_spread <- function(spread_table, stype) {
  spread_table %>%
    summarize(across(starts_with("t"), mean)) %>%
    pivot_longer(everything()) %>%
    mutate(
      tenor = ddays(parse_number(name)),
      q = tenor / duration(3, units = "month"),
      type = stype
    )
}


# Main --------------------------------------------------------------------


# load data
ed <- read_rds("data/eurodollar.rds") # eurodollar
ff <- read_rds("data/fedfunds.rds")   # fed funds

cash <- readxl::read_excel(
  "E:/RData/DATASTREAM/Fedfunds-Libor.xlsm",
  col_types = c("date", "numeric", "numeric")) # cash rates

# add basis risk to cash rates
cash <- cash %>%
  transmute(day = as_date(Name),
            libor = `RFV US DOLLAR 3M DEPOSIT - MIDDLE RATE`,
            fedfunds = `US FED FUNDS EFF RATE (D) - MIDDLE RATE`) %>%
  as_tsibble(index = day) %>%
  # Add future fed funds average to compare with libor
  mutate(
   ffavg = slider::slide_index_dbl(
    fedfunds,
    day,
    mean,
    .before = 0,
    .after = days(89),
    .complete = TRUE
  ),
  basis_risk = libor - ffavg
)

# Using Fed funds with expiration out to SIX months
# and Eurodollar futures with expiration out to five years

# Fed funds since Dec 1992
ff_mod <- filter(ff, day >= "1992-12-01") %>%
  transmute(day, name, value, rate = 100-value,
            duration = last_trade - days(15) - day) %>%
  group_by(day) %>%
  mutate(contract = 1,
         contract = cumsum(contract)) %>%
  ungroup() %>%
  #add fed funds and calculate spread
  left_join(select(cash, day, fedfunds), by = 'day') %>%
  # calculate spread
  mutate(spread = rate - fedfunds) %>%
  # select only shorter than 6 months
  filter(duration < 183)

# Eurodollar since Dec 1992
ed_mod <- filter(ed, day >= "1992-12-01") %>%
  transmute(day, name, value, rate = 100-value,
            duration = last_trade +days(45) - day) %>%
  group_by(day) %>%
  mutate(contract = 1,
         contract = cumsum(contract)) %>%
  ungroup() %>%
  # add libor and calculate spread
  left_join(select(cash, day, fedfunds), by = 'day') %>%
  # calculate spread
  mutate(spread = rate - fedfunds) %>%
  # select only shorter than 5 years
  filter(duration < 1826)


# Run the function to create the futures tables.
#  runs as a local service and writes the output (hopefully)



bind_rows(
  filter(ff_spread_table, day < "2002-07-01") %>%
    summarize(across(starts_with("t"), mean)) %>%
    pivot_longer(everything()) %>%
    mutate(
      tenor = ddays(parse_number(name)),
      q = tenor / duration(3, units = "month"),
      type = 'ff'
    )
  ,
  filter(ed_spread_table, day < "2002-07-01") %>%
    summarize(across(starts_with("t"), mean)) %>%
    pivot_longer(everything()) %>%
    mutate(
      tenor = ddays(parse_number(name)),
      q = tenor / duration(3, units = "month"),
      type = 'ed'
    )
) %>%
  ggplot(aes(x=q, y = value, group = type)) + geom_line() +
  scale_x_continuous(breaks = scales::breaks_extended(n = 20))





sel_date <- "2022-03-01"
sel_ff <- filter(ff_mod, day == sel_date)
ffinterp <-  splinefun(x = sel_ff$duration, y = sel_ff$spread, method = "natural")

ffinterp(90)
ggplot(sel_ff, aes(x=duration, y=spread)) + geom_line()

ff_fit("2022-03-01", 90, ff_mod)


ff_mod <- ff %>%
  arrange(day) %>%
  mutate(rate = 100 - value,
         tenor = ceiling_date(day %m+% months(4), 'months'),
         duration = last_trade - day) %>%
  filter(last_trade < tenor) %>%
  group_by(day) %>%
  mutate(contract = 1,
         contract = cumsum(contract)) %>%
  # four contracts after this date
  filter(day >= "1992-12-01") %>%
  select(day, name, value, rate, last_trade, duration, contract) %>%
  ungroup()

# join with fed funds overnight and plot the average
ff_mod %>%
  left_join(select(cash, day, fedfunds), by = 'day') %>%
  # calculate spread
  mutate(spread = rate - fedfunds) %>%
  group_by(duration) %>%
  summarize(avg = mean(spread),
            min = min(spread),
            max = max(spread),
            q1 = avg - 1.5*IQR(spread),
            q3 = avg + 1.5*IQR(spread),
            nobs = n()) %>%
  ggplot(aes(x=duration)) +
  geom_line(aes(y=avg)) +
  geom_line(aes(y=q1), color = 'blue') +
  geom_line(aes(y=q3), color = 'blue')


ed_mod %>%
  filter(duration == 730) %>%
  ggplot(aes(x = day, y = spread)) +
  geom_line() +
  geom_smooth() +
  geom_hline(yintercept = mean(spread, na.rm=TRUE), color = 'black')



ed_spread_table %>%
  ggplot(aes(x = day, y = t1080)) + geom_line() +
  scale_x_date(
    breaks = scales::breaks_pretty(n = 20),

  )

