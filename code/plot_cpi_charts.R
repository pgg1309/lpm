
cpiboard <- pins::board_folder("E:/RData")
cpi <- pins::pin_read(cpiboard, "USCPI")

core.cpi # peso 79.282
core.goods <- "CUSR0000SACL1E4" # peso 17.556 (exclui carros)
rentofshelter <- "CUSR0000SAS2RS" # peso 32.563
shelter <- "CUSR0000SAH1" # peso 32.946
services.xe <- "CUSR0000SASLE" # peso 57.583

services.xes # peso 24.637

# Core services (ex energy) ----------------------------------------------------
core_services <- filter(cpi, series_id == "CUSR0000SASLE") %>%
  select(month, value) %>%
  mutate(key = "core_services") %>%
  update_tsibble(key = "key")

core_services %>%
  mutate(value = value / lag(value,12) - 1) %>%
  filter_index("1983 Jan" ~ . ) %>%
  autoplot() +
  scale_y_continuous(
    labels = scales::label_percent()
  )

# core goods (ex vehicles) -----------------------------------------------------
core_goods <- filter(cpi, series_id == "CUSR0000SACL1E4") %>%
  select(month, value) %>%
  mutate(key = "core_goods") %>%
  update_tsibble(key = "key")

core_goods %>%
  mutate(value = value / lag(value,12) - 1) %>%
  filter_index("1983 Jan" ~ . ) %>%
  autoplot() +
  scale_y_continuous(
    labels = scales::label_percent()
  )

# core CPI ---------------------------------------------------------------------
core_cpi <- filter(cpi, series_id == "CUSR0000SA0L1E") %>%
  select(month, value) %>%
  mutate(key = "core_cpi") %>%
  update_tsibble(key = "key")

core_cpi %>%
  mutate(value = value / lag(value,12) - 1) %>%
  filter_index("1983 Jan" ~ . ) %>%
  autoplot() +
  scale_y_continuous(
    labels = scales::label_percent()
  )

# shelter ----------------------------------------------------------------------
shelter <- filter(cpi, series_id == "CUSR0000SAH1") %>%
  select(month, value) %>%
  mutate(key = "shelter") %>%
  update_tsibble(key = "key")

shelter %>%
  mutate(value = value / lag(value,12) - 1) %>%
  filter_index("1983 Jan" ~ . ) %>%
  autoplot() +
  scale_y_continuous(
    labels = scales::label_percent()
  ) + labs(title = "Shelter")


# Core Services ex SHELTER
core_core_services <- bind_rows(core_services, shelter) %>%
  group_by_key() %>%
  mutate(value = value / lag(value) - 1) %>%
  ungroup() %>%
  pivot_wider(names_from = key, values_from = value) %>%
  arrange(month) %>%
  drop_na() %>%
  mutate(
    core_core_services = (core_services - 32.946/57.583*shelter)/(1 - 32.946/57.583),
    value = cumprod(core_core_services + 1),
    base = if_else(year(month) %in% 1982:1984, value, NA_real_),
    base = mean(base, na.rm = TRUE)/100,
    value = value / base,
    key = "core_core_services") %>%
  select(month, value, key) %>%
  update_tsibble(key = key)

core_core_services %>%
  mutate(value = value / lag(value,12) - 1) %>%
  filter_index("1983 Jan" ~ . ) %>%
  autoplot() +
  scale_y_continuous(
    labels = scales::label_percent()
  ) + labs(title = "Core core services")




# Core inflation ---------------------------------------------------------------

bind_rows(core_goods, core_services, core_cpi, shelter) %>%
  mutate(value = value / lag(value,12) - 1) %>%
  filter_index("1983 Jan" ~ . ) %>%
  autoplot(size = 1) +
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 0.1),
    breaks = scales::breaks_extended(n = 15),
    sec.axis = dup_axis()
  ) +
  scale_x_yearmonth(
    breaks = scales::breaks_pretty(n = 20),
    labels = scales::label_date_short()
  ) +
  labs(title = "Inflation", x = NULL, y = NULL) +
  theme(legend.position = 'bottom')

# Core over 2 years ------------------------------------------------------------

bind_rows(core_goods, core_services, core_cpi, shelter) %>%
  mutate(value = (value / lag(value,24))^(1/2) - 1) %>%
  filter_index("1983 Jan" ~ . ) %>%
  autoplot(size = 1) +
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 0.1),
    breaks = scales::breaks_extended(n = 15),
    sec.axis = dup_axis()
  ) +
  scale_x_yearmonth(
    breaks = scales::breaks_pretty(n = 20),
    labels = scales::label_date_short()
  ) +
  labs(title = "Inflation (2 years)", x = NULL, y = NULL) +
  theme(legend.position = 'bottom')

# Services inflation -----------------------------------------------------------

bind_rows(core_services, shelter, core_core_services) %>%
  mutate(value = value / lag(value,12) - 1) %>%
  filter_index("1983 Jan" ~ . ) %>%
  autoplot(size = 1) +
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 0.1),
    breaks = scales::breaks_extended(n = 15),
    sec.axis = dup_axis()
  ) +
  scale_x_yearmonth(
    breaks = scales::breaks_pretty(n = 20),
    labels = scales::label_date_short()
  ) +
  labs(title = "Services inflation", x = NULL, y = NULL) +
  theme(legend.position = 'bottom')


# Services inflation (2 years) -------------------------------------------------

bind_rows(core_services, shelter, core_core_services) %>%
  mutate(value = (value / lag(value,24))^(1/2) - 1) %>%
  filter_index("1983 Jan" ~ . ) %>%
  autoplot(size = 1) +
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 0.1),
    breaks = scales::breaks_extended(n = 15),
    sec.axis = dup_axis()
  ) +
  scale_x_yearmonth(
    breaks = scales::breaks_pretty(n = 20),
    labels = scales::label_date_short()
  ) +
  labs(title = "Services inflation (2 years)", x = NULL, y = NULL) +
  theme(legend.position = 'bottom')
