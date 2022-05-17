check_data <- function(catcode, catlabel, xmarts=marts, xmrts=mrts, xretail=retail_data) {

  adv_sales <- xmarts$data %>%
    filter(
      cat_idx == xmarts$categories %>%  filter(cat_code == catcode) %>% pull(cat_idx),
      dt_idx == "1",
      # sales monthly $mln
      et_idx == "0",
      geo_idx == "1",
      is_adj == "1"
    ) %>%
    left_join(xmarts$timeperiods, by = 'per_idx') %>%
    transmute(
      month = yearmonth(per_name, "%b%Y"),
      value = parse_number(val),
      key = "advance"
    ) %>%
    as_tsibble(index = month,
               regular = TRUE,
               key = key)

  sales <- xmrts$data %>%
    filter(
      cat_idx == xmrts$categories %>%  filter(cat_code == catcode) %>% pull(cat_idx),
      dt_idx == "1",
      # sales monthly $mln
      et_idx == "0",
      geo_idx == "1",
      is_adj == "1"
    ) %>%
    left_join(xmrts$timeperiods, by = 'per_idx') %>%
    transmute(
      month = yearmonth(per_name, "%b%Y"),
      value = parse_number(val),
      key = "actual"
    ) %>%
    as_tsibble(index = month,
               regular = TRUE,
               key = key)

  bea_sales <- xretail %>%
    filter(series_name == catlabel) %>%
    transmute(month, value, key = 'bea')

  bind_rows(foodserv_adv_sales, foodserv_sales, foodserv_bea_sales) %>%
    filter_index("2021 Jul" ~ .) %>% pivot_wider(names_from = key)
}
