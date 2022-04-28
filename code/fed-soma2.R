library(readr)
soma <- read_csv(
  "data/20220216-Fed-Soma_holdings.csv",
  col_types = cols(
    `As Of Date` = col_date(format = "%Y-%m-%d"),
    `Security Description` = col_character(),
    Term = col_skip(),
    `Maturity Date` = col_date(format = "%Y-%m-%d"),
    Issuer = col_character(),
    `Spread (%)` = col_number(),
    `Coupon (%)` = col_number(),
    `Current Face Value` = col_number(),
    `Par Value` = col_number(),
    `Inflation Compensation` = col_number(),
    `Percent Outstanding` = col_number(),
    `Change From Prior Week` = col_number(),
    `Change From Prior Year` = col_number(),
    `is Aggregated` = col_skip()
  )
)


filter(soma, `Security Type` == "NotesBonds") %>%
  group_by(maturity = year(`Maturity Date`)) %>%
  summarize(total = sum(`Par Value`)) %>%
  filter(maturity < 2030) %>%
  ggplot(aes(x = maturity, y = total)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(
    labels = scales::label_dollar(scale = 1e-9)
  ) +
  scale_x_continuous(
    breaks = scales::breaks_width(1)
  ) +
  labs(
    title = "Maturity of notes and bonds held by the Fed",
    x = NULL,
    y = "$ bn"
  )

filter(soma, `Security Type` == "Bills") %>%
  group_by(maturity = year(`Maturity Date`)) %>%
  summarize(total = sum(`Par Value`)) %>%
  filter(maturity < 2030) %>%
  ggplot(aes(x = maturity, y = total)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(
    labels = scales::label_dollar(scale = 1e-9)
  ) +
  scale_x_continuous(
    breaks = scales::breaks_width(1)
  ) +
  labs(
    title = "Maturity of Bills held by the Fed",
    x = NULL,
    y = "$ bn"
  )


filter(soma, `Security Type` == "Agency Debts") %>%
  group_by(maturity = year(`Maturity Date`)) %>%
  summarize(total = sum(`Par Value`)) %>%
  filter(maturity < 2030) %>%
  ggplot(aes(x = maturity, y = total)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(
    labels = scales::label_dollar(scale = 1e-9)
  ) +
  scale_x_continuous(
    breaks = scales::breaks_width(1)
  ) +
  labs(
    title = "Maturity of Agency Debts held by the Fed",
    x = NULL,
    y = "$ bn"
  )
