
# Create futures tables  --------------------------------------------------

require(fpp3)
require(tidyverse)

# Create table with spreads (obs: SLOW)

c <- 1
n <- length(unique(ff_mod$day))
prazo <- duration(seq(30, 810, 30)/30, 'months') / duration(1, 'days')
nome <- paste0('t', seq(30, 810, 30))
res <- matrix(NA, nrow = n, ncol = length(nome))

for (d in unique(ff_mod$day)) {
  for (tt in 1:length(prazo)) {
    res[c,tt] <- curve_fit(d, prazo[tt], ff_mod)
  }
  if(c %% 90 == 0) cat("OK after step", c, " of ", n, " ref(", format(d), ")\n")
  c <- c + 1
}

#colnames(res) <- nome
ff_spread_table <- tibble(day = unique(ff_mod$day)) %>%
  bind_cols(res)

names(ff_spread_table) <- c('day', nome)
write_rds(ff_spread_table, "output/ff_spread_table.rds")




# REPEAT for eurodollar
c <- 1
n <- length(unique(ed_mod$day))
prazo <- duration(seq(90, 1800, 90)/30, 'months') / duration(1, 'days')
nome <- paste0('t', seq(90, 1800, 90))
res <- matrix(NA, nrow = n, ncol = length(nome))

for (d in unique(ed_mod$day)) {
  for (tt in 1:length(prazo)) {
    res[c, tt] <- curve_fit(d, prazo[tt], ed_mod)
  }
  if(c %% 90 == 0) cat("OK after step", c, " of ", n, " ref(", format(d), ")\n")
  c <- c + 1
}

ed_spread_table <- tibble(day = unique(ed_mod$day)) %>%
  bind_cols(res)

names(ed_spread_table) <- c('day', nome)
write_rds(ed_spread_table, "output/ed_spread_table.rds")




# ff_spread_table <- tibble(day = tail(unique(ff_mod$day))) %>%
#   mutate(t30  = map_dbl(day, curve_fit,  30.4375, ff_mod),
#          t60  = map_dbl(day, curve_fit,  60.875,  ff_mod),
#          t90  = map_dbl(day, curve_fit,  90.3125, ff_mod),
#          t120 = map_dbl(day, curve_fit, 121.75,   ff_mod),
#          t150 = map_dbl(day, curve_fit, 152.1875, ff_mod),
#          t180 = map_dbl(day, curve_fit, 182.625,  ff_mod),
#          t210 = map_dbl(day, curve_fit, 213.0625, ff_mod),
#          t240 = map_dbl(day, curve_fit, 243.5,    ff_mod),
#          t270 = map_dbl(day, curve_fit, 273.9375, ff_mod),
#          t300 = map_dbl(day, curve_fit, 304.375,  ff_mod),
#          t330 = map_dbl(day, curve_fit, 334.8125, ff_mod),
#          t360 = map_dbl(day, curve_fit, 365.25,   ff_mod),
#          t390 = map_dbl(day, curve_fit, 395.6875, ff_mod),
#          t420 = map_dbl(day, curve_fit, 426.125,  ff_mod),
#          t450 = map_dbl(day, curve_fit, 456.5625, ff_mod),
#          t480 = map_dbl(day, curve_fit, 487.0,    ff_mod),
#          t510 = map_dbl(day, curve_fit, 517.4375, ff_mod),
#          t540 = map_dbl(day, curve_fit, 547.875,  ff_mod),
#          t570 = map_dbl(day, curve_fit, 578.3125, ff_mod),
#          t600 = map_dbl(day, curve_fit, 608.75,   ff_mod),
#          t630 = map_dbl(day, curve_fit, 639.1875, ff_mod),
#          t660 = map_dbl(day, curve_fit, 669.625,  ff_mod),
#          t690 = map_dbl(day, curve_fit, 700.0625, ff_mod),
#          t720 = map_dbl(day, curve_fit, 730.5,    ff_mod),
#          t750 = map_dbl(day, curve_fit, 760.9375, ff_mod),
#          t780 = map_dbl(day, curve_fit, 791.375,  ff_mod),
#          t810 = map_dbl(day, curve_fit, 821.8125, ff_mod)
#   )
#


#
#
# # Create table with spreads (obs: SLOW)
# ed_spread_table <- tibble(day = unique(ed_mod$day)) %>%
#   mutate(t90  = map_dbl(day, curve_fit,  90.3125, ed_mod),
#          t180 = map_dbl(day, curve_fit, 182.625, ed_mod),
#          t270 = map_dbl(day, curve_fit, 273.9375, ed_mod),
#          t360 = map_dbl(day, curve_fit, 365.25, ed_mod),
#          t450 = map_dbl(day, curve_fit, 456.5625, ed_mod),
#          t540 = map_dbl(day, curve_fit, 547.875, ed_mod),
#          t630 = map_dbl(day, curve_fit, 639.1875, ed_mod),
#          t720 = map_dbl(day, curve_fit, 730.5, ed_mod),
#          t810 = map_dbl(day, curve_fit, 821.8125, ed_mod),
#          t900 = map_dbl(day, curve_fit, 913.125, ed_mod),
#          t990 = map_dbl(day, curve_fit, 1004.438, ed_mod),
#          t1080 = map_dbl(day, curve_fit, 1095.75, ed_mod),
#          t1170 = map_dbl(day, curve_fit, 1187.062, ed_mod),
#          t1260 = map_dbl(day, curve_fit, 1278.375, ed_mod),
#          t1350 = map_dbl(day, curve_fit, 1369.688, ed_mod),
#          t1440 = map_dbl(day, curve_fit, 1461, ed_mod),
#          t1530 = map_dbl(day, curve_fit, 1552.312, ed_mod),
#          t1620 = map_dbl(day, curve_fit, 1643.625, ed_mod),
#          t1710 = map_dbl(day, curve_fit, 1734.938, ed_mod),
#          t1800 = map_dbl(day, curve_fit, 1826.25, ed_mod)
#   )
#
# write_rds(ed_spread_table, "output/ed_spread_table.rds")
#
#
