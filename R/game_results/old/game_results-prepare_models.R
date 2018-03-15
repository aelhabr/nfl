
file_name <- "pfref_game_results"
file_path_import <- paste0("data/", file_name, "_cleaned.csv")
d <- read.csv(file_path_import, stringsAsFactors = FALSE)

# Prepare data set for modeling. ------------------------------------------------
require("dplyr")
rbind_prep_1 <-
  d %>%
  mutate(
    line_1 = line_home,
    line_2 = -line_home,
    tm_1 = tm_home,
    tm_2 = tm_away,
    pts_1 = pts_home,
    pts_2 = pts_away,
    pd_1 = pts_1 - pts_2,
    pd_2 = pts_2 - pts_1
  )

# Need to figure out why I can't reverse pd_1 and pd_2 too.
rbind_prep_2 <-
  d %>%
  mutate(
    line_1 = -line_home,
    line_2 = line_home,
    tm_1 = tm_away,
    tm_2 = tm_home,
    pts_1 = pts_away,
    pts_2 = pts_home,
    # pd_1 = pts_2 - pts_1,
    # pd_2 = pts_1 - pts_2
    pd_1 = pts_1 - pts_2,
    pd_2 = pts_2 - pts_1
  )

rbind_prep <-
  rbind_prep_1 %>%
  bind_rows(rbind_prep_2) %>%
  arrange(season, week, tm_home, tm_away)

require("Hmisc")
rm(list = Cs(rbind_prep_1, rbind_prep_2))

determine_winner <- function(x_1, x_2, x_match) {
  ifelse(x_1 == x_match, 1,
         ifelse(x_2 == x_match, 0, as.integer(NA)))
}

cols_group <- Cs(season, tm_1)

# td stands for "to date".
rbind_prep_calc <-
  rbind_prep %>%
  mutate(
    g = 1,
    hfa = ifelse(tm_1 == tm_home, 1, 0),
    w = determine_winner(tm_1, tm_2, tm_winner_straight),
    w_ats = determine_winner(tm_1, tm_2, tm_winner_spread)
  ) %>%
  group_by_at(cols_group) %>%
  mutate(
    gtd = cumsum(g),
    wtd = cumsum(!is.na(w) & w == 1),
    ltd = cumsum(!is.na(w) & w == 0),
    ttd = cumsum(is.na(w)),
    pftd = cummean(pts_1),
    patd = cummean(pts_2),
  ) %>%
  # mutate(
  #   pdtd_1 = cummean(pd_1),
  #   pdtd_2 = cummean(pd_2)
  # ) %>%
  mutate(
    pdtd = (pftd - patd),
    wptd = wtd / gtd
  ) %>%
  ungroup()


eoy_records <-
  rbind_prep_calc %>%
  group_by_at(cols_group) %>%
  summarise(wp_eoy = round(100 * max(wtd) / max(gtd), 1)) %>%
  ungroup()
eoy_records


require("magrittr")
cols_join <- cols_group
rbind_prep_calc %<>%
  inner_join(eoy_records, by = cols_join)

require("stringr")
cols_lag1 <-
  names(rbind_prep_calc) %>%
  str_subset("td$")
cols_lag1

require("magrittr")
rbind_prep_calc %<>%
  group_by(season, tm_1) %>%
  mutate_at(vars(cols_lag1), funs(lag1 = lag)) %>%
  ungroup()

require("zoo")
rbind_mavg_test <-
  rbind_prep_calc %>%
  arrange(season, week, tm_1) %>%
  group_by(tm_1)


create_lm_fmla <- function(names_x, name_y, with_intercept = TRUE) {
  fmla <- paste(name_y, paste(names_x, collapse = " + "), sep = " ~ ")
  if(with_intercept == FALSE) {
    fmla <- paste0(fmla, " + 0")
  }
  fmla
}

n_last <- 20
cols_metrics <- Cs(n, r_sq, mae, coef_var1, coef_var2)
metrics1 <- matrix(nrow = n_last, ncol = length(cols_metrics))
colnames(metrics1) <- cols_metrics
metrics2 <- metrics1

for(n in 1:n_last) {
  # n <- 10
  d_temp <-
    rbind_mavg_test %>%
    mutate(w_lastn = rollapplyr(w, width = n, FUN = mean, na.rm = TRUE, partial = TRUE),
           pd_lastn = rollapplyr(pts_1 - pts_2, width = n, FUN = mean, na.rm = TRUE, partial = TRUE)) %>%
    mutate(w_lastn_lag1 = lag(w_lastn),
           pd_lastn_lag1 = lag(pd_lastn)) %>%
    ungroup()

  for(i in 1:2) {
    # i <- 2
    if(i == 1) {
      name_y_temp <- "w"
      names_x_temp <- Cs(w_lastn_lag1)
    } else {
      name_y_temp <- "pd_1"
      names_x_temp <- Cs(pd_lastn_lag1)
    }
    fmla_temp <- create_lm_fmla(names_x_temp, name_y_temp, FALSE)
    lm_temp <- lm(fmla_temp, data = d_temp)
    summary(lm_temp)
    row_temp <-
      c(
        n,
        summary(lm_temp)$r.sq,
        mean(abs(lm_temp$residuals)),
        as.numeric(coef(lm_temp)[1]),
        as.numeric(coef(lm_temp)[2])
      )
    if(i == 1) {
      metrics1[n,] <- row_temp
    } else {
      metrics2[n,] <- row_temp
    }
  }
}

metrics1_2 <- metrics1 %>% as_tibble()
metrics2_2 <- metrics2 %>% as_tibble()

which.max(metrics1_2$r_sq)
which.max(metrics2_2$r_sq)

which.min(metrics1_2$mae)
which.min(metrics2_2$mae)

ggplot(metrics1_2, aes(x = n)) + geom_line(aes(y = r_sq))
ggplot(metrics2_2, aes(x = n)) + geom_line(aes(y = r_sq))

ggplot(metrics1_2, aes(x = n)) + geom_line(aes(y = mae))
ggplot(metrics2_2, aes(x = n)) + geom_line(aes(y = mae))

metrics1_diff <- diff(metrics1[, -1])
metrics2_diff <- diff(metrics2[, -1])

metrics1_diff_2 <- tibble(n = 2:n_last) %>% bind_cols(as_tibble(metrics1_diff))
metrics2_diff_2 <- tibble(n = 2:n_last) %>% bind_cols(as_tibble(metrics2_diff))

which.max(metrics1_diff_2$r_sq)
which.max(metrics2_diff_2$r_sq)

which.min(metrics1_diff_2$mae)
which.min(metrics2_diff_2$mae)

ggplot(metrics1_diff_2, aes(x = n)) + geom_line(aes(y = r_sq))
ggplot(metrics2_diff_2, aes(x = n)) + geom_line(aes(y = r_sq))

ggplot(metrics1_diff_2, aes(x = n)) + geom_line(aes(y = mae))
ggplot(metrics2_diff_2, aes(x = n)) + geom_line(aes(y = mae))

ggplot(data = metrics1, aes(x = n)) +
  geom_line(aes(y = mae), color = "red") +
  geom_line(aes(y = r_sq), color = "blue") +
  geom_line(aes(y = coef_var1), color = "green")

# The partial argument is really only necessary for provided a value for the very
# first n values in the entire data set (otherwise NA would be returned).
# Can't use pdtd for pd_lastn.
calculate_wavg <- function(x, w = seq(n), n = length(x), remove_na = TRUE) {
  weighted.mean(x, w, na.rm = remove_na)
}

rbind_prep_calc_2 <-
  rbind_prep_calc %>%
  arrange(season, week, tm_1) %>%
  group_by(tm_1) %>%
  mutate(
    # w_lastn = RcppRoll::roll_sum(w, n = 10, align = "right", fill = NA),
    w_lastn = rollapplyr(w, width = 10, FUN = sum, na.rm = TRUE, partial = TRUE),
    # pd_1_lastn = rollapplyr(pd_1, width = 10, FUN = mean, na.rm = TRUE, partial = TRUE),
    # pd_2_lastn = rollapplyr(pd_2, width = 10, FUN = mean, na.rm = TRUE, partial = TRUE),
    pd_lastn = rollapplyr(pts_1 - pts_2, width = 10, FUN = mean, na.rm = TRUE, partial = TRUE),
    pd_wlastn = rollapplyr(pts_1 - pts_2, width = 10, FUN = calculate_wavg, partial = TRUE),
    wp_lastn = rollapplyr(w, width = 10, FUN = mean, na.rm = TRUE, partial = TRUE),
    wp_wlastn = rollapplyr(w, width = 10, FUN = calculate_wavg, by.column = FALSE, partial = TRUE)
  ) %>%
  ungroup()

# # Debug.
# rbind_prep_calc_2 %>%
#   filter(season == 2016) %>%
#   filter(tm_1 == "DAL") %>%
#   View()

cols_lastn_lag1 <-
  names(rbind_prep_calc_2) %>%
  str_subset("lastn$")
cols_lastn_lag1

rbind_prep_calc_2 %<>%
  group_by(season, tm_1) %>%
  mutate_at(vars(cols_lastn_lag1), funs(lag1 = lag)) %>%
  ungroup()

cols_int <-
  names(rbind_prep_calc_2) %>%
  str_subset("^.td$|^.td_lag1$|^._lastn$|^._lastn_lag1$")
cols_int

rbind_prep_calc_2 %<>%
  mutate_at(vars(cols_int), funs(as.integer))

cols_remove <-
  names(rbind_prep_calc_2) %>%
  str_subset("td$|lastn$|^.td_lag1$")
cols_remove

rbind_prep_calc_3 <-
  rbind_prep_calc_2 %>%
  select(-one_of(cols_remove))

rbind <- rbind_prep_calc_3
# rm(list = Cs(rbind_prep_calc, rbind_prep_calc_2, rbind_prep_calc_3))

rbind <- rbind %<>% filter(season >= 2000)

cols_select <-
  names(rbind) %>%
  str_subset("season|week|tm|pts|^pd|line|eoy|lag1$|lastn")
cols_select

rbind_2 <-
  rbind  %>%
  select(one_of(cols_select)) %>%
  inner_join(
    select(rbind, one_of(cols_select)),
    by = c(
      "season",
      "week",
      "tm_home",
      "tm_away",
      "pts_home",
      "pts_away",
      "line_home",
      "tm_winner_spread",
      "tm_winner_straight",
      "line_1" = "line_2",
      "line_2" = "line_1",
      "tm_1" = "tm_2",
      "tm_2" = "tm_1",
      "pts_1" = "pts_2",
      "pts_2" = "pts_1",
      "pd_1" = "pd_2",
      "pd_2" = "pd_1"
    )
  )

# rm(rbind)

col_key_last_idx <-
  names(rbind_2) %>%
  str_which("pd_2")

cols_key <- names(rbind_2)[1:col_key_last_idx]
cols_key

names(rbind_2) %<>%
  str_replace_all("\\.x$", "_1") %>%
  str_replace_all("\\.y$", "_2") %>%
  str_replace_all("_lag1", "")

rbind_3 <-
  rbind_2 %>%
  mutate(
    wp_eoy_diff = wp_eoy_1 - wp_eoy_2,
    pftd_diff = pftd_1 - pftd_2,
    patd_diff = patd_1 - patd_2,
    pdtd_diff = wptd_1 - pdtd_2,
    wptd_diff = wptd_1 - wptd_2,
    wp_lastn_diff = wp_lastn_1 - wp_lastn_2,
    wp_wlastn_diff = wp_wlastn_1 - wp_wlastn_2,
    pd_lastn_diff = pd_lastn_1 - pd_lastn_2,
    pd_wlastn_diff = pd_wlastn_1 - pd_wlastn_2
    )


file_name <- "pfref_game_results"
file_path_export <- paste0("data/", file_name, "_prepared.csv")
write.csv(rbind_3, file_path_export, row.names = FALSE)

