#' ---
#' author: Tony
#' title: "thing"
#' date: "`r format(Sys.time(), '%B %d, %Y')`"
#' ouptut:
#'   html_document:
#'     toc: true
#'     toc_float: true
#'     toc_depth: 6
#'     theme: united
#'     highlight: textmate
#' ---
#+ global_options, include = FALSE, cache = FALSE
knitr::opts_chunk$set(
  fig.align = "center",
  # results = "hide",
  echo = TRUE,
  warning = FALSE,
  message = FALSE
)

# rmarkdown::render("classification.R")
# knitr::spin("classification.R", knit = FALSE)

#'
#+ setup
rm(list = ls())

#'
#+ import_data
require("readxl")
require("dplyr")

db_file_path <- "data/db_nfl.xlsm"
excel_sheets(db_file_path)
win_total_results_import <-
  read_excel(db_file_path, sheet = "nfl_win_total_results")

#'
#+ d_model_no_picks
require("Hmisc")
cols_win_total_results <-
  Cs(id,
     yr_start,
     team,
     w,
     w_sportsbook,
     odds_over,
     odds_under,
     result)

win_total_results <-
  d_win_total_results_import %>%
  dplyr::select(one_of(cols_win_total_results))

win_total_results_1 <-
  d_win_total_results %>%
  dplyr::select(one_of(cols_win_total_results)) %>%
  arrange(yr_start, team) %>%
  group_by(team) %>%
  mutate(w_lag1 = lag(w),
         w_diff2 = (lag(w) - lag(w, 2))) %>%
  ungroup()

win_total_results_2 <-
  d_win_total_results_1 %>%
  mutate(result =
           ifelse(w > w_sportsbook, "O", "U")) %>%
  ungroup() %>%
  group_by(team) %>%
  mutate(result_lag1 = lag(result),
         w_sportsbook_err1 = (lag(w) - lag(w_sportsbook))) %>%
  ungroup() %>%
  mutate(
    prob_implied_over =
      ifelse(
        odds_over < 0,-odds_over / (-odds_over + 100),
        ifelse(odds_over > 0, 100 / (odds_over + 100), 0)
      ),
    prob_implied_under =
      ifelse(
        odds_under < 0,-odds_under / (-odds_under + 100),
        ifelse(odds_under > 0, 100 / (odds_under + 100), 0)
      ),
    prob_implied_combo = prob_implied_over - prob_implied_under,
    underdog_combo = ifelse(odds_over > 0, 1, ifelse(odds_under > 0,-1, 0))
  ) %>%
  dplyr::select(-odds_over,-odds_under,-prob_implied_over,-prob_implied_under)

model_no_picks <-
  d_win_total_results_2 %>%
  mutate_at(vars(result_lag1, underdog_combo, result), funs(as.factor(.)))

#'
#+ d_model_with_picks
win_total_picks_import <-
  read_excel(db_file_path, sheet = "nfl_win_total_picks")

cols_win_total_picks <-
  Cs(win_total_results_id,
     person,
     pick,
     confidence)

win_total_picks <-
  d_win_total_picks_import %>%
  dplyr::select(one_of(cols_win_total_picks))

require("magrittr")
win_total_picks %<>%
  na_if("na")

# Need to do this to use confidence in calculations.
win_total_picks %<>%
  mutate_at(vars(confidence), funs(as.numeric(.)))

win_totals_with_picks <-
  d_win_total_results_2 %>%
  left_join(d_win_total_picks, by = c("id" = "win_total_results_id")) %>%
  # dplyr::select(-id) %>%
  mutate(
    pick_result =
      if_else(pick == result, 1, 0),
    w_pick_confidence =
      if_else(
        is.na(confidence),
        ifelse(
          pick == "O",
          round(w_sportsbook + 1.5, 0),
          round(w_sportsbook - 1.5, 0)
        ),
        if_else(pick == "O",
               round(w_sportsbook + (3 - confidence / 10), 0),
               round(w_sportsbook - (3 - confidence / 10), 0))
      )
    )

require("ggplot2")
require("forcats")
win_totals_with_picks %>%
  mutate_if(is.character, fct_inorder(.))

viz <-
  d_win_totals_with_picks %>%
  mutate_if(is.character, as.factor) %>%
  filter(!(is.na(result) | is.na(w_lag1)))

viz %>%
  group_by(person, yr_start, result) %>%
  count(result)


ggplot(filter(d_win_totals_with_picks, person == "a")) +
  geom_col(aes(x = yr_start, y = result))

require("tidyr")
model_with_picks <-
  d_win_totals_with_picks %>%
  mutate_at(vars(result_lag1, underdog_combo, result), funs(as.factor(.))) %>%
  gather(key, value, pick:w_pick_confidence, factor_key = TRUE) %>%
  spread(person, value)

#'
#+ d_cor_all
model <- d_model_no_picks %>% filter(!is.na(w_lag1) & !is.na(result))
names(d_model)
names_x_all <- names(d_model)[-c(1:4, 6, 8)]
names_x_all
model_x <- d_model %>% dplyr::select(one_of(names_x_all))
require("polycor")
cor_all <- hetcor(as.data.frame(d_model_x))
cor_all

#'
#+ lr
name_y <- "result"
fmla_all <- paste(name_y, paste(names_x_all, collapse = " + "), sep = " ~ ")

lr_all_fit_full <- glm(fmla_all, data = d_model, family = binomial)
summary(lr_all_fit_full)$coef
coef(lr_all_fit_full)

#'
#+ subset
require("leaps")
name_y <- "result"

# regsbusets() requires inputs to be matrices.
x_all <- data.matrix(dplyr::select(d_model, one_of(names_x_all)))
y <- data.matrix(dplyr::select(d_model, one_of(name_y)))

# Set nvmax equal to the maximum allowable number of variables.
subset_fit_full <- regsubsets(d_x_all, d_y, nvmax = length(names_x_all))
subset_fit_full

subset_summary <- summary(subset_fit_full)
coef(subset_fit_full, which.max(subset_summary$adjr2))
coef(subset_fit_full, which.min(subset_summary$cp))
coef(subset_fit_full, which.min(subset_summary$bic))
