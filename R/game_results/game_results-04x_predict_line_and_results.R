
filename <- "pfref_game_results"
filepath_import <- str_c("data/", filename, "_prepared.csv")
d <- read.csv(filepath_import, stringsAsFactors = FALSE)

# Prepare data to predict line/point differential. -----------------------------

# Identify strongest predictors. -----------------------------------------------
# Should evaluate what are best predictors first, but I assume wp_wlastn_
# and pd_wlastn_ are the best.
name_y_test1 <- "pd_1"
# names_x_valid <- names(d)[!(names(d) == name_y_test1)]

# Use only the variables after pd_2 in d.
library("dplyr")
library("stringr")
names(d)
last_nonx_idx <- names(d) %>% str_which("pd_2")
names_x_valid <- names(d)[-c(1:last_nonx_idx)]
names_x_valid

names_x_test1 <-
  names_x_valid %>%
  str_subset("^wp")
names_x_test1

fmla_test1 <- paste(name_y_test1, paste(names_x_test1, collapse = " + "), sep = " ~ ")

# d_no_na <- d %>% filter(!(is.na(pftd_1) | is.na(pftd_2)))
d_no_na <- d %>% na.omit()
glm_test1 <- glm(fmla_test1, data = d_no_na)
summary(glm_test1)

library("leaps")
m_y <- d_no_na %>% select(one_of(name_y_test1)) %>% data.matrix()
m_x <- d_no_na %>% select(one_of(names_x_test1)) %>% data.matrix()

subset_test_fit_exh <- regsubsets(m_x, m_y, nvmax = length(names_x_valid), method = "exhaustive")
subset_test_summary_exh <- summary(subset_test_fit_exh)
which.max(subset_test_summary_exh$adjr2)
which.min(subset_test_summary_exh$cp)
which.min(subset_test_summary_exh$bic)
subset_test_summary_exh$adjr2[which.max(subset_test_summary_exh$adjr2)]

coef(subset_test_fit_exh, which.max(subset_test_summary_exh$adjr2))
coef(subset_test_fit_exh, which.min(subset_test_summary_exh$cp))
coef(subset_test_fit_exh, which.min(subset_test_summary_exh$bic))

library("glmnet")
# lambdas <- 10 ^ seq(10, -2, length = 10)
# ridge_fit_test1 <- glmnet(m_x, m_y, alpha = 0, lambda = lambdas)
# Alternatively, just let the glmnet() function choose lambda values.
# It chooses 100 by default.
ridge_fit_test1 <- glmnet(m_x, m_y, alpha = 0)
summary(ridge_fit_test1)
dim(coef(ridge_fit_test1))
# need to get the coefficeints for the best lambda.

set.seed(1)
train <- sample(1:nrow(m_x), nrow(m_x) / 2)
test <- (!train)

set.seed(1)
ridge_fit_test1_cv <- cv.glmnet(m_x[train, ], m_y[train, ], alpha = 0)
# plot(ridge_fit_cv)
ridge_fit_test1_cv$lambda.min
ridge_test1_coefs <- predict(ridge_fit_test1, type = "coefficients", s = ridge_fit_test1_cv$lambda.min)
ridge_test1_coefs

names_x_test2 <-
  names_x_valid %>%
  str_subset("^pd") %>%
  str_subset("lag1")
names_x_test2

fmla_test2 <- paste(name_y_test1, paste(names_x_test2, collapse = " + "), sep = " ~ ")
glm_test2 <- glm(fmla_test2, data = d_no_na)
summary(glm_test2)


# Create a cleaned/filtered version of d (i.e. grid_summary_calc). -------------

# Could simply build model using all records,
# but attempting a "binning" approach here.

round_to <- function(x, n = 1) {
  if(n >= 1) {
    round(x * n) / n
  } else {
    round(x / n) * n
  }
}

cols_join <- Hmisc::Cs(wp_wlastn_1, wp_wlastn_2)
cols_join

cols_join_extra <- Hmisc::Cs(pd_wlastn_1, pd_wlastn_2)

cols_round <- c(cols_join, cols_join_extra)
cols_round_not <- Hmisc::Cs(home_1, pd_1, line_1)

library("dplyr")
grid_prep_calc <-
  d %>%
  # mutate(debug = paste(season, week, tm_1, tm_2, sep = "_")) %>%
  mutate(home_1 = ifelse(tm_1 == tm_home, 1, 0)) %>%
  select(one_of(cols_round, cols_round_not))

library("magrittr")
resolution <- 0.05
grid_prep_calc %<>%
  mutate_at(vars(cols_round), funs(round_to), resolution)

grid_nums <-
  expand.grid(seq(from = 0, to = 1, by = resolution),
              seq(from = 0, to = 1, by = resolution))

names(grid_nums) <- cols_join

grid <-
  grid_nums %>%
  left_join(grid_prep_calc, by = cols_join)

cols_group <- cols_join

grid_summary_1 <-
  grid %>%
  group_by_at(cols_group) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  filter(n > 1)

cols_avg <- names(grid_prep_calc) %>% str_subset("^[^w].")
cols_avg

grid_summary_2 <-
  grid %>%
  group_by_at(cols_group) %>%
  summarise_at(vars(cols_avg), funs(avg = mean(.)), na.rm = TRUE) %>%
  # filter(!is.na(line_home_avg)) %>%
  ungroup()

grid_summary <-
  grid_summary_1 %>%
  inner_join(grid_summary_2, by = cols_group)

rm(list = Hmisc::Cs(grid_summary_1, grid_summary_2))

grid_summary_calc <-
  grid_summary %>%
  mutate(wp_wlastn_diff = wp_wlastn_1 - wp_wlastn_2) %>%
  mutate(pd_wlastn_diff = pd_wlastn_1_avg - pd_wlastn_2_avg) %>%
  arrange(line_1_avg, wp_wlastn_diff)

grid_summary_calc %<>%
  distinct(n, line_1_avg, .keep_all = TRUE)
grid_summary_calc

# Predicting the line. ---------------------------------------------------------

name_y_1 <- "line_1_avg"

# Evaluating a model incorporating each team's weighted wp in n last games.
fmla_1a <-
  paste(name_y_1, paste(cols_join, collapse = " + "), sep = " ~ ")
lm_1a <- lm(fmla_1a, data = grid_summary_calc)
summary(lm_1a)
mean(abs(lm_1a$residuals))

# This "2" version of the "_a" model is probably more desireable
# because it uses hfa as an explicity predictor.
# (hfa is more like an implicit predictor in the
# previous model).
# The fact that the coefficient for hfa is ~3 (when the intercept is
# accounted for) confirms that this model is realistic.
fmla_1a2 <- str_c(fmla_1a, " + home_1_avg")
lm_1a2 <- lm(fmla_1a2, data = grid_summary_calc)
summary(lm_1a2)
mean(abs(lm_1a2$residuals))

# Evaluating whether or not using pd instead of wp might be better.
fmla_1b <-
  paste(name_y_1, paste(str_c(cols_join_extra, "_avg"), collapse = " + "), sep = " ~ ")
lm_1b <- lm(fmla_1b, data = grid_summary_calc)
summary(lm_1b)
mean(abs(lm_1b$residuals))

# Indeed, the fact that the pd coefficients for the following _c model
# (which uses both wp and pd as predictors) are more statistically significant
# indicate that pd is probably a better predictor.
fmla_1c <-
  paste(name_y_1, paste(
    paste(cols_join, collapse = " + "),
    paste(str_c(cols_join_extra, "_avg"), collapse = " + "),
    sep = " + "
  ), sep = " ~ ")
lm_1c <- lm(fmla_1c, data = grid_summary_calc)
summary(lm_1c)
mean(abs(lm_1c$residuals))

# This is probably better than "_c" because it simply looks at the difference
# between pd for each team.
# The coefficient for the single predictor for this model has nearly
# the same absolute value as the two coefficients for the "_c" model.
fmla_1d <- paste(name_y_1, "pd_wlastn_diff", sep = " ~ ")
lm_1d <- lm(fmla_1d, data = grid_summary_calc)
summary(lm_1d)
mean(abs(lm_1d$residuals))

# As with the "2" verison of the "_a" model,
# the following "2" version of the "_d" model is probably
# better because it incorporates hfa explicitly.
# fmla_1d2 <- str_c(fmla_1d, " + home_1_avg + 0")
fmla_1d2 <- str_c(fmla_1d, " + home_1_avg")
lm_1d2 <- lm(fmla_1d2, data = grid_summary_calc)
summary(lm_1d2)
mean(abs(lm_1d2$residuals))

# TODO: Do proper testing/training and evaluate R^2.

# glm_1d2 <- glm(fmla_1d2, data = grid_summary_calc)
# glm_1d2

library("ggplot2")
theme_set(theme_minimal())
# Visualizing the "_a" model.
ggplot(data = grid_summary_calc,
       aes(x = wp_wlastn_1 - wp_wlastn_2,
           name_y = line_1_avg)) +
  geom_point() +
  geom_smooth(method = "loess", color = "red") +
  geom_smooth(method = "glm", color = "blue")

# Visualizing the "_b" (and "_d") models.
ggplot(data = grid_summary_calc,
       aes(x = pd_wlastn_1_avg - pd_wlastn_2_avg,
           name_y = line_1_avg)) +
  geom_point() +
  geom_smooth(method = "loess", color = "red") +
  geom_smooth(method = "glm", color = "blue")

# Visualizing the "_b" (and "_d") models with the "raw" (unfiltered) data.
ggplot(data = distinct(d, season, week, tm_home, tm_away, .keep_all = TRUE),
       aes(x = pd_wlastn_1 - pd_wlastn_2, name_y = line_1)) +
  geom_point() +
  geom_smooth(method = "loess", color = "red") +
  geom_smooth(method = "glm", color = "blue")

# Predicting point differential. -----------------------------------------------

name_y_2 <- "pd_1_avg"
fmla_2a <- str_replace(fmla_1a, name_y_1, name_y_2)
lm_2a <- lm(fmla_2a, data = grid_summary_calc)
summary(lm_2a)
mean(abs(lm_2a$residuals))
# histogram(lm_2a$residuals)

fmla_2b <- str_replace(fmla_1b, name_y_1, name_y_2)
lm_2b <- lm(fmla_2b, data = grid_summary_calc)
summary(lm_2b)
mean(abs(lm_2b$residuals))

fmla_2c <- str_replace(fmla_1c, name_y_1, name_y_2)
lm_2c <- lm(fmla_2c, data = grid_summary_calc)
summary(lm_2c)
mean(abs(lm_2c$residuals))

fmla_2d <- str_replace(fmla_1d, name_y_1, name_y_2)
lm_2d <- lm(fmla_2d, data = grid_summary_calc)
summary(lm_2d)
mean(abs(lm_2d$residuals))

fmla_2d2 <- str_replace(fmla_1d2, name_y_1, name_y_2)
lm_2d2 <- lm(fmla_2d2, data = grid_summary_calc)
summary(lm_2d2)
mean(abs(lm_2d2$residuals))

# Attempting CV on best models for each response variable evaluated. -----------

library("boot")
set.seed(42)
glm_1d2 <- glm(fmla_1d2, data = grid_summary_calc)
summary(glm_1d2)$coef
# summary(lm_1d2)$coef  # same as that for "glm_" version.
summary(lm_1d2)$r.sq

glm_1d2_cv_k10 <- cv.glm(data = grid_summary_calc, glmfit = glm_1d2, K = 10)
glm_1d2_cv_k10$delta

mean(abs(glm_1d2$residuals))
mean(abs(lm_1d2$residuals))

# This is the same as the above code.
# lm_1d2_predict <- predict(lm_1d2, data = grid_summary_calc)
# lm_1d2_resids <- grid_summary_calc$line_1_avg - lm_1d2_predict
# mean(abs(lm_1d2_resids))

set.seed(42)
glm_2d2 <- glm(fmla_2d2, data = grid_summary_calc)
summary(glm_2d2)$coef

head(glm_2d2$fitted.values)
head(glm_2d2$residuals)
head(grid_summary_calc$pd_1_avg)

glm_2d2_cv_k10 <- cv.glm(data = grid_summary_calc, glmfit = glm_2d2, K = 10)
glm_2d2_cv_k10$delta
mean(abs(glm_2d2$residuals))

