

rm(list = ls())
setwd("O:/_other/projects/nfl/")

# Parameters. ----
filename_base <- "pfref_game_results"
dir_import <- "data/created/"
library("stringr")
filepath_import <- str_c(dir_import, filename_base, "_cleaned.csv")
game_results <- read.csv(filepath_import, stringsAsFactors = FALSE)
export <- FALSE

# Prepare data set for modeling. ----
library("dplyr")
rbind_prep_1 <-
	game_results %>%
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
	game_results %>%
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

rm(list = c("rbind_prep_1", "rbind_prep_2"))

determine_winner <- function(x_1, x_2, x_match) {
	ifelse(x_1 == x_match, 1,
				 ifelse(x_2 == x_match, 0, as.integer(NA)))
}

cols_group <- c("season", "tm_1")

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
		wtd_ats = cumsum(!is.na(w_ats) & w_ats == 1),
		ltd_ats = cumsum(!is.na(w_ats) & w_ats == 0),
		ttd_ats = cumsum(is.na(w_ats)),
		pftd = cummean(pts_1),
		patd = cummean(pts_2)
	) %>%
	# mutate(
	#   pdtd_1 = cummean(pd_1),
	#   pdtd_2 = cummean(pd_2)
	# ) %>%
	mutate(
		pdtd = (pftd - patd),
		wptd = wtd / gtd,
		wptd_ats = wtd_ats / gtd
	) %>%
	ungroup()

last_week <-
	rbind_prep_calc %>%
	# filter(season <= 1980) %>%
	group_by(season, week) %>%
	summarise(g_count = n()) %>%
	ungroup() %>%
	filter(g_count <= (2 * 4)) %>%
	arrange(week) %>%
	group_by(season) %>%
	mutate(week_count = row_number(week)) %>%
	ungroup() %>%
	filter(week_count == 1) %>%
	mutate(week_rs_last = week - 1) %>%
	select(-week, -g_count, -week_count)
last_week

eoy_records <-
	rbind_prep_calc %>%
	inner_join(last_week, by = "season") %>%
	filter(week <= week_rs_last) %>%
	# filter(week <= 17) %>%
	group_by_at(cols_group) %>%
	summarise(wp_eoy = round(100 * max(wtd) / max(gtd), 1)) %>%
	ungroup()

eoy_records_export <-
	rbind_prep_calc %>%
	inner_join(last_week, by = "season") %>%
	filter(week <= week_rs_last) %>%
	# filter(week <= 17) %>%
	group_by_at(cols_group) %>%
	summarise(
		g = max(gtd),
		w = max(wtd),
		l = max(ltd),
		t = max(ttd),
		wp = round(100 * max(wtd) / max(gtd), 1),
		w_ats = max(wtd_ats),
		l_ats = max(ltd_ats),
		t_ats = max(ttd_ats),
		wp_ats = round(100 * max(wtd_ats) / max(gtd), 1)
	) %>%
	ungroup() %>%
	rename(tm = tm_1)

eoy_records_export
if (export == TRUE) {
	filename_records <- "data/created/nfl_eoy_records.csv"
	write.csv(eoy_records_export, filename_records, row.names = FALSE)
}

library("magrittr")
cols_join <- cols_group
rbind_prep_calc <-
	rbind_prep_calc %>%
	inner_join(eoy_records, by = cols_join)

cols_lag1 <-
	names(rbind_prep_calc) %>%
	str_subset("td$")
cols_lag1

rbind_prep_calc <-
	rbind_prep_calc %>%
	group_by(season, tm_1) %>%
	mutate_at(vars(cols_lag1), funs(lag1 = lag)) %>%
	ungroup()

library("zoo")
rbind_mavg_test <-
	rbind_prep_calc %>%
	arrange(season, week, tm_1) %>%
	group_by(tm_1)


create_lm_fmla <-
	function(names_x, name_y, include_intercept = TRUE) {
		fmla <- paste(name_y, paste(names_x, collapse = " + "), sep = " ~ ")
		if (include_intercept == FALSE) {
			fmla <- str_c(fmla, " + 0")
		}
		fmla
	}

evaluate_mavg_window <-
	function(game_results,
					 name_x,
					 name_y = name_x,
					 n_last,
					 include_intercept = FALSE,
					 include_hfa = FALSE) {
		library("dplyr")
		library("zoo")
		library("magrittr")
		# library("Hmisc")

		if (include_intercept == TRUE) {
			if (include_hfa == TRUE) {
				n_names_x <- 3
			}
		} else if (include_hfa == TRUE) {
			n_names_x <- 2
		} else {
			n_names_x <- 1
		}

		names_coefs <- "coef1"
		if (n_names_x > 1) {
			for (i in 2:n_names_x) {
				names_coefs <- c(names_coefs, str_c("coef", i))
			}
		}

		cols_metrics <- c(Hmisc::Cs(n, r_sq, mae), names_coefs)
		metrics <- matrix(nrow = n_last, ncol = length(cols_metrics))
		colnames(metrics) <- cols_metrics

		for (n in 1:n_last) {
			# game_results <- rbind_prep_calc
			# name_x <- "pts_1"
			game_results <-
				game_results %>%
				arrange(season, week, tm_1) %>%
				group_by(tm_1)

			if (name_x %in% "pd_1") {
				d_n <-
					game_results %>%
					mutate(temp = rollapplyr(
						pts_1 - pts_2,
						width = n,
						FUN = mean,
						na.rm = TRUE,
						partial = TRUE
					))
			} else {
				quo_y <- quo(name_x)
				d_n <-
					game_results %>%
					mutate_at(
						vars(!!quo_y),
						funs(temp = rollapplyr),
						width = n,
						FUN = mean,
						na.rm = TRUE,
						partial = TRUE
					)
			}

			if (missing(d_n) | is.null(d_n)) {
				return(NULL)
			}

			d_n <-
				d_n %>%
				mutate(temp_lag1 = lag(temp)) %>%
				ungroup()

			names_x <- Hmisc::Cs(temp_lag1)
			if (include_hfa == TRUE) {
				names_x <- c(names_x, "hfa")
			}

			fmla_n <-
				create_lm_fmla(names_x, name_y, include_intercept = include_intercept)
			lm_n <- lm(fmla_n, data = d_n)
			# summary(lm_n)

			row_n <-
				c(n,
					summary(lm_n)$r.sq,
					mean(abs(lm_n$residuals)),
					as.numeric(coef(lm_n)[1]))
			if (n_names_x > 1) {
				for (i in 2:n_names_x) {
					row_n <- c(row_n, coef(lm_n)[i])
				}
			}
			metrics[n,] <- row_n
		}

		metrics
	}

n_last <- 16
metrics1 <-
	evaluate_mavg_window(rbind_prep_calc, "w", "pd_1", n_last = n_last)
metrics2 <-
	evaluate_mavg_window(rbind_prep_calc, "pd_1", "pd_1", n_last = n_last)
metrics3 <-
	evaluate_mavg_window(rbind_prep_calc, "pts_1", "pd_1", n_last = n_last)
metrics4 <-
	evaluate_mavg_window(rbind_prep_calc, "pts_2", "pd_1", n_last = n_last)

get_diffs <- function(m, n_last) {
	library("dplyr")
	diffs <- diff(m[,-1])
	tibble(n = 2:n_last) %>% bind_cols(as_tibble(diffs))
}
metrics1_diff <- get_diffs(metrics1, n_last)
metrics2_diff <- get_diffs(metrics2, n_last)
metrics3_diff <- get_diffs(metrics3, n_last)
metrics4_diff <- get_diffs(metrics4, n_last)

metrics1 <- metrics1 %>% as_tibble()
metrics2 <- metrics2 %>%  as_tibble()
metrics3 <- metrics3 %>% as_tibble()
metrics4 <- metrics4 %>% as_tibble()

which.max(metrics1$r_sq)
which.min(metrics1$mae)

ggplot(metrics1, aes(x = n)) + geom_line(aes(y = r_sq))
ggplot(metrics2, aes(x = n)) + geom_line(aes(y = r_sq))
ggplot(metrics3, aes(x = n)) + geom_line(aes(y = r_sq))
ggplot(metrics4, aes(x = n)) + geom_line(aes(y = r_sq))

library("scales")
ggplot_colors <- hue_pal()(4)
ggplot() +
	geom_line(data = metrics1, aes(x = n, y = mae, group = "w"),  color = ggplot_colors[1]) +
	geom_line(data = metrics2,
						aes(x = n, y = mae, group = "pd"),
						color = ggplot_colors[2]) +
	geom_line(data = metrics3,
						aes(x = n, y = mae, group = "pf"),
						color = ggplot_colors[3]) +
	geom_line(data = metrics4,
						aes(x = n, y = mae, group = "pa"),
						color = ggplot_colors[4]) +
	scale_color_manual(values = ggplot_colors, labels = Hmisc::Cs(w, pd, pf, pa))


which.max(metrics1_diff$r_sq)
which.min(metrics1_diff$mae)

ggplot(metrics1_diff, aes(x = n)) + geom_line(aes(y = r_sq))
ggplot(metrics2_diff, aes(x = n)) + geom_line(aes(y = r_sq))
ggplot(metrics3_diff, aes(x = n)) + geom_line(aes(y = r_sq))
ggplot(metrics4_diff, aes(x = n)) + geom_line(aes(y = r_sq))


# The partial argument is really only necessary for provided a value for the very
# first n values in the entire data set (otherwise NA would be returned).
# Can't use pdtd for pd_lastn.
calculate_wavg <-
	function(x,
					 w = seq(n),
					 n = length(x),
					 remove_na = TRUE) {
		weighted.mean(x, w, na.rm = remove_na)
	}

rbind_prep_calc_2 <-
	rbind_prep_calc %>%
	arrange(season, week, tm_1) %>%
	group_by(tm_1) %>%
	mutate(
		# w_lastn = RcppRoll::roll_sum(w, n = 10, align = "right", fill = NA),
		w_lastn = rollapplyr(
			w,
			width = 10,
			FUN = sum,
			na.rm = TRUE,
			partial = TRUE
		),
		# pd_1_lastn = rollapplyr(pd_1, width = 10, FUN = mean, na.rm = TRUE, partial = TRUE),
		# pd_2_lastn = rollapplyr(pd_2, width = 10, FUN = mean, na.rm = TRUE, partial = TRUE),
		pd_lastn = rollapplyr(
			pts_1 - pts_2,
			width = 10,
			FUN = mean,
			na.rm = TRUE,
			partial = TRUE
		),
		pd_wlastn = rollapplyr(
			pts_1 - pts_2,
			width = 10,
			FUN = calculate_wavg,
			partial = TRUE
		),
		wp_lastn = rollapplyr(
			w,
			width = 10,
			FUN = mean,
			na.rm = TRUE,
			partial = TRUE
		),
		wp_wlastn = rollapplyr(
			w,
			width = 10,
			FUN = calculate_wavg,
			by.column = FALSE,
			partial = TRUE
		)
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

rbind_prep_calc_2 <-
	rbind_prep_calc_2 %>%
	group_by(season, tm_1) %>%
	mutate_at(vars(cols_lastn_lag1), funs(lag1 = lag)) %>%
	ungroup()

cols_int <-
	names(rbind_prep_calc_2) %>%
	str_subset("^.td$|^.td_lag1$|^._lastn$|^._lastn_lag1$")
cols_int

rbind_prep_calc_2 <-
	rbind_prep_calc_2 %>%
	mutate_at(vars(cols_int), funs(as.integer))

cols_remove <-
	names(rbind_prep_calc_2) %>%
	str_subset("td$|lastn$|^.td_lag1$")
cols_remove

rbind_prep_calc_3 <-
	rbind_prep_calc_2 %>%
	select(-one_of(cols_remove))

rbind <- rbind_prep_calc_3
# rm(list = Hmisc::Cs(rbind_prep_calc, rbind_prep_calc_2, rbind_prep_calc_3))

rbind <- rbind %>% filter(season >= 2000)

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

# rm("rbind")

col_key_last_idx <-
	names(rbind_2) %>%
	str_which("pd_2")

cols_key <- names(rbind_2)[1:col_key_last_idx]
cols_key

names(rbind_2) <-
	names(rbind_2) %>%
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

if (export == TRUE) {
	filepath_export <- str_c(dir_import, filename_base, "_prepared.csv")
	write.csv(rbind_3, filepath_export, row.names = FALSE)
}
