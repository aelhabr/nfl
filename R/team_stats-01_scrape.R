
rm(list = ls())
setwd("O:/_other/projects/nfl/")

# Parameters. ----
yr_start <- 1978
yr_end <- 2016
yrs <- seq(yr_start, yr_end, by = 1)
url_base <- "https://www.pro-football-reference.com/years/"
num_tms <- 32
export <- FALSE


# Using a matrix or a list caused some issues.
# m <- matrix(NA, nrow = length(yrs) * num_tms, ncol = 13)
# l <- vector("list", length(yrs))

library("dplyr")
library("rvest")
library("stringr")

dir_scrape <- "data/scraped/"
if (dir.exists(dir_scrape) == FALSE) {
  dir.create(dir_scrape)
}

# Main loop. ----
# Note that there may be some errors when for the years 2000 through 2002.
# This might be because there was a different number of teams in each
# conference(?), so the binding gets thrown off.
# If an error is encountered, simply run through the loop interactively.
# Doing this avoids the error (for some unknown reason).
for (i in 1:length(yrs)) {
  # Debugging...
  # for(i in 1:2) {
  # i <- 24
  yr <- yrs[[i]]
  url <- str_c(url_base, yr)
  filename_html <- str_c("team_stats_", yr)
  filepath_html <-
    str_c(getwd(), "/", dir_scrape, filename_html, ".html")
  download.file(url, destfile = filepath_html)

  raw <-
    filepath_html %>%
    read_html() %>%
    html_nodes("table") %>%
    html_table(header = TRUE) %>%
    as.data.frame() %>%
    tbl_df()
  raw

  if (nrow(raw) == 0)
    break

  # Can't rename the columns yet because a "Error: Data source must be a dictionary" error
  # is raised when trying to bind rows with the same names (in a pipeline).
  filtered <-
    raw %>%
    filter(!grepl("FC ", Tm))
  filtered

  idx_col_mid <- (ncol(filtered) / 2)

  # Replace the dots in "W.L." column name in both tables.
  # Replace the ".1" suffix in the second table.
  table_1 <-
    filtered %>%
    select(c(1:idx_col_mid)) %>%
    setNames(., str_replace_all(names(.), "\\.", ""))
  table_2 <-
    filtered %>%
    select((idx_col_mid + 1):(ncol(filtered))) %>%
    setNames(., str_replace_all(names(.), "\\.1|\\.", ""))
  binded <- bind_rows(table_1, table_2)

  cleaned <-
    binded %>%
    mutate(Tm = str_replace_all(Tm, "\\*|\\+", "")) %>%
    mutate(Yr = yr) %>%
    select(Yr, everything()) %>%
    mutate_at(vars(-Tm), funs(as.numeric))

  # Note that the "T" column may be missing. (It does exist in the first year, though.)
  if (i == 1) {
    header <- names(cleaned)
  } else {
    header_diffs <- setdiff(header, names(cleaned))
    if (!is.null(header_diffs)) {
      # Add 0's for missing column.
      cleaned[header_diffs] <- 0
      # Rearrange columns.
      cleaned <- cleaned[header]
    }
  }

  cleaned
  # idxs <- ((i - 1) * nrow(cleaned) + 1):(((i - 1) * nrow(cleaned)) + nrow(cleaned))
  # length(idxs)
  # identical(length(idxs), nrow(cleaned))
  # m[idxs, ] <- as.matrix(cleaned)
  # l[[(length(l) + 1)]] <- cleaned
  # l[[i]] <- cleaned
  if (i == 1) {
    all <- cleaned
  } else {
    all <- bind_rows(all, cleaned)
  }
}

names(output) <- str_to_lower(names(output))

# Export. ----

if (export == TRUE) {

	filename_export <- "pfref_team_stats"
	dir_export <- dir_scrape
	filepath_export <-
		str_c(
			dir_export,
			filename_export,
			"_scraped_",
			format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
			".csv"
		)
  write.csv(output, file = filepath_export, row.names = FALSE)
}
