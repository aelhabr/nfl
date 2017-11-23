
rm(list = ls())
setwd("O:/_other/projects/nfl/")

# Parameters. ----
yr_start <- 1978
yr_end <- 2016
games_per_season <- 256 + 11
rows_per_page <- 100
num_pages <-
  ceiling(games_per_season * (yr_end - yr_start) / rows_per_page)
url_base <-
  "https://www.pro-football-reference.com/play-index/tgl_finder.cgi?request=1&match=game&year_min=1978&year_max=2017&game_type=E&game_num_min=0&game_num_max=99&week_num_min=0&week_num_max=99&temperature_gtlt=lt&c1stat=vegas_line&c1comp=gt&c5val=1.0&order_by=game_date&order_by_asc=Y&offset="
export <- TRUE

# Preallocate memory for data to be scraped..
# The number of columns is known through prior debugging.
m <- matrix(NA, nrow = num_pages * rows_per_page, ncol = 17)

library("dplyr")
library("rvest")
library("stringr")

dir_scrape <- "data/scraped/"
if (dir.exists(dir_scrape) == FALSE) {
  dir.create(dir_scrape)
}

# Main for loop. ----

for (i in 1:num_pages) {
  # Debugging...
  # for(i in 1:2) {
  # i <- 1
  url <- str_c(url_base, (i - 1) * rows_per_page)

  # Don't necessarily need to download each html file with a new name.
  # Could use the same name.
  filename_html <- str_c("temp_", (i - 1) * rows_per_page)

  # Not sure working directory needs to be specified hear in order for download.file() to work.
  filepath_html <-
    str_c(getwd(), "/", dir_scrape, filename_html, ".html")
  # filepath_html <- str_c("~/data/nfl/downloaded/", filename_html, ".html")
  download.file(url, destfile = filepath_html)

  # Convert to data.frame first on purpose (even though converstion to a tibble
  # could be done directly. This avoid a "Error: Column 1 must be named" error.
  # The column names get converted to Var.1, Var.2, etc.
  raw <-
    filepath_html %>%
    read_html() %>%
    html_nodes("table") %>%
    html_table(header = TRUE) %>%
    as.data.frame() %>%
    tbl_df()

  if (nrow(raw) == 0)
    break

  # Column names are mistakenly captured in the first row.
  # Save these names to add on at the end.
  if (i == 1)
    header <- raw %>% slice(1) %>% as.matrix() %>% c()

  # Filter out extra rows that are embedded in the html tables.
  filtered <- filtered %>% filter(Var.1 != "Rk")

  # There may be a simpler way of stroing the data frame into the matrix,
  # but this works.
  # Perhaps doen't need to convert to a data frame at all beforehand.
  idxs <-
    ((i - 1) * rows_per_page + 1):(((i - 1) * rows_per_page) + nrow(filtered))
  m[idxs, ] <- as.matrix(filtered, dimnames = NULL)
}

# Finalize. ----
output <- as_tibble(m)
head(output)
tail(output)

output <- output %>% filter(!is.na(V1))
names(output)
names(output) <- str_to_lower(header)
names(output) <-
  names(output) %>%
  str_replace_all("\\s|/", "_") %>%
  str_replace_all("\\.|#", "")
names(output)

# Export. ----

# Save with a timestamp in order to prevent accidentally overwriting anything.
# Go back and rename this file manually without the timestamp when
# it is determined that it is good.

if (export == TRUE) {
	filename_export <- "pfref_game_results"
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
