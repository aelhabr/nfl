
url_base <- "https://www.teamrankings.com/"
url_base_nfl_odds <- str_c(url_base, "nfl/odds-history/")
# url_suffix <- "spread"
url_suffix <- "spread-and-over-under"
url <- str_c(url_base_nfl_odds,  url_suffix)
html_filename_w_ext <- str_c("teamrankings_", url_suffix, ".html")
html_file <-
  save_html_from_url(
    url = url,
    save_dir = str_c("data/"),
    save_filename_w_ext = html_filename_w_ext
  )
d_raw <- get_stats_from_html(html_file)

library("dplyr")
header <- d_raw %>% slice(1) %>% as.matrix() %>% c()

d <- d_raw %>% slice(-1)

m_stats <- matrix(NA, nrow = pages * rows_per_page, ncol = 17)

for (i in 1:pages) {
  # for(i in 1:2) {
  # i <- 1
  url <- str_c(url_base, (i - 1) * rows_per_page)
  html_filename_w_ext <-
    str_c("temp_", (i - 1) * rows_per_page, ".html")
  html_file <-
    save_html_from_url(
      url = url,
      save_dir = str_c(getwd(), "/data/"),
      save_filename_w_ext = html_filename_w_ext
    )
  # html_file <- str_c("data/", html_filename_w_ext)

  d_raw <- get_stats_from_html(html_file)
  if (i == 1) {
    header <- d_raw %>% slice(1) %>% as.matrix() %>% c()
  }

  d <-
    d_raw %>%
    filter(Var.1 != "Rk")

  m_stats[((i - 1) * rows_per_page + 1):(((i - 1) * rows_per_page) + nrow(d)), ] <-
    as.matrix(d, dimnames = NULL)
}

library("tibble")
d <- as_tibble(m_stats)
d <-
  d %>%
  filter(!is.na(V1))

head(d)
tail(d)

names(d)
d <- setNames(d, tolower(header))
names(d) <- gsub("\\s|/", "_", names(d))
names(d) <- gsub("\\.|#", "", names(d))
names(d)

filename <- "pfref_game_results"
filepath_export <- str_c("data/", filename, "_scraped.csv")
write.csv(d, file = filepath_export, row.names = FALSE)

file.remove(dir("data/",
                pattern = ".html$",
                full.names = TRUE))

