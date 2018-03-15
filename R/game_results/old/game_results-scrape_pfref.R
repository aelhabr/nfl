
yr_start <- 1978
yr_end <- 2016
games_per_season <- 256 + 11
rows_per_page <- 100
pages <-
  ceiling(games_per_season * (yr_end - yr_start) / rows_per_page)
url_base <-
  "https://www.pro-football-reference.com/play-index/tgl_finder.cgi?request=1&match=game&year_min=1978&year_max=2017&game_type=E&game_num_min=0&game_num_max=99&week_num_min=0&week_num_max=99&temperature_gtlt=lt&c1stat=vegas_line&c1comp=gt&c5val=1.0&order_by=game_date&order_by_asc=Y&offset="

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

