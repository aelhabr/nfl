
conn <- DBI::dbConnect(RSQLite::SQLite(), dbname = "test.sqlite")
DBI::dbWriteTable(conn = conn, name = "nfl_game_results", value = game_results)
dbListTables(conn)
dbListFields(conn, "nfl_game_results")
z <- dbReadTable(conn, "nfl_game_results") %>% as_tibble()
z
tail(z)
dbDisconnect(conn)
