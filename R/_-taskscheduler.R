
library("taskscheduleR")
script <- stringr::str_c("O:/_other/projects/mail", "/", "_-download_nfl_betting_stats_v01.R")
# taskscheduler_create(
#   taskname = "download_nfl_betting_stats_daily",
#   rscript = script,
#   schedule = "DAILY",
#   starttime = "10:00",
#   startdate = format(Sys.Date(), "%m/%d/%Y")
# )
taskscheduler_create(
  taskname = "download_nfl_betting_stats_once",
  rscript = script,
  schedule = "ONCE",
  starttime = "09:43",
  startdate = format(Sys.time(), "%m/%d/%Y")
)
# taskscheduler_delete("download_nfl_betting_stats_daily")
# taskscheduler_delete("download_nfl_betting_stats_every_once")
# taskscheduler_delete("test_taskscheduleR.R")
# taskscheduler_ls() -> z
# z$`Task To Run`
# z$Comment
