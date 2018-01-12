

.validate_download_args <- function(download, filepath_download) {

  output <- TRUE
  if(download == FALSE) {
    if(file.exists(filepath_download) == FALSE) {
      # stop("can't find filepath_download")
      # warning("can't find existing filepath_download (for direct data extraction)")
      warning("can't find existing", filepath_download, " (for direct data extraction)")
      output <- FALSE
    }
  } else {
    if(is.null(filepath_download) == TRUE) {
      # stop("please specify filepath_download")
      warning("please specify filepath_download for download")
      output <- FALSE
    }
  }
  output
}

.create_download_backup <-
  function(keep_download_backup,
           filepath_original,
           suffix_download_backup = NULL) {

    if(is.null(suffix_download_backup) == TRUE) {
      suffix_download_backup <- str_c("_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"))
      message("saving backup file with suffix '", suffix_download_backup, "'")
    }

    if (keep_download_backup == TRUE) {
      filepath_original_base <-
        tools::file_path_sans_ext(filepath_original)
      filepath_original_ext <-
        str_c(".", tools::file_ext(filepath_original))
      filepath_backup <-
        stringr::str_c(filepath_original_base,
                       suffix_download_backup,
                       filepath_original_ext)
      try(file.copy(from = filepath_original, to = filepath_backup))
      message("saving backup file at ", filepath_backup)
    }
  }

get_lines_predictiontracker <-
  function(url = "http://www.thepredictiontracker.com/nflpredictions.csv",
           # xpath = "/html/body/pre/pre[1]/text()[2]",
           # xpath_header = "/html/body/pre/pre[1]/b",
           # extract_header = FALSE,
           header = NULL,
           clean = TRUE,
           download = TRUE,
           filename_download = "lines",
           dir_download = stringr::str_c(getwd(), "/"),
           ext_download = ".csv",
           filepath_download = stringr::str_c(dir_download, filename_download, ext_download),
           keep_download = TRUE,
           keep_download_backup = FALSE,
           suffix_download_backup = NULL) {

    download_args_valid <- .validate_download_args(download, filepath_download)
    if(download_args_valid == FALSE) {
      return(NULL)
    }
    download.file(url, destfile = filepath_download)
    filepath_backup <- .create_download_backup(keep_download_backup, filepath_download, suffix_download_backup)

    raw <- readr::read_csv(filepath_download)

    if (keep_download == FALSE) {
      unlink(filepath_download)
      # try(unlink(filepath_backup))
    }

    if (clean == TRUE) {
      output <-  janitor::clean_names(raw)
    } else {
      output <- raw
    }
    output
  }

get_totals_predictiontracker <-
  function(url = "http://www.thepredictiontracker.com/nfltotals.html",
           xpath = "/html/body/pre/pre[1]/text()[2]",
           # xpath_header = "/html/body/pre/pre[1]/b",
           extract_header = FALSE,
           header = c(
             "home",
             "visitor",
             "total_open",
             "total_current",
             "total_midweek",
             "pred_avg",
             "pred_median",
             "pred_sd",
             "pred_min",
             "pred_max"
           ),
           clean = TRUE,
           download = TRUE,
           filename_download = "totals",
           dir_download = str_c(getwd(), "/"),
           ext_download = ".html",
           filepath_download = stringr::str_c(dir_download, filename_download, ext_download),
           keep_download = TRUE,
           keep_download_backup = FALSE,
           suffix_download_backup = NULL) {

    download_args_valid <- .validate_download_args(download, filepath_download)
    if(download_args_valid == FALSE) {
      return(NULL)
    }
    download.file(url, destfile = filepath_download)
    filepath_backup <- .create_download_backup(keep_download_backup, filepath_download, suffix_download_backup)

    raw <- rvest::html_nodes(xml2::read_html(filepath_download), xpath = xpath)

    if (extract_header == TRUE) {
      warning("not currently implemented")
      # header <-
      #   filepath_download %>%
      #   read_html() %>%
      #   html_nodes(xpath = xpath_header)
    } else {
      # header <- header
    }

    if (keep_download == FALSE) {
      unlink(filepath_download)
      # try(unlink(filepath_backup))
    }

    if (clean == TRUE) {
      output <- .clean_totals_predictiontracker(raw, header)
    } else {
      output <- raw
      names(output) <- header
    }

    output
  }



.clean_totals_predictiontracker <-
  function(data, header, num_cols = length(header), vars_numeric_regex = "total|pred") {
    # data <- raw
    # str_trim() is for the first and last "cells".
    # regex_split <- "([0-9.\\s]+)"
    # regex_split <- "([A-Za-z\\s]+)"
    `%>%` <- magrittr::`%>%`
    processed <-
      data %>%
      stringr::str_replace_all("\\s+", " ") %>%
      stringr::str_trim() %>%
      stringr::str_split(" ") %>%
      unlist() %>%
      as.character()
    processed

    i <- 1
    while (i <= length(processed)) {
      char <- processed[i]
      # char
      if (char %in% c("New", "LA", "San", "Tampa", "Green", "Kansas", "N.Y.")) {
        # cat(processed[i])
        processed[i] <- stringr::str_c(processed[i], " ", processed[i + 1])
        # processed[(i + 1):(length(processed) - 1)] <- processed[(i + 2):(length(processed))]
        processed[i + 1] <- " "
        # processed <- processed[-length(processed)]
      }
      i <- i + 1
      # cat(processed[i])
    }

    processed <- stringr::str_subset(processed, "^[a-zA-z0-9.]+")
    output <-
      matrix(
        processed,
        ncol = num_cols,
        nrow = length(processed) / num_cols,
        byrow = TRUE
      )

    output <- tibble::as_tibble(output)
    try(names(output) <- header)
    output <- janitor::clean_names(output)
    vars_numeric <- stringr::str_subset(header, vars_numeric_regex)
    output <- dplyr::mutate_at(output, vars_numeric, as.numeric)
  }



save_as_xlsx <- function(data = NULL,
                         check_timestamp = FALSE,
                         timestamp = NULL,
                         colname_timestamp = NULL,
                         colname_arrange = colname_timestamp,
                         ws_name = NULL,
                         filename_save = NULL,
                         dir_save = stringr::str_c(getwd(), "/"),
                         ext_save = ".xlsx",
                         filepath_save = stringr::str_c(dir_save, filename_save, ext_save),
                         overwrite_wb = TRUE,
                         overwrite_data = FALSE) {

  # data = lines_truncated # NULL
  # timestamp = timestamp_download_ymd # NULL
  # colname_timestamp = "timestamp_download_ymd"
  # colname_arrange = colname_timestamp
  # ws_name = "lines" # NULL
  # filename_save = "betting_metrics"
  # dir_save = stringr::str_c(getwd(), "/")
  # ext_save = ".xlsx"
  # filepath_save = stringr::str_c(dir_save, filename_save, ext_save)
  # overwrite_wb = TRUE
  # overwrite_data = FALSE


  if (is.null(data) ||
      is.null(ws_name) ||
      is.null(filename_save) ||
      is.null(dir_save) ||
      is.null(ext_save)) {
    warning("Missing required input. Returning nothing")
    return()
  }

  dir_files <- list.files(dir_save, full.names = TRUE)
  # dir_files
  if (!(filepath_save %in% dir_files)) {
    message(filename_save, ext_save, " does not already exist in ", dir_save, ". It will be created.")
    wb <- openxlsx::createWorkbook()
  } else {
    message(filename_save, ext_save, " already exists in ", dir_save, ". New data will be added to this workbook.")
    wb <- openxlsx::loadWorkbook(filepath_save)
  }

  if (!(ws_name %in% names(wb))) {
    message(ws_name, " worksheet does not exist in ", filename_save, ext_save, ". Adding worksheet.")
    openxlsx::addWorksheet(wb, ws_name)
    openxlsx::writeData(wb, ws_name, as.data.frame(data))
    openxlsx::saveWorkbook(wb, filepath_save, overwrite = overwrite_wb)
    return()

  } else {
    message(ws_name, " worksheet already exists in ", filename_save, ext_save, ".")
  }

  if(check_timestamp == TRUE) {
    if (is.null(colname_timestamp)) {
      warning("Missing required input. Returning nothing")
      return()
    }

    data_existing <- readxl::read_excel(filepath_save, sheet = ws_name)
    # data_existing <- dplyr::mutate_if(data_existing, is.POSIXct, as.Date)

    #  Both of these work.
    # timestamps_existing <- as.Date(format(unique(dplyr::pull(data_existing, colname_timestamp)), "%Y-%m-%d"))
    timestamps_existing <- lubridate::ymd(unique(dplyr::pull(data_existing, colname_timestamp)))
    if (!(timestamp %in% timestamps_existing)) {

      # data_all <- dplyr::arrange_at(dplyr::bind_rows(data_existing, data), colname_arrange)
      data_all <- dplyr::arrange_at(rbind(data_existing, data), colname_arrange)
      openxlsx::writeData(wb, ws_name, as.data.frame(data_all))
      openxlsx::saveWorkbook(wb, filepath_save, overwrite = overwrite_wb)
      message("New data appended to ", ws_name, " in ", filename_save, ".")
    } else {
      if(overwrite_data == FALSE) {
        message("Data not written to ", ws_name, " because data for ", timestamp, " already exists (and overwrite == FALSE).")
      } else {
        message("Adding data (because overwrite == TRUE) even though data for ", timestamp, " already exists.")
      }
    }
  }
}

# save_as_xlsx(lines_truncated, timestamp_download_ymd, "timestamp_download_ymd", ws_name = "lines")

