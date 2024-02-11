#' Import raw STIC sensor data
#'
#' @description
#' load_STIC() is a tool to import and tidy raw STIC sensor data.
#'
#'
#' @import fs
#' @import tidyr
#' @import purrr
#' @import lubridate
#' @import stringr
#' @importFrom utils read.csv
#'
#' @param data A dataframe
#' @param dir A directory with raw STIC files
#'
#'
#' @return the formatted data
#'
#'
#'
#' @examples
#' clean_STIC <- load_STIC(data = "https://raw.githubusercontent.com/matthewberens/STICr/data/raw_hobo_export.csv")
#'
#'
#' @rdname load_STIC
#' @export
#'

#Update files
load_STIC <- function(data = NULL, dir = NULL){

 if(is.null(dir)){
   read.csv(data) %>%
    dplyr::select(-c(1)) %>%
    dplyr::rename_with(.cols = contains("Date"),
                       .fn = function(x){"DateTime"}) %>%
    dplyr::rename_with(.cols = contains("Temp"),
                       .fn = function(x){"TEMP_C"}) %>%
    dplyr::rename_with(.cols = contains("Intensity"),
                       .fn = function(x){"INTENSITY_LUX"}) %>%
    dplyr::mutate(TEMP_C = as.numeric(TEMP_C),
                  INTENSITY_LUX = as.numeric(INTENSITY_LUX),
                  DateTime = mdy_hm(DateTime))}

  else{dir %>%
      dir_ls(regexp = "\\.csv$") %>%
      purrr::map_dfr(readr::read_csv, .id = "FileID", show_col_types = FALSE) %>%
      dplyr::select(-"#") %>%
      dplyr::rename_with(.cols = contains("Date"),
                         .fn = function(x){"DateTime"}) %>%
      dplyr::rename_with(.cols = contains("Temp"),
                         .fn = function(x){"TEMP_C"}) %>%
      dplyr::rename_with(.cols = contains("Intensity"),
                         .fn = function(x){"INTENSITY_LUX"}) %>%
      dplyr::mutate(TEMP_C = as.numeric(TEMP_C),
                    INTENSITY_LUX = as.numeric(INTENSITY_LUX),
                    DateTime = mdy_hm(DateTime)) %>%
      dplyr::mutate(FileID = gsub(paste(dir, "/", sep = ""), '', FileID),
                    Site = str_extract(FileID, "[^_]+"),
                    STIC_ID = str_match(FileID, "_\\s*(.*?)\\s*_")[,2],
                    Group = str_extract(Site, "[^.]+")) %>%
      dplyr::select(-"FileID") %>%
      dplyr::select(Group, Site, DateTime, INTENSITY_LUX, TEMP_C)
  }
}

