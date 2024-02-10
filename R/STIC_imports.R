


#Update files

loadSTIC <- function(rawSTIC){
  read.csv(rawSTIC) %>%
    dplyr::select(-c(1)) %>%
    dplyr::rename_with(.cols = contains("Date"),
                       .fn = function(x){"DateTime"}) %>%
    dplyr::rename_with(.cols = contains("Temp"),
                       .fn = function(x){"TEMP_C"}) %>%
    dplyr::rename_with(.cols = contains("Intensity"),
                       .fn = function(x){"INTENSITY_LUX"}) %>%
    dplyr::mutate(TEMP_C = as.numeric(TEMP_C),
                  INTENSITY_LUX = as.numeric(INTENSITY_LUX),
                  DateTime = mdy_hm(DateTime))
}



loadSTICdir <- function(Path){
  Path %>%
    dir_ls(regexp = "\\.csv$") %>%
    map_dfr(read_csv, .id = "FileID", show_col_types = FALSE) %>%
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
    dplyr::mutate(FileID = gsub(paste(P, "/", sep = ""), '', FileID),
                  Site = str_extract(FileID, "[^_]+"),
                  STIC_ID = str_match(FileID, "_\\s*(.*?)\\s*_")[,2],
                  Group = str_extract(Site, "[^.]+")) %>%
    dplyr::select(-"FileID") %>%
    dplyr::select(Group, Site, DateTime, INTENSITY_LUX, TEMP_C)
}
