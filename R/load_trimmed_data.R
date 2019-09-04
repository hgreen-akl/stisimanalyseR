#' Plots raw trimmed data by time for visual data check
#'
#' @name load_trimmed_data
#'
#'
#' @param x filename
#'
#' @return A dataframe
#'
#' @import utils
#' @importFrom magrittr %>%
#' @importFrom dplyr if_else mutate
#' @importFrom tibble as_tibble
#' @importFrom readr read_csv
#' @import stringr
#' @export

load_trimmed <- function(x) {

    save_location <- str_replace(x, "_trimmed", "_checked")

    existing_file <- if_else(file.exists(save_location), TRUE, FALSE)

    if (existing_file == TRUE) {
        recheck <- menu(c("Recheck","Stop"), graphics = TRUE, title = paste(basename(x), "checked csv data already exists do you wish to overwrite")) %>% as.numeric()
        if(recheck == 2) {
            print("skipping file")
            next()
        }
    }
    csv_to_check <- read_csv(x, col_names = TRUE) %>% data.frame() %>% as_tibble()
    csv_to_check <- csv_to_check %>% mutate(session = rep("F",times = nrow(csv_to_check)) ,
                                            scenario = rep("Country",times = nrow(csv_to_check)))
}
