#' Plots raw trimmed data by time for visual data check
#'
#' @name save_checked_data
#'
#'
#' @param x A dataframe of the drivesim parameters
#' @param save_location The filename you want to save the data as
#'
#' @return None - Saves a csv of "_checked.csv"
#'
#' @import utils
#' @importFrom magrittr %>%
#' @importFrom dplyr if_else
#' @import stringr
#' @export


save_checked_data <- function(x, save_location) {

    existing_file <- if_else(file.exists(save_location), TRUE, FALSE)

    code <- basename(save_location)

    if(existing_file == TRUE) {
        overwrite <- menu(c("Overwrite","Stop"), graphics = TRUE, title = "checked csv data already exists do you wish to overwrite") %>% as.numeric()
        if(overwrite == 2) {
            stop("ERROR:: file already exists")
        } else {
            write.csv(x, file = save_location, row.names = FALSE)
            print(paste0(code,"_checked.csv is saved"))
        } }
    else if (existing_file == FALSE) {
        write.csv(x, file = save_location, row.names = FALSE)
        return(paste0(code,"_checked.csv is saved"))
    } else {
        stop("ERROR :: error in saving process save manually")
    }
}
