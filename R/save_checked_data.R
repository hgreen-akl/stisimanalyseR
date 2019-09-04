#' Plots raw trimmed data by time for visual data check
#'
#' @name save_checked_data
#'
#'
#' @param x A dataframe of the drivesim parameters
<<<<<<< HEAD
#' @param save_location The filename of the file being checked
=======
#' @param trimmed_filename The filename of the file being checked
>>>>>>> 81cc21f10d6a04d1ba43fa6dc76d581c67462beb
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

    code <- paste0(x$ID[1] %>% as.character(),"_" ,x$Run_Number[1] %>% as.character())

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
