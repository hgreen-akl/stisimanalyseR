#' Plots raw trimmed data by time for visual data check
#'
#'
#'
#'
#' @param x A dataframe of the drivesim parameters
#' @param file_to_check The filename of the file being checked
#'
#' @return A faceted figure of all variables recorded over distance
#'
#' @import utils
#' @importFrom magrittr %>%
#' @importFrom dplyr if_else
#' @import stringr
#' @export

save_checked_data <- function(x, file_to_check) {

    save_location <- str_replace(file_to_check, "trimmed", "checked")

    existing_file <- if_else(file.exists(save_location), TRUE, FALSE)

    code <- paste0(x$ID[1] %>% as.character(),"_" ,x$Run_Number[1] %>% as.character())

    if(existing_file == TRUE) {
        overwrite <- menu(c("Overwrite","Stop"), graphics = TRUE, title = "trimmed csv data already exists do you wish to overwrite") %>% as.numeric()
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
