#' Cleans a Raw Dat file
#'
#' After  from STISIM and returns two files. 1. a csv file ending in "_trimmed.csv", and a txt
#' file containing a summary of traffic violations and other notable events
#' @name clean_dat_file
#'
#'
#' @param x A filename of a raw .Dat file
#'
#' @return NONE - instead saves two files of tidied data, "*_trimmed.csv" and "_summary.txt"
#'
#' @importFrom  magrittr %>%
#' @import utils
#' @import stringr
#'
#' @export

clean_dat_file <- function (x) {
    #creates the destination output to within a new sub folder called trimmed
    destination_dirname <- paste0(dirname(x),"/trimmed/")

    #changes the output file name from .Dat to a .csv
    destination_basename <- basename(x) %>% str_replace(".Dat", "_trimmed.csv")

    destination_fname <- paste0(destination_dirname,destination_basename)
    dir.create(destination_basename, showWarnings = FALSE)

    if(length(list.files(destination_fname)) >= 1) {
        print(paste(x , "is Trimmed"))

    } else {

        filename <-  x

        ## Define the column names that are in the DAT files and then sorting them in a more preferred order for later functions.

        Columns <- c("Elapsed_Time","Longitudinal_Accel","Lateral_Accel","Longitudinal_Veloc","Lateral_Veloc","Total_dist","Lateral_Lane_Pos","Vehic_curve","Road_Curve","Steering_angle","Throttle_input","Brake_input","Crashes_comp","Marker","Speed","Steering_count","Throttle_count","Brake_Count","System_Time","RT","Trigger_Number","Speed_Limit","Left_indicator","Right_Indicator", "Driving_Tickets")

        sorted_columns <- c("Elapsed_Time","Total_dist","Longitudinal_Veloc","Speed","Longitudinal_Accel", "Lateral_Lane_Pos", "Lateral_Veloc", "Lateral_Accel", "Throttle_input", "Brake_input", "Steering_angle","Throttle_count", "Brake_Count", "Steering_count", "Left_indicator", "Right_Indicator","Vehic_curve","Road_Curve", "Speed_Limit","Driving_Tickets", "Trigger_Number", "Crashes_comp", "Marker", "RT","System_Time")

        #imports the file
        raw_data <- read.table(x,sep = "", col.names = Columns, header = FALSE, fill = TRUE,
                               blank.lines.skip = FALSE, stringsAsFactors = FALSE)
        #finds the start of data based on a pattern identified in the .Dat file
        data_start <- raw_data[raw_data$Elapsed_Time == "Block",] %>%
            row.names.data.frame() %>%
            as.numeric() + 2
        #finds the end of data based on a pattern identified in the .Dat file
        data_end <- raw_data[raw_data$Elapsed_Time == "Tailgating",] %>%
            row.names.data.frame() %>%
            as.numeric() - 3
        #defines the lines where data is contained
        cleaned_data <- raw_data[data_start:data_end,sorted_columns]

        ## Get the summary data of performance and violations. Reads the file into a character string format then select lines starting from the start of the summary data in the file to the end of the file.

        raw_txt <- readLines(x)
        last_line <- length(raw_txt)
        summary_report <- raw_txt[grep("Tailgating results",raw_txt):last_line]

        summary_report %>% head()

        summary_report %>% tail()

        #finds the particiapnts ID and run Number to add these as columns later
        Participant_ID <- raw_data[3,2]
        Run_number <- raw_data[10,3]

        ## Obtain the name & path of the data file and create file names for the cleaned data
        ## There is also a function at the bottom which adds additoinal columns for the particpant ## ID and the run number.

        summary_filename <- destination_fname %>% str_replace("_trimmed.csv","_summary.txt")
        Pattern <- paste(Participant_ID,"_",Run_number,".Dat", sep = "")

        cleaned_data$ID <- Participant_ID %>% rep(nrow(cleaned_data))
        cleaned_data$Run_Number <- Run_number %>% rep(nrow(cleaned_data))
        cleaned_data$Date <- file.info(filename)$mtime %>% as.Date() %>% rep(nrow(cleaned_data))

        ## Write the cleaned and summary data into a csv and .txt
        ## The raw csv file is "..._cleaned.csv" with the .txt file in "..._summary.txt" file.

        cleaned_data %>% write.csv(file = destination_fname, row.names = FALSE)
        write(summary_report,file = summary_filename)

        print(paste(x, "has now been Trimmed")) }
}
