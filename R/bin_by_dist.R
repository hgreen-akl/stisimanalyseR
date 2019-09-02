#' bin_by_dist
#'
#' This function is not yet operational
#'
#' @name bin_by_dist
#'
#' @param x A checked_csv filename that you want to bin
#' @param bin_size the size of each bin in metres
#'
#' @return None - Saves a csv file ending in _binned_by_*bin_size*.csv
#' @export
#' @import tibble
#' @import stats
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @import signal
#' @importFrom readr read_csv
#'
#'
#'

if(getRversion() >= "2.15.1")  utils::globalVariables(c("Speed","Lateral_Lane_Pos","Longitudinal_Veloc", "Road_Curve", "Elapsed_Time", "Metric",'Throttle_input','Brake_input',"Vehic_curve",
                                                        "filtered_lane_pos","ID","Run_Number","Date","Scenario","List_of_checked_files","pattern_name","Session","Steering_angle",
                                                        "scenario",""))

bin_by_dist <- function(x, bin_size = 100) {
    #Check to see data structure and contents is sufficient for the binning function.
    pat <- x
    file_to_bin <- list.files(path = "G:/Team Drives/Research Team/Projects/2018 Driving Sim Reproducibility/7_Filtered_data/", pattern = paste0("?",pat,"_checked"), recursive = TRUE, full.names = TRUE)

    csv_to_bin <- read_csv(file_to_bin, col_names = TRUE, col_types = "ddddddddddddddddddddddddtcdccc") %>%
        as.tibble() %>%   select(-15,-16, -20,-21,-22,-23,-24,-25)
    bfhigh <- butter(1, c(0,0.1), type = "high")
    csv_to_bin <- csv_to_bin %>% mutate(filtered_speed = filter(bfhigh,Speed),
                                        filtered_lane_pos = filter(bfhigh,Lateral_Lane_Pos + 6))

    binning_function <- function(bin_start, bin_end, bin_ranges, bin_number) {
        csv_to_bin %>%
            dplyr::filter(Total_dist >= bin_start, Total_dist < bin_end) %>% as.tibble()
    }

    if(!"data.frame" %in% class(csv_to_bin)) {
        stop("Object is Not a Data Frame, cannot bin this object")
    }
    if(!"Total_dist" %in% names(csv_to_bin)){
        stop("No distance column in data frame")
    }
    ##pop up to confirm whether the file was from the country or urban task.
    Task <- if_else(csv_to_bin$scenario[1] == "Country", 1,2)

    max_bin <- if_else(Task == 1, 31000, 9000)

    bins <- c(seq(from = 100, to = max_bin-bin_size, by= bin_size))


    bin_number <- seq(from = 1, to = length(bins), by = 1)
    bin_start <- bins
    bin_end <- bins + bin_size
    bin_ranges <-  paste(bin_start,"-", bin_end, sep = "")
    Run <- List_of_checked_files %>% dplyr::filter(pattern_name == pat) %>% select(Run)
    binned_data <- data.frame(bin_start, bin_end, bin_ranges, bin_number)
    Session_extracted <- List_of_checked_files %>% dplyr::filter(pattern_name == pat) %>% select(Session) %>% as.character()

    binned_data <- binned_data %>% mutate(nested = pmap(binned_data, binning_function)) %>%
        as_tibble() %>%
        unnest() %>%
        group_by(bin_number) %>%
        summarise(n_of_obs = n(),
                  start_dist = min(Total_dist),
                  end_dist = max(Total_dist),
                  bin_duration = max(Elapsed_Time) - min(Elapsed_Time),
                  avg_speed = mean(Speed),
                  sd_speed = sd(Speed),
                  avg_lanepos = mean(Lateral_Lane_Pos),
                  sd_lanepos = sd(Lateral_Lane_Pos),
                  avg_throttle = mean(Throttle_input),
                  sd_throttle = sd(Throttle_input),
                  avg_brake = mean(Brake_input),
                  sd_brake = sd(Brake_input),
                  avg_steering = mean(Steering_angle),
                  sd_steering = sd(Steering_angle),
                  following_road = mean(abs(Road_Curve - Vehic_curve)),
                  ang_filt_lanepos = mean(filtered_lane_pos),
                  sd_filt_lanepos = sd(filtered_lane_pos),
                  ID = ID[1],
                  File_Number = Run_Number[1],
                  Date = Date[1],
                  Trial = Session_extracted[1],
                  Scenario = scenario[1],
                  Run = Run[1])

    binned_data$Run_Number <- rep(Run[1], nrow(binned_data))

    save_location <- str_replace(file_to_bin,"checked",
                                 paste0("binned_by_dist_",bin_size,"filtered"))

    existing_file <- if_else(file.exists(save_location), TRUE, FALSE)

    code <- paste0(csv_to_bin$ID[1] %>% as.character(),"_" ,csv_to_bin$Run_Number[1] %>% as.character())

    if(existing_file == TRUE) {
        overwrite <- menu(c("Overwrite","Stop"), graphics = TRUE,
                          title = paste0(code,"binned by dist",
                                         bin_size,"already exists do you wish to overwrite?")) %>%
            as.numeric()

        if(overwrite == 2) {
            stop("ERROR:: file already exists")
        } else {
            write.csv(binned_data, file = save_location, row.names = FALSE)
            print(paste0(code,"_binned_by_dist_", bin_size,"filtered.csv is saved"))
        } }
    else if (existing_file == FALSE) {
        write.csv(binned_data, file = save_location, row.names = FALSE)
        return(paste0(code,"_binned_by_dist_filtered", bin_size,".csv is saved"))
    } else {
        stop("ERROR :: error in saving process save manually")
    }

}
