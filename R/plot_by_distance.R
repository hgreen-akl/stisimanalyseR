#' Plots raw trimmed data by time for visual data check
#'
#'
#'
#'
#' @param x A dataframe of the drivesim parameters
#'
#' @return A faceted figure of all variables recorded over distance
#'
#' @importFrom tidyr gather
#' @import ggplot2
#' @export

plot_by_distance <- function(x){

    data_to_plot <- x %>% gather(Longitudinal_Veloc:Road_Curve, key = "Variable", value = "value")
    ggplot(data_to_plot, aes(Total_dist, value)) + geom_line() + facet_wrap(~ Variable, scales = "free")
}
