#' Plots raw trimmed data by time for visual data check
#'
#' @name plot_by_distance
#'
#'
#' @param x A dataframe of the drivesim parameters
#'
#' @return A faceted figure of all variables recorded over distance
#'
#' @importFrom tidyr gather
#' @import ggplot2
#'
#' @export

if(getRversion() >= "2.15.1")  utils::globalVariables(c("Longitudinal_Veloc", "Road_Curve", "Total_dist", "Metric"))

plot_by_distance <- function(x){

    data_to_plot <- x %>% gather(Longitudinal_Veloc:Road_Curve, key = "Variable", value = "Metric")

    ggplot(data_to_plot, aes(Total_dist, Metric)) + geom_line() + facet_wrap(~ Variable, scales = "free")
}
