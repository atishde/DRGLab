#create boxplot function#
#' Boxplot Function
#'
#'This function creates a box plot of DRG codes vs. a chosen payment variable
#' @param fun.data This is the dataframe of DRG data to be used (DRG_data.csv in this lab)
#' @param fun.y This is the payment/charge variable to be plotted
#'
#' @return A box plot of the chosen fun.y payment or charge variable over different DRGs
#' @export
#'
#' @examples
#' DRGData <- read.csv("DRG_data.csv")
#' DRG_boxplot(DRGdata, fun.y = "Average.Medicare.Payments")
DRG_boxplot <- function(fun.data, fun.y) { ##create function with arguments for dataset and y
  fun.data$fun.y <- fun.data[, fun.y] ##define y variable within function (needed for setting y variable below)
  fun.data %>%
    mutate(DRG_num = factor(substr(DRGdata$DRG.Definition, 0,3))) %>% ##create new column of DRG code number as a factor
    ggplot(aes(DRG_num, fun.y)) + ##create ggplot environment with DRG code number and y variable
    geom_boxplot() + ##create boxplot
    scale_y_continuous(fun.y) + ##set y label to the specified y variable argument
    labs(x = "DRG code") + ##change x axis label to character string
    theme(axis.text.x = element_text(angle=90)) ##move x axis ticks 90 degrees for visibility
}
