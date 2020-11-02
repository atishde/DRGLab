#create statistics function#
#' Statistics by DRG Code Function
#'
#' @param df This is the dataframe of DRG data to be used (DRG_data.csv in this lab)
#' @param stat This is the statistic you would like to generate a table of: mean (mean), median (median), or standard deviation (sd)
#'
#' @return An HTML table of the mean, median or SD of the payment variables over different DRG codes
#' @export
#'
#' @examples
#' DRGdata <- read.csv("DRG_data.csv")
#' DRG_stat(DRGdata, stat = "sd")
#'
DRG_stat <- function(df, stat){ ##create function with arguments for dataframe and a statistic
  stat_table <- function(df, stat){ ## create first function of making the specified dataframe
    if(stat=="mean"){ ##if function for if stat argument is set to mean (copied below for median and sd)
      df %>%
        mutate(DRG_num = factor(substr(DRGdata$DRG.Definition, 0,3))) %>% ##mutate the dataframe to add a new column for just the DRG code number as a factor
        group_by(DRG_num) %>% ##group dataset by DRG code number
        summarize_at(vars(Average.Covered.Charges:Average.Medicare.Payments),mean) ##summarize means of payment variables by DRG code number
    }
    else if (stat=="median"){
      df %>%
        mutate(DRG_num = factor(substr(DRGdata$DRG.Definition, 0,3))) %>%
        group_by(DRG_num) %>%
        summarize_at(vars(Average.Covered.Charges:Average.Medicare.Payments),median) ##summarize medians of payment variables by DRG code number
    }
    else if (stat=="sd"){
      df %>%
        mutate(DRG_num = factor(substr(DRGdata$DRG.Definition, 0,3))) %>%
        group_by(DRG_num) %>%
        summarize_at(vars(Average.Covered.Charges:Average.Medicare.Payments),sd) ##summarize sd's of payment variables by DRG code number
    } else
      stop(sQuote(x), " not implemented") ##create message for if different argument is entered for stat
  }
  knitr::kable(head(stat_table(df, stat)), "html")   ##create table with HTML output of the specified stat dataframe


}
