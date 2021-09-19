
define_means <- function (variable_of_interest_name, include_slope = FALSE, constrain_group_means_to_be_equal = FALSE) {
  level = paste0(variable_of_interest_name,"_level ~ 0*1")
  if (!include_slope) {
    return(level)
  }
  slope_constraint = if(constrain_group_means_to_be_equal) "c(SM,SM)*1" else "1"
  slope = paste0(variable_of_interest_name,"_slope ~ ", slope_constraint)
  paste(level,slope, sep = "\n")
}
