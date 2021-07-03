
define_means <- function (variable_of_interest_name, include_slope = FALSE) {
  level = paste0(variable_of_interest_name,"_level ~ 0*1")
  if (!include_slope) {
    return(level)
  }
  slope = paste0(variable_of_interest_name,"_slope ~ 1")
  paste(level,slope, sep = "\n")
}
