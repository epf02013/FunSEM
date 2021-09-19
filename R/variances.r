define_level_slope_variances <- function (variable_of_interest_name, include_slope = FALSE,constrain_group_variances_to_be_equal = FALSE) {
  level_name = paste0(variable_of_interest_name,"_level")
  constraint_for_level_level = if(constrain_group_variances_to_be_equal) "c(LV,LV)*" else ""
  level_to_level = paste0(level_name, " ~~ ", constraint_for_level_level,level_name)
  if (!include_slope) {
    return(level_to_level)
  }
  slope_name = paste0(variable_of_interest_name,"_slope")
  constraint_for_slope_slope = if(constrain_group_variances_to_be_equal) "c(SV,SV)*" else ""
  slope_to_slope = paste0(slope_name, " ~~ ", constraint_for_slope_slope,slope_name)

  constraint_for_level_slope = if(constrain_group_variances_to_be_equal) "c(LSV,LSV)*" else ""
  level_to_slope = paste0(level_name, " ~~ ", constraint_for_level_slope, slope_name)
  paste(level_to_level, level_to_slope, slope_to_slope, sep = "\n")
}
