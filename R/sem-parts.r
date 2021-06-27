define_factor_line <- function (df, is_weak = FALSE){
  function (factor_name) {
    variables <- df[[factor_name]]

    if (is_weak) {
      var_res = mapply(function (var_name, index){
        paste0("lambda", index, "*", var_name)
      }, variables, 1:length(variables))
      return(
        paste0(factor_name, " =~ NA*", variables[1], " + ", paste(var_res, collapse = " + "))
      )
    }
    paste0(factor_name, " =~ NA*", variables[1], " + lambda1*", variables[1], " + ", paste(tail(variables, -1), collapse = " + "))
  }
}


define_factors <- function (factor_sets, is_weak = FALSE) {
  factor_names = colnames(factor_sets)
  define_factor_lineResult <- define_factor_line(factor_sets, is_weak)
  factor_lines = sapply(factor_names, define_factor_lineResult)
  paste(factor_lines, collapse = "\n")
}


define_intercept_line <- function (df,strong, unconstrained_parcel_indices = c()){
  function (factor_name) {
    if(!strong) {
      variables <- df[[factor_name]]
      tailResult <- sapply(tail(variables, -1), function(varr) {
        paste0(varr, " ~ 1")
      })
      return(
        paste0(variables[1], " ~ i1*1\n", paste(tailResult, collapse = "\n"))
      )
    }
    variables <- df[[factor_name]]
    tailResult <- mapply(function(varr,index) {
      if(index %in% unconstrained_parcel_indices) {
        return(paste0(varr, " ~ 1"))
      }
      paste0(varr, " ~ i", index, "*1")
    }, variables, 1:length(variables))
    paste(tailResult, collapse = "\n")
  }
}

define_intercepts <- function (factor_sets, strong = FALSE, unconstrained_parcel_indices = c()) {
  if(length(factor_sets[,1]) /2 < length(unconstrained_parcel_indices)) {
    stop("must constrain at least half of the parcels when defining a partial strong model")
  }
  factor_names = colnames(factor_sets)
  define_factor_lineResult <- define_intercept_line(factor_sets, strong, unconstrained_parcel_indices)
  factor_lines = sapply(factor_names, define_factor_lineResult)
  paste(factor_lines, collapse = "\n\n")
}


define_variance_line <- function (df, unique){
  function (factor_name) {
    if(!unique) {
      variables <- df[[factor_name]]
      tailResult <- sapply(variables, function(varr) {
        paste0(varr, " ~~ ", varr)
      })
      return(paste(tailResult, collapse = "\n"))
    }
    variables <- df[[factor_name]]
    tailResult <- mapply(function(varr, index) {
      paste0(varr, " ~~ u", index, "*", varr)
    }, variables, 1:length(variables))
    return(paste(tailResult, collapse = "\n"))
  }
}
define_variances <- function (factor_sets, unique = FALSE) {
  factor_names = colnames(factor_sets)
  define_factor_lineResult <- define_variance_line(factor_sets, unique)
  factor_lines = sapply(factor_names, define_factor_lineResult)
  paste(factor_lines, collapse = "\n\n")
}


define_covariance_line <- function (column_pairs, df){
  function (column_index) {
    col_names <- column_pairs[,column_index]
    v1 = df[[col_names[1]]]
    v2 = df[[col_names[2]]]
    tailResult <- mapply(function(varr, varr2) {
      paste0(varr, " ~~ ", varr2)
    },v1,v2 )
    paste(tailResult, collapse = "\n")
  }
}


define_covariances <- function (factor_sets) {
  factor_names = colnames(factor_sets)
  # get our column pairs
  column_pairs = combn(factor_names,2)
  indexes = 1:length(column_pairs[1,])
  define_factor_lineResult <- define_covariance_line(column_pairs, factor_sets)
  factor_lines = sapply(indexes, define_factor_lineResult)
  paste(factor_lines, collapse = "\n\n")
}



define_latent_variable_means <- function (factor_sets) {
  factor_names = colnames(factor_sets)
  paste0(
    factor_names[1], " ~ 0*1\n",
    paste(tail(factor_names, -1), collapse = " ~ 1\n"),
    " ~ 1"
  )
}

define_latent_variable_variances <- function (factor_sets) {
  factor_names = colnames(factor_sets)
  tailResult <- sapply(tail(factor_names, -1), function(p) { paste(p," ~~ ",p, sep = "")})
  paste0(
    factor_names[1],
    " ~~ 1*", factor_names[1],
    "\n",
    paste(tailResult, collapse = "\n")
  )
}




define_latent_variable_covariance_line <- function (column_pairs, constrain_to_zero){
  function (column_index) {
    col_names <- column_pairs[,column_index]
    if(constrain_to_zero) {
      return(paste0(col_names[1], " ~~ 0*", col_names[2]))
    }
    paste0(col_names[1], " ~~ ", col_names[2])
  }
}


define_latent_variable_covariances <- function (factor_sets, constrain_to_zero = FALSE) {
  factor_names = colnames(factor_sets)
  column_pairs = combn(factor_names,2)
  indexes = 1:length(column_pairs[1,])
  define_covariance_for_factor_pair <- define_latent_variable_covariance_line(column_pairs, constrain_to_zero)
  factor_lines = sapply(indexes, define_covariance_for_factor_pair)
  paste(factor_lines, collapse = "\n")
}
