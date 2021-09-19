#' @export
define_longitudinal_measurement_invariance_model = function(df, model_strength = "CONFIGURAL", unconstrained_parcel_indices = c()){
  is_weak <- model_strength == "WEAK"
  is_strong <- model_strength == "STRONG" || model_strength == "PARTIAL_STRONG"
  is_strict <- model_strength == "STRICT"
  constrain_all_means_to_zero <- model_strength == "CONFIGURAL"
  paste0(
    "# Define factors\n\n",
    define_factors(df, is_weak || is_strong || is_strict),
    "\n\n# Intercepts\n",
    define_intercepts(df, is_strong || is_strict, unconstrained_parcel_indices),
    "\n\n# Unique variances and covariances\n",
    define_variances(df, is_strict),
    "\n\n",
    define_covariances(df),
    "\n\n# Latent variable means\n",
    define_latent_variable_means(df, constrain_all_means_to_zero),
    "\n\n# Latent variable variances and covariances\n",
    define_latent_variable_variances(df),
    "\n\n",
    define_latent_variable_covariances(df)
  )
}

