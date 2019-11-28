.onAttach <- function(libname = find.package("Rcan"), pkgname = "Rcan") {
  packageStartupMessage("Type example(Rcan) to see demo of the function.")
  
}

# CRAN Note avoidance
  if(getRversion() >= "2.15.1") 
    utils::globalVariables(
      # sample file names from taxstats
      c(
		"CSU_A","CSU_BY","CSU_C","CSU_EAPC","CSU_LOW","CSU_P","CSU_RANK","CSU_ST","CSU_T",
		"CSU_UP","CSU_Xaxes","CSU_Y","CSU_age_factor","CSU_age_label","CSU_cohort",
		"CSU_dum_by","CSU_rate","CSU_smooth","CSU_RESULT","age_factor","asr","correction","dummy_top",
		"id_group","max_year","nb_age_group","pop15","pop16","pop17","rank_value","rate",
		"smooth_value","st_err","temp","temp_age","temp_label","temp_label2","total",
		"total_known","cases","ICD_group", "LABEL", "ICD", "age_group", "age_group_label", 
		"temp1", "temp2", "ICD_ungroup", "N", "group_id", ".", "patterns", "asr_plot", 
		"CSU_ASR","asr_label", "CSU_BAR", "asr_round", "age_span", "cumrisk", "var_cum_risk"
		)
	)