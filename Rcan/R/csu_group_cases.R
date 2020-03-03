csu_group_cases <- function(df_data, var_age ,group_by=NULL,var_cases = NULL,df_ICD = NULL,var_ICD=NULL,var_year = NULL, all_cancer=FALSE) {

  if (!is.null(df_ICD) & is.null(var_ICD)) {
    stop(paste0("There is no ICD variable defined (var_ICD = NULL) to use with the ICD file defined\n 
      see documentation: Help(", deparse(substitute(csu_group_cases)), ")"))
  }

    if (is.null(df_ICD) & !is.null(var_ICD)) {
    stop(paste0("There is no ICD list defined (df_ICD = NULL) to use with the ICD variable defined\n 
      see documentation: Help(", deparse(substitute(csu_group_cases)), ")"))
  }

    if (is.null(var_ICD) & all_cancer) {
    stop(paste0("There is no ICD variable defined (var_ICD = NULL) and all_cancer=TRUE\n 
      see documentation: Help(", deparse(substitute(csu_group_cases)), ")"))
  }



  core.error_variable(df_data, var_cases, csu_group_cases)
  core.error_variable(df_data, var_age, csu_group_cases)

  dt_warning <- as.vector(df_data[[var_age]])
  if (any(dt_warning) < 0)
  {
    cat("Warning:\n", sum(dt_warning<0, na.rm=TRUE), " cases have negative age value and will be removed from the calculation", "\n", sep="")
  }

  if (any(dt_warning) > 150)
  {
    cat("Warning:\n", sum(dt_warning>150, na.rm=TRUE), " cases have age value > 150 and will be considered as unknown age", "\n", sep="")
  }

  dt_result <- core.csu_group_cases(df_data, var_age ,group_by=group_by,var_cases = var_cases,df_ICD = df_ICD,var_ICD=var_ICD,var_year = var_year, all_cancer=all_cancer)

  if (all_cancer) {



    ICD_all <- data.frame(ICD=(c("C00-97")), LABEL=c("All cancers"))
    ICD_allbskin <- data.frame(ICD=(c("C00-43", "C45-97")), LABEL=c("All cancers but NMSC", "All cancers but NMSC"))

    
    df_all <- core.csu_group_cases(df_data, var_age ,group_by=group_by,var_cases = var_cases,df_ICD = ICD_all,var_ICD=var_ICD,var_year = var_year)
    df_allbskin <- core.csu_group_cases(df_data, var_age ,group_by=group_by,var_cases = var_cases,df_ICD = ICD_allbskin,var_ICD=var_ICD,var_year = var_year)

    dt_result <- rbind(dt_result, df_all, df_allbskin)
  }



  return (dt_result)
}


