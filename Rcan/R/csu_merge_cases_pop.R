
csu_merge_cases_pop <- function(df_cases,df_pop, var_age,var_cases="cases",var_py=NULL,group_by=NULL) {


  core.error_variable(df_data, var_cases, csu_merge_cases_pop)


  bool_year <- FALSE
  bool_temp <- FALSE
  regex_year  <- "(18|19|20)\\d{2}"



  dt_pop <- as.data.table(df_pop)
  dt_cases <- as.data.table(df_cases)

  if (!is.null(var_py)) {
    
    core.error_variable(df_data, var_py, csu_merge_cases_pop)

    setnames(dt_pop, var_py, "CSU_P")
    for (colyear_pop in colnames(dt_pop)[!colnames(dt_pop) %in% c(var_age,"CSU_P")]) {
      bool_wide = (all(grepl(regex_year,unique(dt_pop[[colyear_pop]]))))
      if (bool_wide) {
        bool_year <- TRUE
        break
      }
    }
    if (bool_year) {
      setnames(dt_pop, colyear_pop, "year")
      dt_pop[,year:=as.numeric(year)]
    }
  }
  else {
    bool_long <- any(grepl(regex_year,colnames(dt_pop)))
     if (bool_long) {
      bool_year <- TRUE
      dt_pop <- melt(dt_pop, c(group_by,var_age), patterns(regex_year), "year", "CSU_P")
      dt_pop[, year:= as.numeric(gsub(".*?((?:18|19|20)\\d{2}).*$", "\\1", year, perl=TRUE))]
      var_py <- "py"
     }
     else {
      stop(paste0("The var_py argument is NULL and there is no year information in the df_pop columns names\n 
          see documentation: Help(", deparse(substitute(csu_merge_cases_pop)), ")"))
     }
  }
  
  merge_col <- c(group_by, var_age)
  if (bool_year) {
    merge_col <- c(merge_col, "year")
  }

  dt_pop[,c(var_age) :=  as.numeric(gsub(".*?(\\d{1,3}).*$", "\\1",get(var_age), perl=TRUE))]

  if (max(dt_pop[[var_age]]) > 25) {
    dt_pop[,c(var_age) := round((get(var_age)/5)+1)]
  }

  dt_pop[get(var_age) >18, c(var_age) := 18 ]
  dt_pop <- dt_pop[, .(CSU_P = sum(CSU_P)), by=merge_col]

  dt_cases[,c(var_age) :=  as.numeric(gsub(".*?(\\d{1,3}).*$", "\\1",get(var_age), perl=TRUE))]

  if (max(dt_cases[[var_age]]) > 25) {
    dt_cases[,c(var_age) := round((get(var_age)/5)+1)]
  }


 
  if (bool_year) {
    for (colyear in colnames(dt_cases)[!colnames(dt_cases) %in% c(var_age,var_cases)]) {
      bool_temp = (all(grepl(regex_year,unique(dt_cases[[colyear]]))))
      if (bool_temp) {
        setnames(dt_cases, colyear, "year")
        break
      }
    }
    if (!bool_temp) {
      stop(paste0("Population data have a variable year: ",colyear_pop,"\nCases data do not have any year variable\n 
          see documentation: Help(", deparse(substitute(csu_merge_cases_pop)), ")"))
    }
  }


  
  


  dt_data <- merge(dt_cases, dt_pop, by= merge_col, all.x=TRUE)
  dt_data[is.na(CSU_P), CSU_P:=0]

  setnames(dt_data, "CSU_P", var_py)

  df_data <- as.data.frame(dt_data)
  return(df_data)
}
