
csu_merge_cases_pop <- function(df_cases,df_pop, var_age,var_cases="cases",var_py=NULL,group_by=NULL) {


  core.error_variable(df_cases, var_cases, csu_merge_cases_pop)
  core.error_variable(df_cases, var_age, csu_merge_cases_pop,type= "")
  core.error_variable(df_pop, var_age, csu_merge_cases_pop,type= "")

  if (!is.null(group_by)){
    for (var in group_by) {
      core.error_variable(df_cases, var, csu_merge_cases_pop,type= "")
      core.error_variable(df_pop, var, csu_merge_cases_pop,type= "")
    }
  }

  temp <- NULL
  bool_year <- FALSE
  bool_year_declared <- FALSE
  bool_temp <- FALSE
  regex_year  <- "(18|19|20)\\d{2}"




  dt_pop <- as.data.table(df_pop)
  dt_cases <- as.data.table(df_cases)

  if (!is.null(var_py)) {
    
    core.error_variable(df_pop, var_py, csu_merge_cases_pop)

    setnames(dt_pop, var_py, "CSU_P")
    for (colyear_pop in colnames(dt_pop)[!colnames(dt_pop) %in% c(var_age,"CSU_P")]) {
      bool_wide = (all(grepl(regex_year,unique(dt_pop[[colyear_pop]]))))
      if (bool_wide) {
        bool_year <- TRUE
        break
      }
    }
    if (bool_year) {
      temp <- colyear_pop
      col_year_final <- "year"
      if (colyear_pop %in% group_by) {
        col_year_final <- colyear_pop 
        group_by <- group_by[!group_by  %in% c(colyear_pop)]
      }
      setnames(dt_pop, colyear_pop, col_year_final)
      dt_pop[,c(col_year_final):=as.numeric(get(col_year_final))]
    }
  }
  else {
    bool_long <- any(grepl(regex_year,colnames(dt_pop)))
     if (bool_long) {
      bool_year <- TRUE
      col_year_final <- "year"
      temp <- colnames(dt_pop)[grepl(regex_year,colnames(dt_pop))]
      dt_pop <- melt(dt_pop, c(group_by,var_age), patterns(regex_year), col_year_final, "CSU_P")
      dt_pop[, c(col_year_final):= as.numeric(gsub(".*?((?:18|19|20)\\d{2}).*$", "\\1", get(col_year_final), perl=TRUE))]
      var_py <- "py"
     }
     else {
      stop(paste0("The var_py argument is NULL and there is no year information in the df_pop columns names\n 
          see documentation: Help(", deparse(substitute(csu_merge_cases_pop)), ")"))
     }
  }
  
 

  merge_col <- c(group_by, var_age)
  if (bool_year) {
    merge_col <- c(merge_col,col_year_final)
  }

  dt_pop[,c(var_age) :=  as.numeric(gsub(".*?(\\d{1,3}).*$", "\\1",get(var_age), perl=TRUE))]

  if (max(dt_pop[[var_age]]) > 25) {
    dt_pop[,c(var_age) := round((get(var_age)/5)+1)]
  }

  dt_pop[get(var_age) >18, c(var_age) := 18 ]
  dt_pop <- dt_pop[, .(CSU_P = sum(CSU_P)), by=merge_col]


  #keep cases age format 
  dt_cases[, temp_label:=get(var_age)]
  dt_cases[,c(var_age) :=  as.numeric(gsub(".*?(\\d{1,3}).*$", "\\1",get(var_age), perl=TRUE))]

  if (max(dt_cases[[var_age]]) > 25) {
    dt_cases[,c(var_age) := round((get(var_age)/5)+1)]
  }


  if (bool_year) {
    for (colyear in colnames(dt_cases)[!colnames(dt_cases) %in% c(var_age,var_cases)]) {
      bool_temp = (all(grepl(regex_year,unique(dt_cases[[colyear]]))))
      if (bool_temp) {
        setnames(dt_cases, colyear, col_year_final)
        break
      }
    }
    if (!bool_temp) {
      stop(paste0("Population data have a variable year: ",colyear_pop,"\nCases data do not have any year variable\n 
          see documentation: Help(", deparse(substitute(csu_merge_cases_pop)), ")"))
    }
  }

  var_data_pop <- colnames(df_pop)
  var_data_pop <- var_data_pop[!var_data_pop  %in% c(var_py, merge_col, temp)]

  if (length(var_data_pop) > 0) {
        warning(paste0('The population dataset variable: ',var_data_pop,' is not present in the group_by option.\nPopulation data might have been summed, please check carefully.\n\n'))
  }

  
  dt_data <- merge(dt_cases, dt_pop, by= merge_col, all.x=TRUE) #first error
  dt_data[is.na(CSU_P), CSU_P:=0]

  

  dt_data[,c(var_age):=temp_label]
  dt_data[,temp_label:=NULL]

  setnames(dt_data, "CSU_P", var_py)





  df_data <- as.data.frame(dt_data)
  return(df_data)
}
