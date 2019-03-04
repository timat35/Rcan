csu_merge_inc_pop <- function(inc_file,
                              pop_file,
                              var_cases = "CASES",
                              var_age = "AGE_GROUP",
                              var_age_label = "AGE_GROUP_LABEL",
                              var_pop = "COUNT",
                              var_ref_count = "REFERENCE_COUNT",
                              group_by = NULL,
                              column_group_list = NULL){
  
  df_inc <- read.table(inc_file, header=TRUE, sep="\t")
  df_pop <- read.table(pop_file, header=TRUE, sep="\t")
  
  dt_inc <- data.table(df_inc)
  dt_pop <- data.table(df_pop)
  
  setnames(dt_inc, var_cases, "CSU_C")
  
  column_group_list[[1]]  <- intersect(column_group_list[[1]],colnames(dt_inc))
  group_by <- intersect(group_by,colnames(dt_inc))
  
  dt_inc <- dt_inc[, c(var_age, group_by, "CSU_C"), with = FALSE]
  dt_inc <-  dt_inc[,list(CSU_C = sum(CSU_C)), by=eval(colnames(dt_inc)[!colnames(dt_inc) %in% c("CSU_C")])]
  
  if (!is.null(column_group_list)){
    cj_var <- colnames(dt_inc)[!colnames(dt_inc) %in% unlist(c("CSU_C",lapply(column_group_list, `[`, -1)))]
  } else {
    cj_var <-colnames(dt_inc)[!colnames(dt_inc) %in% c("CSU_C")]
  }
  
  dt_temp = dt_inc[, do.call(CJ, c(.SD, unique=TRUE)), .SDcols=cj_var]
  
  ##keep ICD group label
  if (!is.null(column_group_list)){
    nb_group <- length(column_group_list)
    for( i in 1:nb_group) {
      dt_col_group <- unique(dt_inc[, column_group_list[[i]], with=FALSE])
      dt_temp <- merge(dt_temp, dt_col_group,by= column_group_list[[i]][[1]], all.x=TRUE)
    }
  }
  
  dt_inc <- merge(dt_temp, dt_inc,by=colnames(dt_temp), all.x=TRUE)[, CSU_C := ifelse(is.na(CSU_C),0, CSU_C )]
  
  if (nrow(dt_inc[!SEX %in% c(1,2)]) > 0){
    
    var_group2 <- "ICD10GROUP"
    if ("BASIS" %in% group_by) {
      var_group2 <- c(var_group2,"BASIS" )
    }
    
    dt_inc <- canreg_attr_missing_sex(dt_inc, var_age, var_group2)
  }
  
  dt_pop <- dt_pop[get(var_pop) != 0,]
  dt_pop[[var_ref_count]] <-  dt_pop[[var_ref_count]]*100
  
  dt_all <- merge(dt_inc, dt_pop,by=intersect(colnames(dt_inc),colnames(dt_pop)), all.x=TRUE)
  
  #create ICD10color if not existing (take care of NA when using the color) and add cancer_label
  dt_all$cancer_label <- canreg_cancer_info(dt_all)$cancer_label
  if (!"ICD10GROUPCOLOR" %in% colnames(dt_all)) {
    
    dt_color_map <- csu_cancer_color(unique(canreg_cancer_info(dt_all)$cancer_label))
    dt_all <- merge(dt_all, dt_color_map, by = c("cancer_label"), all.x=TRUE, sort=F )
  }
  
  setnames(dt_all,var_age,"CSU_A")
  setnames(dt_all,var_pop,"CSU_P")
  
  
  dt_all[is.na(get(var_age_label)), CSU_A := max(CSU_A)]
  dt_all <-  dt_all[,list(CSU_C = sum(CSU_C), CSU_P = sum(CSU_P)), by=eval(colnames(dt_all)[!colnames(dt_all) %in% c("CSU_C", "CSU_P")])]
  
  setnames(dt_all,"CSU_P",var_pop)
  setnames(dt_all,"CSU_A",var_age)
  setnames(dt_all,"CSU_C",var_cases)
  return(dt_all)
}