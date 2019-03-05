csu_group_cases <- function(df_data, var_age ,cross_by=NULL,group_by=NULL,var_cases = NULL,df_ICD = NULL,var_ICD=NULL,var_year = NULL) {


  group_by <- unique(c(cross_by,group_by))

  dt_data <- data.table(df_data)

  if (is.null(var_cases)) {
    var_cases <- "cases"
    dt_data[, cases:= 1]
  } else {
    setnames(dt_data, var_cases, "cases")
  }

  dt_data <- dt_data[, unique(c(var_cases, var_age,group_by,var_ICD,var_year)), with = FALSE]

  if (!is.null(var_year)) {
    dt_data$year <-  core.csu_year_extract(dt_data[[var_year]])
    dt_data[[var_year]] <- NULL
    cross_by <- c(cross_by, "year")

  }


  if (!is.null(df_ICD)) {
  # merge with ICD 
    dt_ICD <- data.table(df_ICD)
    setkeyv(dt_ICD,c("LABEL", "ICD")) 
    dt_ICD[, ICD_group:= sapply(LABEL, function(x) {core.csu_icd_group(as.vector(dt_ICD[LABEL == x, ]$ICD))})]
    list_ICD <- dt_ICD$ICD

    dt_data$temp <- as.character(dt_data[[var_ICD]])
    dt_data[, ICD := list_ICD[match(dt_data$temp, list_ICD)]]
    dt_data[!is.na(ICD), temp := NA]
    dt_data[, temp:= substr(temp, 1, 3)] 
    dt_data[!is.na(temp), ICD:=list_ICD[match(dt_data[!is.na(temp)]$temp, list_ICD)]]
    dt_data <- dt_data[!is.na(ICD), ]
    dt_data <- merge(dt_data, dt_ICD, by=c("ICD"))
    dt_data[,c("temp", var_ICD,"ICD") := list(NULL, NULL,NULL)]
    dt_ICD[,ICD := NULL]


    group_by <- c(group_by, "LABEL")
    cross_by <- c(cross_by, "ICD_group")

    dt_ICD <- NULL

  }

  #  create age group 
  dt_data[, age_group:= cut(get(var_age), c(seq(0, 85, 5), 150), include.lowest = TRUE, right=FALSE)]
  dt_data[, age_group_label := as.character(age_group)]
  dt_data[, temp1 := sub("\\[(\\d{1,3}),(\\d{1,3}).+", "\\1",age_group_label)]
  dt_data[, temp2 := as.numeric(sub("\\[(\\d{1,3}),(\\d{1,3}).+", "\\2",age_group_label))]
  dt_data[, age_group :=  ifelse(temp2 == 150 ,18,temp2/5)]
  dt_data[, age_group_label := ifelse(temp2 == 150, paste0(temp1,"+"), paste0(temp1,"-", as.character(temp2-1)))] 
  dt_data[is.na(age_group), age_group :=  19]
  dt_data[,c("temp1","temp2", var_age) := list(NULL, NULL, NULL)]

  dt_data <-  dt_data[,list(cases = sum(cases)),by=eval(colnames(dt_data)[!colnames(dt_data) %in% c("cases")])]

  group_by <- c(group_by, c("age_group_label"))
  cross_by <- c(cross_by, "age_group")

  dt_CJ = dt_data[, do.call(CJ, c(.SD, unique=TRUE)), .SDcols=cross_by]

  group_by <- group_by[!group_by %in% cross_by]


  ##keep ICD group label
  if (!is.null(group_by)){
    for (var in group_by) {
      for(base in cross_by) {
        dt_test <- unique(dt_data[, c(var,base), with=FALSE])
        dt_base <- unique(dt_test[, c(base), with=FALSE])
        if (nrow(dt_test) == nrow(dt_base)) {
          dt_CJ <- merge(dt_CJ, dt_test, by=base, all.x=TRUE)
        }
      }
    }
  }

  dt_data <- merge(dt_CJ, dt_data,by=colnames(dt_CJ), all.x=TRUE)[, cases := ifelse(is.na(cases),0, cases )]
    return (dt_data) 
}



