
icd_group <- function(icd_list) {

  bool_follow <- FALSE 
  icd_first <- icd_list[1]
  icd_long <- icd_first

  code_active = as.numeric(sub(".+?(\\d+)", "\\1", icd_first))

  for (code in icd_list[-1]) {

    code_new = as.numeric(sub(".+?(\\d+)", "\\1", code))
    bool_follow <- code_new == code_active + 1

    if (bool_follow) {
      icd_long <- paste0(icd_first, '-', as.character(code_new))

    }
    else {
      icd_long <- paste0(icd_long, ',', as.character(code))
      icd_first <- icd_long
    }
    code_active <- code_new

  }
  return(icd_long)
}


data_group <- function(inc_file, var_age ,cross_by=NULL,group_by=NULL,var_cases = NULL,ICD_file = NULL,var_ICD=NULL,var_year = NULL) {

  group_by <- unique(c(cross_by,group_by,var_year))

  df_inc <- read.table(inc_file, header = TRUE, sep = ",")
  dt_inc <- data.table(df_inc)

  if (is.null(var_cases)) {
    var_cases <- "cases"
    dt_inc[, cases:= 1]
  } else {
    setnames(dt_inc, var_cases, "cases")
  }

  dt_inc <- dt_inc[, unique(c(var_cases, var_age,group_by,var_ICD)), with = FALSE]


  if (!is.null(ICD_file)) {
  # merge with ICD 
    df_ICD <- read.table(ICD_file, header=TRUE, sep=",")
    dt_ICD <- data.table(df_ICD)
    setkeyv(dt_ICD,c("LABEL", "ICD")) 
    dt_ICD[, ICD_group:= sapply(LABEL, function(x) {icd_group(as.vector(dt_ICD[LABEL == x, ]$ICD))})]
    list_ICD <- dt_ICD$ICD

    dt_inc[, temp := as.character(get(var_ICD))]
    dt_inc[, ICD := list_ICD[match(dt_inc$temp, list_ICD)]]
    dt_inc[!is.na(ICD), temp := NA]
    dt_inc[, temp:= substr(temp, 1, 3)] 
    dt_inc[, ICD:=list_ICD[match(dt_inc$temp, list_ICD)]]
    dt_inc <- dt_inc[!is.na(ICD), ]
    dt_inc <- merge(dt_inc, dt_ICD, by=c("ICD"))
    dt_inc[,c("temp", var_ICD,"ICD") := list(NULL, NULL,NULL)]
    dt_ICD[,ICD := NULL]


    group_by <- c(group_by, "LABEL")
    cross_by <- c(cross_by, "ICD_group")

    dt_ICD <- NULL

  }

  #  create age group 
  dt_inc[, age_group:= cut(get(var_age), c(seq(0, 85, 5), 150), include.lowest = TRUE, right=FALSE)]
  dt_inc[, age_group_label := as.character(age_group)]
  dt_inc[, temp1 := sub("\\[(\\d{1,3}),(\\d{1,3}).+", "\\1",age_group_label)]
  dt_inc[, temp2 := as.numeric(sub("\\[(\\d{1,3}),(\\d{1,3}).+", "\\2",age_group_label))]
  dt_inc[, age_group :=  ifelse(temp2 == 150 ,18,temp2/5)]
  dt_inc[, age_group_label := ifelse(temp2 == 150, paste0(temp1,"+"), paste0(temp1,"-", as.character(temp2-1)))] 
  dt_inc[is.na(age_group), age_group :=  19]
  dt_inc[,c("temp1","temp2", var_age) := list(NULL, NULL, NULL)]

  dt_inc <-  dt_inc[,list(cases = sum(cases)),by=eval(colnames(dt_inc)[!colnames(dt_inc) %in% c("cases")])]

  group_by <- c(group_by, c("age_group_label"))
  cross_by <- c(cross_by, "age_group")

  dt_CJ = dt_inc[, do.call(CJ, c(.SD, unique=TRUE)), .SDcols=cross_by]

  group_by <- group_by[!group_by %in% cross_by]


  ##keep ICD group label
  if (!is.null(group_by)){
    for (var in group_by) {
      for(base in cross_by) {
        dt_test <- unique(dt_inc[, c(var,base), with=FALSE])
        dt_base <- unique(dt_test[, c(base), with=FALSE])
        if (nrow(dt_test) == nrow(dt_base)) {
          dt_CJ <- merge(dt_CJ, dt_test, by=base, all.x=TRUE)
          print(dt_CJ)
        }
      }
    }
  }

  dt_inc <- merge(dt_CJ, dt_inc,by=colnames(dt_temp), all.x=TRUE)[, cases := ifelse(is.na(cases),0, cases )]
  return (dt_inc) 
}

