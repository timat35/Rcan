csu_group_cases <- function(df_data, var_age ,group_by=NULL,var_cases = NULL,df_ICD = NULL,var_ICD=NULL,var_year = NULL) {

  if (!is.null(df_ICD) & is.null(var_ICD)) {
    stop(paste0("There is no ICD variable defined (var_ICD = NULL) to use with the ICD file defined\n 
      see documentation: Help(", deparse(substitute(csu_group_cases)), ")"))
  }

    if (!is.null(df_ICD) & is.null(var_ICD)) {
    stop(paste0("There is no ICD list defined (df_ICD = NULL) to use with the ICD variable defined\n 
      see documentation: Help(", deparse(substitute(csu_group_cases)), ")"))
  }


  label_by <- NULL
  dt_data <- data.table(df_data)
  

  if (is.null(var_cases)) {
    var_cases <- "cases"
    dt_data[, cases:= 1]
  } else {
    setnames(dt_data, var_cases, "cases")
  }

  dt_data <- dt_data[, unique(c(var_cases, var_age,group_by,var_ICD,var_year)), with = FALSE]

  if (!is.null(var_year)) {
    dt_data$year <-  Rcan:::core.csu_year_extract(dt_data[[var_year]])
    dt_data[, (var_year) := NULL]  
    group_by <- c(group_by, "year")

  }


  if (!is.null(df_ICD)) {
  # merge with ICD 
    dt_ICD <- data.table(df_ICD)
    setkeyv(dt_ICD,c("LABEL", "ICD")) 
    dt_ICD <- unique(dt_ICD)

    #ungroup ICD code 
    dt_table <- data.table()
    for (row in 1:nrow(dt_ICD)) {

      icd_group <- as.character(dt_ICD[row]$ICD)
      temp <- Rcan:::core.csu_icd_ungroup(paste(icd_group, collapse=","))
      temp <- data.table(ICD_ungroup = temp, ICD =icd_group )
      
      dt_table <- rbind(dt_table, temp)

    }

    dt_ICD <- merge(dt_ICD, dt_table, by="ICD")
    dt_ICD[, ICD:=NULL]
    dt_ICD <- unique(dt_ICD)

    dt_ICD_unique <- setDT(dt_ICD)[, .N, keyby=ICD_ungroup][N>1,]  

    if (!is.null(dt_ICD_unique)) {

      dt_ICD_unique <- merge(dt_ICD_unique,dt_ICD,by="ICD_ungroup", all.x=TRUE) 
      dt_ICD_unique <- merge(dt_ICD_unique, df_ICD, by="LABEL")
      dt_ICD_unique <- dt_ICD_unique[,c("LABEL", "ICD") , with=FALSE]
      cat("-----\n")
      print(as.data.frame(dt_ICD_unique))
      stop(paste0("There is different label with the same ICD code in the ICD file defined")) 

    }

    setnames(dt_ICD, "ICD_ungroup", "ICD")

    dt_ICD[, ICD_group:= sapply(LABEL, function(x) {Rcan:::core.csu_icd_group(as.vector(dt_ICD[LABEL == x, ]$ICD))})]
    list_ICD <- dt_ICD$ICD

    dt_data$temp <- as.character(dt_data[[var_ICD]])
    dt_data[, ICD := list_ICD[match(dt_data$temp, list_ICD)]]
    dt_data[!is.na(ICD), temp := NA]
    dt_data[, temp:= substr(temp, 1, 3)] 
    dt_data[!is.na(temp), ICD:=list_ICD[match(dt_data[!is.na(temp)]$temp, list_ICD)]]
    dt_data <- dt_data[!is.na(ICD), ]
    dt_data <- merge(dt_data, dt_ICD, by=c("ICD"))
    dt_data[,c("temp", var_ICD,"ICD") := list(NULL, NULL,NULL)]
    dt_data[, LABEL := factor(LABEL)]
    dt_data[, ICD_group := factor(ICD_group)]

    label_by <- c(label_by,"LABEL")
    group_by <- c(group_by, "ICD_group")

    dt_ICD <- NULL

  }

  #  create age group 
  dt_data[, age_group:= cut(get(var_age), c(seq(0, 85, 5), 150), include.lowest = TRUE, right=FALSE)]
  dt_data[, age_group_label := as.character(age_group)]
  dt_data[, temp1 := as.numeric(sub("\\[(\\d{1,3}),(\\d{1,3}).+", "\\1",age_group_label))]
  dt_data[, temp2 := as.numeric(sub("\\[(\\d{1,3}),(\\d{1,3}).+", "\\2",age_group_label))]
  dt_data[, age_group :=  ifelse(temp2 == 150 ,18,temp2/5)]
  dt_data[, temp1 := sprintf("%02d", temp1)]
  dt_data[, age_group_label := ifelse(temp2 == 150, paste0(temp1,"+"), paste0(temp1,"-",  sprintf("%02d", temp2-1)))] 
  dt_data[is.na(age_group), age_group :=  19]
  dt_data[,c("temp1","temp2", var_age) := list(NULL, NULL, NULL)]

  dt_data <-  dt_data[,list(cases = sum(cases)),by=eval(colnames(dt_data)[!colnames(dt_data) %in% c("cases")])]

  label_by  <- c(label_by, c("age_group_label"))
  group_by  <- c(group_by, "age_group")

  dt_CJ = dt_data[, do.call(CJ, c(.SD, unique=TRUE)), .SDcols=group_by]

  ##add ICD group label (but #dad is pink)
  if (!is.null(df_ICD)) {
    dt_temp <- unique(dt_data[, c("ICD_group","LABEL"), with=FALSE])
    dt_CJ <- merge(dt_CJ, dt_temp, by="ICD_group", all.x=TRUE)
  }

  ##add age group label
  dt_temp <- unique(dt_data[, c("age_group","age_group_label"), with=FALSE])
  dt_CJ <- merge(dt_CJ, dt_temp, by="age_group", all.x=TRUE)


  dt_data <- merge(dt_CJ, dt_data,by=colnames(dt_CJ), all.x=TRUE)[, cases := ifelse(is.na(cases),0, cases )]


  keep_by <- group_by[!group_by %in% c("year", "age_group", "ICD_group")]

  setkeyv(dt_data, keep_by)
  dt_data[ , group_id := .GRP, by = key(dt_data)]
  dt_data[,temp := sum(cases),by=group_id]
  dt_data <- dt_data[temp>0,]
  dt_data[, c("temp","group_id") := NULL]
  setkeyv(dt_data, group_by)



  return (as.data.frame(dt_data)) 
}


