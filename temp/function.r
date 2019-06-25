core.csu_group_cases <- function(df_data, var_age ,group_by=NULL,var_cases = NULL,df_ICD = NULL,var_ICD=NULL,var_year = NULL, all_cancer=FALSE) {


  label_by <- NULL
  dt_data <- data.table(df_data)

  bool_dum_by <- FALSE
  if (is.null(group_by)) {
      
    dt_data$CSU_dum_by <- "dummy_by"
    group_by <- "CSU_dum_by"
    bool_dum_by <- TRUE
  }
  

  if (is.null(var_cases)) {
    var_cases <- "CSU_C"
    dt_data[, CSU_C:= 1]
  } else {
    setnames(dt_data, var_cases, "CSU_C")
  }

  dt_data <- dt_data[, unique(c("CSU_C", var_age,group_by,var_ICD,var_year)), with = FALSE]

  if (!is.null(var_year)) {
    dt_data$year <-  core.csu_year_extract(dt_data[[var_year]])
    if (var_year != "year") {
      dt_data[, (var_year) := NULL]  
    }
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
      temp <- core.csu_icd_ungroup(paste(icd_group, collapse=","))
      temp <- data.table(ICD_ungroup = temp, ICD =icd_group )
      
      dt_table <- rbind(dt_table, temp)

    }

    dt_ICD <- merge(dt_ICD, dt_table, by="ICD")
    dt_ICD[, ICD:=NULL]
    dt_ICD <- unique(dt_ICD)

    dt_ICD_unique <- setDT(dt_ICD)[, .N, keyby=ICD_ungroup][N>1,]  

    if (nrow(dt_ICD_unique) > 0) {

      dt_ICD_unique <- merge(dt_ICD_unique,dt_ICD,by="ICD_ungroup", all.x=TRUE) 
      dt_ICD_unique <- merge(dt_ICD_unique, df_ICD, by="LABEL")
      dt_ICD_unique <- dt_ICD_unique[,c("LABEL", "ICD") , with=FALSE]
      cat("-----\n")
      print(as.data.frame(dt_ICD_unique))
      stop(paste0("There is different label with the same ICD code in the ICD file defined")) 

    }

    setnames(dt_ICD, "ICD_ungroup", "ICD")

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
    dt_data[, LABEL := factor(LABEL)]
    dt_data[, ICD_group := factor(ICD_group)]

    label_by <- c(label_by,"LABEL")
    group_by <- c(group_by, "ICD_group")

    dt_ICD <- NULL

  }

  # create age group
  dt_data[, age_group:= ((get(var_age) - (get(var_age) %% 5))/5)+1]
  dt_data[, age_group :=  ifelse(age_group > 18 & age_group <31,18,age_group)]
  dt_data[, age_group :=  ifelse(age_group > 18 ,19,age_group)]
  dt_data[, c(var_age) := NULL]

  dt_data <-  dt_data[,list(CSU_C = sum(CSU_C)),by=eval(colnames(dt_data)[!colnames(dt_data) %in% c("CSU_C")])]

  #complete missing age group
  if (max(dt_data$age_group) == 19) {
    temp <- c(1:19)
  }
  else {
     temp <- c(1:18)
  }

  dt_CJ = dt_data[, do.call(CJ, c(.SD,list(age_group=temp), unique=TRUE)), .SDcols=group_by]
  temp <- copy(colnames(dt_CJ))

  # add age_group label
  dt_CJ[, temp1 := sprintf("%02d",(age_group-1)*5)]
  dt_CJ[, temp2 := (age_group*5)-1]
  dt_CJ[, age_group_label := ifelse(temp2 == 89, paste0(temp1,"+"), paste0(temp1,"-",  sprintf("%02d", temp2)))] 
  dt_CJ[age_group == 19 , age_group_label :=  "Unknown"]
  dt_CJ[,c("temp1","temp2") := list(NULL, NULL)]

  group_by  <- c(group_by, "age_group")

  ##add ICD group label (but #dad is pink)
  if (!is.null(df_ICD)) {
    dt_temp <- unique(dt_data[, c("ICD_group","LABEL"), with=FALSE])
    dt_CJ <- merge(dt_CJ, dt_temp, by="ICD_group", all.x=TRUE)
  }

  dt_data <- merge(dt_CJ, dt_data,by=temp, all.x=TRUE)[, CSU_C := ifelse(is.na(CSU_C),0, CSU_C )]
  keep_by <- group_by[!group_by %in% c("year", "age_group", "ICD_group")]

  setkeyv(dt_data, keep_by)
  dt_data[ , group_id := .GRP, by = key(dt_data)]
  dt_data[,temp := sum(CSU_C),by=group_id]
  dt_data <- dt_data[temp>0,]
  dt_data[, c("temp","group_id") := NULL]
  setkeyv(dt_data, group_by)



  if (var_cases=="CSU_C") {
    setnames(dt_data, "CSU_C","cases")
    var_cases <- NULL
  }
  else {
    setnames(dt_data, "CSU_C",var_cases)
  }

  if (bool_dum_by) {
    dt_data$CSU_dum_by <- NULL
  }

  dt_data <- as.data.frame(dt_data)


  return (dt_data)
}


