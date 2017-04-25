csu_asr <-
function(df_data,
	var_age="age",
	var_cases="cases",
	var_py="py",
	group_by=NULL,
	var_age_group=NULL,
	missing_age = NULL,
	db_rate = 100000,
	first_age = 1,
	last_age = 18,
	pop_base = "SEGI",
	var_st_err=NULL,
	correction_info=FALSE,
	var_asr="asr",
	age_dropped = FALSE) {
  
  
  bool_dum_by <- FALSE
  bool_dum_age <- FALSE
  
  if (first_age < 1 | first_age > 17 ) {
    stop('The argument "first_age" must be comprise between 1 (0-4) and 17 (80-85), see documentation: help(csu_asr)')
  }
  
  if (last_age < 2 | last_age > 18 ) {
    stop('The argument "last_age" must be comprise between 2 (5-9) and 18 (85+), see documentation: help(csu_asr)')
  }
  
  if (!(var_age%in% colnames(df_data))) {
    
    stop('var_age value is not a variable name of the data, see documentation: Help(csu_asr)')
    
  }
  
  if (!(var_cases%in% colnames(df_data))) {
    
    stop('var_cases value is not a variable name of the data, see documentation: Help(csu_asr)')
    
  }
  
  if (!(var_py%in% colnames(df_data))) {
    
    stop('var_py value is not a variable name of the datae, see documentation: Help(csu_asr)')
    
  }
  

    
  if (is.null(group_by)) {
    
    df_data$CSU_dum_by <- "dummy_by"
    group_by <- "CSU_dum_by"
    bool_dum_by <- TRUE
    
  }
  
  
  if (is.null(var_age_group)) {
    
    df_data$CSU_dum_age <- "dummy_age_gr"
    var_age_group <- "CSU_dum_age"
    group_by <- c(group_by, "CSU_dum_age")
    bool_dum_age <- TRUE
    
  }
  
  
  dt_data <- data.table(df_data, key = group_by) 
  setnames(dt_data, var_age, "CSU_A")
  setnames(dt_data, var_cases, "CSU_C")
  setnames(dt_data, var_py, "CSU_P")
  
  temp <- dt_data[, lapply(.SD, function(x) is.numeric(x)) ]
  
  if (!temp[["CSU_A"]]) {
    
    stop('The variable "age" must be numeric, see documentation: help(csu_asr)')
    
  }
  
  if (!temp[["CSU_P"]]) {
    
    stop('The variable "population" must be numeric, see documentation: help(csu_asr)')
    
  }
  
  if (!temp[["CSU_C"]]) {
    
    stop('The variable "age" must be numeric, see documentation: help(csu_asr)')
    
  }
  
  temp <- NULL
  
  # create index to keep order
  index_order <- c(1:nrow(dt_data))
  dt_data$index_order <- index_order
  
  # create world population DF for different nb of age group
  SEGI_pop <- c(12000,10000,9000,9000,8000,8000,6000,6000,6000,6000,5000,4000,4000,3000,2000,1000,500,500)
  EURO_pop <- c(8000,7000,7000,7000,7000,7000,7000,7000,7000,7000,7000,6000,5000,4000,3000,2000,1000,1000)
  
  
  if (pop_base == "EURO") {
    pop <- EURO_pop
  } else {
    pop <- SEGI_pop
  }
  
  # calculated total pop for age selected 
  total_pop <- sum(pop[first_age:last_age])
  
  Standard_pop <- data.table(pop = pop, age_factor= c(1:18))
  
  pop[17] <- pop[17]+ pop[18]
  pop[18] <- 0
  Standard_pop$pop17 <- pop
  pop[16] <- pop[16]+ pop[17]
  pop[17] <- 0
  Standard_pop$pop16 <- pop
  pop[15] <- pop[15]+ pop[16]
  pop[16] <- 0
  Standard_pop$pop15 <- pop
  
  
  
  
  
  # missing age 
  
  dt_data[dt_data$CSU_A==missing_age,CSU_A:=NA ] 
  dt_data[is.na(dt_data$CSU_A),CSU_P:=0 ] 
  
  #create age dummy: 1 2 3 4 --- 19
  dt_data$age_factor <- c(as.factor(dt_data$CSU_A))
  
  # correction factor 
  dt_data$correction <- 1 
  if (!is.null(missing_age)) {
    
    
    dt_data[, total:=sum(CSU_C), by=group_by] #add total
    dt_data[!is.na(dt_data$age_factor) , total_known:=sum(CSU_C), by=group_by] #add total_know
    dt_data$correction <- dt_data$total / dt_data$total_know 
    dt_data[is.na(dt_data$correction),correction:=1 ] 
    dt_data$total <- NULL
    dt_data$total_known <- NULL
    
  }
  
  
  
  
  

  
  #age dropped option
  if (age_dropped) {
    
    dt_data$age_factor <- dt_data$age_factor + first_age -1   
    
  }
  
  # keep age selected 
  dt_data=dt_data[dt_data$age_factor %in% c(first_age:last_age) | is.na(dt_data$age_factor), ]
  
  # calculated maximum age group with population data
  if (last_age == 18) {
    dt_data <- merge(dt_data, dt_data[dt_data$CSU_P != 0,list(nb_age_group = max(age_factor)), by=var_age_group], by=var_age_group)  
  } else {
    dt_data$nb_age_group <- 18
  }
  
  # show population with less than 18 age group
  if (last_age == 18) {
    temp <- subset(dt_data,nb_age_group <18, select= c(var_age_group, "nb_age_group"))
    if (nrow(temp) >0) {
      setkey(temp,NULL)
      cat("\n")
      cat("Population with less than 18 age group:\n\n" )
      print
      print(unique(temp), row.names = FALSE)
      cat("\n")
    }
    temp <- NULL
  }
  
  #regroup case for population with nb of age group <  18 
  
  
  for (i in 15:17) {
    
    if (i %in% dt_data$nb_age_group) {
      
      dt_data[dt_data$nb_age_group == i & dt_data$age_factor >= i , CSU_C:=sum(CSU_C), by=group_by] #add total_know
      dt_data[dt_data$nb_age_group == i & dt_data$age_factor > i & !is.na(dt_data$age_factor), CSU_C := 0] 
      
    } 
  }
  
  
  
  #add world pop to database 
  
  dt_data <- merge(dt_data,Standard_pop, by =c("age_factor"), all.x=TRUE )
  Standard_pop <- NULL
  dt_data[dt_data$nb_age_group==17, pop:=dt_data$pop17[dt_data$nb_age_group==17]]
  dt_data[dt_data$nb_age_group==16, pop:=dt_data$pop16[dt_data$nb_age_group==16]]
  dt_data[dt_data$nb_age_group==15, pop:=dt_data$pop15[dt_data$nb_age_group==15]]
  
  
  #calcul ASR
  
  dt_data[dt_data$CSU_P != 0,rate:= dt_data$CSU_C[dt_data$CSU_P != 0]/ dt_data$CSU_P[dt_data$CSU_P != 0] * db_rate]
  dt_data$asr <- dt_data$rate * dt_data$pop
  dt_data[is.na(dt_data$asr),asr:=0 ] 
  
  dt_data$st_err <- ( dt_data$rate * (dt_data$pop^2) * (db_rate - dt_data$rate))/dt_data$CSU_P
  dt_data[is.na(dt_data$st_err),st_err:=0 ] 
  
  # to check order 
  dt_data<- dt_data[order(dt_data$index_order ),]
  
  dt_data<-  dt_data[,list( CSU_C=sum(CSU_C), CSU_P=sum(CSU_P),asr=sum(asr),st_err = sum(st_err),correction = max(correction)), by=group_by]
  
  dt_data$asr <- dt_data$asr / total_pop
  dt_data$asr <- dt_data$asr * dt_data$correction
  dt_data$st_err <- (dt_data$st_err / (total_pop^2))^(1/2)
  dt_data$st_err <- dt_data$st_err * dt_data$correction
  
  dt_data$asr <- round(dt_data$asr, digits = 2)
  dt_data$st_err <- round(dt_data$st_err, digits = 2)
  dt_data$correction <- round((dt_data$correction-1)*100, digits = 1)
  
  if (is.null(var_st_err)) {
    dt_data$st_err <- NULL
  } else {
    setnames(dt_data, "st_err", var_st_err)
  }

  if (var_asr!="asr") {
    setnames(dt_data, "asr", var_asr)
  }
  
  if (!correction_info) {
    dt_data$correction <- NULL
  }
  
  
  df_data <- data.frame(dt_data)
  

  
  if (bool_dum_age) {
    df_data$CSU_dum_age <- NULL
  }
  if (bool_dum_by) {
    df_data$CSU_dum_by <- NULL
  }


  setnames(df_data, "CSU_C", var_cases)
  setnames(df_data,  "CSU_P", var_py)
  
  temp <- last_age*5-1
  if (last_age == 18)  temp <- "99+"
  cat("ASR have been computed for the age group ", (first_age-1)*5,"-", temp , "\n",  sep="" )
  temp<- NULL
  
  return(df_data)
  
  

}
