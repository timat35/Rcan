
test_that("Test csu_group_cases: 1",{

  #input data
  data(ICD_group_GLOBOCAN)
  data(data_individual_file)

  #output result
  output_test <- csu_group_cases(data_individual_file,
    var_age="age",
    group_by=c("sex", "regcode", "reglabel"),
    df_ICD = ICD_group_GLOBOCAN,
    var_ICD  ="site") 

  #saveRDS(output_test,"csu_group_cases_test1.rds")
  expect_test <- readRDS(system.file("testdata","csu_group_cases_test1.rds",package="Rcan"))
  #test
  expect_equal(output_test, expect_test)
})

test_that("Test csu_merge_cases_pop: 1",{

  #input data
  data(ICD_group_GLOBOCAN)
  data(data_individual_file)
  data(data_population_file)

  #output result
  df_data_year <- csu_group_cases(data_individual_file,
    var_age="age",
    group_by=c("sex", "regcode"),
    df_ICD = ICD_group_GLOBOCAN,
    var_ICD  ="site",
    var_year = "doi")     


  output_test <- csu_merge_cases_pop(
    df_data_year, 
    data_population_file, 
    var_age = "age_group",
    var_cases = "cases",
    var_py = "pop",
    group_by = c("sex"))

  expect_test <- readRDS(system.file("testdata","csu_merge_cases_pop1.rds",package="Rcan"))
  #test
  expect_equal(output_test, expect_test)
})


test_that("Test csu_asr: 1",{

  #input data
  data(csu_registry_data_1)

  #output result
  output_test <- csu_asr(csu_registry_data_1, 
                  "age", "cases", "py",
                  group_by = c("registry", "registry_label" ),
                  var_age_group = c("registry_label"))

  expect_test <- readRDS(system.file("testdata","csu_asr_test1.rds",package="Rcan"))

  #test
  expect_equal(output_test, expect_test)
})

test_that("Test csu_cumrisk: 1",{

  #input data
  data(csu_registry_data_1)

  #output result
  output_test <- csu_cumrisk(csu_registry_data_1, 
                  "age", "cases", "py",
                  group_by = c("registry", "registry_label" ))

  expect_test <- readRDS(system.file("testdata","csu_cumrisk_test1.rds",package="Rcan"))

  #test
  expect_equal(output_test, expect_test)
})


test_that("Test age specific: 1",{

  #input data
  
  data(csu_registry_data_1)
  data(csu_registry_data_2)
  data_test <- csu_registry_data_1[csu_registry_data_1$registry_label=="Colombia, Cali",]

  #output result
  output_test <- csu_ageSpecific(data_test,plot_title = "Colombia, Liver, male")

  #expect result
 
  expect_test <- readRDS( system.file("testdata","csu_graph_ageSpecific_test1.rds",package="Rcan"))
  #test
  expect_equal(output_test, expect_test)


})

test_that("Test trend: 1",{
  
  #input data
  data(csu_registry_data_2)
  data_test <- csu_registry_data_2[csu_registry_data_2$registry_label=="Colombia, Cali",]
  
  
  #output result
  data_test$sex <- factor(data_test$sex, levels=c(1,2), labels=c("Male", "Female"))
  data_test <- csu_asr(data_test,missing_age = 99,
                    group_by  = c("registry", "registry_label", "year", "sex"),
                    var_age_group =  c("registry", "registry_label"))
  
  
  
  output_test <- csu_time_trend(data_test, group_by="sex",logscale=TRUE,
                           plot_title = "Colombia, Liver",
                           smoothing = 0.3)
  
  #expect result
 
  expect_test <- readRDS( system.file("testdata","csu_trend_test1.rds",package="Rcan"))
  #test
  expect_equal(output_test, expect_test)

  
  
})


test_that("Test eapc: 1",{
  
  #input data
  data(csu_registry_data_2)
  
  #output result
  data_test <- csu_asr(csu_registry_data_2, 
                    "age", "cases", "py",
                    group_by = c("registry", "registry_label", "sex", "year", "ethnic" ),
                    var_age_group = c("registry_label", "year"), 
                    missing_age = 99)
  
  output_test <- csu_eapc(data_test,
                   "asr", "year",
                   group_by=c("registry", "registry_label", "sex", "ethnic" ))
  
  #expect result
  expect_test <- readRDS(system.file("testdata","csu_eapc_test1.rds",package="Rcan"))
  #test
  expect_equal(output_test, expect_test)

})
