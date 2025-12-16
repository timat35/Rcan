library(Rcan)

setwd('c:/project/Rcan/temp')
load('Mathieu.Rdata')

# Age-specific rates above 500 are not plotted. 

p <- csu_trendCohortPeriod(df_data=colrec,
                           var_age = 'age', 
                           var_cases = 'age_specific_number',
                           var_py = 'age_specific_population',
                           plot_title = "",
                           type="Cohort",
                           first_age=5,
                           last_age=17, 
                           logscale = FALSE)

