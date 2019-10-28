## Apology 2
I'm aware packages should be update no more than every 1-2 months. 
This package wil be used in the IARC summer course the first week of July, and the team found new warnings/errors, 
difficulties for the course. They are corrected in this version.

## Test environments
* local windows 10 (devel and release)
* win-builder (devel)

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs. 

## News
* Improve warnings system
* Force csu_ageSpecific_top grup_by parameter to factor
* "year" accepted as variable name in csu_group_cases
* ICD grouping accepted other letters than C
* keep age format in csu_merge_cases_pop
* Allow year in group_by parameter for csu_merge_cases_pop
* Create missing age group for csu_group_cases
* fix missing age in csu_trendCohortPeriod
* add don't test for long example