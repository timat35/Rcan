#####################################
## EXERCISE 0 - DATA MANIPULATION  ##
#####################################

# SET WORKING DIRECTORY 
setwd("D:/LMICourse/Exercises/0-Manipulation")

## SOURCE: ARTIFICIAL REAL LIFE CANCER REGISTRY DATA
## SOURCE: ARTIFICIAL REAL LIFE CANCER REGISTRY DATA
## SOURCE: ARTIFICIAL REAL LIFE CANCER REGISTRY DATA

#####################################
# ------ IMPORT THE DATA FILE -------
#####################################

# READ THE DATA FILE
dat = read.csv(file="Exercise0.csv", header=TRUE, sep=",")

# LOOK AT THE INTERNAL STRUCTURE OF THE DATASET
str(dat)

# DISPLAY FIRST LINES
head(dat)

# DISPLAY VARIABLES NAMES
names(dat)


#####################################
# ------ TRANSFORM THE DATA FILE ----
#####################################

# VARIABLES SELECTION
dat = dat[,c("AGE", "INCID", "SEX", "BIRTHD", "LABEL", "I10", "TOP")]

# RENAME A VARIABLE
names(dat)[names(dat) == "LABEL"] = "CANCER_LAB"

# ADDING A VARIABLE
dat$REGISTRY_LAB <- "COUNTRY_1"


##################################
# ------ DESCRIBE THE DATA -------
##################################

# SUMMARISES VARIABLES
summary(dat)

# SEX CATEGORISATION
table(dat$SEX)

# CANCER CATEGORISATION
table(dat$CANCER_LAB)

# AGE RANGE
range(dat$AGE)
range(dat$AGE[dat$AGE!=999])
# Number of records having code missing for AGE
nrow(dat[dat$AGE==999,]) #the data frame
length(dat$AGE[dat$AGE==999]) #the vector

# AGE DISTRIBUTION
hist(dat$AGE[dat$AGE!=999])


#################################
# ------ CONVERT THE DATA -------
#################################

# REPLACE MISSING AGE
dat$AGE[is.na(dat$AGE)] <- 999
dat$AGE[dat$AGE %in% c(""," ")] <- 999
table(dat$AGE, exclude = NULL)
sum(table(dat$AGE))


#####################################
# ------ GENERATE NEW VARIABLES ----
#####################################

# AGE GROUPS
dat = transform(dat, 
        AGEGRP = as.integer(cut(AGE, breaks=c(seq(0, 85, by=5),100), right=FALSE)))

table(dat$AGEGRP)

dat$AGEGRP[dat$AGE==100] <- 18

dat$AGEGRP[is.na(dat$AGEGRP)] <- 19

sum(table(dat$AGEGRP))

with(dat, tapply(AGE, AGEGRP, mean))

# YEAR OF INCIDENCE
dat$YEAR = as.numeric(substr(dat$INCID,1,4))

table(dat$YEAR)

sum(table(dat$YEAR))

# DATE OF BIRTH
dat$BIRTH_DATE = as.Date(dat$BIRTHD, format='%Y%m%d')

# Fixing last day of month in dates
dat$BIRTHD[substr(dat$BIRTHD,5,8) == "0230"] <- paste(substr(dat$BIRTHD[substr(dat$BIRTHD,5,8) == "0230"],1,4),"0228",sep="")
dat$BIRTHD[substr(dat$BIRTHD,5,8) == "0231"] <- paste(substr(dat$BIRTHD[substr(dat$BIRTHD,5,8) == "0231"],1,4),"0228",sep="")
dat$BIRTHD[substr(dat$BIRTHD,5,8) == "0229"] <- paste(substr(dat$BIRTHD[substr(dat$BIRTHD,5,8) == "0229"],1,4),"0228",sep="")
dat$BIRTHD[substr(dat$BIRTHD,5,8) == "0431"] <- paste(substr(dat$BIRTHD[substr(dat$BIRTHD,5,8) == "0431"],1,4),"0430",sep="")
dat$BIRTHD[substr(dat$BIRTHD,5,8) == "0631"] <- paste(substr(dat$BIRTHD[substr(dat$BIRTHD,5,8) == "0631"],1,4),"0630",sep="")
dat$BIRTHD[substr(dat$BIRTHD,5,8) == "0931"] <- paste(substr(dat$BIRTHD[substr(dat$BIRTHD,5,8) == "0931"],1,4),"0930",sep="")
dat$BIRTHD[substr(dat$BIRTHD,5,8) == "1131"] <- paste(substr(dat$BIRTHD[substr(dat$BIRTHD,5,8) == "1131"],1,4),"1130",sep="")

dat$BIRTH_DATE = as.Date(dat$BIRTHD, format='%Y%m%d')

# Date conversion which fails
nrow(dat[is.na(dat$BIRTH_DATE),])
nrow(dat[substr(dat$BIRTHD,1,4) == "9999",])

# REPLACE MISSING MONTH AND DAY INFORMATION
dat$BIRTH_DATE[substr(dat$BIRTHD,5,8) == "9999" & substr(dat$BIRTHD,1,4) != "9999"] = as.Date(paste(substr(dat$BIRTHD[substr(dat$BIRTHD,5,8) == "9999" & substr(dat$BIRTHD,1,4) != "9999"],1,4),"0715",sep=""), format='%Y%m%d')
nrow(dat[is.na(dat$BIRTH_DATE),])

# GENERATING CANCER GROUPS
summary(dat$I10)
dat$ICD = as.factor(substr(dat$I10,0,3))
table(dat$ICD)

dat$CANCER = 29
dat$CANCER[dat$TOP >= 500 & dat$TOP <= 509] = 14
dat$CANCER[dat$TOP >= 320 & dat$TOP <= 329] = 10
dat$CANCER[dat$TOP >= 670 & dat$TOP <= 679] = 21
dat$CANCER[dat$TOP >= 530 & dat$TOP <= 539] = 15
dat$CANCER[dat$TOP >= 190 & dat$TOP <= 209] = 62
dat$CANCER[dat$TOP >= 0 & dat$TOP <= 89] = 1
dat$CANCER[dat$TOP >= 110 & dat$TOP <= 119] = 2
dat$CANCER[(dat$TOP >= 90 & dat$TOP <= 109) | (dat$TOP >= 120 & dat$TOP <= 149)] = 3
dat$CANCER[dat$TOP >= 150 & dat$TOP <= 159] = 4
dat$CANCER[dat$TOP >= 160 & dat$TOP <= 169] = 5
dat$CANCER[dat$TOP >= 180 & dat$TOP <= 189] = 61
dat$CANCER[dat$TOP >= 220 & dat$TOP <= 229] = 7
dat$CANCER[dat$TOP >= 230 & dat$TOP <= 249] = 8
dat$CANCER[dat$TOP >= 250 & dat$TOP <= 259] = 9
dat$CANCER[dat$TOP >= 330 & dat$TOP <= 349] = 11
dat$CANCER[dat$TOP >= 430 & dat$TOP <= 439] = 12
dat$CANCER[dat$TOP >= 460 & dat$TOP <= 469] = 13
dat$CANCER[dat$TOP >= 540 & dat$TOP <= 549] = 16
dat$CANCER[dat$TOP >= 560 & dat$TOP <= 569] = 17
dat$CANCER[dat$TOP >= 610 & dat$TOP <= 619] = 18
dat$CANCER[dat$TOP >= 620 & dat$TOP <= 629] = 19
dat$CANCER[dat$TOP >= 640 & dat$TOP <= 669] = 20
dat$CANCER[dat$TOP >= 700 & dat$TOP <= 729] = 22
dat$CANCER[dat$TOP >= 730 & dat$TOP <= 739] = 23

table(dat$CANCER)

# ASSIGNING LABEL TO CANCER GROUPS
dat$fCANCER = factor(dat$CANCER, 
              levels = c(1, 2, 3, 4, 5, 61, 62, 7, 8, 9, 10, 11, 12, 13, 
                         14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29),
              labels = c("Lip oral cavity",
                          "Nasopharynx",
                          "Other pharynx",
                          "Oesophagus",
                          "Stomach",
                          "Colon",
                          "Rectum" , 
                          "Liver",
                          "Gallbladder",
                          "Pancreas",
                          "Larynx",
                          "Lung",
                          "Melanoma of skin",
                          "Kaposi sarcoma",
                          "Breast",
                          "Cervix uteri",
                          "Corpus uteri",
                          "Ovary",
                          "Prostate",
                          "Testis",
                          "Kidney",
                          "Bladder",
                          "Brain, nervous system",
                          "Thyroid",
                          "Hodgkin lymphoma",
                          "Non-Hodgkin lymphoma",
                          "Multiple myeloma",
                          "Leukaemia",
                          "All cancers excl. non-melanoma skin cancer",
                          "All other cancers")
                          )

table(dat$fCANCER)


###############################
# ------ CHECK THE DATA -------
###############################

# FUNCTION TO DISPLAY THE NUMBER OF 'NA' VALUES
NAPerVariable = function(X) {
  D = is.na(X)
  colSums(D)
}

NAPerVariable(dat)

# FUNCTION TO DISPLAY THE NUMBER OF '0' VALUES
ZerosPerVariable = function(X) {
  D = (X == 0)
  colSums(D)
}

ZerosPerVariable(dat)

# FUNCTION TO DISPLAY THE NUMBER OF 'N' VALUES
NumPerVariable = function(X, N) {
  D = (X == N)
  colSums(D)
}

NumPerVariable(dat,999)

# CHECK THE BIRTH DATE

# YEAR
sort(unique(substr(dat$BIRTHD,1,4)))
# MONTH
sort(unique(substr(dat$BIRTHD,5,6)))
# DAY
sort(unique(substr(dat$BIRTHD,7,8)))

# DISTRIBUTION OF BIRTH YEAR
barplot(table(substr(dat$BIRTHD,1,4)), las=2, cex.axis = 0.8, cex.names = 0.6)

# CHECK THE DIAGNOSIS DATE

# YEAR
sort(unique(substr(dat$INCID,1,4)))
# MONTH
sort(unique(substr(dat$INCID,5,6)))
# DAY
sort(unique(substr(dat$INCID,7,8)))

# DISTRIBUTION OF DIAGNOSIS YEAR
barplot(table(substr(dat$INCID,1,4)), las=2, cex.axis = 0.8, cex.names = 0.7)

# COMPARE AGE WITH DIAGNOSIS - BIRTH DATE
dat$YEARc = as.Date(paste(as.character(dat$YEAR),"0715",sep=""), format='%Y%m%d')
age = with(dat, (YEARc - BIRTH_DATE) / 365.25)
class(age)
attr(age,'units') = 'years'
head(age)
check = dat[!is.na(age) & abs(dat$AGE - age) > 2,]
check$warning = "Age, Date of Birth and Date of Diagnosis not consistent"

# EXPORT OF CONSISTENCY CHECK REPORT
write.table(check, paste(dat$REGISTRY_LAB[1],"_checkreport.txt",sep=""), row.names=FALSE, sep="\t")


###################################
# ------ AGGREGATE THE DATA -------
###################################

# INITIALIZATION OF THE GROUPING
dat$CASES = 1

# AGGREGATE BY SEX, AGE GROUP AND YEAR
datag = aggregate(CASES ~ SEX+AGEGRP+YEAR+CANCER_LAB+REGISTRY_LAB,data=dat,sum)

sum(datag$CASES)


###################################
# ------ MERGE WITH OTHER DATA ----
###################################

## Source: ARTIFICIAL REAL LIFE CANCER POPULATION DATA, FOR YEAR 2007 ONLY

# IMPORT THE POPULATION DATA
pop = read.table(file="Exercise0-population.txt", header=TRUE, sep="\t")

# CHECK TOTAL POP BY SEX
as.table(by(pop$pop, pop$sex, sum))

# SUBSET THE 2007 CANCER REGISTRY DATA
datag.2007 = datag[datag$YEAR == 2007,]

# MERGE THE TWO DATA SETS
datag.pop = merge(x=datag.2007, y=pop, by.x=c("SEX", "AGEGRP"), by.y=c("sex", "age"))


##############################################
# ------ EXPORT THE RESULTING DATA SET -------
##############################################

# IN R ENVIRONMENT
save(datag, file="myfile.Rdata") # then use load('myfile.Rdata') to retrieve it later on

# AS A TAB DELIMITED TEXT FILE
write.table(datag, "myfile.txt") # then use dat = read.table('myfile.txt') to retrieve it later on


