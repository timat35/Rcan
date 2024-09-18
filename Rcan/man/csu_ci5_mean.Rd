\name{csu_ci5_mean}
\alias{csu_ci5_mean}
\docType{data}
\title{
cancer registry data 
}
\description{
CI5 XII data, all population grouped.   
}
\usage{data("csu_ci5_mean")}
\format{
  A data frame with 1026 observations on the following 5 variables.
  \describe{
  	\item{\code{ci5_cancer_code}}{cancer code}
  	\item{\code{ci5_cancer_label}}{cancer label}
    \item{\code{CSU_age_factor}}{age variable from 1 to 18. 1 is 0-4 years, 2 is 5-9, etc..., 17 is 80-84, 18 is 85+.} 
    \item{\code{CSU_C}}{Number of cases (incidence)}
    \item{\code{CSU_P}}{Population-year: Reference population}
    
  }
}
\details{
This dataset have been created in order to compare age specific rate with the CI5XII mean directly on the age specific rate graph.
Here is the table of cancer_label and cancer_code available:
\tabular{ll}{
	ci5_cancer_code \tab ci5_cancer_label \cr
	1 \tab Lip \cr
	2 \tab Tongue \cr
	3 \tab Mouth \cr
	4 \tab Salivary glands \cr
	5 \tab Tonsil \cr
	6 \tab Other oropharynx \cr
	7 \tab Nasopharynx \cr
	8 \tab Hypopharynx \cr
	9 \tab Pharynx unspecified \cr
	10 \tab Oesophagus \cr
	11 \tab Stomach \cr
	12 \tab Small intestine \cr
	13 \tab Colon \cr
	14 \tab Rectum \cr
	15 \tab Anus \cr
	16 \tab Liver \cr
	17 \tab Gallbladder \cr
	18 \tab Pancreas \cr
	19 \tab Nose, sinuses \cr
	20 \tab Larynx \cr
	21 \tab Lung \cr
	23 \tab Bone \cr
	24 \tab Melanoma of skin \cr
	25 \tab Other skin \cr
	26 \tab Mesothelioma \cr
	27 \tab Kaposi sarcoma \cr
	28 \tab Connective and soft tissue \cr
	29 \tab Breast \cr
	30 \tab Vulva \cr
	31 \tab Vagina \cr
	32 \tab Cervix uteri \cr
	33 \tab Corpus uteri \cr
	35 \tab Ovary \cr
	37 \tab Placenta \cr
	38 \tab Penis \cr
	39 \tab Prostate \cr
	40 \tab Testis \cr
	42 \tab Kidney \cr
	43 \tab Renal pelvis \cr
	44 \tab Ureter \cr
	45 \tab Bladder \cr
	47 \tab Eye \cr
	48 \tab Brain, nervous system \cr
	49 \tab Thyroid \cr
	50 \tab Adrenal gland \cr
	51 \tab Other endocrine \cr
	52 \tab Hodgkin disease \cr
	53 \tab Non-Hodgkin lymphoma \cr
	54 \tab Immunoproliferative diseases \cr
	55 \tab Multiple myeloma \cr
	56 \tab Lymphoid leukaemia \cr
	57 \tab Myeloid leukaemia \cr
	58 \tab Leukaemia unspecified \cr
	59 \tab Myeloproliferative disorders \cr
	60 \tab Myelodysplastic syndromes \cr
	62 \tab All sites \cr
	63 \tab All sites but skin \cr
}

See: \code{\link{csu_ageSpecific}}
}
\source{
\url{https://ci5.iarc.fr/ci5-xii/}
}
\seealso{
	\code{\link{csu_ageSpecific}}
}

\examples{

data(csu_registry_data_1)
test <- subset(csu_registry_data_1 , registry_label == "Colombia, Cali")
csu_ageSpecific(test,
                plot_title = "Colombia, Liver, male",
                CI5_comparison = "Liver")
 
#See more examples here:
help(csu_ageSpecific)

}
\keyword{datasets}
