#' Synthetic Medical Expenditure Panel Survey (MEPS) Disparities Dataset
#'
#' @description A simulated, nationally representative cross-sectional dataset of
#'   healthcare utilization, expenditures, and clinical need designed to mimic the
#'   structure of the Medical Expenditure Panel Survey (MEPS) used in McGuire et al.
#'   (2006) and Cook et al. (2010).
#'
#' @format A data frame with 3,000 rows and 15 variables:
#' \describe{
#'   \item{id}{Integer unique patient identifier.}
#'   \item{race}{Factor with 3 levels: \code{"White"}, \code{"Black"}, and \code{"Hispanic"}.}
#'   \item{age}{Integer age in years (18-85).}
#'   \item{female}{Binary indicator of female sex (1 = female, 0 = male).}
#'   \item{chronic_conditions}{Count of diagnosed chronic physical health conditions (0-6).}
#'   \item{sf12_physical}{Short-Form 12 (SF-12) physical component summary score (0-100; higher = better health).}
#'   \item{sf12_mental}{Short-Form 12 (SF-12) mental component summary score (0-100; higher = better mental health).}
#'   \item{activity_limitations}{Factor indicating limitations in daily activities: \code{"None"}, \code{"Some"}, \code{"Severe"}.}
#'   \item{income_poverty_ratio}{Ratio of family income to federal poverty level.}
#'   \item{education}{Factor with 4 levels: \code{"Less than High School"}, \code{"High School Graduate"}, \code{"Some College"}, \code{"College Graduate"}.}
#'   \item{insurance}{Factor with 4 levels: \code{"Private"}, \code{"Medicaid"}, \code{"Medicare"}, \code{"Uninsured"}.}
#'   \item{any_mental_health_use}{Binary indicator (1 = any outpatient mental health visit/drug use, 0 = none).}
#'   \item{mental_health_expenditure}{Total annual outpatient mental health expenditures in US dollars (semi-continuous with mass at 0).}
#'   \item{outpatient_visits}{Annual count of outpatient physician/specialist visits.}
#'   \item{survey_weight}{Sampling weight for population-representative estimation.}
#' }
#'
#' @source Simulated based on parameters from Cook, B. L., McGuire, T. G., &
#'   Zaslavsky, A. M. (2012). \emph{Health Services Research}, 47(3 Pt 2), 1232-1254.
#'
#' @examples
#' data(meps_synthetic)
#' summary(meps_synthetic)
"meps_synthetic"
