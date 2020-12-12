#' @title Swiss Fertility and Socioeconomic Indicators (1888) Data
#'
#'@description Standardized fertility measure and socio-economic indicators for each of 47 French-speaking provinces
#'of Switzerland at about 1888.
#'@details Switzerland, in 1888, was entering a period known as the demographic transition; i.e.,
#'its fertility was beginning to fall from the high level typical of underdeveloped countries.
#'The data collected are for 47 French-speaking “provinces” at about 1888.
#'Here, all variables are scaled to [0, 100], where in the original, all but "Catholic" were scaled to [0, 1].
#'@docType data
#'@keywords datasets
#'@name swiss
#'@usage swiss
#'
#' @format A data frame with 47 observations on 6 variables, each of which is in percent, i.e., in [0, 100].
#' \describe{
#'   \item{\code{Fertility}}{Ig, ‘common standardized fertility measure’.}
#'   \item{\code{Agriculture}}{% of males involved in agriculture as occupation.}
#'   \item{\code{Examination}}{% draftees receiving highest mark on army examination.}
#'   \item{\code{Education}}{% education beyond primary school for draftees.}
#'   \item{\code{Catholic}}{% ‘catholic’ (as opposed to ‘protestant’).}
#'   \item{\code{Infant.Mortality}}{live births who live less than 1 year.}
#'
#'  @source The data is from the \code{swiss} dataset in R.
#' @example \dontrun{slreg(swiss)}
"swiss"
