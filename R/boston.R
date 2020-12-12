#' @title Housing Values in Suburbs of Boston
#'
#'@description The Boston data frame has 506 rows and 14 columns.
#'@docType data
#'@keywords datasets
#'@name Boston
#'@usage Boston
#'
#' @format A data frame that contains the following columns:
#' \describe{
#'   \item{\code{crim}}{per capita crime rate by town.}
#'   \item{\code{zn}}{proportion of residential land zoned for lots over 25,000 sq.ft.}
#'   \item{\code{indus}}{proportion of non-retail business acres per town.}
#'   \item{\code{chas}}{Charles River dummy variable (= 1 if tract bounds river; 0 otherwise).}
#'   \item{\code{nox}}{nitrogen oxides concentration (parts per 10 million).}
#'   \item{\code{rm}}{average number of rooms per dwelling.}
#'   \item{\code{age}}{proportion of owner-occupied units built prior to 1940.}
#'   \item{\code{dis}}{weighted mean of distances to five Boston employment centres.}
#'   \item{\code{rad}}{index of accessibility to radial highways.}
#'   \item{\code{tax}}{full-value property-tax rate per \$10,000.}
#'   \item{\code{ptratio}}{pupil-teacher ratio by town.}
#'   \item{\code{black}}{1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town.}
#'   \item{\code{lstat}}{pupil-teacher ratio by town.}
#'   \item{\code{medv}}{median value of owner-occupied homes in \$1000s.}
#' }
#'  @source The data is from the \code{Boston} dataset in the
#' \code{link{MASS}{Boston}} dataset.
#' @example \dontrun{stepreg(Boston)}
"Boston"



