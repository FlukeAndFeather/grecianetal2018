#' GPS tracks of adult and immature Northern Gannets
#'
#' A dataset containing the at-sea trips of 31 adult and 15 immature Northern
#' Gannets from the Bass Rock colony in Scotland
#'
#' @format A data frame with 283166 rows and 8 variables: \describe{
#'   \item{id}{id of individual animal} \item{trip}{id of an at-sea trip}
#'   \item{datetime_utc}{time of relocation in UTC, POSIXct}
#'   \item{lon}{longitude} \item{lat}{latitude} \item{UTM_x}{Eastings in UTM
#'   30N} \item{UTM_y}{Northings in UTM 30N} \item{age}{age class of individual
#'   animal, "adult" or "immature} }
#' @source \doi{10.1098/rsif.2018.0084}
"noga_tracks"
