#' Assists in collapsing categories for health majors
#' @param a A file with AcadPlan including various health major designations
#' @export



collapse_health <- function(a){

  a1 <- data.frame(a)
  a1$AcadPlan1 <- ifelse(grepl("Health, BS|Health, DEG UN", a1$AcadPlan), "Health, BS",

                         ifelse(grepl("Health, BS PB|Health, DEG UN PB|Health, NDO UN PB", a1$AcadPlan), "Health, BS PB", a1$AcadPlan))

  return(a1)

}
