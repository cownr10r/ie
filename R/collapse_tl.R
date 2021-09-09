#' Assists with collapsing all kinds of designations of teaching and learning major
#' @param a A file containing AcadPlan with various designations of teaching and learning majors
#' @export




collapse_tl <- function(a){

  a1 <- data.frame(a)

  a1$AcadPlan1 <- ifelse(grepl("Teaching & Learning, DEG UN PB|Teaching & Learning, NDO UN PB|Teaching and Learning, BS PB", a1$AcadPlan), "Teaching and Learning, BS PB",

                         ifelse(grepl("Teaching and Learning, BS|Teaching and Learning, DEG UN", a1$AcadPlan), "Teaching and Learning, BS", a1$AcadPlan))
  return(a1)

}
