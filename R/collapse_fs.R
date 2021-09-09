#' Assists in collapsing various designation of family studies majors
#' @param a A file containing AcadPlan with various designations of family studies majors
#' @export



collapse_fs <-function(a){

  a1 <- data.frame(a)

  a1$AcadPlan1 <- ifelse(grepl("Hum Dev & Fam Stds, BA|Hum Dev & Fam Stds, BS|Hum Dev Family Studies, DEG UN", a1$AcadPlan), "Hum Dev & Fam Stds, BA/BS",

                         ifelse(grepl("Hum Dev & Fam Stds, BA PB|Hum Dev & Fam Stds, BS PB|Hum Dev & Fam Stds, NDO UN PB", a1$AcadPlan), "Hum Dev & Fam Stds, BA/BS PB", a1$AcadPlan))

  return(a1)

}
