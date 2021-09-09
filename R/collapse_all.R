#' Assists in collapsing various designations of majors into more efficient major designations
#'@param a A file with AcadPlan with various designations of various majors
#'@export



collapse_all<- function(a){

a1 <- data.frame(a)


a1$AcadPlan1 <- ifelse(grepl("Health, BS|Health, DEG UN", a1$AcadPlan), "Health, BS",

                       ifelse(grepl("Health, BS PB|Health, DEG UN PB|Health, NDO UN PB", a1$AcadPlan), "Health, BS PB",


                              ifelse(grepl("Teaching & Learning, DEG UN PB|Teaching & Learning, NDO UN PB|Teaching and Learning, BS PB", a1$AcadPlan), "Teaching and Learning, BS PB",

                                     ifelse(grepl("Teaching and Learning, BS|Teaching and Learning, DEG UN", a1$AcadPlan), "Teaching and Learning, BS",


                                            ifelse(grepl("Hum Dev & Fam Stds, BA|Hum Dev & Fam Stds, BS|Hum Dev Family Studies, DEG UN", a1$AcadPlan), "Hum Dev & Fam Stds, BA/BS",

                                                   ifelse(grepl("Hum Dev & Fam Stds, BA PB|Hum Dev & Fam Stds, BS PB|Hum Dev & Fam Stds, NDO UN PB", a1$AcadPlan), "Hum Dev & Fam Stds, BA/BS PB", a1$AcadPlan))))))

return(a1)

}
