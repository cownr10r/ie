



#' Converts a term into a semester and year
#' @param a a file with the "Term" variable duly noted and named in the dataframe
#' @export


term_to_year <- function(a){


  a$Term <- as.numeric(a$Term)

  a$Term_t_Sem_Year <- ifelse(a$Term == 1860, "Spring 2012",
                              ifelse(a$Term == 1870, "Summer 2012",
                                     ifelse(a$Term == 1880, "Fall 2012",
                                            ifelse(a$Term == 1890, "Spring 2013",
                                                   ifelse(a$Term == 1900, "Summer 2013",
                                                          ifelse(a$Term == 1910, "Fall 2013",
                                                                 ifelse(a$Term == 1920, "Spring 2014",
                                                                        ifelse(a$Term == 1930, "Summer 2014",
                                                                               ifelse(a$Term == 1940, "Fall 2014",
                                                                                      ifelse(a$Term == 1950, "Spring 2015",
                                                                                             ifelse(a$Term == 1960, "Summer 2015",
                                                                                                    ifelse(a$Term == 1970, "Fall 2015",
                                                                                                           ifelse(a$Term == 1980, "Spring 2016",
                                                                                                                  ifelse(a$Term == 1990, "Summer 2016",
                                                                                                                         ifelse(a$Term == 2000, "Fall 2016",
                                                                                                                                ifelse(a$Term == 2010, "Spring 2017",
                                                                                                                                       ifelse(a$Term == 2020, "Summer 2017",
                                                                                                                                              ifelse(a$Term == 2030, "Fall 2017",
                                                                                                                                                     ifelse(a$Term == 2040, "Spring 2018",
                                                                                                                                                            ifelse(a$Term == 2050, "Summer 2018",
                                                                                                                                                                   ifelse(a$Term == 2060, "Fall 2018",
                                                                                                                                                                          ifelse(a$Term == 2070, "Spring 2019",
                                                                                                                                                                                 ifelse(a$Term == 2080, "Summer 2019",
                                                                                                                                                                                        ifelse(a$Term == 2090, "Fall 2019",
                                                                                                                                                                                               ifelse(a$Term == 2100, "Spring 2020",
                                                                                                                                                                                                      ifelse(a$Term == 2120, "Fall 2020",
                                                                                                                                                                                                             ifelse(a$Term == 2130, "Spring 2021",
                                                                                                                                                                                                                    ifelse(a$Term == 2140, "Summer 2021",
                                                                                                                                                                                                                           ifelse(a$Term == 2150, "Fall 2021",
                                                                                                                                                                                                                                  ifelse(a$Term == 2160, "Spring 2022",
                                                                                                                                                                                                                                         ifelse(a$Term == 2170, "Summer 2022", "NA")))))))))))))))))))))))))))))))
  return(a)
}
