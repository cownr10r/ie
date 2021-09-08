#' recommend_initial is a package to help make institutional effectiveness easier by tidying data to one row to one case and other actions
#' @param a A file generated from Pearson downloads for students seeking initial certification
#' @param b The academic year in question, surrounded in quotes
#' @import "stats"
#' @return A data frame with decisions to recommend or not.
#' @export






recommend_initial <- function(a,b) {

  a1 <- a[,c("Examinee Name", "TEAID", "Exam", "Exam Date", "Pass / Fail Status")]

  names(a1)[5] <- "Pass"



  a1$Exam1 <- gsub("[[:space:]][[:alpha:]].*","", a1$Exam)

  a1$test_result <- paste(a1$Exam1, a1$Pass, sep = " ")

  a1$tests <- paste(a1$Exam, a1$Pass, sep = " ")

  a1 <- a1[order(a1$TEAID, a1$Exam),]

  a4 <- aggregate(test_result ~TEAID, a1, c)

  a3 <- aggregate(tests ~ TEAID, a1, c)

  a2 <- aggregate(Exam1 ~ TEAID, a1, c)

  a5 <- aggregate(Exam ~ TEAID, a1, c)



  data <- merge(a4,a2, by = "TEAID", all.x = T)

  dataa <- merge(data, a3, by ="TEAID", all.x = T)

  data1 <- merge(dataa, a5, by =  "TEAID", all.x = T)





  data1$CertType <- ifelse(grepl("801|802|803|804|805|901|902|903|904|905", data1$Exam),"CORE EC-6",

                           ifelse(grepl("115", data1$Exam), "MATH 4-8",

                                  ifelse(grepl("116", data1$Exam), "SCIENCE 4-8",

                                         ifelse(grepl("117", data1$Exam), "ELAR 4-8",

                                                ifelse(grepl("231", data1$Exam), "ELAR 7-12",

                                                       ifelse(grepl("118", data1$Exam), "SOC STUDIES 4-8",

                                                              ifelse(grepl("177", data1$Exam), "MUSIC EC-12",

                                                                     ifelse(grepl("233", data1$Exam), "HISTORY 7-12",

                                                                            ifelse(grepl("235", data1$Exam), "MATH 7-12",

                                                                                   ifelse(grepl("613", data1$Exam), "LOTE SPANISH",

                                                                                          ifelse(grepl("713", data1$Exam), "LOTE PACT",

                                                                                                 ifelse(grepl("005", data1$Exam), "ART EC-12",

                                                                                                        ifelse(grepl("279", data1$Exam), "DANCE 6-12",

                                                                                                               ifelse(grepl("243",data1$Exam), "PHYSICS/MATH 7-12",

                                                                                                                      ifelse(grepl("236", a1$Exam), "SCIENCE 7-12",

                                                                                                                             ifelse(grepl("232",data1$Exam), "SOC STUDIES 7-12",

                                                                                                                                    ifelse(grepl("180",data1$Exam), "THEATRE EC-12",

                                                                                                                                           ifelse(grepl("164|190", data1$Exam), "BILINGUAL ED",

                                                                                                                                                  ifelse(grepl("161", data1$Exam), "SPECIAL EDUCATION EC-12",

                                                                                                                                                         ifelse(grepl("163", data1$Exam), "SPECIAL EDUCATION SUPP","NA"))))))))))))))))))))





  #SPECIAL EDUCATION EC-12



  data1$All_Tests <- ifelse(grepl("801", data1$Exam) & grepl("802", data1$Exam) & grepl("803", data1$Exam) & grepl("804", data1$Exam) & grepl("805", data1$Exam) &
                              grepl("293", data1$Exam) & grepl("161",data1$Exam) & grepl("160", data1$Exam), "Complete", "NotComplete")







  data1$All_Tests <- ifelse(grepl("901", data1$Exam) & grepl("902", data1$Exam)& grepl("903", data1$Exam) & grepl("904", data1$Exam)& grepl("905", data1$Exam)&
                              grepl("293", data1$Exam) & grepl("161",data1$Exam) & grepl("160", data1$Exam), "Complete", data1$All_Tests)



  #SPECIAL EDUCATION SUPPLEMENTAL



  data1$All_Tests <- ifelse(grepl("901", data1$Exam) & grepl("902", data1$Exam)& grepl("903", data1$Exam) & grepl("904", data1$Exam)& grepl("905", data1$Exam)&
                              grepl("293", data1$Exam) & grepl("163",data1$Exam) & grepl("160", data1$Exam), "Complete", data1$All_Tests)





  data1$All_Tests <- ifelse(grepl("801", data1$Exam) & grepl("802", data1$Exam) & grepl("803", data1$Exam)& grepl("804", data1$Exam) & grepl("805", data1$Exam) &
                              grepl("293", data1$Exam) & grepl("163",data1$Exam) & grepl("160", data1$Exam), "Complete", data1$All_Tests)







  #CORE



  data1$All_Tests <- ifelse(grepl("801", data1$Exam) & grepl("802", data1$Exam) & grepl("803", data1$Exam)& grepl("804", data1$Exam) & grepl("805", data1$Exam)&
                              grepl("293", data1$Exam) & grepl("160", data1$Exam), "Complete", data1$All_Tests)





  data1$All_Tests <- ifelse(grepl("901", data1$Exam)  & grepl("902", data1$Exam)& grepl("903", data1$Exam) & grepl("904", data1$Exam)& grepl("905", data1$Exam)&
                              grepl("293", data1$Exam) & grepl("160", data1$Exam), "Complete", data1$All_Tests)







  #BILINGUAL



  data1$All_Tests <- ifelse(grepl("801", data1$Exam) & grepl("802", data1$Exam) & grepl("803", data1$Exam) & grepl("804", data1$Exam) & grepl("805", data1$Exam) &
                              grepl("293", data1$Exam) & grepl("164", data1$Exam) & grepl("190", data1$Exam) & grepl("160", data1$Exam), "Complete", data1$All_Tests)







  data1$All_Tests <- ifelse(grepl("901", data1$Exam) & grepl("902", data1$Exam) & grepl("903", data1$Exam) & grepl("904", data1$Exam)& grepl("905", data1$Exam)&
                              grepl("293", data1$Exam) & grepl("164", data1$Exam) & grepl("190", data1$Exam) & grepl("160", data1$Exam), "Complete", data1$All_Tests)


  data1$All_Tests <- ifelse(grepl("115", data1$Exam) & grepl("160", data1$Exam), "Complete", data1$All_Tests)

  data1$All_Tests <- ifelse(grepl("116", data1$Exam) & grepl("160", data1$Exam), "Complete", data1$All_Tests)

  data1$All_Tests <- ifelse(grepl("117", data1$Exam) & grepl("160", data1$Exam), "Complete", data1$All_Tests)

  data1$All_Tests <- ifelse(grepl("231", data1$Exam) & grepl("160", data1$Exam), "Complete", data1$All_Tests)

  data1$All_Tests <- ifelse(grepl("118", data1$Exam) & grepl("160", data1$Exam), "Complete", data1$All_Tests)

  data1$All_Tests <- ifelse(grepl("177", data1$Exam) & grepl("160", data1$Exam), "Complete", data1$All_Tests)

  data1$All_Tests <- ifelse(grepl("233", data1$Exam) & grepl("160", data1$Exam), "Complete", data1$All_Tests)

  data1$All_Tests <- ifelse(grepl("235", data1$Exam) & grepl("160", data1$Exam), "Complete", data1$All_Tests)

  data1$All_Tests <- ifelse(grepl("613", data1$Exam) & grepl("160", data1$Exam), "Complete", data1$All_Tests)

  data1$All_Tests <- ifelse(grepl("713", data1$Exam) & grepl("160", data1$Exam), "Complete", data1$All_Tests)

  data1$All_Tests <- ifelse(grepl("005", data1$Exam) & grepl("160", data1$Exam), "Complete", data1$All_Tests)

  data1$All_Tests <- ifelse(grepl("279", data1$Exam) & grepl("160", data1$Exam), "Complete", data1$All_Tests)

  data1$All_Tests <- ifelse(grepl("243", data1$Exam) & grepl("160", data1$Exam), "Complete", data1$All_Tests)

  data1$All_Tests <- ifelse(grepl("236", data1$Exam) & grepl("160", data1$Exam), "Complete", data1$All_Tests)

  data1$All_Tests <- ifelse(grepl("232", data1$Exam) & grepl("160", data1$Exam), "Complete", data1$All_Tests)

  data1$All_Tests <- ifelse(grepl("180", data1$Exam) & grepl("160", data1$Exam), "Complete", data1$All_Tests)





  # DETERMINE IF TEST RESULT IS ALL PASSES AND RESULT IN RECOMMEND OR NOT RECOMMEND



  data1$PA <- ifelse(grepl("901[[:space:]]P", data1$test_result) & grepl("902[[:space:]]P", data1$test_result) & grepl("903[[:space:]]P", data1$test_result) & grepl("904[[:space:]]P", data1$test_result) & grepl("905[[:space:]]P", data1$test_result)

                     & grepl("293[[:space:]]P", data1$test_result)& grepl("160[[:space:]]P", data1$test_result) & grepl("CORE EC-6", data1$CertType), "Recommend", "Not Recommend")



  data1$PA <- ifelse(grepl("901[[:space:]]P", data1$test_result) & grepl("902[[:space:]]P", data1$test_result) & grepl("903[[:space:]]P", data1$test_result) & grepl("904[[:space:]]P", data1$test_result) & grepl("905[[:space:]]P", data1$test_result)

                     & grepl("293[[:space:]]P", data1$test_result)& grepl("160[[:space:]]P", data1$test_result) & grepl("164[[:space:]]P", data1$test_result) & grepl("190[[:space:]]P", data1$test_result) & grepl("BILINGUAL ED", data1$CertType), "Recommend", data1$PA)



  data1$PA <- ifelse(grepl("901[[:space:]]P", data1$test_result) & grepl("902[[:space:]]P", data1$test_result) & grepl("903[[:space:]]P", data1$test_result) & grepl("904[[:space:]]P", data1$test_result) & grepl("905[[:space:]]P", data1$test_result)

                     & grepl("293[[:space:]]P", data1$test_result)& grepl("160[[:space:]]P", data1$test_result) & grepl("163", data1$test_result) & grepl("SPECIAL EDUCATION EC-12", data1$CertType), "Recommend",data1$PA)





  data1$PA <- ifelse(grepl("901[[:space:]]P", data1$test_result) & grepl("902[[:space:]]P", data1$test_result) & grepl("903[[:space:]]P", data1$test_result) & grepl("904[[:space:]]P", data1$test_result) & grepl("905[[:space:]]P", data1$test_result)

                     & grepl("293[[:space:]]P", data1$test_result)& grepl("160[[:space:]]P", data1$test_result) & grepl("161", data1$test_result) & grepl("SPECIAL EDUCATION SUPP", data1$CertType), "Recommend", data1$PA)







  data1$PA <-ifelse(grepl("115[[:space:]]P", data1$test_result) & grepl("160[[:space:]]P", data1$test_result) & grepl("MATH 4-8", data1$CertType), "Recommend", data1$PA)



  data1$PA <- ifelse(grepl("116[[:space:]]P", data1$test_result) & grepl("160[[:space:]]P", data1$test_result) & grepl("SCIENCE 4-8", data1$CertType), "Recommend", data1$PA)

  data1$PA <- ifelse(grepl("117[[:space:]]P", data1$test_result) & grepl("293[[:space:]]P",data1$test_result) &  grepl("160[[:space:]]P", data1$test_result) & grepl("ELAR 4-8", data1$CertType), "Recommend", data1$PA)

  data1$PA <- ifelse(grepl("231[[:space:]]P", data1$test_result) & grepl("160[[:space:]]P", data1$test_result) & grepl("ELAR 7-12", data1$CertType), "Recommend", data1$PA)



  data1$PA <- ifelse(grepl("118[[:space:]]P", data1$test_result) & grepl("160[[:space:]]P", data1$test_result) & grepl("SOC STUDIES 4-8", data1$CertType), "Recommend", data1$PA)

  data1$PA <- ifelse(grepl("177[[:space:]]P", data1$test_result) & grepl("160[[:space:]]P", data1$test_result) & grepl("MUSIC EC-12", data1$CertType), "Recommend", data1$PA)

  data1$PA <- ifelse(grepl("233[[:space:]]P", data1$test_result) & grepl("160[[:space:]]P", data1$test_result) & grepl("HISTORY 7-12", data1$CertType), "Recommend", data1$PA)

  data1$PA <- ifelse(grepl("235[[:space:]]P", data1$test_result) & grepl("160[[:space:]]P", data1$test_result) & grepl("MATH 7-12", data1$CertType), "Recommend", data1$PA)

  data1$PA <- ifelse(grepl("613[[:space:]]P", data1$test_result) & grepl("160[[:space:]]P", data1$test_result) & grepl("LOTE SPANISH", data1$CertType), "Recommend", data1$PA)

  data1$PA <- ifelse(grepl("713[[:space:]]P", data1$test_result) & grepl("160[[:space:]]P", data1$test_result) & grepl("LOTE PACT", data1$CertType), "Recommend", data1$PA)

  data1$PA <- ifelse(grepl("005[[:space:]]P", data1$test_result) & grepl("160[[:space:]]P", data1$test_result) & grepl("ART EC-12", data1$CertType), "Recommend", data1$PA)

  data1$PA <- ifelse(grepl("279[[:space:]]P", data1$test_result) & grepl("160[[:space:]]P", data1$test_result) & grepl("DANCE 6-12", data1$CertType), "Recommend", data1$PA)

  data1$PA <- ifelse(grepl("243[[:space:]]P", data1$test_result) & grepl("160[[:space:]]P", data1$test_result) & grepl("PHYSICS/MATH 7-12", data1$CertType), "Recommend", data1$PA)

  data1$PA <- ifelse(grepl("236[[:space:]]P", data1$test_result) & grepl("160[[:space:]]P", data1$test_result) & grepl("SCIENCE 7-12", data1$CertType), "Recommend", data1$PA)

  data1$PA <- ifelse(grepl("232[[:space:]]P", data1$test_result) & grepl("160[[:space:]]P", data1$test_result) & grepl("SOC STUDIES 7-12", data1$CertType), "Recommend", data1$PA)

  data1$PA <- ifelse(grepl("180[[:space:]]P", data1$test_result) & grepl("160[[:space:]]P", data1$test_result) & grepl("THEATRE EC-12", data1$CertType), "Recommend", data1$PA)





  # merge exam and pf status

  data1 <- data1[,c(1,2, 4,6,7,8)]

  data1$Year <- rep(b, length(data1$TEAID))


  return(data1)

}
