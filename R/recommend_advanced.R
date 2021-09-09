#' a function to help to determine if advanced students are ready for changes in status towards certification
#' @param a Test scores from Pearson, untouched
#' @param b Test from ETS, the PASL scores 368 for principal certification
#' @param d SSN numbers for students
#' @param e SSN and ETS ID numbers for students
#' @param f The academic year in question, in quotes; viz. "19-20"
#' @export



recommend_advanced <- function(a,b,d,e,f) {




  a1 <- a[,c("Examinee Name", "TEAID", "Exam", "Exam Date", "Pass / Fail Status")]



  names(a1)[5] <- "Pass"



  names(a1)[1] <- "Name"



  a1 <- a1[grepl("151|153|253|152|068|268|064|195", a1$Exam),]


  a1$Exam1 <- gsub("[[:space:]][[:alpha:]].*","", a1$Exam)



  a1$test_result <- paste(a1$Exam1, a1$Pass, sep = " ")



  a1$tests <- paste(a1$Exam, a1$Pass, sep = " ")



  a1 <- a1[order(a1$TEAID, a1$Exam),]


  names(b)[3] <- "TestTaker_ID"




  b1 <- b[,c("SSN","Test Name", "Pass / Not Pass")]



  names(b1)[3]<- "PASLpass"



  names(b1)[2]<- "TestName"



  a4 <- aggregate(test_result ~TEAID, a1, c)



  a3 <- aggregate(tests ~ TEAID, a1, c)



  a2 <- aggregate(Exam1 ~ TEAID, a1, c)



  a5 <- aggregate(Exam ~ TEAID, a1, c)



  a6 <- aggregate(Name  ~ TEAID, a1, c)


  data <- merge(a4,a2, by = "TEAID", all.x = T)



  dataa <- merge(data, a3, by ="TEAID", all.x = T)



  dataaa <- merge(dataa, a5, by =  "TEAID", all.x = T)



  data1 <- merge(dataaa, a6, by = "TEAID", all.x = T)


  data1$CertType <- ifelse(grepl("253|153", data1$Exam),"ED DIAG",



                           ifelse(grepl("151", data1$Exam), "READING SPECIALIST",



                                  ifelse(grepl("152", data1$Exam), "SCHOOL COUNSELOR",



                                         ifelse(grepl("268", data1$Exam), "PRINCIPAL",



                                                ifelse(grepl("064|195", data1$Exam), "SUPERINTENDENT", "NA")))))




  data1$All_Tests <- ifelse(grepl("151", data1$Exam), "Complete", "NotComplete")



  data1$All_Tests <- ifelse(grepl("153|253", data1$Exam), "Complete", data1$All_Tests)



  data1$All_Tests <- ifelse(grepl("152", data1$Exam), "Complete", data1$All_Tests)



  data1$All_Tests <- ifelse(grepl("268", data1$Exam), "Complete", data1$All_Tests)



  data1$All_Tests <- ifelse(grepl("064|195", data1$Exam),  "Complete", data1$All_Tests)



  # DETERMINE IF TEST RESULT IS ALL PASSES AND RESULT IN RECOMMEND OR NOT RECOMMEND




  data1$PA<- ifelse(grepl("151[[:space:]]P", data1$test_result) & grepl("READING SPECIALIST", data1$CertType), "Recommend", "NOT recommend")



  data1$PA <- ifelse(grepl("153[[:space:]]P|253[[:space:]]P", data1$test_result) & grepl("ED DIAG", data1$CertType), "Recommend", data1$PA)



  data1$PA <- ifelse(grepl("152[[:space:]]P", data1$test_result) & grepl("SCHOOL COUNSELOR", data1$CertType), "Recommend", data1$PA)



  data1$PA <- ifelse(grepl("268[[:space:]]P", data1$test_result) & grepl("PRINCIPAL", data1$CertType), "Recommend", data1$PA)



  data1$PA <- ifelse(grepl("064[[:space:]]P|195[[:space:]]P", data1$test_result) & grepl("SUPERINTENDENT", data1$CertType), "Recommend", data1$PA)



  d$SSN <- gsub("\\-", "", d$SSN)



  b1$SSN <- gsub("\\-", "", b1$SSN)



  b1$PASLtest <- paste(b1$TestName,b1$PASLpass, sep=" ")




  b1$PASLtest <- sub(".*\\(PASL\\)","PASL", b1$PASLtest)



  b1$PASLrec<- ifelse(grepl("PASL[[:space:]]Passed", b1$PASLtest), "RecommendPASL", "NA")



  data1a <- merge(data1, d, by = "TEAID", all.x = T)

  data2ab<- merge(data1a, e, by = "SSN", all.x = T)





  data2 <- merge(data2ab, b1, by = "SSN", all.x = T)



  data2$PRINCIPALrec <- ifelse(grepl("Recommend", data2$PA) & grepl("RecommendPASL", data2$PASLrec), "RECOMMEND", "NOT RECOMMEND")

  data2 <- data2[,c(1,2,11,3,5,8,9,10,13,16)]



  names(data2)[3] <- "ETS_ID"


  names(data2)[4] <- "CONTENTresult"



  names(data2)[5] <- "CONTENTtests"



  names(data2)[8] <- "RECOMMEND"

  data2$Year <- rep(f, length(data2$SSN))


  return(data2)

}
