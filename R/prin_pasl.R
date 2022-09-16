#' a function to merge 268 and 368 scores to determine readiness for principal certification
#' @param a Test scores from Pearson, untouched
#' @param b Test from ETS, the PASL scores 368 for principal certification
#' @export






prinpasl <- function(a,b){
  a1 <- dplyr::filter(a, `Exam Code` == '268')
  data <- dplyr::left_join(a1,b, by ='TEAID')
  data1 <- dplyr::select(data, 'Examinee Name','TEAID', 'Exam Name', 'Exam Code', 'Exam Version','Exam Date', 'Pass / Fail Status', 'PASL-PASS')
  data2 <- dplyr::distinct(data1)
  return(data2)
}
