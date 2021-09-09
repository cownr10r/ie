#' Assists in updating PASL scores from day to day.
#' @param a The new daily download from ETS
#' @param b old file with accurate SSN numbers
#' @export




update_pasl <- function(a,b){
  b1 <- b[,c("Test Taker Id", "SSN")]
  dat <- merge(a,b1, by = "Test Taker Id", all.x = T)
  dat1 <- dat[,c(2,3,1,4,65,6,7:64)]
  names(dat1)[5] <- "SSN"
  return(dat1)
}
