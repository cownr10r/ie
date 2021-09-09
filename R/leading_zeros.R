#' Assists with creating leading zeros for either SSNs, IDs or TEAIDs
#' @param e A file with either SSNs, IDs, or TEAIDs
#' @param f A choice that must be made of filling out either SSN, ID, or TEAID
#' @export




leading_zeros<- function(e,f){

  e <- data.frame(e)

  if(f =="SSN") {e$SSN <- formatC(e$SSN, width = 9, format = "d", flag = "0")}
  if(f == "ID"){e$ID <- formatC(e$ID, width = 7, format = "d", flag = "0")}
  if(f == "TEAID"){e$TEAID <- formatC(e$TEAID, width = 7, format = "d", flag = "0")}

  return(e)
}
