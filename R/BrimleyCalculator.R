#' Calculate Brimley/Cacoon line statistics
#'
#' @param birtdate Date of person
#'
#' @return date person will cross Brimley/Cacoon line
#' @export
#' @import glue scales
#' @examples
#' results <- BrimleyCalculator(as.Date("1973-12-18"))
BrimleyCalculator <- function(birthdate){

  #18,530 is the BrimleyCacoon line
  brimleyLine <- 18530

  currentDate <- Sys.Date()

  #How many days old is person
  daysOld <- currentDate-birthdate
  daysOldf <- format(as.numeric(daysOld), big.mark=",")

  #When will they cross line
  dateReachLine=birthdate+brimleyLine

  #Days until line
  daysUntil = brimleyLine - daysOld
  daysUntilf = format(as.numeric(daysUntil), big.mark=",")


  #% of Brimley
  percentBrimley <- paste0(round(daysOld/brimleyLine*100, digits=1),"%")

  #Print results
  cat(paste0("a person born on ", birthdate, ":\n"))
  cat(paste0("\t- is ", daysOldf, " days old\n"))
  cat(paste0("\t- is ",percentBrimley, " the age Wilford Brimley was when Cacoon premeired\n"))
  cat(paste0("\t- will reach the Brimley/Cacoon Line on ", dateReachLine, "\n"))
  cat(paste0("\t- will reach line in ", daysUntilf, " days\n"))

  return(dt)
}

# results <- BrimleyCalculator::BrimleyCalculator(as.Date("1973-12-18"))
# results <- BrimleyCalculator::BrimleyCalculator(as.Date("1971-01-20"))
# results <- BrimleyCalculator::BrimleyCalculator(as.Date("1973-08-26"))
# results <- BrimleyCalculator::BrimleyCalculator(as.Date("2006-08-26"))

