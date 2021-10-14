#' Calculate Brimley/Cacoon line statistics
#'
#' @param birtdate Date of person
#'
#' @return date person will cross Brimley/Cacoon line
#' @export
#' @import glue
#' @examples
#' results <- BrimleyCalculator(as.Date("1973-12-18"))
BrimleyCalculator <- function(bdate){

  #18,530 is the BrimleyCacoon line
  brimleyLine <- 18530

  currentDate = Sys.Date()

  #How many days old is person
  daysOld <- currentDate-bdate

  #When will they cross line
  dateReachLine=bdate+brimleyLine

  #Days until line
  daysUntil = brimleyLine - daysOld
  devtools::create("myfirstpackage")
  devtools::create("BrimleyCalculator")

  #% of Brimley
  percentBrimley <- daysOld/brimleyLine
  #Print results
  print(glue("a person born on {bdate}"))
  print(glue("\tis {daysOld} days old"))
  print(glue("\tis {percentBrimley}% the age Wilford Brimley was when Cacoon premeired"))
  print(glue("\twill reach the Brimley/Cacoon Line on {dateReachLine}"))
  print(glue("\twill reach line in {daysUntil} days"))

  return(dt)
}

# results <- BrimleyCalculator(as.Date("1973-12-18"))
#
