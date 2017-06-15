#Thanks to  https://www.r-bloggers.com/text-mining-the-complete-works-of-william-shakespeare/,
#'
#' Prepare Shakespeare Text file
#'
#' @param 
#' @return shakepeare text prepared
#' @export
load_shakespeare <- function(){
  shakespeare <- shakespeare[-(1:173)]
  shakespeare <- shakespeare[-(124195:length(shakespeare))]
  
  shakespeare <- paste(shakespeare, collapse = " ")
  shakespeare <- strsplit(shakespeare, "<<[^>]*>>")[[1]]
  dramatis.personae <- grep("Dramatis Personae", shakespeare, ignore.case = TRUE)
  shakespeare <- shakespeare[-dramatis.personae]
  return(shakespeare)
}