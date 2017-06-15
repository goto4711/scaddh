#'
#' This function creates an English hate vocabulary based on hatebase.org 
#'
#' @param API key
#' @return A df of hate vocabulary
#' @export
hate_vocabulary <- function(k){
  # For pagigation first check the number of results by running a query manually 
  # At the time of writing 566 for eng
  # You can also get this with a quick call to the API and round(as.numeric(cont$number_of_results)/100)
  
  hate_df <- data.frame(matrix(nrow=566,ncol=4))
  colnames(hate_df) = c("word","meaning","offensiveness","number_of_sightings")
  version <- 'v3-0'
  key <- k
  output <- 'json' #can also be xml
  request_type <- 'vocabulary' #can also be sightings
  lang <- 'eng' 
  for(i in 1:6) {#566 / 100 ~= 6
    url <- paste0('https://api.hatebase.org/',version,'/',key,'/',
                  request_type,'/',output,'/page=',i ,'%7clanguage=',lang)
    #print(url)
    r <- GET(url)
    cont <- content(r, as = "parsed", type = "application/json")
    n <- as.numeric(cont$number_of_results_on_this_page)
    for(j in 1:n) {
      row <- ((i-1)*100)+j
      #print(row)
      hate_df[row,1] <- cont$data$datapoint[[j]]$vocabulary
      hate_df[row,2] <- cont$data$datapoint[[j]]$meaning
      hate_df[row,3] <- cont$data$datapoint[[j]]$offensiveness
      hate_df[row,4] <- cont$data$datapoint[[j]]$number_of_sightings			
    }		
  }
  return(hate_df)
}

#' BigramTokenizer
#'
#' @param Text
#' @return bigrams
#' @export
BigramTokenizer <- function(x) {
  unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
}

#' Cleaning the text of Tweets
#'
#' @param Text
#' @return Text
#' @export
clean_tweets <- function(txt) {
  require(tm)
  txt <- iconv(txt, 'UTF-8', 'ASCII')
  txt <- gsub("\\b[a-zA-Z0-9]{1,2}\\b", "", txt) # replace words shorter than 3
  txt <- unlist(strsplit(txt, " "))
  return (paste(txt[!txt %in% stopwords("en")], collapse = " "))
}
