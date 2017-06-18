#With thanks to S Munzert, C Rubba, P Meißner, D Nyhuis, Automated data collection with R: A practical guide to web scraping and text mining


#'
#' Helper function to extract the names of legislators
#'
#' @param String
#' @return String
#' @export
clean_string <- function(s){
  s <- str_trim(str_replace_all(s, "\\[.*", ""))
  s <- str_trim(str_replace_all(s, "\\S*\\.\\s", ""))
  return(s)
}


#'Create a list of all sponsors containing bill name, sponsor and a vector of co-sponsors
#'
#' @param directory of bills
#' @return sponsor list
#' @export
sponsors_list <- function(bill_dir){

  all_files <- dir(bill_dir)

  bill_sponsors_list <- list()
  sparse_sponsors <- list()

  for (i in 1:length(all_files))  {
    tryCatch({
      f <- str_c(bill_dir, all_files[i])
      b <- gsub(pattern = "\\.html$", "", basename(f))
      html_f <- read_html(f)
      #Sponsors
      s_t <- html_nodes(html_f, xpath = '//*[@id="content"]/div[1]/div[4]/div[1]/table')
      s_t <- as.data.frame(html_table(s_t))[1,2]
      s <- clean_string(s_t)
      #Cosponsors
      c_t <- html_nodes(html_f, xpath = '//*[@id="content"]/div[24]/table')
      c_t <- as.data.frame(html_table(c_t, fill = TRUE))
      c <- clean_string(c_t$Cosponsor)
      bill_sponsors_list[[i]] <- list(bill = b, sponsor = s, cosponsors = c)
      sparse_sponsors[[i]] <- c(s, c)
    },
    error=function(cond) {
      #message("Here's the original error message:")
      message(cond)
      #print(i)
    }
    )
  }

  #Drop list rows with errors.

  bill_sponsors_list <- bill_sponsors_list[lapply(bill_sponsors_list,length)>0]
  sparse_sponsors <- sparse_sponsors[lapply(sparse_sponsors,length)>0]

  #Get all bills, sponsors and cosponsors

  all_bills <- unique(unlist(lapply(bill_sponsors_list, "[[", 1)))
  all_sponsors <- unique(unlist(lapply(bill_sponsors_list, "[[", 2)))
  all_cosponsors <- unique(unlist(lapply(bill_sponsors_list, "[[", 3)))

  all_sponsors_cosponsors <- unique(c(all_sponsors,all_cosponsors))

  #Create a sponsor matrix

  sponsor_matrix <- matrix(NA, nrow = length(all_bills), ncol = length(all_sponsors_cosponsors))
  colnames(sponsor_matrix) <- all_sponsors_cosponsors
  rownames(sponsor_matrix) <- all_bills

  for(i in 1:length(bill_sponsors_list)){
    tryCatch({
      sponsor_matrix[i, which(all_sponsors_cosponsors == bill_sponsors_list[[i]]$sponsor)] <- "Sponsor"
      if(length(bill_sponsors_list[[i]]$cosponsors) > 0){
        for(j in 1:length(bill_sponsors_list[[i]]$cosponsors)){
          sponsor_matrix[i, which(all_sponsors_cosponsors == bill_sponsors_list[[i]]$cosponsors[j])] <- "Cosponsor"
        }
      }
    }, error=function(e){})
  }
  return(list(all_sponsors_cosponsors, sponsor_matrix, sparse_sponsors))
}


#'
#'get bios from http://bioguide.congress.gov/biosearch/biosearch1.asp
#'
#' @param position
#' @return bio df
#' @export
get_bio <- function(pos){
  temp_file <- "bios_temp.html"
  site <- postForm(
    uri = "http://bioguide.congress.gov/biosearch/biosearch1.asp",
    lastname = "",
    firstname = "",
    position = pos,
    state = "",
    party = "",
    congress = "114",
    style = "POST"
  )
  write(site, temp_file)
  site <- readLines(temp_file, encoding = "UTF-8")
  site <- str_c(site, collapse = "")
  result <- readHTMLTable(site, encoding="UTF-8")[[2]]
  result <- as.data.frame(sapply(result, as.character), stringsAsFactors = FALSE)
  if (file.exists(temp_file)) file.remove(temp_file)
  return (result)
}

#'
#'Arules support functions
#'
#' @param sparse sponsors
#' @return sponsor assoc
#' @export
arules_load_data <- function(sparse_sponsors){
  temp_file <- "arules_output.txt"
  for(i in 1:length(sparse_sponsors)){
    cat(paste(sparse_sponsors[[i]], collapse = '; '), "\n", file = temp_file, append = TRUE)
  }
  options(warn=-1)
  sponsor_assoc <- read.transactions(temp_file, sep = ";")
  options(warn=0)
  if (file.exists(temp_file)) file.remove(temp_file)
  return(sponsor_assoc)
}

#'
#'Plot network view
#'
#' @param Null
#' @return sponsor graph
#' @export
network_congress <- function() {
  # Create edge list
  edgelist_sponsors <- matrix(NA, nrow = 0, ncol = 2)
  for(i in 1:nrow(sponsor_matrix)){
    if(length(which(!is.na(sponsor_matrix[i,]))) > 1){
      edgelist_sponsors <- rbind(edgelist_sponsors,
                                 t(combn(colnames(sponsor_matrix)[which(!is.na(sponsor_matrix[i,]))], 2)))
    }
  }

  # Create graph object
  sponsor_network <- graph.edgelist(edgelist_sponsors, directed = FALSE)
  result <- matrix(
    NA,
    ncol = ncol(sponsor_matrix),
    nrow = 2,
    dimnames = list(
      c("Sponsor", "Cosponsor"),
      colnames(sponsor_matrix))
  )

  #Sponsorship count
  for(i in 1:ncol(sponsor_matrix)){
    result[1, i] <- sum(sponsor_matrix[, i] == "Cosponsor", na.rm = TRUE)
    result[2, i] <- sum(sponsor_matrix[, i] == "Sponsor", na.rm = TRUE)
  }

  result <- t(result)

  #Get adjacency list
  adj_sponsor <- get.adjacency(sponsor_network)

  #Find strong collaborators
  #min(sort(adj_sponsor, decreasing = TRUE)[1:10])
  #max_indices <- which(adj_sponsor >= min(sort(adj_sponsor, decreasing = TRUE)[1:10]), arr.in = T)
  #export_names <- matrix(NA, ncol = 2, nrow = 10)

  #for(i in 1:nrow(max_indices)){
  #export_names[i, 1] <- rownames(adj_sponsor)[max_indices[i,1]]
  #export_names[i, 2] <- colnames(adj_sponsor)[max_indices[i,2]]
  #}

  #Simplify network
  E(sponsor_network)$weight <- 1
  sponsor_network_weighted <- simplify(sponsor_network)
  head(E(sponsor_network_weighted)$weight)

  #Thin and plot network
  plot_sponsor <- sponsor_network_weighted
  plot_sponsor <- delete.edges(plot_sponsor, which(E(plot_sponsor)$weight < (mean(E(plot_sponsor)$weight) + sd(E(plot_sponsor)$weight))))

  return(plot_sponsor)
}
