#' Receives a sorted hashmap with found entities from a dictionary
#'
#' @param dictionary Character vector that is the name of a dicitonary having pre-calculated stats. This can be
#' MeSH, DrugBank, Agrovoc, EpSO, ESSO, or EPILONT
#' @param database the name of the MongoDB database to be used
#' @param collection the name of the MongoDB collection to be used
#' @return a sorted hashmap containing all found entities from the respective dictionaries with frequencies
#' @export
#' @importFrom mongolite mongo
#' @importFrom hash hash
#' @examples
#' \dontrun{
#' mesh <- getTermMatrix("MeSH")
#' }
#'
getTermMatrix <- function(dictionary, database, collection) {
  dictionaries <- list(
    "MeSH" = "MeSH", 
    "DrugBank" = "DrugBank", 
    "Agrovoc" = "Agrovoc", 
    "EpSO" = "EpSO",
    "ESSO" = "ESSO",
    "EPILONT" = "EPLIONT",
    "EPISEM" = "EPISEM")
  
  ## TDM Server
  serverip <- "localhost"
  mydb <- database
  mycoll <- collection # "meshepilepsietotalcounts2018"
  
  mydbcoll <- sprintf('%s.%s', mydb, mycoll)
  #dictionary = "MeSH"
  con <- mongolite::mongo(collection = mycoll, db = mydb, url = paste('mongodb://', serverip, sep = ""), verbose = TRUE)
  
  
  
  #  s <- paste('{"CONCEPTCLASS" : "', , '"}', collapse='')
  s <- sprintf('{"dict" : "%s"}', dictionary)
  
  cat('\nSearching for entries from dictionary', dictionary, 'with', s)
  
  # iter <- con$iterate(query = s, fields = '{}', limit = 1)
  iter <- con$iterate(query = s, fields = '{"dict": 1, "concept":1, "doccount":1}')
  
  ## create the counter
  counter = 1

  conceptlist <- c()
  countlist <- c()
  while(length(doc <- iter$one())) {
    curdoc <- doc
    
    curdict <- curdoc$dict
    curconcept <- curdoc$concept
    curcount <- curdoc$doccount
    #cat ("", curdict, curconcept, curcount)
    if (curdict==dictionary) {
      #if (!filterErrorConcepts(curconcept)) {
      #cat("\n\tAdded ", curconcept)(
      conceptlist <- c(conceptlist, curconcept)
      countlist <- c(countlist, curcount)
      #} else {
      #cat("\n\tDropped ", curconcept)
      #}
      
    }
    
    counter = counter + 1
    if (counter %% 100 == 0) {
      cat("\nProcessing: ", counter)
    }
  }
  hash::hash(keys = conceptlist, values = countlist)
  cat("\nProcessed ", counter , "concepts")
  
  concepts <- conceptlist
  frequencies <- as.integer(countlist)
  m <- matrix(frequencies, nrow=length(concepts))
  rownames(m) <- concepts
  res <- sort(rowSums(m), decreasing = TRUE)
  return (res)
}

filterErrorConcepts <- function (curconcept) {
  errorConcepts <- c(
    "Nitric Oxide",
    "Artenusate",
    "L-Proline",
    "L-Isoleucine",
    "L-Asparagine",
    "L-Aspartic Acid",
    "L-Leucine"
  )
  return (curconcept %in% errorConcepts)
}

#' Clears object that was loaded from harddrive into a list of terms sorted by frequency
#'
#' @param topfreqdictraw list with terms from a dictionary sorted by frequency
#'
#' @return a sorted list of terms
#' @export
#'
#' @examples
#' \dontrun{
#' genDictListFromRawFreq(epi)
#' }
genDictListFromRawFreq <- function (topfreqdictraw) {
  # remove last element from all lists because it is falsely a 1 from previous processing
  la = length(topfreqdictraw)
  topfreqdictraw <- topfreqdictraw[-la]
  a <- attributes(topfreqdictraw)$names
  a
}