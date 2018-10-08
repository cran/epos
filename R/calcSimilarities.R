#' Calculate the jaccard coefficient for two lists a and b
#'
#' @param a list with elements that should be of same type as in list b
#' @param b list with elements
#'
#' @return ja list with length of set b containing the jaccard similarity coefficient for each list element
#' @export
#'
#' @examples
#' calcJaccard(c(1,2), c(2,3))
calcJaccard <- function (a, b) {
  la = length(a)
  lb = length(b)
  lo = lb
  for (i in 1:lo) {
    
    cinter <- length(intersect(a[1:i], b[1:i]))
    cunion <- length(union(x = a[1:i], y = b[1:i]))
    
    curalength <- i
    if (i>la) {
      curalength <- la  
    } 
    curblength <- i
    if (i > lb) {
      curblength <- lb
    } 
    
    cjac <- jaccard (cinter, cunion, curalength, curblength)
    
    
    if (i == 1) {
      ja = cjac
    } else {
      ja <- c(ja, cjac)
    }
  }
  return (ja)
}

#' Calculate jaccard similarity metric for two sets a and b
#'
#' @param ainterb integer value with number of intersecting elements between set a and b
#' @param aunionb integer value with number of union elements between set a and b
#' @param lengtha length of set a
#' @param lengthb length of set b
#'
#' @return jac double value with the jaccard similarity coefficient 
#' @export
#'
#' @examples
#' jaccard(1,3, 2, 3)
jaccard <- function (ainterb, aunionb, lengtha, lengthb) {
  jaccoefficient <- (ainterb) / (lengtha+lengthb-ainterb)
  jac <- 1-jaccoefficient
  return (jac)
}

#' Calculate the dice similarity metric for two lists a and b
#'
#' @param a list with elements that should be of same type as in list b
#' @param b list with elements
#'
#' @return di list with length of set b containing the dice similarity coefficient at each list element
#' @export
#'
#' @examples
#' calcDice(c(1,2), c(2,3))
calcDice <- function (a, b) {
  la = length(a)
  lb = length(b)
  lo = lb
  
  for (i in 1:lo) {
    cinter <- length(intersect(a[1:i], b[1:i]))
    curalength <- i
    if (la<i) {
      curalength <- la  
    } 
    curblength <- i
    if (lb < i) {
      curblength <- lb
    } 
    cdice <- dice (cinter, curalength, curblength)
    if (i == 1) {
      di = cdice
    } else {
      di <- c(di, cdice)
    }
  }
  di
}

#' Calculate dice similarity metric
#'
#' @param ainterb integer value with number of intersecting elements between set a and b
#' @param lengtha integer value with the number of items in set a
#' @param lengthb integer value with the number of items in set b
#'
#' @return dice double vlaue with the dice similarity coefficient 
#' @export
#'
#' @examples
#' dice(1, 3, 4)
dice <- function (ainterb, lengtha, lengthb) {
  dice <- 2*(ainterb/(lengtha+lengthb))
  dice <- 1- dice
  return (dice)
}

#' Calculate the cosine similarity metric for two lists a and b
#'
#' @param a list with elements that should be of same type as in list b
#' @param b list with elements
#'
#' @return co list with length of set b containing the cosine similarity coefficient at each position
#' @export
#'
#' @examples
#' calcCosine(c(1,2), c(2,3))
calcCosine <- function (a, b) {
  la = length(a)
  lb = length(b)
  lo = lb
  
  for (i in 1:lo) {
    cinter <- length(intersect(a[1:i], b[1:i]))
    curalength <- i
    if (la<i) {
      curalength <- la  
    } 
    curblength <- i
    if (lb < i) {
      curblength <- lb
    } 
    ccos <- cosine (cinter, curalength, curblength)
    if (i == 1) {
      co = ccos
    } else {
      co <- c(co, ccos)
    }
    #print(cjac)
  }
  return (co)
}

#' Calculate cosine similarity metric
#'
#' @param ainterb integer value with number of intersecting elements between set a and b
#' @param lengtha integer value with the number of items in set a
#' @param lengthb integer value with the number of items in set b
#'
#' @return cosine double vlaue with the cosine similarity coefficient 
#' @export
#'
#' @examples
#' cosine(1,3,4)
cosine <- function (ainterb, lengtha, lengthb) {
  cosine <- (ainterb) / (lengtha^(1/2) * lengthb^(1/2))
  cosine <- 1 - cosine
  return (cosine)
}