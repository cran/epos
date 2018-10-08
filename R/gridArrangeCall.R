#' Does the full plot on one page
#'
#' @param cosinemeshplot plot with cosine coefficients against MeSH
#' @param cosinedrugbankplot plot with cosine coefficients against DrugBank
#' @param cosineepilepsyplot plot with cosine coefficients of Epilepsy Ontologies
#' @param dicemeshplot plot with dice coefficients against MeSH
#' @param dicedrugbankplot plot with dice coefficients against DrugBank
#' @param diceepilepsyplot plot with dice coefficients of Epilepsy Ontologies
#' @param jaccardmeshplot plot with jaccard coefficients against MeSH
#' @param jaccarddrugbankplot plot with jaccard coefficients against DrugBank
#' @param jaccardepilepsyplot plot with jaccard coefficients of Epilepsy Ontologies
#'
#' @importFrom gridExtra grid.arrange
#'
#' @return full
#'
#' @examples
#' \dontrun{
#' full <- doFullPlot (cosinemeshplot, 
#'                     cosinedrugbankplot, 
#'                     cosineepilepsyplot, 
#'                     dicemeshplot, 
#'                     dicedrugbankplot, 
#'                     diceepilepsyplot, 
#'                     jaccardmeshplot, 
#'                     jaccarddrugbankplot, 
#'                     jaccardepilepsyplot)
#' }
doFullPlot <- function (cosinemeshplot, 
                        cosinedrugbankplot, 
                        cosineepilepsyplot, 
                        dicemeshplot, 
                        dicedrugbankplot, 
                        diceepilepsyplot, 
                        jaccardmeshplot, 
                        jaccarddrugbankplot, 
                        jaccardepilepsyplot) {
full <- gridExtra::grid.arrange(cosinemeshplot, 
                        cosinedrugbankplot, 
                        cosineepilepsyplot, 
                        dicemeshplot, 
                        dicedrugbankplot, 
                        diceepilepsyplot, 
                        jaccardmeshplot, 
                        jaccarddrugbankplot, 
                        jaccardepilepsyplot)
  return (full)
}