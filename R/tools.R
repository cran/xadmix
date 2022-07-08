#' Admixture Data Subsetting
#'
#' Subset function optimized for admixture data.
#' Filters for the percentages of any number of ancestry (K) columns and prints progress. Also allows passing additional arguments to filter columns with.
#' @param data Data frame containing the admixture data.
#' @param anc Vector of ancestry column names to use for pairwise subsetting with percentage vector. Must be of same length as the supplied percentage vector.
#' @param pct Vector of percentage values to use for pairwise subsetting with ancestry column name vector. Only ancestries with values above the percentage are kept.
#' @param comparison What comparison operator to use for the subsetting. Can either be "greater" or "less"; default is "greater". Also accepts "gt", "lt", ">" and "<".
#' @param quiet Whether to print progress or not; default is "FALSE".
#' @param ... Variable number of additional vectors for subsetting. Looking at the column with argument name, keeps only those observations with values which are elements of the argument vector.
#' @return A subset of the provided data frame.
#' @examples
#' # load simulated admixture data
#' data("xadmixture")
#' 
#' # keep only observations with K1 > 0.1 and K2 > 0.01
#' subset1 <- admix_subset(xadmixture, 
#'                         anc = c("K1", "K2"), 
#'                         pct = c(0.1, 0.01))
#'
#' # keep only observations with K2 < 0.4 and K3 < 0.1
#' subset2 <- admix_subset(xadmixture, 
#'                         anc = c("K2", "K3"), 
#'                         pct = c(0.4, 0.1), 
#'                         comparison = "less")
#' 
#' # keep only observations with values "GBR" or "FRA" in column 
#' # "country" and values "lorem" or "dolor" in column "species"
#' subset3 <- admix_subset(xadmixture, 
#'                         country = c("GBR", "FRA"), 
#'                         species = c("lorem", "dolor"))
#' 
#' # keep only observations with K1 > 0.1 and K4 < 0.3, 
#' # without printing progress; subsets can be chained 
#' # using the pipe operator from package `magrittr` 
#' library(magrittr)
#' subset4 <- admix_subset(xadmixture, 
#'                         anc = "K1", 
#'                         pct = 0.1, 
#'                         quiet = TRUE) %>% 
#'  admix_subset(anc = "K4", 
#'               pct = 0.3, 
#'               comparison = "less", 
#'               quiet = TRUE)
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom stats na.omit
#' @importFrom methods hasArg
#' @export
admix_subset <- function(data, anc = NULL, pct = NULL, comparison = "greater", quiet = FALSE, ...) {
    # ancestries <anc> and percentages <pct> can be vectors
    # however, they must be of the same length!
    # the first entries each form a pair, then the second ones...

    # Error handling - check for same length vectors
    if (length(anc) != length(pct)) stop("Ancestry and percentage vectors must be of same length!")

    # reassign data arg for clarity
    asub <- data

    # print total observations
    if (!quiet) {
        cat("observations:", nrow(asub), "\n")
    }

    args <- list(...) # get additional arguments
    arg_names <- names(args) # get argument names

    # use additional args for subsetting
    if (length(arg_names) > 0) {
        for (i in 1:length(arg_names)) {
            asub <- asub %>% filter(asub[[arg_names[i]]] %in% args[[i]])
            # print progress
            if (!quiet) {
                cat("keeping only specified values in col:", arg_names[i], "\n")
                cat("\tobservations left after this step:", nrow(asub), "\n")
            }
        }
    }

    # loop over all ancestry-percentage pairs,
    # only selecting those with percentage higher or lower than cutoff
    # generating one subset
    if (hasArg(anc) && hasArg(pct)) {
        for (i in 1:length(anc)) {
            if (comparison == "gt" || comparison == "greater" || comparison == ">") {
                asub <- asub %>% filter(asub[[anc[i]]] > pct[i])
                compc <- ">"
            } else if (comparison == "lt" || comparison == "less" || comparison == "<") {
                asub <- asub %>% filter(asub[[anc[i]]] < pct[i])
                compc <- "<"
            } else {
                stop("comparison must be either 'gt' or 'lt'")
            }
            asub <- na.omit(asub)
            # print progress
            if (!quiet) {
                cat(i, ". subset, ", anc[i], " ", compc, " ", pct[i], "\n", sep = "")
                cat("\tobservations:", nrow(asub), "\n")
            }
        }
    }
    return(asub)
}
