#' @details
#' If not explicitly overridden in the \code{buildmerControl} argument, the function changes a few buildmer defaults to make more sense for permutation testing. This includes setting \code{direction='order'} and \code{quiet=TRUE}. Set these options explicitly to override this behavior.
#' Because model comparisons are used for efficient permutation testing, REML is not supported.
#' Omega squared is not available for mixed-effects ANOVA; the \code{w2} column will not be included in the resulting \code{permutes} object.
#' The returned object provides the raw bootstrap samples in the \code{pb} attribute. These are used to compute a corrected p-value using Maris & Oostenveld's (2007) cluster mass statistic, but can also be used by the user to compute other such statistics. These obviously significantly increase the size of the R object; if this becomes a problem, there is no harm in deleting them using e.g.\ \code{attr(x,'perms') <- NULL}.
#' @return A data frame.
#' @importFrom stats gaussian
