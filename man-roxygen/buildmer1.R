#' @param buildmerControl Options overriding the defaults in \code{buildmerControl}.
#' @param nperm The number of permutations.
#' @param type A character string of either \code{'anova'} or \code{'regression'}. The former runs an analysis of variance and returns F-values and p-values based on the explained variance of each factor in the design. The latter runs a linear-regression analysis and returns t-values and p-values based on individual effects. When running ANOVA, it is advised to use orthogonal predictors, as type III sums of squares are used.
