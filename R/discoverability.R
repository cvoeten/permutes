#' Cluster-based permutation tests for time series data, based on mixed-effects models or other \code{buildmer} models. This is an alias for \code{clusterperm.lmer}, except that random effects are explicily disallowed.
#' @param formula A normal formula without random effects. This can also be a buildmer terms object, provided \code{dep} is passed in \code{buildmerControl}. Only a single response variable is supported. For binomial models, the \code{cbind} syntax is not supported; please convert your dependent variable to a proportion and use weights instead.
#' @param family The family.
#' @param data The data.
#' @template weightsoffset
#' @param series.var A one-sided formula giving the variable grouping the time series.
#' @template buildmer1
#' @param parallel Whether to parallelize the permutation testing using plyr's \code{parallel} option. Needs some additional set-up; see the plyr documentation.
#' @param progress A plyr \code{.progress} bar name, see the plyr documentation. If not \code{'none'} while \code{parallel=TRUE}, an ad-hoc solution will be used, which will be visible if the cluster nodes were created with \code{outfile=''}.
#' @template buildmer2
#' @examples
#' perms <- clusterperm.lm(Fz ~ dev*session,data=MMN,series.var=~time)
#' @seealso clusterperm.lmer
#' @importFrom stats gaussian
#' @export
clusterperm.lm <- function (formula,data=NULL,family=gaussian(),weights=NULL,offset=NULL,series.var,buildmerControl=list(direction='order',crit='LRT',quiet=TRUE,ddf='lme4'),nperm=1000,type='regression',parallel=FALSE,progress='none',...) {
	if (type == 'regression') {
		pkgcheck('buildmer')
	} else {
		pkgcheck(c('buildmer','car'))
	}
	if (!is.data.frame(formula)) {
		if ('dep' %in% names(buildmerControl)) stop('Something is wrong --- formula is not a dataframe, but dep has been passed to buildmerControl')
		buildmerControl$dep <- as.character(formula[2])
		formula <- buildmer::tabulate.formula(formula)
	}
	if (!all(is.na(formula$grouping))) {
		stop('Random effects detected --- please use clusterperm.lmer instead')
	}
	mc <- match.call()
	e <- parent.frame()
	mc[[1]] <- clusterperm.lmer
	eval(mc,e)
}

#' Cluster-based permutation tests for time series data, based on generalized linear mixed-effects models or other \code{buildmer} models. This is an alias for \code{clusterperm.lmer} provided for discoverability.
#' @param ... Arguments to be passed to \code{clusterperm.lmer}.
#' @examples
#' # Testing a single EEG electrode, with random effects by participants
#' \donttest{
#' perms <- clusterperm.glmer(Fz ~ dev*session + (dev*session|ppn),data=MMN,series.var=~time)
#' }
#' \dontshow{
#' perms <- clusterperm.glmer(Fz ~ dev*session + (1|ppn),data=MMN[MMN$time > 200 & MMN$time < 205,],series.var=~time,nperm=list(nsim=2),type='anova')
#' perms <- clusterperm.glmer(Fz ~ dev*session + (1|ppn),data=MMN[MMN$time > 200 & MMN$time < 205,],series.var=~time,nperm=list(nsim=2),type='regression')
#' perms <- clusterperm.glmer(Fz ~ session*cond + (1|ppn),data=within(MMN[MMN$time > 200 & MMN$time < 205,],{session <- factor(session); cond <- factor(cond)}),series.var=~time,nperm=list(nsim=2),type='regression')
#' }
#' @seealso clusterperm.lmer
#' @export
clusterperm.glmer <- function (...) clusterperm.lmer(...)

#' Cluster-based permutation tests for time series data, based on generalized linear models or other \code{buildmer} models. This is an alias for \code{clusterperm.lmer} provided for discoverability.
#' @param ... Arguments to be passed to \code{clusterperm.lm}.
#' @examples
#' perms <- clusterperm.glm(Fz ~ dev*session,data=MMN,series.var=~time)
#' @seealso clusterperm.lm, clusterperm.lmer
#' @export
clusterperm.glm <- function (...) clusterperm.lm(...)
