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
#' \donttest{perms <- clusterperm.lm(Fz ~ Deviant * Session,data=MMN,series.var=~Time)}
#' \dontshow{perms <- clusterperm.lm(Fz ~ Deviant * Session,data=MMN[MMN$Time > 200 & MMN$Time < 205,],series.var=~Time,nperm=2)}
#' @seealso clusterperm.lmer
#' @importFrom stats gaussian
#' @export
clusterperm.lm <- function (formula,data=NULL,family=gaussian(),weights=NULL,offset=NULL,series.var,buildmerControl=list(direction='order',crit='LRT',quiet=TRUE,ddf='lme4'),nperm=1000,type='regression',parallel=FALSE,progress='none') {
	if (type == 'regression') {
		pkgcheck('buildmer')
	} else {
		pkgcheck(c('buildmer','car'))
	}
	if (!is.data.frame(formula)) {
		if ('dep' %in% names(buildmerControl)) stop('Something is wrong --- formula is not a data frame, but dep has been passed to buildmerControl')
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
#' \donttest{
#' # Testing a single EEG electrode, with random effects by participants
#' perms <- perm.glmer(Fz ~ Deviant * Session + (Deviant * Session | Subject),data=MMN)
#' # Testing a single EEG electrode, with random effects by participants, ANOVA inference
#' perms <- perm.glmer(Fz ~ Deviant * Session + (Deviant * Session | Subject),data=MMN,type='anova')
#' }
#' \dontshow{
#' perms <- perm.glmer(Fz ~ Deviant*Session + (1|Subject),data=MMN[MMN$Time > 200 & MMN$Time < 205,],nperm=2,type='anova')
#' perms <- perm.glmer(Fz ~ Deviant*Session + (1|Subject),data=MMN[MMN$Time > 200 & MMN$Time < 205,],nperm=2,type='regression')
#' perms <- perm.glmer(Fz ~ Session + (1|Subject),data=within(MMN[MMN$Time > 200 & MMN$Time < 205,],{Session <- factor(Session)}),nperm=2,type='regression')
#' }
#' @seealso clusterperm.lmer
#' @export
clusterperm.glmer <- function (...) clusterperm.lmer(...)

#' Cluster-based permutation tests for time series data, based on generalized linear models or other \code{buildmer} models. This is an alias for \code{clusterperm.lm} provided for discoverability.
#' @param ... Arguments to be passed to \code{clusterperm.lm}.
#' @examples
#' \donttest{perms <- clusterperm.glm(Fz ~ Deviant * Session,data=MMN,series.var=~Time)}
#' \dontshow{perms <- clusterperm.glm(Fz ~ Deviant * Session,data=MMN[MMN$Time > 200 & MMN$Time < 205,],series.var=~Time,nperm=2)}
#' @seealso clusterperm.lm, clusterperm.lmer
#' @export
clusterperm.glm <- function (...) clusterperm.lm(...)

#' A general permutation test for mixed-effects models or other \code{buildmer} models. This is an alias for \code{clusterperm.lmer}, except that random effects are explicily disallowed.
#' @param formula A normal formula, possibly using \code{lme4}-style random effects. This can also be a buildmer terms object, provided \code{dep} is passed in \code{buildmerControl}. Only a single response variable is supported. For binomial models, the \code{cbind} syntax is not supported; please convert your dependent variable to a proportion and use weights instead.
#' @param family The family.
#' @param data The data.
#' @template weightsoffset
#' @template buildmer1
#' @param progress Logical indicating whether to print progress messages during the permutation testing.
#' @template buildmer2
#' @examples
#' \donttest{
#' # Testing a single EEG electrode, with random effects by participants
#' perms <- perm.lmer(Fz ~ Deviant * Session + (Deviant * Session | Subject),data=MMN)
#' # Testing a single EEG electrode, with random effects by participants, ANOVA inference
#' perms <- perm.lmer(Fz ~ Deviant * Session + (Deviant * Session | Subject),data=MMN,type='anova')
#' }
#' \dontshow{
#' perms <- perm.lmer(Fz ~ Deviant*Session + (1|Subject),data=MMN[MMN$Time > 200 & MMN$Time < 205,],nperm=2,type='anova')
#' perms <- perm.lmer(Fz ~ Deviant*Session + (1|Subject),data=MMN[MMN$Time > 200 & MMN$Time < 205,],nperm=2,type='regression')
#' perms <- perm.lmer(Fz ~ Session + (1|Subject),data=within(MMN[MMN$Time > 200 & MMN$Time < 205,],{Session <- factor(Session)}),nperm=2,type='regression')
#' }
#' @importFrom stats gaussian
#' @export
perm.lm <- function (formula,data=NULL,family=gaussian(),weights=NULL,offset=NULL,buildmerControl=list(direction='order',crit='LRT',quiet=TRUE,ddf='lme4'),nperm=1000,type='regression',progress=TRUE) {
	if (type == 'regression') {
		pkgcheck('buildmer')
	} else {
		pkgcheck(c('buildmer','car'))
	}
	if (!is.data.frame(formula)) {
		if ('dep' %in% names(buildmerControl)) stop('Something is wrong --- formula is not a data frame, but dep has been passed to buildmerControl')
		buildmerControl$dep <- as.character(formula[2])
		formula <- buildmer::tabulate.formula(formula)
	}
	if (!all(is.na(formula$grouping))) {
		stop('Random effects detected --- please use perm.lmer instead')
	}
	mc <- match.call()
	e <- parent.frame()
	mc[[1]] <- perm.lmer
	eval(mc,e)
}

#' A general permutation test for mixed-effects models or other \code{buildmer} models. This is an alias for \code{perm.lmer} provided for discoverability.
#' @param ... Arguments to be passed to \code{perm.lmer}.
#' @examples
#' \donttest{
#' # Testing a single EEG electrode, with random effects by participants
#' perms <- perm.glmer(Fz ~ Deviant * Session + (Deviant * Session | Subject),data=MMN)
#' # Testing a single EEG electrode, with random effects by participants, ANOVA inference
#' perms <- perm.glmer(Fz ~ Deviant * Session + (Deviant * Session | Subject),data=MMN,type='anova')
#' }
#' \dontshow{
#' perms <- perm.glmer(Fz ~ Deviant*Session + (1|Subject),data=MMN[MMN$Time > 200 & MMN$Time < 205,],nperm=2,type='anova')
#' perms <- perm.glmer(Fz ~ Deviant*Session + (1|Subject),data=MMN[MMN$Time > 200 & MMN$Time < 205,],nperm=2,type='regression')
#' perms <- perm.glmer(Fz ~ Session + (1|Subject),data=within(MMN[MMN$Time > 200 & MMN$Time < 205,],{Session <- factor(Session)}),nperm=2,type='regression')
#' }
#' @seealso perm.lmer
#' @export
perm.glmer <- function (...) perm.lmer(...)

#' A general permutation test for mixed-effects models or other \code{buildmer} models. This is an alias for \code{perm.lm} provided for discoverability.
#' @param ... Arguments to be passed to \code{perm.lm}.
#' @examples
#' \donttest{perms <- perm.glm(Fz ~ Deviant * Session,data=MMN)}
#' \dontshow{perms <- perm.glm(Fz ~ Deviant * Session,data=MMN[MMN$Time > 200 & MMN$Time < 205,],nperm=2)}
#' @seealso perm.lm, perm.lmer
#' @export
perm.glm <- function (...) perm.lm(...)
