#' Cluster-based permutation tests for time series data, based on mixed-effects models or other \code{buildmer} models.
#' @param formula A normal formula, possibly using \code{lme4}-style random effects. This can also be a buildmer terms object, provided \code{dep} is passed in \code{buildmerControl}. Only a single response variable is supported. For binomial models, the \code{cbind} syntax is not supported; please convert your dependent variable to a proportion and use weights instead.
#' @param family The family.
#' @param data The data.
#' @template weightsoffset
#' @param series.var A one-sided formula giving the variable grouping the time series.
#' @template buildmer1
#' @param parallel Whether to parallelize the permutation testing using plyr's \code{parallel} option. Needs some additional set-up; see the plyr documentation.
#' @param progress A plyr \code{.progress} bar name, see the plyr documentation. If not \code{'none'} while \code{parallel=TRUE}, an ad-hoc solution will be used, which will be visible if the cluster nodes were created with \code{outfile=''}.
#' @template buildmer2
#' @examples
#' \donttest{
#' # Testing a single EEG electrode, with random effects by participants
#' perms <- clusterperm.lmer(Fz ~ Deviant * Session + (Deviant * Session | Subject),
#' 	data=MMN,series.var=~Time)
#' # Testing a single EEG electrode, with random effects by participants, ANOVA inference
#' perms <- clusterperm.lmer(Fz ~ Deviant * Session + (Deviant * Session | Subject),
#' 	data=MMN,series.var=~Time,type='anova')
#' }
#' \dontshow{
#' perms <- clusterperm.lmer(Fz ~ Deviant*Session + (1|Subject),data=MMN[MMN$Time > 200 & MMN$Time < 205,],series.var=~Time,nperm=2,type='anova')
#' perms <- clusterperm.lmer(Fz ~ Deviant*Session + (1|Subject),data=MMN[MMN$Time > 200 & MMN$Time < 205,],series.var=~Time,nperm=2,type='regression')
#' perms <- clusterperm.lmer(Fz ~ Session + (1|ppn),data=within(MMN[MMN$Time > 200 & MMN$Time < 205,],{Session <- factor(Session)}),series.var=~Time,nperm=2,type='regression')
#' }
#' @importFrom stats gaussian
#' @export
clusterperm.lmer <- function (formula,data=NULL,family=gaussian(),weights=NULL,offset=NULL,series.var=~0,buildmerControl=list(direction='order',crit='LRT',quiet=TRUE,ddf='lme4'),nperm=1000,type='regression',parallel=FALSE,progress='none') {
	if (length(type) != 1 || !type %in% c('anova','regression')) {
		stop("Invalid 'type' argument (specify one of 'anova' or 'regression')")
	}
	if (type == 'regression') {
		pkgcheck(c('buildmer','permuco'))
	} else {
		pkgcheck(c('buildmer','car','permuco'))
	}
	dep <- if ('dep' %in% names(buildmerControl)) buildmerControl$dep else as.character(formula[2])
	if (all(is.null(weights))) {
		weights <- rep(1,length(data[[dep]]))
	}
	if (all(is.null(offset))) {
		offset <- rep(0,length(data[[dep]]))
	}
	ix <- !is.na(data[[dep]]) & !is.na(weights) & !is.na(offset)
	data <- data[ix,]
	if ('.weights' %in% names(data)) {
		stop("Please remove/rename the column named '.weights' from your data; this column name is used internally by permutes")
	}
	if ('.offset' %in% names(data)) {
		stop("Please remove/rename the column named '.offset' from your data; this column name is used internally by permutes")
	}
	data$.weights <- weights[ix]
	data$.offset <- offset[ix]
	if (!inherits(series.var,'formula')) {
		series.var <- stats::reformulate(series.var)
	}
	series.var <- attr(terms(series.var),'term.labels')
	has.series <- length(series.var)
	if (has.series == 0) {
		timepoints <- rep(0,sum(ix))
	} else {
		if (has.series != 1) {
			stop('series.var does not appear to contain exactly one variable')
		}
		timepoints <- data[[series.var]]
		if (is.null(timepoints)) {
			stop('series.var ',series.var,' not found in data')
		}
	}
	if (is.character(family)) {
		family <- get(family)
	}
	if (is.function(family)) {
		family <- family()
	}
	wrap <- function (t,fun,formula,data,family,timepoints,buildmerControl,nperm,type,verbose) {
		errfun <- function (e) {
			# error in permutation-test function, return an empty result for this timepoint
			warning(e)
			data.frame(Factor=NA,p=NA)
		}
		ix <- timepoints == t
		data <- data[ix,]
		model <- tryCatch(fun(t,formula,data,family,timepoints,buildmerControl,nperm,type,verbose),error=errfun)
	}
	if (has.series) {
		if (isTRUE(parallel)) {
			verbose <- progress != 'none'
			progress <- 'none'
		} else {
			if (!is.character(progress)) {
				stop("Invalid 'progress' specified for non-ad-hoc parallel solution (it has to be a character string)")
			}
			verbose <- FALSE
		}
	} else {
		verbose <- if (is.logical(progress)) progress else progress != 'none'
		progress <- 'none'
	}
	results <- plyr::alply(sort(unique(timepoints)),1,wrap,fit.buildmer,formula,data,family,timepoints,buildmerControl,nperm,type,verbose,.parallel=parallel,.progress=progress,.inform=FALSE)
	terms <- lapply(results,`[[`,'terms')
	perms <- lapply(results,`[[`,'perms')
	df <- plyr::ldply(results,`[[`,'df',.id=series.var)
	for (i in seq_along(perms)) {
		# alply drops names for some very strange reason, so we saved them in the 'terms' element and restore them here
		names(perms[[i]]) <- terms[[i]]
	}

	df$p <- df$cluster_mass <- df$cluster <- NA
	# We need to invert the double-nested list from perm$time$factor to perm$factor$time
	for (x in unique(df$Factor)) {
		this.factor <- lapply(perms,`[[`,x) #all timepoints for this one factor
		df.LRT <- max(sapply(this.factor,function (x) x$df),na.rm=TRUE) #these will all be the same (because they are the same model comparison and these are ndf), except possibly in cases of rank-deficiency, hence why max is correct
		thresh <- stats::qchisq(.95,df.LRT)
		samp   <- sapply(this.factor,function (x) c(x$LRT,x$perms)) #columns are time, rows are samples
		p      <- apply(samp,2,function (x) sum(x[-1] >= x[1],na.rm=TRUE) / sum(!is.na(x)))
		stat   <- if (has.series) permuco::compute_clustermass(samp,thresh,sum,'greater')$main else NA
		df[df$Factor == x,c('p','cluster_mass','p.cluster_mass','cluster')] <- c(p,stat)
	}

	if (has.series) {
		df <- cbind(df[,1],Measure=as.character(dep),df[,-1])
		colnames(df)[1] <- series.var
	} else {
		df[,1] <- df$cluster <- df$cluster_mass <- df$p.cluster_mass <- NULL
	}
	attr(df,'permutations') <- results
	class(df) <- c('permutes','data.frame')
	return(df)
}

fit.buildmer <- function (t,formula,data,family,timepoints,buildmerControl,nperm,type,verbose) {
	buildmerControl$direction <- 'order'
	if (is.null(buildmerControl$quiet)) {
		buildmerControl$quiet <- TRUE
	}
	if (is.null(buildmerControl$ddf)) {
		buildmerControl$ddf <- 'lme4'
	}

	# First, make sure we only work with the tabular representation of the formula
	if (inherits(formula,'formula')) {
		if (is.null(buildmerControl$dep)) {
			buildmerControl$dep <- as.character(formula[2])
		}
		formula <- buildmer::tabulate.formula(formula)
	}
	fixed <- is.na(formula$grouping)
	terms <- stats::setNames(,formula$term[fixed])

	if (type == 'regression') {
		# Convert factor terms into individual parameters
		env <- environment(formula)
		for (term in terms) {
			if (is.factor(data[[term]])) {
				X <- stats::model.matrix(stats::as.formula(paste0('~',term)),data)[,-1,drop=FALSE]
				if (is.vector(X)) {
					next #only one contrast
				}
				formula <- formula[!(formula$term == term & fixed),]
				colnames(X) <- paste0(term,'_',colnames(X))
				if (length(bad <- intersect(colnames(X),colnames(data)))) {
					stop('Please rename the columns ',paste(bad,collapse=', '),' in your data')
				}
				for (x in colnames(X)) {
					data[[x]] <- X[,x] #we can't use cbind as that will drop contrasts
				}
				formula <- rbind(formula,data.frame(index=NA,grouping=NA,term=colnames(X),code=unname(term),block=unname(term)))
				fixed <- is.na(formula$grouping)
			}
		}
		environment(formula) <- env
		terms <- stats::setNames(,formula$term[fixed])
	}

	.weights <- data$.weights; .offset <- data$.offset #silence R CMD check warning
	bm <- buildmer::buildmer(formula=formula,data=data,family=family,buildmerControl=buildmerControl,weights=.weights,offset=.offset)
	perms <- lapply(terms,function (term) {
		if (verbose) {
			time <- Sys.time()
			nmodels <- length(terms) * length(unique(timepoints))
		}

		# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3883440/ propose that, to test a random effect:
		# 1. Get the marginal errors calculated by the alternative model
		# 2. To account for these errors' non-independence: weight the errors by the inverse of the random-effects correlation matrix
			# V0 = sigma^2_b_1_0 * ZtZ + sigma^2_e_0I
		# 3. Weigh by Ut0^-1, where U0 = chol(V0)
			# lme4 parameterizes sigma^2_b_1_0 Z = Z Lambda_theta and we really want their transpose
		# 4. Permute the unweighted errors, then reweight the permuted data
		# 5. Reestimate *both* models with the fixed effects removed, which is necessary if any random effects happened to be similar

		# Our problem is much simpler than the above, however, since we only test fixed effects.
		# For us, the marginal errors are therefore based only on the fixed effects, and therefore *are* exchangeable under the null.
		# According to https://amstat.tandfonline.com/doi/abs/10.1080/01621459.1994.10476890, these marginal errors indeed are exchangeable even across subjects and items.

		# 1. Get the marginal errors based on quantities from the alternative model
		# These are y - XB - Zu, with the effect of interest *removed* from X
		# It's easier for us to just take the residuals and add this effect back in, but we need to figure out its name...
		if (inherits(bm@model,'merMod')) {
			X <- lme4::getME(bm@model,'X')
			B <- lme4::fixef(bm@model)
		} else {
			X <- stats::model.matrix(formula(bm@model),data)
			B <- stats::coef(bm@model)
		}
		if (any(i <- !is.finite(B))) {
			B[i] <- 0
		}
		if (term == '1') {
			# If it's the intercept, things are simple
			X[colnames(X) != '(Intercept)'] <- 0
			e <- stats::resid(bm@model) + X %*% B
			X <- X[,colnames(X) == '(Intercept)']
		} else {
			# If it's not the intercept, things are more complicated: we need to figure out the name in the model matrix
			# Keep in the intercept because otherwise model.matrix() will not process factor levels
			tab.restricted <- formula[formula$term %in% c('1',term) & is.na(formula$grouping),]
			formula.restricted <- buildmer::build.formula(NULL,tab.restricted)
			X.restricted <- stats::model.matrix(formula.restricted,data)
			if ('1' %in% tab.restricted$term) {
				# Now we drop the intercept again, and we will only be left with the focal effect
				X.restricted <- X.restricted[,-1,drop=FALSE]
			}
			if (!NCOL(X.restricted)) {
				return(list(perms=0*1:nperm,LRT=0,df=0))
			}
			normalized.colnames <- function (X) { #because interaction terms may have been wickedly reordered between colnames(X) and colnames(X.restricted)
				split <- strsplit(colnames(X),':')
				norm <- lapply(split,sort)
				sapply(norm,paste0,collapse=':')
			}
			want <- normalized.colnames(X) %in% normalized.colnames(X.restricted)
			X[,!want] <- 0
			e <- stats::resid(bm@model) + X %*% B
			X <- X[,want]
		}

		# 2/3. Random effects have already been partialed out, so these are independent and exchangeable
		# 4/5. Permute them and estimate a null and alternative model on the permuted data
		# The offset has been partialed out already, so will be ignored
		fit <- if (bm@p$is.gaussian) function (formula,data) stats::lm(formula,data,weights=.weights) else function (formula,data) suppressWarnings(stats::glm(formula,family=family,data=data,weights=.weights))
		perms <- lapply(1:nperm,function (i) try({
			s <- sample(seq_along(e))
			data <- list(
				y = family$linkinv(e[s]),
				X = X,
				.weights = data$.weights[s]
			)
			m1 <- fit(y ~ 0+X,data)
			m0 <- fit(y ~ 0,data)
			as.numeric(2*(stats::logLik(m1)-stats::logLik(m0)))
		},silent=TRUE))
		bad <- sapply(perms,inherits,'try-error')
		if (any(bad)) {
			perms[bad] <- NA
		}
		perms <- unlist(perms)

		# Wrap up
		data$y <- family$linkinv(e)
		data$X <- X
		ll1 <- stats::logLik(fit(y ~ 0+X,data))
		ll0 <- stats::logLik(fit(y ~ 0,data))
		LRT <- as.numeric(2*(ll1-ll0))
		df  <- attr(ll1,'df') - attr(ll0,'df')
		if (verbose) {
			diff <- Sys.time() - time
			cat('One of the',nmodels,'permutation tests finished in',diff,attr(diff,'units'),'\n')
		}
		list(perms=perms,LRT=LRT,df=df)
	})
	LRT <- sapply(perms,`[[`,'LRT')
	scale.est <- !family(bm@model)$family %in% c('binomial','poisson')
	is.mer <- inherits(bm@model,'merMod')
	if (type == 'regression') {
		se <- sqrt(diag(as.matrix(stats::vcov(bm@model)))) #as.matrix needed to work around 'Error in diag(vcov(bm@model)) : long vectors not supported yet: array.c:2186'
		if (inherits(bm@model,'merMod')) {
			beta <- lme4::fixef(bm@model)
			if (length(beta) < length(terms)) { #rank-deficiency
				missing <- setdiff(names(terms),names(beta))
				names(se) <- names(beta)
				beta[missing] <- NA
				se[missing] <- NA
				beta <- beta[names(terms)]
				se <- se[names(terms)]
			}
		} else {
			beta <- stats::coef(bm@model)
		}
		tname <- if (scale.est) 't' else 'z'
		df <- data.frame(Factor=unname(terms),LRT=unname(LRT),beta=unname(beta),t=unname(beta/se))
		colnames(df)[4] <- tname
		list(terms=terms,perms=perms,df=df)
	} else {
		if (inherits(bm@model,'gam')) {
			anovatab <- stats::anova(bm@model) #is Type III
			Fvals <- anovatab$pTerms.chi.sq / anovatab$pTerms.df
			Fname <- 'F'
			df <- anovatab$pTerms.df
		} else {
			if (is.mer) {
				anovatab <- car::Anova(bm@model,type=3,test='Chisq')
				if (scale.est) {
					Fvals <- anovatab$Chisq / anovatab$Df
					Fname <- 'F'
				} else {
					Fvals <- anovatab$Chisq
					Fname <- 'Chisq'
				}
			} else {
				if (scale.est) {
					anovatab <- car::Anova(bm@model,type=3,test='F')
					Fvals <- anovatab$F
					Fname <- 'F'
				} else {
					anovatab <- car::Anova(bm@model,type=3,test='Wald')
					Fvals <- anovatab$Chisq
					Fname <- 'Chisq'
				}
			}
			df <- anovatab$Df
		}
		names(Fvals) <- rownames(anovatab)
		if (length(Fvals) < length(terms)) { #rank-deficiency
			missing <- setdiff(names(terms),names(Fvals))
			Fvals[missing] <- NA
			Fvals <- Fvals[names(terms)]
		}
		df <- data.frame(Factor=unname(terms),df=df,LRT=unname(LRT),F=unname(Fvals))
		colnames(df)[4] <- Fname
		list(terms=terms,perms=perms,df=df)
	}
}
