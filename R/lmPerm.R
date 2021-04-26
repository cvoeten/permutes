#' Permutation tests for time series data, based on classic linear regression or ANOVA
#' This is a legacy function that is primarily provided for backward compatibility. You should probably use \code{permutelm} or \code{permutelmer} instead.
#' \code{permu.test} is the only function to support multivariate responses, although multivariate data can trivially be recoded into a univariate model.
#' \code{permu.test} does not support random effects or corrected p-values (e.g. the cluster mass test), which are supported by \code{permutelm}.
#' @param formula A formula of the following form: \code{outcome ~ predictors | series variable}. Multivariate outcomes (e.g. 32 EEG electrodes) are supported; use \code{cbind(Fp1,Fp2,etc) ~ predictors | series}.
#' @param data The dataset referencing these predictors.
#' @param subset If specified, will only analyze the specified subset of the data.
#' @param type A character string of either \code{'anova'} or \code{'regression'}. The former runs an analysis of variance and returns F-values and p-values based on the explained variance of each factor in the design. The latter runs a linear-regression analysis and returns t-values and p-values based on individual effects. When running ANOVA, it is advised to use orthogonal predictors, as type III sums of squares are used.
#' @param parallel Whether to parallelize the permutation testing using plyr's \code{parallel} option. Needs some additional set-up; see the plyr documentation.
#' @param progress A plyr \code{.progress} bar name, see the plyr documentation. Ignored if parallel=TRUE.
#' @param ... Other arguments to be passed to \code{lmp}/\code{aovp}
#' @return A data frame.
#' @seealso \code{clusterperm.lm}, \code{clusterperm.lmer}
#' @examples
#' \donttest{
#' # EEG data example using the MMN dataset
#' 
#' # Run permutation tests on all electrodes and timepoints, reporting p-values for the three
#' # manipulated factors
#' perms <- permu.test(cbind(Fp1,AF3,F7,F3,FC1,FC5,C3,CP1,CP5,P7,P3,Pz,PO3,O1,Oz,O2,PO4,P4,
#' 	P8,CP6,CP2,C4,FC6,FC2,F4,F8,AF4,Fp2,Fz,Cz) ~ Deviant * Session | Time,data=MMN)
#' 
#' # Run the tests in parallel on two CPU threads
#' # first, set up the parallel backend
#' library(doParallel)
#' cl <- makeCluster(2)
#' registerDoParallel(cl)
#' perms <- permu.test(cbind(Fp1,AF3,F7,F3,FC1,FC5,C3,CP1,CP5,P7,P3,Pz,PO3,O1,Oz,O2,PO4,P4,
#' 	P8,CP6,CP2,C4,FC6,FC2,F4,F8,AF4,Fp2,Fz,Cz) ~ Deviant * Session | Time,data=MMN,
#' 	parallel=TRUE)
#' stopCluster(cl)
#' 
#' # Plot the results by F-value, removing points that were not significant in the
#' # permutation tests
#' plot(perms,sig='p')
#' 
#' # t-values instead of F-values
#' perms <- permu.test(cbind(Fp1,AF3,F7,F3,FC1,FC5,C3,CP1,CP5,P7,P3,Pz,PO3,O1,Oz,O2,PO4,P4,
#' 	P8,CP6,CP2,C4,FC6,FC2,F4,F8,AF4,Fp2,Fz,Cz) ~ Deviant * Session | Time,data=MMN,
#' 	type='regression')
#' }
#' \dontshow{
#' perms <- permu.test(Fp1 ~ Deviant*Session | Time,data=MMN[MMN$Time > 200 & MMN$Time < 205,])
#' perms <- permu.test(cbind(Fp1,Fp2) ~ Deviant*Session | Time,data=MMN[MMN$Time > 200 & MMN$Time < 205,])
#' perms <- permu.test(Fp1 ~ Deviant*Session | Time,data=MMN[MMN$Time > 200 & MMN$Time < 205,],type='regression')
#' perms <- permu.test(cbind(Fp1,Fp2) ~ Deviant*Session | Time,data=MMN[MMN$Time > 200 & MMN$Time < 205,],type='regression')
#' }
#' @export
permu.test <- function (formula,data,subset=NULL,type='anova',parallel=FALSE,progress='text',...) {
	pkgcheck('lmPerm')
	if (formula[[1]] != '~') stop("Invalid formula (first operator is not '~')")
	indep <- formula[[3]]
	if (indep[[1]] != '|') stop("Invalid formula (the rightmost term should start with '|', followed by your timepoint variable)")
	timepoint.var <- as.character(indep[[3]])
	formula[[3]] <- indep[[2]]
	if (!is.null(subset)) {
		data <- data[subset,]
	}
	if (length(type) != 1 || !type %in% c('anova','regression')) {
		stop("Invalid 'type' argument (specify one of 'anova' or 'regression')")
	}
	timepoints <- data[[timepoint.var]]
	if (is.null(timepoint.var)) {
		stop('Series variable not found in data')
	}
	dots <- list(...)

	fun <- if (type == 'regression') fit.lmp else fit.aovp
	wrap <- function (t,fun,formula,data,timepoints,dots) {
		errfun <- function (e) {
			# error in permutation-test function, return an empty result for this timepoint
			warning(e)
			data.frame(factor=NA,p=NA)
		}
		data <- data[timepoints == t,]
		model <- tryCatch(fun(t,formula,data,timepoints,dots),error=errfun)
	}
	ret <- plyr::adply(sort(unique(timepoints)),1,wrap,fun,formula,data,timepoints,dots,.id=timepoint.var,.parallel=parallel,.progress=ifelse(parallel,'none',progress))
	if (is.null(ret$measure)) {
		ret <- cbind(ret[,1],measure=as.character(formula[[2]]),ret[,-1])
		colnames(ret)[1] <- timepoint.var
	}
	ret$measure <- sub(' ?Response +','',ret$measure)
	ret$factor <- gsub(' +$','',ret$factor)
	class(ret) <- c('permutes','data.frame')
	return(ret)
}

fit.aovp <- function (t,formula,data,timepoints,dots) {
	lmp <- lmPerm::lmp #work around improper namespacing in lmPerm
	mod <- do.call(lmPerm::aovp,c(list(formula,data,settings=FALSE,center=FALSE),dots))
	smy <- summary(mod)
	plyr::ldply(smy,function (res) {
		if (ncol(res) != 5) {
			stop('Timepoint ',t,' did not have more observations than predictors; the model is unidentifiable')
		}
		nr <- nrow(res)
		factors <- rownames(res)[-nr] #the last row is the residuals
		p <- res[[5]][-nr]
		df <- res[[1]]
		SS <- res[[2]]
		dff <- df[-nr]
		dfe <- df[ nr]
		SSf <- SS[-nr]
		SSe <- SS[ nr]
		MSf <- SSf/dff
		MSe <- SSe/dfe
		Fval <- MSf/MSe
		w2 <- (SSf - dff * MSe) / (sum(SS) + MSe)
		w2 <- pmax(w2,0)
		data.frame(factor=factors,F=Fval,p=p,w2=w2,stringsAsFactors=FALSE)
	},.id='measure')
}

fit.lmp <- function (t,formula,data,timepoints,dots) {
	mod <- do.call(lmPerm::lmp,c(list(formula,data,settings=FALSE,center=FALSE),dots))
	smy <- summary(mod)
	if (is.null(ncol(mod$coefficients))) {
		smy <- list(smy) #univariate outcome
	}
	plyr::ldply(smy,function (res) {
		if (is.null(mod$coefficients) || all(is.na(mod$coefficients))) {
			stop('Timepoint ',t,' did not have more observations than predictors; the model is unidentifiable')
		}
		coef <- res$coefficients
		factors <- rownames(coef)
		beta <- coef[,1]
		se <- sqrt(diag(stats::vcov(mod)))
		p <- coef[,3]
		data.frame(factor=factors,beta=beta,t=beta/se,p=p,stringsAsFactors=FALSE)
	},.id='measure')
}
