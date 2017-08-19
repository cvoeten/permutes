#' Permutation tests for time series data.
#' @param formula A formula of the following form: `outcome ~ predictors | timepoint variables'. Multivariate outcomes (e.g. 32 EEG electrodes) are supported; use `cbind(Fp1,Fp2,etc) ~ predictors | timepoint'.
#' @param data The dataset referencing these predictors.
#' @param subset If specified, will only analyze the specified subset of the data.
#' @param parallel Whether to parallelize the permutation testing using plyr's `parallel' option. Needs some additional set-up; see the plyr documentation.
#' @return A dataframe of p-values.
#' @import plyr lmPerm
#' @export
permu.test <- function (formula,data,subset=NULL,parallel=FALSE) {
	errfun <- function (e) {
		warning(e)
		return(data.frame(timepoint=NA,factor=NA,p=NA,w2=NA))
	}
	if (formula[[1]] != '~') stop("Invalid formula (first operator is not '~')")
	indep <- formula[[3]]
	if (indep[[1]] != '|') stop("Invalid formula (the rightmost term should start with '|', followed by your timepoint variable)")
	timepoint.var <- as.character(indep[[3]])
	formula[[3]] <- indep[[2]]
	if (!is.null(subset)) data <- data[subset,]
	timepoints <- data[,timepoint.var]
	ret <- adply(sort(unique(timepoints)),1,function (t) {
		library(lmPerm)
		cat(paste('Testing timepoint:',t))
		test <- tryCatch(aovp(formula,data[timepoints == t,]),error=errfun)
		if (all(class(test) == 'data.frame')) return(test) #permutation test failed with an error
		ldply(summary(test),function (res) {
			if (ncol(res) != 5) return(errfun(paste0('Timepoint ',t,' did not have more observations than predictors; the ANOVA is unidentifiable')))
			factors <- rownames(res)
			pvals <- res[[5]]
			df <- res[[1]]
			SS <- res[[2]]
			nr <- length(df)
			dff <- df[-nr]
			dfe <- df[ nr]
			SSf <- SS[-nr]
			SSe <- SS[ nr]
			MSe <- SSe/dfe
			w2 <- (SSf - dff * MSe) / (sum(SS) + MSe)
			w2 <- pmax(w2,0)
			w2 <- c(w2,NA) #the residuals
			data.frame(timepoint=t,factor=factors,p=pvals,w2=w2,stringsAsFactors=F)
		},.parallel=F,.id='measure')
	},.parallel=parallel,.id=NULL)
	if (ncol(ret) < 4) ret <- cbind(as.character(formula[[2]]),ret,stringsAsFactors=F) #ldply will not have generated the first column if the outcome was univariate
	colnames(ret)[2] <- timepoint.var
	ret$measure <- sub('^ Response ','',ret$measure)
	ret$factor <- sub(' +$','',ret$factor)
	ret <- ret[ret$factor != 'Residuals',]
	class(ret) <- c('permutes','data.frame')
	ret
}

#' Create a heatmap of the results of permutation testing.
#' @param data Output of permu.test. You may want to subset it if you want to simulate zooming in.
#' @param plot Either 'p', to create a heatmap of the p-values, or 'w2' to use the effect size measure (omega squared) instead.
#' @param breaks The granularity of the labels of the x axis. Pass `unique(data[,2])' to get a tick for every timepoint. Combine this trick with subsetting of your dataset, and perhaps averaging over all your dependent variables, to `zoom in' on your data to help you determine precisely where significance begins and stops to occur.
#' @return A ggplot2 object containing a heatmap of p-values.
#' @import ggplot2 viridis
#' @export
plot.permutes <- function (x,y=NULL,plot=c('p','w2'),breaks=NULL) {
	if (!is.null(y)) {
		# User provided vectors instead of a dataframe
		if ('data.frame' %in% class(x)) stop("Unable to understand 'y' argument if 'x' is not a vector")
		p <- ggplot(aes(x=x,y=y))
	} else {
		# User provided a dataframe of permutation results
		if (!'data.frame' %in% class(x)) stop("Error: 'x' is not a dataframe")
		data <- x
		p <- ggplot(data=data,aes_string(x=colnames(data)[2],y=colnames(data)[1]))
	}
	plot <- plot[1]
	p <- p + geom_tile(aes_string(fill=plot)) + scale_fill_viridis(option='plasma',direction=ifelse(plot == 'p',-1,1))
	p <- p + if (is.null(breaks)) scale_x_continuous(expand=c(0,0)) else scale_x_continuous(expand=c(0,0),breaks=breaks)
	p <- p + if (length(unique(data$factor)) == 1) scale_y_discrete(expand=c(0,0)) else facet_wrap(~factor,ncol=1)
	p <- p + xlab(colnames(data)[2]) + ylab(colnames(data)[1])
	return(p)
}
