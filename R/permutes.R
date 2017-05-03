#' Permutation tests for time series data.
#' @param formula A formula of the following form: `outcome ~ predictors | timepoint variables'. Multivariate outcomes (e.g. 32 EEG electrodes) are supported; use `cbind(Fp1,Fp2,etc) ~ predictors | timepoint'.
#' @param data The dataset referencing these predictors.
#' @param parallel Whether to parallelize the permutation testing using plyr's `parallel' option. Needs some additional set-up; see the plyr documentation.
#' @return A dataframe of p-values.
#' @import plyr lmPerm
#' @export
permu.test <- function (formula,data,parallel=FALSE) {
	errfun <- function (e) {
		warning(e)
		return(data.frame(timepoint=NA,factor=NA,p=NA))
	}
	if (formula[[1]] != '~') stop("Invalid formula (first operator is not '~')")
	indep <- formula[[3]]
	if (indep[[1]] != '|') stop("Invalid formula (the rightmost term should start with '|', followed by your timepoint variable)")
	timepoint.var <- as.character(indep[[3]])
	formula[[3]] <- indep[[2]]
	timepoints <- data[,timepoint.var]
	ret <- adply(unique(timepoints),1,function (t) {
		cat(paste('Testing timepoint:',t))
		test <- tryCatch(aovp(formula,data[timepoints == t,]),error=errfun)
		if (all(class(test) == 'data.frame')) return(test) #permutation test failed with an error
		ldply(summary(test),function (res) {
			if (ncol(res) != 5) return(errfun(paste0('Timepoint ',t,' did not have more observations than datapoints')))
			factors <- rownames(res)
			pvals <- res[[5]]
			data.frame(timepoint=t,factor=factors,p=pvals,stringsAsFactors=F)
		},.parallel=F,.id='measure')
	},.parallel=parallel,.id=NULL)
	if (ncol(ret) < 4) ret <- cbind(as.character(formula[[2]]),ret,stringsAsFactors=F) #ldply will not have generated the first column if the outcome was univariate
	colnames(ret)[1:2] <- c('measure',timepoint.var)
	ret$measure <- sub('^ Response ','',ret$measure)
	ret$factor <- sub(' +$','',ret$factor)
	ret[ret$factor != 'Residuals',]
}

#' Create a heatmap of the results of permutation testing.
#' @param data Output of permu.test. You may want to subset it if you want to simulate zooming in.
#' @param breaks The granularity of the labels of the x axis. Pass `unique(data[,2])' to get a tick for every timepoint. Combine this trick with subsetting of your dataset, and perhaps averaging over all your dependent variables, to `zoom in' on your data to help you determine precisely where significance begins and stops to occur.
#' @return A ggplot2 object containing a heatmap of p-values.
#' @import ggplot2 viridis
#' @export
permu.plot <- function (data,breaks=NULL) {
	p <- ggplot(data=data,aes(x=data[,2],y=data[,1]))
	p <- p + geom_tile(aes(fill=p)) + scale_fill_viridis(option='plasma',direction=-1)
	p <- p + if (is.null(breaks)) scale_x_continuous(expand=c(0,0)) else scale_x_continuous(expand=c(0,0),breaks=breaks)
	p <- p + scale_y_continuous(expand=c(0,0)) + facet_wrap(~factor,ncol=1)
	p <- p + xlab(colnames(data)[2]) + ylab(colnames(data)[1])
	return(p)
}
