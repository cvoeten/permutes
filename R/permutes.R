#' Permutation tests for time series data.
#' @param formula A formula of the following form: `outcome ~ predictors | timepoint variables'. Multivariate outcomes (e.g. 32 EEG electrodes) are supported; use `cbind(Fp1,Fp2,etc) ~ predictors | timepoint'.
#' @param data The dataset referencing these predictors.
#' @param parallel Whether to parallelize the permutation testing using plyr's `parallel' option. Needs some additional set-up; see the plyr documentation.
#' @return A dataframe of p-values.
#' @import plyr lmPerm
#' @export
permu.test <- function (formula,data,parallel=FALSE) {
	if (formula[[1]] != '~') stop("Invalid formula (first operator is not '~')")
	indep <- formula[[3]]
	if (indep[[1]] != '|') stop("Invalid formula (the rightmost term should start with '|', followed by your timepoint variable)")
	timepoint.var <- as.character(indep[[3]])
	formula[[3]] <- indep[[2]]
	timepoints <- data[,colnames(data) == timepoint.var]
	ret <- adply(unique(timepoints),1,function (t) {
		cat(paste('Testing timepoint:',t))
		test <- aovp(formula,data[timepoints == t,])
		ldply(summary(test),function (res) {
			factors <- rownames(res)
			pvals <- res[[5]]
			data.frame(timepoint=t,factor=factors,p=pvals,stringsAsFactors=F)
		},.parallel=F,.id='measure')
	},.parallel=parallel,.id=NULL)
	colnames(ret)[2] <- timepoint.var
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
	p <- p + facet_wrap(~factor,ncol=1)
	p <- p + xlab(colnames(data)[2]) + ylab(colnames(data)[1])
	return(p)
}
