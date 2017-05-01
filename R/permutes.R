#' Permutation tests for time series data.
#' @param formula A formula of the following form: outcome ~ predictors | timepoint variables. Multivariate outcomes (e.g. 32 EEG electrodes) are supported; use `cbind(Fp1,Fp2,etc) ~ predictors | timepoint'.
#' @param data The dataset referencing these predictors.
#' @param parallel Whether to parallelize the permutation testing using plyr's `parallel' option. Needs some additional set-up; see the plyr documentation.
#' @return A dataframe of p-values.
permu.test <- function (formula,data,parallel=FALSE) {
	if (formula[[1]] != '~') stop("Invalid formula (first operator is not '~')")
	dep <- formula[[2]]
	rem <- formula[[3]]
	if (rem[[1]] != '|') stop("Invalid formula (the rightmost term should start with '|', followed by your timepoint variable)")
	timepoint.var <- rem[[3]]
	predictors <- as.character(rem[2])
	formula.real <- as.formula(paste0(dep,'~',predictors))
	timepoints <- data[,timepoint.var]
	ret <- adply(unique(timepoints),1,function (t) {
		cat(paste('Testing timepoint:',t))
		library(lmPerm)
		test <- aovp(formula.real,data[data[,timepoint.var]==t,])
		ldply(summary(test),function (res) {
			factors <- rownames(res)
			pvals <- res[[5]]
			data.frame(timepoint=t,factor=factors,p=pvals,stringsAsFactors=F)
		},.parallel=F,.id='measure')
	},.parallel=parallel,.id=NULL)
	colnames(ret)[1] <- timepoint.var
	ret[!grepl('Residuals',ret$factor),]
}

#' Create a heatmap of the results of permutation testing
#' @param data Output of permu.test. You may want to subset it if you want to simulate zooming in.
#' @return A ggplot2 object containing a heatmap of p-values. You may want to add a `scale_x_continuous(expand=c(0,0),breaks=(...))' to it to increase the granularity of the spacing to your satisfaction.
permu.plot <- function (data) {
	p <- ggplot(data=data,aes(x=data[,1],y='measure'))
	p <- p + geom_tile(aes(fill=p)) + scale_fill_viridis(option='plasma,direction=-1') + scale_x_continuous(expand=c(0,0))
	p <- p + facet_wrap(~factor,ncol=1)
}
