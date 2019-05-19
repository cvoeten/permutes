#' Permutation tests for time series data.
#' @param formula A formula of the following form: `outcome ~ predictors | timepoint variables'. Multivariate outcomes (e.g. 32 EEG electrodes) are supported; use `cbind(Fp1,Fp2,etc) ~ predictors | timepoint'.
#' @param data The dataset referencing these predictors.
#' @param subset If specified, will only analyze the specified subset of the data.
#' @param parallel Whether to parallelize the permutation testing using plyr's `parallel' option. Needs some additional set-up; see the plyr documentation.
#' @param progress A plyr `.progress' bar name, see the plyr documentation. Ignored if parallel=TRUE.
#' @param ... Other arguments to be passed to `aovp'.
#' @return A dataframe of p-values.
#' @import plyr lmPerm
#' @export
permu.test <- function (formula,data,subset=NULL,parallel=FALSE,progress='text',...) {
	errfun <- function (e) {
		warning(e)
		data.frame(timepoint=NA,factor=NA,p=NA,w2=NA)
	}
	if (formula[[1]] != '~') stop("Invalid formula (first operator is not '~')")
	indep <- formula[[3]]
	if (indep[[1]] != '|') stop("Invalid formula (the rightmost term should start with '|', followed by your timepoint variable)")
	timepoint.var <- as.character(indep[[3]])
	formula[[3]] <- indep[[2]]
	if (!is.null(subset)) data <- data[subset,]
	timepoints <- data[,timepoint.var]
	dots <- list(...)
	ret <- plyr::adply(sort(unique(timepoints)),1,function (t) {
		fun <- lmPerm::aovp #needs to be referred to by namespace in case we are running on a cluster node that has not loaded lmPerm
		test <- tryCatch(do.call(fun,c(list(formula,data[timepoints == t,],settings=F),dots)),error=errfun)
		if (all(class(test) == 'data.frame')) return(test) #permutation test failed with an error
		summary <- summary(test)
		if (length(summary) == 1) summary <- summary[[1]] #univariate outcome
		ret <- plyr::ldply(summary(test),function (res) {
			if (ncol(res) != 5) return(errfun(paste0('Timepoint ',t,' did not have more observations than predictors; the ANOVA is unidentifiable')))
			nr <- nrow(res)
			factors <- rownames(res)[-nr] #the last row is the residuals
			pvals <- res[[5]][-nr]
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
			data.frame(timepoint=t,factor=factors,F=Fval,p=pvals,w2=w2,stringsAsFactors=F)
		},.id='measure')
	},.id=NULL,.parallel=parallel,.progress=ifelse(parallel,'none',progress))
	if (is.null(ret$measure)) ret <- cbind(as.character(formula[[2]]),ret) else ret$measure <- sub('^ Response ','',ret$measure)
	colnames(ret)[2] <- timepoint.var
	ret$factor <- sub(' +$','',ret$factor)
	class(ret) <- c('permutes','data.frame')
	ret
}

#' Create a heatmap of the results of permutation testing.
#' @param x Output of permu.test. You may want to subset it if you want to simulate zooming in.
#' @param type The quantity to plot; one of 'F' (default), 'p', or 'w2' (omega squared).
#' @param breaks The granularity of the labels of the x axis. Pass `unique(data[,2])' to get a tick for every timepoint. Combine this trick with subsetting of your dataset, and perhaps averaging over all your dependent variables, to `zoom in' on your data to help you determine precisely where significance begins and stops to occur.
#' @param ... Other arguments, which will be ignored (the ellipsis is provided for consistency with the generic plot() method).
#' @return A ggplot2 object containing a heatmap of p-values.
#' @import ggplot2 viridis
#' @export
plot.permutes <- function (x,type=c('F','p','w2'),breaks=NULL,...) {
	if (!'data.frame' %in% class(x)) stop("Error: 'x' is not a dataframe")
	data <- x
	p <- ggplot(data=data,aes_string(x=colnames(data)[2],y=colnames(data)[1]))
	plot <- type[1]
	p <- p + geom_tile(aes_string(fill=plot)) + scale_fill_viridis(option='plasma',direction=ifelse(plot == 'p',-1,1))
	p <- p + theme(panel.background=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank())
	p <- p + if (is.null(breaks)) scale_x_continuous(expand=c(0,0)) else scale_x_continuous(expand=c(0,0),breaks=breaks)
	p <- p + if (length(unique(data$factor)) == 1) scale_y_discrete(expand=c(0,0)) else facet_wrap(~factor,ncol=1)
	p <- p + xlab(colnames(data)[2]) + ylab(colnames(data)[1])
	return(p)
}
