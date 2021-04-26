#' Create a heatmap of the results of cluster-based permutation testing.
#' @param x Output of \code{permu.test} or \code{clusterperm.lmer} or associated functions. You may want to subset it if you want to simulate zooming in.
#' @param type The quantity to plot. For ANOVA, the options are \code{'LRT'} (default; this is the permutation statistic, which is a likelihood-ratio-test statistic), \code{'F'}, \code{'p'} (this is the permutation p-value of the permutation statistic), or \code{'w2'} (omega squared; not available for mixed-effects models). For regression, the options are \code{'t'}, \code{'beta'}, or \code{'p'}.
#' @param breaks The granularity of the labels of the x axis. Pass `unique(x[,2])' to get a tick for every timepoint. Combine this trick with subsetting of your dataset, and perhaps averaging over all your dependent variables, to `zoom in' on your data to help you determine precisely where significance begins and stops to occur.
#' @param sig An optional column name. If specified, only cases with a value of <.05 in this column will be plotted. NULL by default, but suggested to be set to \code{'p.cluster_mass'} for easy visualization of the cluster-mass-test results, or \code{'p'} to directly use the permutation p-values.
#' @param ... Other arguments, which will be ignored (the ellipsis is provided for consistency with the generic plot() method).
#' @return A ggplot2 object.
#' @export
plot.permutes <- function (x,type=c('LRT','F','t','beta','w2','cluster_mass'),breaks=NULL,sig=NULL,...) {
	if (!requireNamespace('ggplot2')) stop('Please install package ggplot2')
	if (!requireNamespace('viridis')) stop('Please install package viridis')
	if (!'data.frame' %in% class(x))  stop("Error: 'x' is not a data frame")

	plot <- intersect(type,colnames(x))[1]
	if (is.null(plot)) {
		stop("Requested column in 'type' could not be found in the data")
	}
	x <- x[!is.na(x$Factor),]

	if (!is.null(sig)) {
		x[,plot] <- ifelse(x[[sig]] < .05,x[,plot],NA)
	}
	x$Factor <- factor(x$Factor,levels=unique(x$Factor)) #make sure ggplot uses the same order as the permutation model

	scale <- if (is.numeric(x[,2])) ggplot2::scale_x_continuous else ggplot2::scale_x_discrete
	p <- ggplot2::ggplot(data=x,ggplot2::aes_string(x=colnames(x)[1],y=colnames(x)[2]))
	p <- p + ggplot2::geom_tile(ggplot2::aes_string(fill=plot,color=plot)) + viridis::scale_fill_viridis(option='plasma',direction=if (plot %in% c('p','p.cluster_mass')) -1 else 1) + viridis::scale_color_viridis(option='plasma',direction=if (plot %in% c('p','p.cluster_mass')) -1 else 1)
	p <- p + ggplot2::theme(panel.background=ggplot2::element_blank(),panel.grid.major=ggplot2::element_blank(),panel.grid.minor=ggplot2::element_blank())
	p <- p + if (is.null(breaks)) scale(expand=c(0,0)) else scale(expand=c(0,0),breaks=breaks)
	p <- p + if (length(unique(x$Factor)) == 1) ggplot2::scale_y_discrete(expand=c(0,0)) else ggplot2::facet_wrap(~Factor,ncol=1)
	p <- p + ggplot2::xlab(colnames(x)[1]) + ggplot2::ylab(colnames(x)[2])
	return(p)
}
