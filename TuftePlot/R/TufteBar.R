library(ggplot2)
library(extrafont)
library(ggthemes)
tufteBar = function (dat, x, y=NULL, font="Arial", font_size= 20, col = "black") {
  # Count values in a column
  if (is.null(y)) gplot = ggplot(dat, aes(x = order)) + geom_bar(stat="bin", width=0.6, fill=col)
  
  # Sum of values in column y, with label in column x
  else gplot = ggplot(dat, aes_string(x = x, y = y)) + geom_bar(stat="identity", width=0.6, fill=col)
  if (exists("gplot")) tufteTheme(gplot, font, font_size)
}
tufteTheme = function (tuftePlot, font, font_size) {
  # Get ticks on y axis
  tuftePlot = tuftePlot + theme_tufte(base_family=font, base_size=font_size, ticks=F)
  #
  y.axis.ticks = ggplot_build(tuftePlot)$panel$ranges[[1]]$y.major_source
  tuftePlot = tuftePlot + theme(axis.line=element_blank(), axis.text.x= element_text(angle = 45, hjust = 1),
                        axis.title=element_blank()) +
    geom_hline(yintercept=y.axis.ticks, col="white", lwd=1.2)
  tuftePlot
}