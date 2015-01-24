require(ggplot2)
require(extrafont)
require(ggthemes)
tufteBar = function (dat, x, y=NULL, color = "black",
                     x.text.rotate = 45, y.text.rotate=0,
                     font="Arial", font_size= 20,
                     x.text.color = "black", y.text.color = "black") {
  # Count values in a column
  if (is.null(y)) gplot = ggplot(dat, aes(x = order)) + geom_bar(stat="bin", width=0.6, fill=color)
  
  # Sum of values in column y, with label in column x
  else gplot = ggplot(dat, aes_string(x = x, y = y)) + geom_bar(stat="identity", width=0.6, fill=color)
  if (exists("gplot")) tufteTheme(gplot, x.text.rotate, y.text.rotate, font, font_size, x.text.color, y.text.color)
}
tufteTheme = function (tuftePlot, x.text.rotate, y.text.rotate, font, font_size, x.text.color, y.text.color) {

  tuftePlot = tuftePlot + theme_tufte(base_family=font, base_size=font_size, ticks=F)
  
  # Get ticks on y axis
  y.axis.ticks = ggplot_build(tuftePlot)$panel$ranges[[1]]$y.major_source
  tuftePlot = tuftePlot + theme(axis.line = element_blank(),
                                axis.text.x = element_text(angle = x.text.rotate, hjust = 1, color = x.text.color), 
                                axis.text.y = element_text(angle=y.text.rotate, color = y.text.color),
                                axis.title = element_blank()) +
                          geom_hline(yintercept=y.axis.ticks, col="white", lwd=1.2)
  tuftePlot
}


