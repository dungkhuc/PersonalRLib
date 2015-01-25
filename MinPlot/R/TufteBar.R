tufteBar = function (dat, x, y=NULL, classify = NULL,
                     width = 0.6, color = "Dark2",
                     x.text.rotate = 0, y.text.rotate=0,
                     font="Arial", font_size= 20,
                     x.text.color = "black", y.text.color = "black") {
  # Count values in a column
  if (is.null(y) && is.null(classify)) barplot = ggplot(dat, aes_string(x = x)) + geom_bar(stat="bin", width = width)
  
  # Dodge identity bar plot
  else if (!is.null(y) && !is.null(classify)) 
    barplot = ggplot(dat, aes_string(x = x, y = y, fill = classify)) +
      geom_bar(stat="identity", position = position_dodge(), width = width) +
      geom_bar(stat="identity", position = position_dodge(), width = width, color = "white", show_guide = FALSE)
  
  # Dodge bin bar plot
  else if (is.null(y) && !is.null(classify)) 
    barplot = ggplot(dat, aes_string(x = x, fill = classify)) + 
      geom_bar(stat="bin", position = position_dodge(), width = width) +
      geom_bar(stat="bin", position = position_dodge(), width = width, color = "white", show_guide = FALSE)
  # Sum of values in column y, with label in column x
  
  # Simple identity bar plot
  else 
    barplot = ggplot(dat, aes_string(x = x, y = y)) + geom_bar(stat="identity", width=0.6, fill=color)
  if (exists("barplot")) {
    barplot =  barplot + scale_fill_brewer(palette = color)
    tufteBarTheme(barplot, x.text.rotate, y.text.rotate, font, font_size, x.text.color, y.text.color)
  }
}
tufteBarTheme = function (tuftePlot, x.text.rotate, y.text.rotate, font, font_size, x.text.color, y.text.color) {

  tuftePlot = tuftePlot + theme_tufte(base_family=font, base_size=font_size, ticks=F)
  
  # Get ticks on y axis
  y.axis.ticks = ggplot_build(tuftePlot)$panel$ranges[[1]]$y.major_source
  tuftePlot = tuftePlot + theme(axis.line = element_blank(),
                                axis.text.x = element_text(angle = x.text.rotate, hjust = 1, color = x.text.color), 
                                axis.text.y = element_text(angle = y.text.rotate, color = y.text.color),
                                axis.title = element_blank()) +
                          geom_hline(yintercept=y.axis.ticks, col="white", lwd=1.2)
  tuftePlot
}