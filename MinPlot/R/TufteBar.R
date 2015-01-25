tufteBar = function (dat, x, y=NULL, classify = NULL,
                     summary = "culmulative", y.breaks = 5,
                     width = 0.6, color = "Dark2",
                     x.text.rotate = 0, y.text.rotate=0,
                     font="Arial", font_size= 20,
                     x.text.color = "black", y.text.color = "black") {
  # Count values in a column
  if (is.null(y) && is.null(classify)) barplot = ggplot(dat, aes_string(x = x)) + geom_bar(stat="bin", width = width)
  
  # Dodge identity bar plot
  else if (!is.null(y) && !is.null(classify)) {
    barplot = ggplot(dat, aes_string(x = x, y = y, fill = classify, width = width))
    if (summary == "culmulative")
      barplot = barplot +
        geom_bar(stat="identity", position = position_dodge(width = 0.7))
    else if (summary == "mean") {
      barplot = barplot + 
        geom_bar(stat="summary", fun.y = mean, position = position_dodge()) +
        geom_bar(stat="summary", fun.y = mean, position = position_dodge(), color = "white", show_guide=FALSE)
    }
  }
  
  # Dodge bin bar plot
  else if (is.null(y) && !is.null(classify)) 
    barplot = ggplot(dat, aes_string(x = x, fill = classify)) + 
      geom_bar(stat="bin", position = position_dodge(width = 0.7), width = width)
  # Sum of values in column y, with label in column x
  
  # Simple identity bar plot
  else {
    barplot = ggplot(dat, aes_string(x = x, y = y))
    
    # Check if user wants to display culmulative value or mean value
    if (summary == "culmulative") {
      barplot = barplot + geom_bar(stat="identity", width=0.6)
    }
    else if (summary == "mean") {
      barplot = barplot + geom_bar(stat="summary", fun.y = mean, width=0.6)
    }
  }
  
  if (exists("barplot")) {
    barplot =  barplot + scale_fill_brewer(palette = color)
    tufteBarTheme(barplot, x.text.rotate, y.text.rotate, font, font_size, x.text.color, y.text.color, y.breaks)
  }
}
tufteBarTheme = function (tuftePlot, x.text.rotate, y.text.rotate, font, font_size, x.text.color, y.text.color, y.breaks) {
  tuftePlot = tuftePlot + scale_y_continuous(breaks = pretty_breaks(n = y.breaks))
  tuftePlot = tuftePlot + theme_tufte(base_family=font, base_size=font_size, ticks=F)
  
  # Get ticks on y axis
  y.axis.ticks = ggplot_build(tuftePlot)$panel$ranges[[1]]$y.major_source
  tuftePlot = tuftePlot + theme(axis.line = element_blank(),
                                axis.text.x = element_text(color = x.text.color), 
                                axis.text.y = element_text(angle = y.text.rotate, color = y.text.color),
                                axis.title = element_blank()) +
                          geom_hline(yintercept=y.axis.ticks, col="white", lwd=1)
  if (x.text.rotate > 0 && x.text.rotate <= 90)
    tuftePlot = tuftePlot + theme(axis.text.x = element_text(angle = x.text.rotate, hjust = 1))
  tuftePlot
}
