#
#
# "plot.inputs" is a the list object from the variance decomposition output
require(ggplot2)
prep_plot_objects <- function(plot.inputs, 
                              fontsize = list(title = 18, axis = 14)){
  
  theme_set(theme_classic() + theme(axis.text.x = element_text(size = fontsize$axis, vjust = -1),
                                    axis.text.y = element_blank(), axis.ticks = element_blank(), 
                                    axis.line = element_blank(), axis.title.x = element_blank(), 
                                    axis.title.y = element_blank(), 
                                    panel.grid.minor = element_blank(), 
                                    panel.border = element_blank()))
  
  traits <- names(plot.inputs$variances)
  units <- as.character(trait.lookup(traits)$units)
  trait.labels <- as.character(trait.lookup(traits)$figid)
  plot.data <- data.frame(trait.labels = ifelse(!is.na(trait.labels), 
                                                trait.labels, 
                                                traits), 
                          units = ifelse(!is.na(units), units, ""), 
                          coef.vars = plot.inputs$coef.vars * 100,
                          elasticities = plot.inputs$elasticities, 
                          variances = plot.inputs$variances, 
                          points = seq_along(traits) - 0.5)
  
  plot.data <- plot.data[order(plot.data$variances, decreasing = FALSE), ]
  
  base.plot <- ggplot(plot.data) + coord_flip()
  list(base.plot, plot.data)
  return(list(base.plot = base.plot, plot.data = plot.data))
}