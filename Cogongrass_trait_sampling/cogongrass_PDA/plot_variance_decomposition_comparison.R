
#plot.inputs_1 <- prior_know_NPP$sudoCogongrass2$variance.decomposition.output
#plot.inputs_2 <- ambient_NPP$cogongrass2$variance.decomposition.output
#description = c("prior", "posterior")
#fontsize = list(title = 18, axis = 14)

#library(PEcAn.uncertainty)
#library(ggplot2)
library(tidyverse)

plot_variance_decomposition_comparison <- function(plot.inputs_1, plot.inputs_2, plot.inputs_3 = NA, description = c("prior", "posterior"),
                                        fontsize = list(title = 18, axis = 14), output_lable = NA, color_scale = NA, legend_pos = "right") {
  
  if(any(is.na(color_scale))){
    if(length(description) == 2){
      color_scale <- c("red","blue" )
    }else if(length(description) == 3){
      color_scale <- c("red","blue", "green" )
    }
    
  }
  
  if(length(description) == 2){
    ## Format inputs and add descriptions  # Could make arbitrary number of runs
    plot_data_1 <- format.plot.input(plot.inputs_1)
    plot_data_2 <- format.plot.input(plot.inputs_2)
    
    plot_data_1$treatment <- description[1]
    plot_data_2$treatment <- description[2]
    
    plot_data_1$sqrt_variances <- sqrt(plot_data_1$variances)
    plot_data_2$sqrt_variances <- sqrt(plot_data_2$variances)
    
    
    ## Check if parameters are comparable
    if(any(sort(plot_data_1$trait.labels) != sort(plot_data_2$trait.labels))){
      print("comparing two different sets of parameters")
      break ## could add a condition to subset to matching parameters 
    }
    
    
    points_1 <- seq_along(plot_data_1$trait.labels) - 0.5
    points <- sort(rep(points_1, 2))
    ## merge into one dataframe
    #wide <- inner_join(plot_data_1, plot_data_2, by = "trait.labels")
    long <- full_join(plot_data_1, plot_data_2)
    
    
    
    
    ## Order the traits
    long <- long[order(long$trait.labels),]
    #long$trait.labels[long$treatment == description[1]] <- " "
    long$trait.labels[long$trait.labels == "Density Dependent Mortality Coefficient"] <- "Mortality Coefficient"
    
    #long$points <-seq_along(long$trait.labels) - 0.5 # add coordinate
    long$points <- points
    
    ## setup theme and base plot
    theme_set(theme_classic() + theme(axis.text.x = element_text(size = fontsize$axis, vjust = -1),
                                      axis.text.y = element_blank(), axis.ticks = element_blank(), 
                                      axis.line = element_blank(), axis.title.x = element_blank(), 
                                      axis.title.y = element_blank(), 
                                      panel.grid.minor = element_blank(), 
                                      panel.border = element_blank()))
    
    base.plot <- ggplot(long) + coord_flip()
    
    trait.plot <- base.plot + ggtitle(paste("Parameters", output_lable )) + 
      geom_text(aes(y = 2, x = points, label = trait.labels, hjust = 1), size = fontsize$axis/3) + 
      scale_y_continuous(breaks = c(0, 0), limits = c(0, 1)) + 
      scale_y_continuous(trans = "log") +
      theme(axis.text.x = element_blank()) + 
      scale_color_manual(values = color_scale)
    
    cv.plot <- base.plot + ggtitle(paste(" CV (%) ", output_lable )) + 
      geom_pointrange(aes(x = points, y = coef.vars, ymin = 0, ymax = coef.vars, color = treatment), size = 1.25) +
      scale_y_continuous(trans = "log") +
      theme(plot.title = element_text(size = fontsize$title)) + 
      scale_color_manual(values = color_scale)
    
    
    el.plot <- base.plot + ggtitle(paste("Elasticity", output_lable )) + 
      theme(plot.title = element_text(size = fontsize$title)) + 
      geom_pointrange(aes(x = points, y = elasticities, ymin = 0, ymax = elasticities, color = treatment), size = 1.25) + 
      scale_color_manual(values = color_scale)
    
    pv.plot <- base.plot + ggtitle(paste("Variance", output_lable )) + 
      theme(plot.title = element_text(size = fontsize$title)) + 
      geom_pointrange(aes(x = points, y = variances, ymin = 0, ymax = variances, color = treatment), size = 1.25) + 
      scale_color_manual(values = color_scale)
    
    pv.2.plot <- base.plot + ggtitle(paste("Partial Variances", output_lable )) + 
      theme(plot.title = element_text(size = fontsize$title)) + 
      geom_pointrange(aes(x = points, y = partial.variances, ymin = 0, ymax = variances, color = treatment), size = 1.25) + 
      scale_color_manual(values = color_scale)
    
    
    
  }else if(length(description) == 3){
    
    ## Format inputs and add descriptions  # Could make arbitrary number of runs
    plot_data_1 <- format.plot.input(plot.inputs_1)
    plot_data_2 <- format.plot.input(plot.inputs_2)
    plot_data_3 <- format.plot.input(plot.inputs_3)
    
    plot_data_1$treatment <- description[1]
    plot_data_2$treatment <- description[2]
    plot_data_3$treatment <- description[3]
    
    plot_data_1$sqrt_variances <- sqrt(plot_data_1$variances)
    plot_data_2$sqrt_variances <- sqrt(plot_data_2$variances)
    plot_data_3$sqrt_variances  <- sqrt(plot_data_3$variances)
    
    
    ## Check if parameters are comparable
    if(any(sort(plot_data_1$trait.labels) != sort(plot_data_2$trait.labels))){ ## TODO add comparison to 3rd plot
      print("comparing two different sets of parameters")
      break ## could add a condition to subset to matching parameters 
    }
    
    
    points_1 <- seq_along(plot_data_1$trait.labels) - 0.30
    points_2 <- seq_along(plot_data_1$trait.labels) - 0.40
    points_3 <- seq_along(plot_data_1$trait.labels) - 0.50
    
    
    points_treat <- sort(c(points_1, points_2, points_3))
    
    points <- sort(rep(points_1, 3))
    
    # plot_data_1$points_treat <- points_1 ## Make a slightly staggered axis for lollipops ## This change seems ed to make the lollipops reorder differently from the lables
    # plot_data_2$points_treat <- seq_along(plot_data_1$trait.labels) - 0.40
    # plot_data_3$points_treat <- seq_along(plot_data_1$trait.labels) - 0.50
    
    ## merge into one dataframe
    #wide <- inner_join(plot_data_1, plot_data_2, by = "trait.labels")
    long_tmp <- full_join(plot_data_1, plot_data_2)
    long <- full_join(long_tmp, plot_data_3)
    
    
    
    ## Order the traits
    long <- long[order(long$trait.labels),]
    #long$trait.labels[long$treatment == description[1]] <- " "
    long$trait.labels[long$trait.labels == "Density Dependent Mortality Coefficient"] <- "Mortality Coefficient"
    
    #long$points <-seq_along(long$trait.labels) - 0.5 # add coordinate
    long$points <- points
    long$points_treat <- points_treat
    
    ## setup theme and base plot
    theme_set(theme_classic() + theme(axis.text.x = element_text(size = fontsize$axis, vjust = -1),
                                      axis.text.y = element_blank(), axis.ticks = element_blank(), 
                                      axis.line = element_blank(), axis.title.x = element_blank(), 
                                      axis.title.y = element_blank(), 
                                      panel.grid.minor = element_blank(), 
                                      panel.border = element_blank(), 
                                      legend.position=legend_pos))
    
    base.plot <- ggplot(long) + coord_flip()
    
    trait.plot <- base.plot + ggtitle(paste("Parameters", output_lable )) + 
      geom_text(aes(y = 2, x = points, label = trait.labels, hjust = 1), size = fontsize$axis/3) + 
      scale_y_continuous(breaks = c(0, 0), limits = c(0, 1)) + 
      scale_y_continuous(trans = "log") +
      theme(axis.text.x = element_blank())
    
    
    cv.plot <- base.plot + ggtitle(paste(" CV (%) ", output_lable )) + 
      geom_pointrange(aes(x = points_treat, y = coef.vars, ymin = 0, ymax = coef.vars, color = treatment), size = 1.25) +
      scale_y_continuous(trans = "log") +
      theme(plot.title = element_text(size = fontsize$title)) + 
      scale_color_manual(values = color_scale)
    
    
    el.plot <- base.plot + ggtitle(paste("Elasticity", output_lable )) + 
      theme(plot.title = element_text(size = fontsize$title)) + 
      geom_pointrange(aes(x = points_treat, y = elasticities, ymin = 0, ymax = elasticities, color = treatment), size = 1.25) + 
      scale_color_manual(values = color_scale)
    
    pv.plot <- base.plot + ggtitle(paste("Variance", output_lable )) + 
      theme(plot.title = element_text(size = fontsize$title)) + 
      geom_pointrange(aes(x = points_treat, y = variances, ymin = 0, ymax = variances, color = treatment), size = 1.25) + 
      scale_color_manual(values = color_scale)
    
    pv.2.plot <- base.plot + ggtitle(paste("Partial Variances", output_lable )) + 
      theme(plot.title = element_text(size = fontsize$title)) + 
      geom_pointrange(aes(x = points_treat, y = partial.variances, ymin = 0, ymax = variances, color = treatment), size = 1.25) + 
      scale_color_manual(values = color_scale)
    
    
    
    
    
  }else{
    print(paste("ERROR: description length is not 2  and is not 3. This function is only for 2 or 3 plot comparisons"))
    break
  }
  return(list(trait.plot = trait.plot, cv.plot = cv.plot, el.plot = el.plot, pv.plot = pv.plot, pv.2.plot = pv.2.plot, input_data = long))
} # plot_variance_decomposition


format.plot.input <- function(plot.inputs, trait.order = c()) {
  
  
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
                          partial.variances = plot.inputs$partial.variances,
                          points = seq_along(traits) - 0.5)
  
  plot.data <- plot.data[order(plot.data$variances, decreasing = FALSE), ]
  
  
  return(plot.data)
}
