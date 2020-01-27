substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}


ComparetoPaper<-function(AGECAT, MEASURE, PAPERMEAN, PAPERSD, PAPERN){
  NUM<-ifelse(AGECAT == "(50,59]", 1, 2)
  t.value<-(mean(df[df$agecat == AGECAT, MEASURE], na.rm = TRUE) - PAPERMEAN)/
    (sd(df[df$agecat == AGECAT, MEASURE], na.rm = TRUE)^2/(table(df[df$agecat == AGECAT,"agecat"])[NUM]) + PAPERSD^2/PAPERN)
  
  p.value<-2*pt(-abs(t.value), df=(table(df[df$agecat == AGECAT,"agecat"])[1]) + PAPERN-1)
  return(p.value)}


# Function to calculate important values:
ggplot2_boxplot <- function(MEAN, SD){
  
  # quartiles <- as.numeric(quantile(x, 
  #                                  probs = c(0.25, 0.5, 0.75)))
  
  quartiles<-c(MEAN-0.675*SD, MEAN, MEAN+0.675*SD )
  
  names(quartiles) <- c("25th percentile", 
                        "50th percentile\n(mean)",
                        "75th percentile")
  
  IQR <- diff(quartiles[c(1,3)])
  
  upper_whisker <- (MEAN + 1.645 * MEAN)
  lower_whisker <- (MEAN - 1.645 * MEAN)
  
  # upper_dots <- x[x > (quartiles[3] + 1.5*IQR)]
  # lower_dots <- x[x < (quartiles[1] - 1.5*IQR)]
  
  return(list("quartiles" = quartiles,
              "25th percentile" = as.numeric(quartiles[1]),
              "50th percentile\n(mean)" = as.numeric(quartiles[2]),
              "75th percentile" = as.numeric(quartiles[3]),
              "IQR" = IQR,
              "upper_whisker" = upper_whisker,
              "lower_whisker" = lower_whisker))
}

ggplot_box_legend <- function(MEAN, SD, family = "serif"){
  
  # Create data to use in the boxplot legend:
  set.seed(100)
  
  sample_df <- data.frame(parameter = "test",
                          values = sample(500))
  
  # Extend the top whisker a bit:
  sample_df$values[1:100] <- 701:800
  # Make sure there's only 1 lower outlier:
  sample_df$values[1] <- -350
  

  
  # Get those values:
  ggplot_output <- ggplot2_boxplot(MEAN, SD)
  
  # Lots of text in the legend, make it smaller and consistent font:
  update_geom_defaults("text", 
                       list(size = 3, 
                            hjust = 0,
                            family = family))
  # Labels don't inherit text:
  update_geom_defaults("label", 
                       list(size = 3, 
                            hjust = 0,
                            family = family))
  
  # Create the legend:
  # The main elements of the plot (the boxplot, error bars, and count)
  # are the easy part.
  # The text describing each of those takes a lot of fiddling to
  # get the location and style just right:
  explain_plot <- ggplot() +
    stat_boxplot(data = sample_df,
                 aes(x = parameter, y=values),
                 geom ='errorbar', width = 0.3) +
    geom_boxplot(data = sample_df,
                 aes(x = parameter, y=values), 
                 width = 0.3, fill = "lightgrey") +
    geom_text(aes(x = 1, y = 950, label = "XX"), hjust = 0.5) +
    geom_text(aes(x = 1.17, y = 950,
                  label = "Number of values"),
              fontface = "bold", vjust = 0.4) +
    theme_minimal(base_size = 5, base_family = family) +
    geom_segment(aes(x = 2.3, xend = 2.3, 
                     y = ggplot_output[["25th percentile"]], 
                     yend = ggplot_output[["75th percentile"]])) +
    geom_segment(aes(x = 1.2, xend = 2.3, 
                     y = ggplot_output[["25th percentile"]], 
                     yend = ggplot_output[["25th percentile"]])) +
    geom_segment(aes(x = 1.2, xend = 2.3, 
                     y = ggplot_output[["75th percentile"]], 
                     yend = ggplot_output[["75th percentile"]])) +
    geom_text(aes(x = 2.4, y = ggplot_output[["50th percentile\n(mean)"]]), 
              label = "Interquartile\nrange", fontface = "bold",
              vjust = 0.4) +
    geom_text(aes(x = c(1.17,1.17), 
                  y = c(ggplot_output[["upper_whisker"]],
                        ggplot_output[["lower_whisker"]]), 
                  label = c("95th percentile",
                            "5th percentile")),
              fontface = "bold", vjust = 0.9) +

    geom_label(aes(x = 1.17, y = ggplot_output[["quartiles"]], 
                   label = names(ggplot_output[["quartiles"]])),
               vjust = c(0.4,0.85,0.4), 
               fill = "white", label.size = 0) +
    ylab("") + xlab("") +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          aspect.ratio = 4/3,
          plot.title = element_text(hjust = 0.5, size = 10)) +
    coord_cartesian(xlim = c(1.4,2.6), ylim = c(-100, 1000)) +
    labs(title = "EXPLANATION")
  
  return(explain_plot) 
  
}


PreptoPlot<-function(PLWH_M1, PLWH_SD1, PLWH_M2, PLWH_SD2, CON_M1, CON_SD1, CON_M2, CON_SD2){
hold<-ggplot2_boxplot(PLWH_M1, PLWH_SD1)
hold1<-data.frame(t(matrix(unlist(hold))))
hold1<-hold1[,-c(1:3)]

hold<-ggplot2_boxplot(PLWH_M2, PLWH_SD2)
hold2<-data.frame(t(matrix(unlist(hold))))
hold2<-hold2[,-c(1:3)]

hold<-ggplot2_boxplot(CON_M1, CON_SD1)
hold3<-data.frame(t(matrix(unlist(hold))))
hold3<-hold3[,-c(1:3)]

hold<-ggplot2_boxplot(CON_M2, CON_SD2)
hold4<-data.frame(t(matrix(unlist(hold))))
hold4<-hold4[,-c(1:3)]

hold<-data.frame(rbind(hold1, hold2, hold3, hold4))
colnames(hold)<-c("TWENTYFIVE", "FIFTY", "SEVENTYFIVE", "IQR", "UPPER", "LOWER")
hold$Age<-c("(50, 59)", "(60, 71)", "(50, 59)", "(60, 71)")
hold$Group<-c("PLWH", "PLWH", "NHANES", "NHANES")

return(hold)}


