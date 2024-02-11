#' Plotting STIC Sensor Data
#'
#' @description
#' `plot_STIC` is a function to make a ggplot plot with STIC data.
#'
#'
#' @import tidyr
#' @import lubridate
#' @import stringr
#' @import ggplot2
#' @import plotly
#' @import RColorBrewer
#' @importFrom utils read.csv
#'
#' @param data A dataframe
#' @param site Location
#' @param date_breaks
#' @param date_labels
#'
#' @return the formatted data
#'
#' @rdname plot_STIC
#' @export
#'

#Update files
plot_STIC <- function(data, site = NULL, date_breaks = "2 weeks", date_labels = "%b-%d", xlab = NULL, plotly=FALSE){

linecolors <- if(is.null(site)){rev(RColorBrewer::brewer.pal(11,"RdYlBu"))}
                else{"#283AB8"}


#Display all sites if no site is specified.
data <- if(is.null(site)){data}
        else{subset(data, Site == site)}


p <- data %>%
    #subset(Site == site) %>%
    ggplot(aes(x = DateTime, y = INTENSITY_LUX, color = Site, group = Site)) +
    geom_line() +
    scale_x_datetime(breaks = date_breaks,
                     date_labels = date_labels,
                     expand = c(0,0)) +
    scale_color_manual(values = linecolors) +
    theme_bw()+
    theme(#text = element_text(family = "Trebuchet MS"),
          legend.position = "top",
          legend.key=element_blank(),
          legend.title = element_text(size = 10, face = "bold"),
          legend.text = element_text(size = 10),
          legend.box.background = element_blank(),
          legend.key.size = unit(1.5, 'lines'),
          panel.border = element_rect(color="black",linewidth =1, fill = NA),
          panel.grid = element_blank(),
          plot.background = element_blank(),
          plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
          plot.subtitle = element_text(hjust = 0.5, size = 12, lineheight = 1.5),
          axis.text.x = element_text(size = 10, color = "black"),
          axis.text.y = element_text(size = 10, color = "black"),
          axis.title = element_text(size = 12, face = "bold", color = "black"),

          # formatting for facets
          panel.background = element_blank(),
          strip.background = element_blank(), #facet formatting
          panel.spacing.x = unit(1.5, "lines"), #facet spacing for x axis
          panel.spacing.y = unit(1.5, "lines"), #facet spacing for x axis
          strip.text.x = element_text(size=10, face="bold"), #facet labels
          strip.text.y = element_text(size=10, face="bold") #facet labels
    ) +
    labs(x = xlab, y = "Intensity")

#Choose to display plot through plotly
if(plotly == FALSE){p}
else{ggplotly(p)}

}

plot_STIC(d, site = "Site5.1")



