##### "Fancy" theme for plots, customization after using function
##### (see: https://ggplot2.tidyverse.org/reference/theme.html)
zew_plotstyle <- function() {
  font <- "Latin Modern Roman 10"
  
  ggplot2::theme(
    text=element_text(family = font),
    plot.title = element_text(face = "bold", size = 12, colour = "black"),
    legend.background = element_rect(fill = "white", size = 4, colour = "white"),
    legend.position = "bottom",
    legend.title = element_text(family = font, colour = "black", size = 12),
    legend.text = element_text(family = font, colour = "black", size = 12),
    legend.key = element_rect(fill = "white"),
    axis.title.y = element_text(family = font, colour = "black", size = 12),
    axis.ticks = element_blank(),
    axis.text = element_text(family = font, colour = "black", size= 12),
    axis.line.x = element_line(colour = "black", size = 1),
    panel.grid.major.y = element_line(colour = "grey70", size = 0.2, linetype = 3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.subtitle = ggplot2::element_text(size = 12, colour = "black", margin=ggplot2::margin(3,0,9,0)),
    panel.background = element_rect(fill = "white"),
    axis.title.x = element_text(family = font, colour = "black", size = 12),
    plot.title.position = "plot"
  )
}


##### Theme for flipped graph
zew_plotstyle.flipped <- function() {
  font <- "Calibri"
  
  ggplot2::theme(
    text=element_text(family = font),
    plot.title = element_text(face = "bold", size = 12, colour = "black"),
    legend.background = element_rect(fill = "white", size = 4, colour = "white"),
    legend.position = "bottom",
    legend.title = element_text(family = font, colour = "black", size = 12),
    legend.text = element_text(family = font, colour = "black", size = 12),
    legend.key = element_rect(fill = "white"),
    axis.title.y = element_text(family = font, colour = "black", size = 12),
    axis.ticks = element_blank(),
    axis.text = element_text(family = font, colour = "black", size= 12),
    axis.line.y = element_line(colour = "black", size = 1),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(colour = "grey70", size = 0.2, linetype = 3),
    panel.grid.minor = element_blank(),
    plot.subtitle = ggplot2::element_text(size = 12, colour = "black", margin=ggplot2::margin(3,0,9,0)),
    panel.background = element_rect(fill = "white"),
    axis.title.x = element_text(family = font, colour = "black", size = 12),
    plot.title.position = "plot"
  )
}


##### Same theme but without legend
zew_plotstyle.nolegend <- function() {
  font <- "Calibri"
  
  ggplot2::theme(
    text=element_text(family = font),
    plot.title = element_text(face = "bold", size = 12, colour = "black"),
    legend.position = "none",
    axis.title.y = element_text(family = font, colour = "black", size = 12),
    axis.ticks = element_blank(),
    axis.text = element_text(family = font, colour = "black", size= 12),
    axis.line.x = element_line(colour = "black", size = 1),
    panel.grid.major.y = element_line(colour = "grey70", size = 0.2, linetype = 3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.subtitle = ggplot2::element_text(size = 12, colour = "black", margin=ggplot2::margin(3,0,9,0)),
    panel.background = element_rect(fill = "white"),
    axis.title.x = element_text(family = font, colour = "black", size = 12),
    plot.title.position = "plot"
  )
}

##### Theme for flipped graph with no legend
zew_plotstyle.flipped.nolegend <- function() {
  font <- "Calibri"
  
  ggplot2::theme(
    text=element_text(family = font),
    plot.title = element_text(face = "bold", size = 12, colour = "black"),
    legend.position = "none",
    axis.title.y = element_text(family = font, colour = "black", size = 12),
    axis.ticks = element_blank(),
    axis.text = element_text(family = font, colour = "black", size= 12),
    axis.line.y = element_line(colour = "black", size = 1),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(colour = "grey70", size = 0.2, linetype = 3),
    panel.grid.minor = element_blank(),
    plot.subtitle = ggplot2::element_text(size = 12, colour = "black", margin=ggplot2::margin(3,0,9,0)),
    panel.background = element_rect(fill = "white"),
    axis.title.x = element_text(family = font, colour = "black", size = 12),
    plot.title.position = "plot"
  )
}

##### Theme for maps (axes, labels, etc. removed)
zew_mapstyle <- function() {
  font <- "Calibri"
  
  ggplot2::theme(
    text=element_text(family = font),
    plot.title = element_text(face = "bold", size = 14, colour = "black"),
    legend.background = element_rect(fill = "white", size = 4, colour = "white"),
    legend.position = "bottom",
    legend.title = element_text(family = font, colour = "black", size = 12),
    legend.text = element_text(family = font, colour = "black", size = 12),
    legend.key = element_rect(fill = "white"),
    legend.direction="horizontal",
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.subtitle = ggplot2::element_text(size = 12, colour = "black", margin=ggplot2::margin(3,0,9,0)),
    panel.background = element_rect(fill = "white"),
    plot.title.position = "plot"
  )
}


##### ZEW colors
zew_greens.fill <- function() {
  ggplot2::scale_fill_gradient(low="#5E6310", high="#b4be28")
}

zew_greens.color <- function() {
  ggplot2::scale_color_gradient(low="#5E6310", high="#b4be28")
}

zew_blues.fill <- function() {
  ggplot2::scale_fill_gradient(low="#1b3f61", high="#587fa3")
}

zew_blues.color <- function() {
  ggplot2::scale_color_gradient(low="#1b3f61", high="#587fa3")
}

zew_reds.fill <- function() {
  ggplot2::scale_fill_gradient(low="#732020", high="#bf1b1b")
}

zew_reds.color <- function() {
  ggplot2::scale_color_gradient(low="#732020", high="#bf1b1b")
}

zew_yellows.fill <- function() {
  ggplot2::scale_fill_gradient(low="#756521", high="#d1ad17")
}

zew_yellows.color <- function() {
  ggplot2::scale_color_gradient(low="#756521", high="#d1ad17")
}

zew_browns.fill <- function() {
  ggplot2::scale_fill_gradient(low="#473e2e", high="#a19b6d")
}

zew_browns.color <- function() {
  ggplot2::scale_color_gradient(low="#756521", high="#a19b6d")
}

zew_colorstyle.fill <- function() {
  ggplot2::scale_fill_manual(values = c("#527ca4","#b4be28","#9c2424","#c7a92f","#958c47","#000000"),
                              labels = zew_legend.labs)
}

zew_colorstyle.color <- function() {
  ggplot2::scale_color_manual(values = c("#527ca4","#b4be28","#9c2424","#c7a92f","#958c47","#000000"),
                              labels = zew_legend.labs)
}