##  Graphs and themes
theme_mylight_base <- function(base_size=11, base_family="sans",
        my.background="white", my.text.color="black", my.text.size=8,
        my.text.axis="grey30", my.gridsize=0.01, my.gridcolor="gray50", my.gridlinetype="dashed",
        my.strip.background="#A68776", my.strip.color="white",
        ...)
{
    half_line <- base_size/2

    theme_light(base_size = base_size, base_family = base_family) %+replace%
    theme(
          panel.border = element_rect(fill = NA, size=my.gridsize, color=my.gridcolor),
          panel.grid.minor = element_line(size=my.gridsize, linetype=my.gridlinetype, color=my.gridcolor),
          panel.grid.major = element_line(size=my.gridsize, linetype=my.gridlinetype, color=my.gridcolor),
          panel.background = element_rect(size=my.gridsize, fill=my.background, color=my.background),

          plot.title=element_text(family=base_family, face="bold", size = rel(1), hjust = 0, vjust = 1, margin = margin(b = half_line)),
          # plot.title=element_blank(),                                               # no title

          axis.text=element_text(size=my.text.size, colour=my.text.axis),
          axis.ticks = element_blank(),

          axis.title = element_text(size=my.text.size, face="bold"),                # 8pt, bold

          ## axis.text.x = element_text(angle=90, vjust=0.5),
          axis.title.x = element_text(margin=margin(t=half_line/2), vjust = 1),
          ## axis.title.x.top = element_text(margin = margin(b = half_line/2), vjust = 0),
          ## axis.title.x = element_blank(),

          axis.title.y = element_text(angle=90, margin=margin(r=half_line/2), vjust = 1),
          ## axis.title.y = element_text(size=my.text.size, face="bold", angle=90, margin=margin(r=half_line/2), vjust = 1, hjust=0.97),   # adjust the y-axis label to the top
          ## axis.title.y.right = element_text(angle = -90, margin = margin(l = half_line/2), vjust = 0)

          legend.title=element_blank(),
          legend.text=element_text(size=my.text.size, color=my.text.color),
          legend.key = element_rect(fill = my.background, colour = "transparent"),
          legend.background = element_rect(fill=my.background, color=my.background),

          strip.background = element_rect(fill=my.strip.background, color=my.gridcolor, size=0.1),
          strip.text = element_text(color=my.strip.color, size=my.text.size, margin=margin(0.8 * half_line, 0.8 * half_line, 0.8 * half_line, 0.8 * half_line)),
          ...
        )
}
