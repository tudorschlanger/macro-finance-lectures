## Select formatting for all charts in lecture 1

packages <- c("ggplot2")
check_packages(packages) # Check whether packages are installed alredy and load them 

format <- theme(
  axis.line = element_line(colour = "black", size = 1, linetype = "solid"),
  axis.text.x = element_text(face = "bold", colour = "black", size = 22),
  axis.text.y = element_text(face = "bold", colour = "black", size = 22),
  axis.title.x = element_text(face = "bold", colour = "black",size = 24),
  axis.title.y = element_text(face = "bold", colour = "black", size = 24),
  axis.ticks.length = unit(5, "pt"),
  plot.title = element_blank(),
  plot.caption = element_text(size = 16, hjust = 0, vjust = 0),
  panel.background = element_blank(),
  panel.grid.major = element_line(color = "gray50", size = 0.1),
  panel.grid.major.x = element_blank(),
  panel.grid.minor = element_blank(),
  legend.background = element_blank(),
  legend.box.background = element_blank(),
  legend.key.size = unit(1.5, "cm"),
  legend.spacing = unit(0, "lines"),
  legend.position = "bottom",
  legend.title = element_blank(),
  legend.text = element_text(face = "bold", size = 22)
  )
blue <- "#3b528b"
purple <- "#440154"
green <- "#21918c"
