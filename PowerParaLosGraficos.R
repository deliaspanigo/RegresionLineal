# https://github.com/BioInfoTools/exportR2Office


install.packages("export")
library(export)
library(ggplot2)
qplot(Sepal.Length, Petal.Length, data = iris, color = Species,
      size = Petal.Width, alpha = I(0.7))
graph2ppt(file="ggplot2_plot.pptx", width=6, height=5, append = T)
