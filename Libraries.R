library(tidyverse)
library(tidymodels)
library(vroom)
library(patchwork)
library(DataExplorer)
library(GGally)


# EDA notes
#dplyr::glimpse(dataset) - lists the variable type of each column
#skimr::skim(dataset) - nice overview of the dataset
#DataExplorer::plot_intro(dataset) - visualization of glimpse()
#DataExplorer::plot_correlation(dataset) - correlation heat map between variables
#DataExplorer::plot_bar(dataset) - bar charts of all discrete variables
#DataExplorer::plot_histrograms(dataset) - histograms of all numerical variables
#DataExplorer::plot_missing(dataset) - percent missing in each column
#GGally::ggpairs(dataset) - 1/2 scatterplot and 1/2 correlation heat map

# Patchwork library notes
#plot1 <- ggplot() + ...2
#plot2 <- ggplot() + ...3
#plot1 + plot2 #side by side4
#plot1 / plot2 #top and bottom5
#(plot1 + plot2) / (plot1 + plot2) #4 panel plot