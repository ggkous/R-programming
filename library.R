#install package
list.of.packages <- c("proto", "gsubfn",
                      "RSQLite","sqldf",
                      "ggplot2","RColorBrewer",
                      "wesanderson","scales","magrittr",
                      "plotrix","plotly",
                      "plyr","jsonlite","rjson","bitops",
                      "RCurl","aws.s3","aws.lambda",
                      "svglite","googleVis","webshot",
                      "dplyr","animation",
                      "png","grid","jpeg",
                      "htmlwidgets","formattable",
                      "htmltools","tibble","gtools",
                      "scatterplot3d","plot3D")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#library
library("gtools")
library('proto')
library('gsubfn')
library('RSQLite')
library('sqldf')
library('ggplot2')
library('RColorBrewer')
library('wesanderson')
library('scales')
library('magrittr')
library('plotrix')
library('plotly')
library("plyr")
library("jsonlite")
library("rjson")
library("bitops")
library("RCurl") 
library("aws.s3")
library("aws.lambda")
library('googleVis')
library('webshot')
library('dplyr')
library('animation')
library('png')
library('grid')
library('jpeg')
library("htmlwidgets")
library("formattable")
library("htmltools")
library("tibble")
library("gtools")
library("scatterplot3d")
library("plot3D")
library("tidyr")
