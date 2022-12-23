#This script is written to run the .rmd file for updating WIC app
library(rmarkdown)
library(dplyr)
#The html report file name
datestring <- Sys.time() %>% format("%Y%m%d")
report_name <- paste(datestring,".html", sep="")

#Pandoc render script
Sys.setenv(RSTUDIO_PANDOC = 'C:\\Program Files\\RStudio\\bin\\quarto\\bin\\pandoc')
rmarkdown::render(
  input = "C:\\Users\\Farshad.Ebrahimi\\OneDrive - City of Philadelphia\\Github Projects\\WIC\\update_wic.Rmd",
  output_file = paste("C:\\Users\\Farshad.Ebrahimi\\OneDrive - City of Philadelphia\\Github Projects\\WIC\\Reports\\", report_name, sep = ""),
  output_dir = "C:\\Users\\Farshad.Ebrahimi\\OneDrive - City of Philadelphia\\Github Projects\\WIC\\Reports\\"
)