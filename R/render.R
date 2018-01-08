Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/RStudio/bin/pandoc")
initial.options <- commandArgs(trailingOnly = FALSE)
file.arg.name <- "--file="
script.name <- sub(file.arg.name, "", initial.options[grep(file.arg.name, initial.options)])
script.basename <- dirname(script.name)
mdpath <- paste(script.basename, '/../', "inst/rmarkdown/templates/simple/skeleton/skeleton.Rmd", sep = "")
mdpath

rmarkdown::render(mdpath, 
    output_dir = getwd(),
    output_file = "report.html")
