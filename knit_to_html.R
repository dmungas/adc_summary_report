setwd('~/Database/Reports/ADC Summary')
require(knitr)
require(markdown)
require(rmarkdown)
knit("ADC_Summary.Rmd", "ADC_Summary.md")
markdownToHTML("ADC_Summary.md", "ADC_Summary.html",
               stylesheet="custom_markdown_4.css",
               title = "ADC_Summary",
               header= "<h1> ADC Summary </h1>")

