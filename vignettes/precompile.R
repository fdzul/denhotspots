# The articles use data that are downloaded (rgeomex) and models that take
# time (INLA), so they are knitted beforehand, not when the package is built.
# Run from the root of the package after a change:
#   source("vignettes/precompile.R")
# To knit only some articles:
#   only <- "hotspots_analysis"; source("vignettes/precompile.R")

# knitr writes each figure as <div class="figure"> with the caption
# "plot of chunk <label>" when there is no fig.cap: keep only the image.
clean_figures <- function(f) {
  x <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  x <- gsub(paste0('<div class="figure">\\s*(<img [^>]*?)alt="plot of chunk ([^"]+)"',
                   '([^>]*/>)\\s*<p class="caption">[^<]*</p>\\s*</div>'),
            '\\1alt="\\2"\\3', x, perl = TRUE)
  writeLines(x, f, useBytes = TRUE)
}

old <- setwd("vignettes")
files <- list.files(pattern = "\\.Rmd\\.orig$")
if (exists("only")) files <- files[sub("\\.Rmd\\.orig$", "", files) %in% only]
for (f in files) {
  out <- sub("\\.orig$", "", f)
  knitr::knit(f, out)
  clean_figures(out)
}
setwd(old)
