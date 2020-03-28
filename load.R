library(tidyverse)
library(tidytext)
library(readtext)

hansards <- readtext(
  "data/source/parl.canadiana.ca/*.txt",
  docvarsfrom = "filenames",
  docvarnames = c("parliament", "session", "volume"),
  dvsep = "-"
)
