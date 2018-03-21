required_packages <-
  c(
    "tidyverse",
    "caret",
    "recipes",
    "modelr",
    "rsample",
    "tidyposterior",
    "ranger",
    "devtools",
    "here",
    "yardstick"
  )

installed_packages <- rownames(installed.packages())

missing_libraries <-
  required_packages[!required_packages %in% installed_packages]

if (length(missing_libraries) > 0){

  install.packages(missing_libraries)

}

if (("FishLife" %in% installed_packages) == F){
devtools::install_github("james-thorson/FishLife")

}
