# First, execute the RProjs .Rprofile if it exists.
if (file.exists("../.Rprofile")) {
  base::sys.source("../.Rprofile", envir = environment())
}

## This makes sure that R loads the workflowr package
## automatically, everytime the project is loaded
if (requireNamespace("workflowr", quietly = TRUE)) {
  message("Loading .Rprofile for the current workflowr project")
  library("workflowr")
} else {
  message("workflowr package not installed, please run install.packages(\"workflowr\") to use the workflowr functions")
}

