# Requirements
requirements <- c(

  "formatR",
  "stargazer",
  "tidyverse"

)

# Create a data frame with installed packages
current_libs <- as.data.frame(installed.packages())
to_install   <- setdiff(requirements, current_libs$Package)

# Download and install the remaining packages
install.packages(to_install, repos = "https://cran.biotools.fr/")

