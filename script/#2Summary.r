#2Summary

#'
#+ library
package_load <- function(pkg.name){
  if(!require(pkg.name, character.only=TRUE)){
    install.packages(pkg.name)
    library(pkg.name, character.only = TRUE)
  } else {
    library(pkg.name, character.only = TRUE)
  }
}

package_load("tidyverse")
package_load("stargazer")