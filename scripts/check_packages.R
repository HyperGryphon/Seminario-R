packages <- c('Luminescence', 'skimr', 'tidyverse', 'minpack.lm', 'Hmisc', 'xlsx',
              'caret','pacman','ggfortify')

package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)
