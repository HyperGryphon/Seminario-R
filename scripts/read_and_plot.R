read_and_plot <- function(file, all_in_one) {
  
  file <- file.choose()
  library(Luminescence)
  library(viridis)
  sample <- read_BIN2R(file)
  
  if (all_in_one==T) {
    osl <- subset(sample, sample@METADATA$RUN==4)
    osl <- data.frame(osl@DATA)
    time <- seq(0.1, 200, 0.1)
    
    matplot(time, cbind(osl), 
            type ='l', lty = 1, lwd = 2, 
            col = plasma(dim(osl)[2]), 
            xlab = 'Stimulation time (s)', log='x', 
            ylab = "OSL (cts/0.1 s)", cex.axis=0.7,
            main=paste('Sample', 
                       stringr::str_sub(unique(sample@METADATA$FNAME),8,12)))
  }
  
  if (all_in_one==F) {
    for (i in unique(sample@METADATA$POSITION)) {
  
    osl <- subset(sample, sample@METADATA$RUN==3 & 
                    sample@METADATA$POSITION==i)
    osl <- data.frame(osl@DATA)
    time <- seq(0.1, 200, 0.1)
    
    matplot(time, cbind(osl), 
         type ='l', lty = 1, lwd = 2, 
         col = rainbow(dim(osl)[2]), 
         xlab = 'Stimulation time (s)', log='x', 
         ylab = "OSL (cts/0.1 s)", cex.axis=0.7,
         main=paste('Sample', 
                    stringr::str_sub(unique(sample@METADATA$FNAME),8,12),
                    'Aliquot', i))
    }
  }
}
