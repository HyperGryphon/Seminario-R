rm(list = ls())
graphics.off()

library(Luminescence); library(Hmisc); library(tidyverse)

#setwd('C:/Users/iande/Downloads/seminario R/')
sample0 <- read_BIN2R('data/Dating_L1415_R2_191220.binx')

name <- stringr::str_sub(sample0@METADATA$FNAME[1], 8, 12)

results0 <- data.frame(matrix(NA,24,7))
for (i in unique(sample0@METADATA$POSITION)) {
  aliquot <- i
  run <- 3
  dose.rate <- sample0@METADATA$IRR_DOSERATE[1]
  sample <- subset(sample0, sample0@METADATA$TEMPERATURE == 125)# & sample@METADATA$RUN>8)
  
  #convertir .binx a RLum, indicar numero de alicuotas
  sample <- Risoe.BINfileData2RLum.Analysis(sample, pos = aliquot)
  
  #pdf(file = paste(sample, '_', aliquot, '.pdf', sep = ''), height = 15, width = 15)
  
  #analizar secuencia pIRIR definiendo canales a usar, background y la secuencia
  sar <- analyse_SAR.CWOSL(sample,
                           signal.integral.min = c(1,1),
                           signal.integral.max = c(8,8),
                           background.integral.min = c(301,301),
                           background.integral.max = c(400,400),
                           xlab = 'Dose (Gy)',
                           ylab = 'Lx/Tx',
                           fit.method = 'EXP',
                           plot = F)
  
  #dev.off()
  par(mar = c(4.5,4.5,4,2))
  dosegy <- sar@data$LnLxTnTx.table[[3]] * dose.rate
  LxTx.Data <- as.data.frame(cbind(dosegy, sar$LnLxTnTx.table[c(12, 13, 6)]))
  growthcurve <- plot_GrowthCurve(LxTx.Data, main = '', mtext = '',
                                  fit.method = 'EXP',
                                  fit.weights = F,
                                  fit.force_through_origin = T,
                                  fit.includingRepeatedRegPoints = F,
                                  xlab = 'Dose (Gy)', ylab = expression('L'[x]*'/T'[x]), 
                                  xlim = c(0, 2600), ylim = c(0, max(LxTx.Data[2])+0.1*max(LxTx.Data[2])),
                                  output.plot = F,
                                  output.plotExtended = F,
                                  output.plotExtended.single = F, cex = 1,
                                  col = 'red',
                                  legend = F)
  
  De.data <- data.frame(De = sar$data$De*dose.rate, De.Error = sar$data$De.Error*dose.rate, D0 = sar$data$D01*dose.rate)
  gc <- coef(growthcurve$Fit)
  
  #growthcurve components of the equation: a y D0
  gc <- data.frame(gc[1],gc[2],gc[3],gc[4])
  gc[is.na(gc)] <- 0
  xlim <- max(LxTx.Data[,1])+0.2*max(LxTx.Data[,1])
  ylim <- max(LxTx.Data[,2])+0.2*max(LxTx.Data[,2])
  x <- 0:3000
  
  #pdf('drc L1202.pdf', width = 10, height = 8)
  par(mar = c(4.5,4.8,2,1.5))
  plot(y = gc$gc.1.*(1-exp(-(x+gc$gc.3.)/gc$gc.2.)+gc$gc.4.*x), #+gc[3]*(1-exp(-(1:2600)/gc[4])), 
       x = x,
       ylab = '', xlab = '', type = 'l', lwd = 2,
       xlim = c(0,xlim), ylim = c(0, ylim),
       xaxs = 'i', yaxs = 'i', yaxt = 'n', xaxt = 'n')
  par(new = T)
  errbar(LxTx.Data[,1], LxTx.Data[,2],
         yplus = LxTx.Data[,2]+LxTx.Data[,3], yminus = LxTx.Data[,2]-LxTx.Data[,3],
         xlab = 'Dose (Gy)', ylab = expression('L'[x]*'/T'[x]), cex = 1.5,
         xlim = c(0,xlim), ylim = c(0, ylim),
         xaxs = 'i', yaxs = 'i', cex.lab = 2, cex.axis = 2)
  par(new = T)
  legend('topleft', legend = c(paste0('Sample ', name, ' aliquot: ', i), paste0('De = ', round(De.data$De, 1), ' \u00B1 ', round(De.data$De.Error,1), ' Gy'),
                               paste0('D0 = ', round(De.data$D0, 0), ' Gy')),
         cex = 1.5, bty='n')
  #l?neas de interpolaci?n
  segments(x0 = 0, x1 = De.data$De, y0 = LxTx.Data$LxTx[1], y1 = LxTx.Data$LxTx[1], col = 'red', lwd = 3, lty = 2)
  segments(x0 = De.data$De, x1 = De.data$De, y0 = 0, y1 = LxTx.Data$LxTx[1], col = 'red', lwd = 3, lty = 2)
  
  #plotear curva OSL como inset
  par(new = T)
  s1 <- sample[[1]]@data[,2]
  s2 <- sample[[3]]@data[,2]
  t <- sample[[1]]@data[,1]
  par(mar = c(5,24,24,2.1))
  plot(x = t, y = s1, xlab = '', ylab = '', xaxt = 'n', yaxt = 'n', main = '', type = 'l', lwd = 2, lty = 1,
       xaxs = 'i', yaxs = 'i', xlim = c(0,40), ylim = c(0, max(s1,s2)+max(s1,s2)*0.05), cex.lab = 1.5, col = 'black')
  par(new = T, mar = c(5,24,24,2.1))
  plot(x = t, y = s2, xlab = '', ylab = '', xaxt = 'n', yaxt = 'n', main = '', type = 'l', lwd = 2, lty = 1,
       xaxs = 'i', yaxs = 'i', xlim = c(0,40), ylim = c(0, max(s1,s2)+max(s1,s2)*0.05), cex.lab = 1.5, col = 'red')
  axis(2, at = seq(0,max(s1,s2),100000), cex.axis = 1.2)
  mtext(side = 2, 'OSL (cts/0.1 s)', line = 2.2, cex = 1.5)
  axis(3, at = seq(0,40,10), cex.axis = 1.2)
  mtext(side = 3, 'Stimulation time (s)', line = 2.2, cex = 1.5)
  legend('topright', legend = c('Natural', paste0('Reg 1: ', round(dosegy[2], 1), ' Gy')), lwd = 2,
         col = c(1,2), bty='n')
  
  #dev.off()
  
  sar$rejection.criteria[c(1,2,4)]
  IR.dep.ratio <- (((t(sar$LnLxTnTx.table$LnLx)[8]-t(sar$LnLxTnTx.table$LnLx.BG)[8])/
                      (t(sar$LnLxTnTx.table$TnTx)[7]-t(sar$LnLxTnTx.table$TnTx.BG)[7]))/
                     ((t(sar$LnLxTnTx.table$LnLx)[7]-t(sar$LnLxTnTx.table$LnLx.BG)[7])/
                        (t(sar$LnLxTnTx.table$TnTx)[7]-t(sar$LnLxTnTx.table$TnTx.BG)[7])))
  IRSL.BLSL.ratio <- sum(sample[15]$IRSL[1:4])/sum(sample[16]$OSL[1:4])*100
  
  results0[i,] <- data.frame(De = De.data$De, De.err = De.data$De.Error, D0 = De.data$D0,
                            Recycling = sar$rejection.criteria$Value[1], Recuperation = sar$rejection.criteria$Value[3]*100)
}

results <- results0[unique(sample0@METADATA$POSITION),]
colnames(results) <- c('De','De.err','D0','Recycling','Recuperation','IR.dep.ratio','IRSL.BLSL.ratio')
#View(results)

results.subset <- subset(results, Recycling<=1.11 & Recycling>=0.89 & Recuperation<=6) %>%
  drop_na(De)

write.csv(results.subset, file = paste0(str_sub(unique(sample0@METADATA$FNAME), 1, -6),'.csv'))

cam <- calc_CentralDose(results.subset,
                        plot = F, log = T)

camres <- get_RLum(cam, 'summary')

plot_AbanicoPlot(results.subset,
                 main = paste('Sample', name, '\U007C CAM: ', paste0(round(camres$de, 1), ' \U00B1 ',
                                                                     round(camres$de_err, 1), ' Gy')),
                 #c('3-parameter Minimum Age Model'),
                 cex = 0.8,
                 #line = mean(De.data$na.omit.df.De.),
                 z.0 = camres$de,
                 mtext = paste('n = ', nrow(results.subset), '/', nrow(results), '|', 'OD = ', paste0(round(cam@data$summary$OD, 1), ' %')),
                 polygon.col = 'none',
                 log.z = T,
                 hist = F,
                 rug = F,
                 frame = 1,
                 pch = 19,
                 bw = 0.1,
                 #zlim = c(0.3, 40),
                 grid.col = 'none')
#summary = c('skewness'),
#line.col = '#cd2828',
#line.label = paste0(round(camres$de, 1), '\U00B1',
#                   round(camres$de_err, 1), ' Gy'),
#line = camres,
#summary.pos = 'topleft')
#mtext = bquote('Parameters: ' ~ sigma[b] == .(get_RLum(mam, 'args')$sigmab) ~ ', ' ~
#                gamma == .(round(log(mamres$de), 1)) ~ ', ' ~
#              sigma == .(round(mamres$sig, 1)) ~ ', ' ~
#            rho == .(round(mamres$p0, 2))))

#plot kernel density
plot_KDE(results.subset, main = '', yaxt = 'n', na.rm = T,
         values.cumulative = T,
         boxplot = F,
         order = T,
         rug = F,
         #summary = c('n'),
         summary.pos = 'topleft',
         summary.method = 'weighted',
         bw = max(na.omit(results.subset$De))/10,
         xlab = 'Dose (Gy)', ylab = 'Density', cex = 1.5,
         pch = 19, lwd = 2,
         yaxs = 'i')

#a?adir lineas al kde plot
abline(v = camres$de,
       col = 'black', lty = 2)
caminfo <- paste('Sample', name, '\U007C CAM:', paste0(round(camres$de, 1), ' \U00B1 ',
                                                        round(camres$de_err, 1), ' Gy'))
aliquot.number <- paste(' (n = ', nrow(results.subset), ' / ', nrow(results), ')', sep = '')
mtext(text = paste0(caminfo, ' | ', round(camres$OD,1), '%',  aliquot.number),
      adj = 0.5,
      line = 0.5,
      col = 'black')

