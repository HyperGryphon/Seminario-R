rm(list = ls())
graphics.off()

library(Luminescence); library(tidyverse); library(xlsx)
#chose directory
#setwd('C:/Users/iande/Downloads/seminario R/')
#chose sample
sample <- 'data/SensParnaiba_primeirolote_150209.binx'

data <- read_BIN2R(sample)
n.aliquots <- length(unique(data@METADATA$POSITION))
#n.samples <- n.aliquots/9

#samples name and how many aliquots per sample
samples <- c(rep('BP75A', 4), rep('BP86', 4), rep('BP89', 4), rep('BP92', 4),
            rep('BP100', 4), rep('BP106', 4), rep('BP204', 4), rep('BP207', 4),
            rep('BP211', 4), rep('BP214', 4), rep('BP244', 4), rep('BP98', 4))
#units name and how many samples per unit
#units <- c(rep('Amazon', 8), rep('Xingu', 8))

#Parnaiba sensitivity sequence
runnat <- 1; runitl <- 3; runirsl <- 4; runosl <- 5; runtl325 <- 6; runtl110 <- 8; runtlbg <- 9

#time for deconvolution
t.stim <- 100

#how many components, max <- 4
components <- 3
#Do you want to plot the deconvolution graphs?
plot.deconv <- F

#How many degrees per channel for TL signals
degree <- 1.8

#integration intervals, check channels/s for sensitivity calculation
natint <- 1:4; natbg <- 361:400; nattot <- 1:400
itlint <- 1:10; itlbg <- 200:250; itltot <- 1:250
irslint <- 1:3; irslbg <- 721:750; irsltot <- 1:750
oslint <- 1:4; oslbg <- 361:400; osltot <- 1:400
tl325int <- (240/degree):(380/degree); tl325bg <- (240/degree):(380/degree); tl325tot <- 1:250
tl110int <- (70/degree):(170/degree); tl110bg <- (70/degree):(170/degree); tl110tot <- 1:250


########################################################################
################ just press ctrl+A and enter to run ####################
########################################################################

#Select signals
nat <- subset(data, data@METADATA$RUN == runnat)
itl <- subset(data, data@METADATA$RUN == runitl)
irsl <- subset(data, data@METADATA$RUN == runirsl)
osl <- subset(data, data@METADATA$RUN == runosl)
tl325 <- subset(data, data@METADATA$RUN == runtl325)
tl325pos <- subset(data, data@METADATA$RUN == runtl325)
tl110 <- subset(data, data@METADATA$RUN == runtl110)
tl110pos <- subset(data, data@METADATA$RUN == runtl110)# & data@METADATA$AN_TEMP==220 & data@METADATA$RUN == 6)
tlbg <- subset(data, data@METADATA$RUN == runtlbg)

#time for deconvolution
ch <- unique(t.stim/osl@METADATA$NPOINTS)
t <- seq(ch,t.stim,ch)

#Estimate doses
dose.rate <- data.frame(data@METADATA$IRR_DOSERATE); dose.time <- data.frame(data@METADATA$IRR_TIME)
doses <- data.frame(dose.rate*dose.time)
colnames(doses) <- c('Dose (Gy)')

nat <- data.frame(nat@DATA)
itl <- data.frame(itl@DATA)
irsl <- data.frame(irsl@DATA)
osl <- data.frame(osl@DATA)
tl325 <- data.frame(tl325@DATA)
tl110 <- data.frame(tl110@DATA)
tlbg <- data.frame(tlbg@DATA)

sens.nat <- 1:n.aliquots
for (i in 1:n.aliquots) {
  sens.nat[i] <- (sum(nat[natint,i])-mean(nat[natbg,i]*length(natint)))/(sum(nat[nattot,i])-mean(nat[natbg,i]*length(natint)))*100
}

sens.itl <- 1:n.aliquots
for (i in 1:n.aliquots) {
  sens.itl[i] <- sum(itl[itlint,i])-(sum(itl[itlbg,i]))*0.2
}

osl.s <- 1:n.aliquots; bg.osl <- 1:n.aliquots
osl.total <- 1:n.aliquots; bg.total <- 1:n.aliquots
sens.osl <- 1:n.aliquots; osl.cts <- 1:n.aliquots
sens <- 1:n.aliquots; sd.bg.osl <- 1:n.aliquots
for (i in 1:n.aliquots) {
  
  osl.s[i] <- sum(osl[oslint,i])
  osl.total[i] <- sum(osl[osltot,i])
  bg.osl[i] <- mean(osl[oslbg,i])*length(oslint)
  sd.bg.osl[i] <- sd(osl[oslbg,i])
  bg.total[i] <- mean(osl[oslbg,i])*length(osltot)
  # OSL in cts/1s
  osl.cts[i] <- (osl.s[i]-bg.osl[i]) #/(doses[i]*8.9) #cts/Gy?mg
  # BOSLF
  sens.osl[i] <- (osl.s[i]-bg.osl[i])/(osl.total[i]-bg.total[i])*100
  
  if (sens.osl[i] < 0) {
    sens[i] <- 0
  } 
  else if (osl.cts[i] < (bg.osl[i]+3*sd.bg.osl[i])) {
    sens[i] <- 0
  } 
  #else  if (osl[i] > (bg.osl[i]+3*sd.bg.osl[i]) & osl[i] < (2*bg.osl[i]+3*sd.bg.osl[i])) {
  #sens[i] <- print('low')
  #}
  else {
    sens[i] <- sens.osl[i]
  }
}

sens.irsl <- 1:n.aliquots; irsl.cts <- 1:n.aliquots
for (i in 1:n.aliquots) {
  irsl.cts[i] <- sum(irsl[irslint,i])-mean(irsl[irslbg,i]*length(irslint))
  sens.irsl[i] <- (sum(irsl[irslint,i])-mean(irsl[irslbg,i]*length(irslint)))/(sum(osl[oslint,i])-mean(osl[oslbg,i]*length(oslint)))*100
}

fast.prop <- 1:n.aliquots; med.prop <- 1:n.aliquots; slow.prop <- 1:n.aliquots; slow.2.prop <- 1:n.aliquots
fast <- 1:n.aliquots; med <- 1:n.aliquots; slow <- 1:n.aliquots; slow.2 <- 1:n.aliquots
for (i in 1:n.aliquots) {
  fit <- fit_CWCurve(data.frame(t,osl[,i]), n.components.max = components, fit.method = "LM",
                     fit.trace = F, fit.failure_threshold = T, 
                     LED.power = 40, LED.wavelength = 470,
                     #log = "x",
                     plot = plot.deconv)
  if (ncol(fit$component.contribution.matrix[[1]]) == 15) {
    fast.prop[i] <- (sum(fit$component.contribution.matrix[[1]][oslint,"cont.c1"])/
                       (sum(fit$component.contribution.matrix[[1]][oslint,"cont.c1"])+
                          sum(fit$component.contribution.matrix[[1]][oslint,"cont.c2"])+
                          sum(fit$component.contribution.matrix[[1]][oslint,"cont.c3"])+
                          sum(fit$component.contribution.matrix[[1]][oslint,"cont.c4"])))*100
    med.prop[i] <- (sum(fit$component.contribution.matrix[[1]][oslint,"cont.c2"])/
                      (sum(fit$component.contribution.matrix[[1]][oslint,"cont.c1"])+
                         sum(fit$component.contribution.matrix[[1]][oslint,"cont.c2"])+
                         sum(fit$component.contribution.matrix[[1]][oslint,"cont.c3"])+
                         sum(fit$component.contribution.matrix[[1]][oslint,"cont.c4"])))*100
    slow.prop[i] <- (sum(fit$component.contribution.matrix[[1]][oslint,"cont.c3"])/
                       (sum(fit$component.contribution.matrix[[1]][oslint,"cont.c1"])+
                          sum(fit$component.contribution.matrix[[1]][oslint,"cont.c2"])+
                          sum(fit$component.contribution.matrix[[1]][oslint,"cont.c3"])+
                          sum(fit$component.contribution.matrix[[1]][oslint,"cont.c4"])))*100
    slow.2.prop[i] <- (sum(fit$component.contribution.matrix[[1]][oslint,"cont.c4"])/
                         (sum(fit$component.contribution.matrix[[1]][oslint,"cont.c1"])+
                            sum(fit$component.contribution.matrix[[1]][oslint,"cont.c2"])+
                            sum(fit$component.contribution.matrix[[1]][oslint,"cont.c3"])+
                            sum(fit$component.contribution.matrix[[1]][oslint,"cont.c4"])))*100
    fast[i] <- data.frame(fit$component.contribution.matrix[[1]][,"cont.c1"])
    med[i] <- data.frame(fit$component.contribution.matrix[[1]][,"cont.c2"])
    slow[i] <- data.frame(fit$component.contribution.matrix[[1]][,"cont.c3"])
    slow.2[i] <- data.frame(fit$component.contribution.matrix[[1]][,"cont.c4"])
  }
  if (ncol(fit$component.contribution.matrix[[1]]) == 12) {
    fast.prop[i] <- (sum(fit$component.contribution.matrix[[1]][oslint,"cont.c1"])/
                       (sum(fit$component.contribution.matrix[[1]][oslint,"cont.c1"])+
                          sum(fit$component.contribution.matrix[[1]][oslint,"cont.c2"])+
                          sum(fit$component.contribution.matrix[[1]][oslint,"cont.c3"])))*100
    med.prop[i] <- (sum(fit$component.contribution.matrix[[1]][oslint,"cont.c2"])/
                      (sum(fit$component.contribution.matrix[[1]][oslint,"cont.c1"])+
                         sum(fit$component.contribution.matrix[[1]][oslint,"cont.c2"])+
                         sum(fit$component.contribution.matrix[[1]][oslint,"cont.c3"])))*100
    slow.prop[i] <- (sum(fit$component.contribution.matrix[[1]][oslint,"cont.c3"])/
                       (sum(fit$component.contribution.matrix[[1]][oslint,"cont.c1"])+
                          sum(fit$component.contribution.matrix[[1]][oslint,"cont.c2"])+
                          sum(fit$component.contribution.matrix[[1]][oslint,"cont.c3"])))*100
    slow.2.prop[i] <- print(NA)
    fast[i] <- data.frame(fit$component.contribution.matrix[[1]][,"cont.c1"])
    med[i] <- data.frame(fit$component.contribution.matrix[[1]][,"cont.c2"])
    slow[i] <- data.frame(fit$component.contribution.matrix[[1]][,"cont.c3"])
    slow.2[i] <- data.frame(print(NA))
  }
  if (ncol(fit$component.contribution.matrix[[1]]) == 9) {
    fast.prop[i] <- (sum(fit$component.contribution.matrix[[1]][oslint,"cont.c1"])/
                       (sum(fit$component.contribution.matrix[[1]][oslint,"cont.c1"])+
                          sum(fit$component.contribution.matrix[[1]][oslint,"cont.c2"])))*100
    med.prop[i] <- (sum(fit$component.contribution.matrix[[1]][oslint,"cont.c2"])/
                      (sum(fit$component.contribution.matrix[[1]][oslint,"cont.c1"])+
                         sum(fit$component.contribution.matrix[[1]][oslint,"cont.c2"])))*100
    slow.prop[i] <- print(NA)
    slow.2.prop[i] <- print(NA)
    fast[i] <- data.frame(fit$component.contribution.matrix[[1]][,"cont.c1"])
    med[i] <- data.frame(fit$component.contribution.matrix[[1]][,"cont.c2"])
    slow[i] <- data.frame(print(NA))
    slow.2[i] <- data.frame(print(NA))
  }
  if(ncol(fit$component.contribution.matrix[[1]]) == 6) {
    fast.prop[i] <- (sum(fit$component.contribution.matrix[[1]][oslint,"cont.c1"])/
                       (sum(fit$component.contribution.matrix[[1]][oslint,"cont.c1"])))*100
    med.prop[i] <- print(NA)
    slow.prop[i] <- print(NA)
    slow.2.prop[i] <- print(NA)
    fast[i] <- data.frame(fit$component.contribution.matrix[[1]][,"cont.c1"])
    med[i] <- data.frame(print(NA))
    slow[i] <- data.frame(print(NA))
    slow.2[i] <- data.frame(print(NA))
  }
}

fast.prop[is.na(fast.prop)] <- 0
med.prop[is.na(med.prop)] <- 0
slow.prop[is.na(slow.prop)] <- 0

sens.tl325 <- 1:n.aliquots; tl325.cts <- 1:n.aliquots
for (i in 1:n.aliquots) {
  tl325.cts[i] <- sum(tl325[tl325int,i])-mean(tlbg[tl325bg,i])*length(tl325int)
  sens.tl325[i] <- ((sum(tl325[tl325int,i])-mean(tlbg[tl325bg,i])*length(tl325int))/(sum(tl325[tl325tot,i])-mean(tlbg[tl325bg,i])*length(tl325tot)))*100 
}

tl325pos <- 1:n.aliquots
for (i in 1:n.aliquots) {
  tl325pos[i] <- which.max(abs(tl325[tl325int,i]))*degree-degree+tl325int[1]*degree-degree
}

sens.tl110 <- 1:n.aliquots; tl110.cts <- 1:n.aliquots
for (i in 1:n.aliquots) {
  tl110.cts[i] <- sum(tl110[tl110int,i])-mean(tlbg[tl110bg,i])*length(tl110int)
  sens.tl110[i] <- ((sum(tl110[tl110int,i])-mean(tlbg[tl110bg,i])*length(tl110int))/(sum(tl110[tl110tot,i])-mean(tlbg[tl110bg,i])*length(tl110tot)))*100 
}

tl110pos <- 1:n.aliquots
for (i in 1:n.aliquots) {
  tl110pos[i] <- which.max(abs(tl110[tl110int,i]))*degree-degree+tl110int[1]*degree-degree
}

#condense data
data <- data.frame(samples, sens.irsl, irsl.cts, sens.osl, fast.prop, med.prop, slow.prop, osl.cts, sens.tl325, tl325.cts, tl325pos, sens.tl110, tl110.cts, tl110pos)
table.samples <- aggregate(data[2:length(data)], list(data$samples), mean, na.rm = T)
table.samples.sd <- aggregate(data[2:length(data)], list(data$samples), sd, na.rm = T)
res.sample <- do.call('data.frame', lapply(1:ncol(table.samples), 
                                     function(j) cbind(ts(table.samples[,j]), 
                                                       ts(table.samples.sd[,j]))))
res.sample <- res.sample[,3:ncol(res.sample)] %>% 
  add_column(unique(samples), .before = 1)
colnames(res.sample) <- c('Sample', 'IRSL/BOSL1s','IRSL/BOSL1s.sd', 'IRSL cts', 'IRSL cts.sd', 
                    'BOSL1s','BOSL1s.sd', 'fast', 'fast.sd', 'medium', 'medium.sd','slow','slow.sd', 
                    'OSL cts', 'OSL cts.sd', 'TL325oC','TL325oC.sd', 'TL325 cts','TL325 cts.sd', 
                    'TL325pos','TL325pos.sd', 'TL110oC','TL110oC.sd', 'TL110oC cts','TL110oC cts.sd', 'TL110pos','TL110pos.sd')
view(res.sample)

#setwd('data')
write.xlsx(data, file = paste0(str_sub(sample, 1, -6),'.xlsx'), sheetName = 'by_aliquot', append = T)
write.xlsx(res.sample, file = paste0(str_sub(sample, 1, -6),'.xlsx'), sheetName = 'by_sample', append = T)
