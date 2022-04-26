file <- 'data/Dating_L1415_R2_191220.binx'

library(Luminescence) #if necessary
sample <- read_BIN2R(file)

curve <- sample@DATA[[1]]

t <- seq(0.1, 40, 0.1)

plot(t, curve)

data <- data.frame(t, curve)

fit.curve <- minpack.lm::nlsLM(formula = curve~(a+(n*b*exp(-b*t))),
                               data = data,
                               start = c(a=0, n=1, b=1),
                               upper = c(1e10, 1e10, 1e10),
                               lower = c(0.1, 0.1, 0.1),
                               control = list(maxiter=1000))

a <- coef(fit.curve)[1]
n <- coef(fit.curve)[2]
b <- coef(fit.curve)[3]
fit.y <- a+(n*b*exp(-b*t))

matplot(t, cbind(fit.y, curve), 
        type = 'l', lty = c(2,1), lwd = c(2,2), 
        col = c(1,2), #log='x',
        xlab = 'Stimulation time (s)',
        ylab = 'OSL cts/0.1 s')

rmse <- round((sqrt(sum((fit.y-curve)^2)/length(curve))/mean(curve))*100,1)

legend('topright', col = c(1,2), lwd = 2,
       legend=c('Curve', paste('Relative RMSE :',rmse,'%')),
       bty = 'n')

#with ggplot2
data <- data.frame(data,fit.y)

ggplot(data)+
  geom_line(aes(x=t,y=curve, colour='Experimental'), lwd = 2)+
  geom_line(aes(x=t,y=fit.y, colour='Fitted'), lwd = 2)+
  labs(x = 'Stimulation time (s)', y = 'OSL cts/0.1 s', 
       color = 'Legend', title = 'Natural OSL vs Fitted')+
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.ticks = element_line(size = 1),
        panel.grid.major = element_line(colour = 'gray'),
        panel.grid.minor = element_line(colour = 'gray'),
        legend.text = element_text(size = 20),
        legend.title = element_blank(),
        legend.position = 'bottom',
        panel.background = element_blank()) #'top'
ggsave("natural vs fitted.pdf", height = 6, width = 9)
