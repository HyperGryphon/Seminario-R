#comments
#sections######################################################

print(1)

typeof(T)

result <- 1+5*4-4/8
result

sequencia <- c(1,2,3,4,5)
sequencia <- seq(1,5,1)
sequencia

data <- data.frame(sequencia, 
                   norm = rnorm(5,0.5,0.5), 
                   str = c('A','ppp','12c','V-V',' '))
data
#View(data)

data$seq
data[,2]
data[1,]
data[4,3]


###functions##################################################
sequencia <- c(4124,2342,5233,3646,2354) #rnorm(5,0.5,0.5)
mean(sequencia)
sd(sequencia)
summary(sequencia)

rm(list = ls())
graphics.off()

#help('mean')
#?summary

getwd()
#setwd()

#defining our own functions
my.fun <- function(x,y) {
  res <- (x^2)+y 
  return(res)
}

my.fun(2,3)

text2num <- function(text) {
  as.numeric(text)
}

text2num(text = c('1','14', '4464'))

#working with data, filtering and such

usp <- read.csv('data/salarios usp 2020.csv', sep = ';')
skimr::skim_without_charts(usp)

max(usp$Liquido)
usp$Nome[usp$Liquido==max(usp$Liquido)]

unique(usp$Unid.Orgao)
unique(usp$Categoria)
length(usp$Nome[usp$Unid.Orgao=='IGc' & usp$Categoria=='Docente'])
nome <- usp$Nome[usp$Unid.Orgao=='IGc' & usp$Categoria=='Docente']
tempo <- usp$Tempo_USP[usp$Unid.Orgao=='IGc' & usp$Categoria=='Docente']
dataframe <- data.frame(nome,tempo)
dataframe[order(dataframe$tempo),]

usp %>% filter(usp$Tempo_USP<5 
               & usp$Unid.Orgao=='IGc' 
               & usp$Depto.Setor!='Inativo') %>% 
  view()


#graphics######################################################################
x <- usp$Tempo_USP[usp$Unid.Orgao=='IGc']
y <- usp$Liquido[usp$Unid.Orgao=='IGc']
plot(x, y, type='p', pch = 16)

x <- 1:100
y1 <- x^2
y2 <- y1*4
y3 <- rnorm(length(x),3*x^2,4000)

#par(mfrow=c(1,2))
#pdf('grafico.pdf', height = 7, width = 9)
matplot(x, cbind(y1,y2,y3), col=rainbow(3), type = 'l', lwd = 2, lty = 1)
#dev.off()

data <- data.frame(x,y1,y2,y3)
ggplot(data)+
  geom_line(aes(x,y1, colour = '1'), lty = 2, lwd = 2)+
  geom_point(aes(x,y2, colour = '2'), pch = 17, size = 2)+
  geom_smooth(aes(x,y3, colour = '3'), lty = 1, lwd = 2)+
  labs(x = 'X axis (units)', y = 'Y axis (units)', color = 'Legend')+
  scale_color_manual(name = 'Curves', values = c('black','blue','orange'))+
  #scale_x_continuous(limits = c(min(x),max(x)))+
  #scale_y_continuous(limits = c(min(y),max(y)))+
  #theme_fivethirtyeight()+
  theme(plot.title = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.ticks = element_line(size = 1),
        panel.grid.major = element_line(colour = 'white'),
        panel.grid.minor = element_line(colour = 'white'),
        legend.text = element_text(size = 25),
        legend.title = element_text(size = 25),
        legend.position = c(0.2,0.8)) #'top'
#ggsave('grafico bonito.pdf', height = 6, width = 7)


#if and for loops#############################################################
number <- 2
if (number > 5) {
  print(paste('the number', number, 'is larger than 5'))
} else print(paste('the number', number, 'is lower than 5'))

ifelse(number > 5,
       paste('the number', number, 'is larger than 5'),
       paste('the number', number, 'is lower than 5'))

for (i in seq(0,100,10)) {
  print(paste('row of', i))
}


#regression and curve fitting#######################################################
x <- 1:100
y <- rnorm(length(x), 0.234*x^2.353, 0)

plot(x, y, type='l', lwd=2)

lm <- lm(y~x)
lm

plot(x,y, type='l', lwd=2)
abline(a=lm$coefficients[1], b=lm$coefficients[2], lwd = 2, lty = 2)
legend('topleft',
       legend=paste('Relative rmse : ', 
                    round((sqrt(mean((lm$fitted.values-y)^2))/mean(y)*100),1),'%'))

data <- data.frame(x,y)
ggplot(data)+
  geom_line(aes(x,y))+
  geom_smooth(aes(x,y),formula = y~x, method = 'lm')

fit <- minpack.lm::nlsLM(formula = y~i*x^j, data = data,
                         start = c(i=0,j=0), upper = c(1e10,1e10),
                         lower = c(0,0), control = list(maxiter = 1000))

coef(fit)[1]; coef(fit)[2]

fit.y <- coef(fit)[1]*x^coef(fit)[2]

matplot(x, cbind(fit.y+100, y), type = 'l', lty = c(2,1), lwd = c(2,1), col = c(1,2))
legend('topleft', col = c(1,2), lwd = 2,
       legend=c('y',
         paste('Relative rmse : ', 
                    round((sqrt(sum((fit$m$fitted()-y)^2)/length(y))/mean(y))*100,1),'%')))

