# Suponga θ = 0.25. Evalue coberturas empıricas mediante simulacion Monte Carlo.
# Observe que ocurre para distintos valores de n.

library(ggplot2)
library(plotly)

tita = 0.25
n = 200
N = 100

calculo_covertura = function(tita_hat, n){
  z = qnorm(0.025)
  variancia = tita_hat*(1-tita_hat)
  return(list(
    'inicio'= tita_hat + z*variancia/sqrt(n),
    'fin' = tita_hat - z*variancia/sqrt(n)
  ))
}

coverturas <- data.frame(N=c(), n=c(), titahat=c(), inicio=c(), fin=c(), cubre=c())


for (n in c(10, 50, 100, 200, 500)){
  for (i in 1:N){
    datos = rbinom(n, 1, tita)
    tita_hat = mean(datos)
    
    covertura = calculo_covertura(tita_hat, n)
    inicio = covertura$inicio
    fin = covertura$fin
    cubre = inicio <= tita && tita <= fin
    
    coverturas = rbind(coverturas, 
                       list('N'=i, 'n'=n, 'titahat'=tita_hat, 'inicio'=inicio, 
                            'fin'=fin, 'cubre'=cubre))
    
  }
}


plot = ggplot(coverturas, aes(y = N, x=inicio, xend=fin, color=cubre, frame=n))+
  geom_segment(aes(yend=N), linewidth=1)+
  geom_vline(xintercept=tita, linetype='dashed', color="black")+
  scale_color_manual(values = c("red", "blue"), labels = c("No cubre", "Cubre"))+
  labs(
    title="Intervalos de confianza para tita",
    x = "Valor de tita",
    y = "Número de simulación",
    color="Cubre?"
  )+
  theme_minimal()+
  theme(axis.text.y = element_blank())

ggplotly(plot)

