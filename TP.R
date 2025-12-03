# Trabajo práctico de estadística
# integrantes: 
#    Franquito
#    Emi
#    yo :3

library(ggplot2)
library(plotly)

set.seed(80)


# 5 - Suponga θ = 0.25. Evalúe coberturas empíricas mediante simulación Monte Carlo.
# Observe qué ocurre para distintos valores de n.

tita = 0.25
N = 100 # cantidad de simulaciones que vamos a hacer
n_s = c(10, 50, 100, 200, 500) # cantidad de muestras que vamos a tomar

calculo_covertura = function(tita_hat, n){
  z = qnorm(0.025) # tomado por izquierda
  variancia = sqrt(tita_hat*(1-tita_hat))
  return(list(
    'inicio'= tita_hat + z*variancia/sqrt(n),
    'fin' = tita_hat - z*variancia/sqrt(n)
  ))
}

coverturas <- data.frame(N=c(), n=c(), titahat=c(), inicio=c(), fin=c(), cubre=c())

# hacemos las simulaciones
for (n in n_s){ 
  for (i in 1:N){
    datos = rbinom(n, 1, tita) # simulamos datos
    tita_hat = mean(datos) # estimamos tita
    
    covertura = calculo_covertura(tita_hat, n)
    inicio = covertura$inicio
    fin = covertura$fin
    cubre = inicio <= tita && tita <= fin
    
    # guardamos los datos
    coverturas = rbind(coverturas, 
                       list('N'=i, 'n'=n, 'titahat'=tita_hat, 'inicio'=inicio, 
                            'fin'=fin, 'cubre'=cubre))
    
  }
}

# graficamos

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

# imprimimos por consola el porcentaje de covertura
for (n in n_s){
  porcentaje_covertura = sum(coverturas[coverturas$n == n, ]$cubre)/N*100
  print(paste0('El porcentaje de covertura con ', n, ' muestras en ', N, 
               ' simulaciones es de ', porcentaje_covertura, '%'))
}

