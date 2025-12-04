# Trabajo práctico de estadística
# integrantes: 
#    Franquito
#    Emi
#    yo :3

library(ggplot2)
library(plotly)

set.seed(80)


################################################
##                Parte 1
################################################


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


################################################
##                Parte 2
################################################
#3. Mostrar con gráficos, para Se = 0.9, Sp = 0.95 y θ = 0.25, cómo cambia p en
# función de:
#  (a) θ dejando fijos Se y Sp,
#  (b) Se dejando fijos θ y Sp, y
#  (c) Sp dejando fijos θ y Se.

Se = 0.9
Sp = 0.95
tita = 0.25

x_s = seq(0.01, 0.99, length.out = 100)

p = function(Se, Sp, tita) {
  return(Se * tita + (1-Sp)*(1-tita))
}

##################################
# solución 1
##################################

p_by_Se = c()
p_by_Sp = c()
p_by_tita = c()


for (x in x_s){
  p_by_Se = c(p_by_Se, p(x, Sp, tita))
  p_by_Sp = c(p_by_Sp, p(Se, x, tita))
  p_by_tita = c(p_by_tita, p(Se, Sp, x))
}

valores_p = data.frame("x"=x_s, 'p_by_Se'=p_by_Se, 
                       'p_by_Sp'=p_by_Sp, 'p_by_tita'=p_by_tita)

ggplot(valores_p, aes(x, p_by_Se))+
  geom_line(color='indianred', linewidth=1)+
  labs(
    title="p según Se",
    x = "Se",
    y = "p"
  )+
  theme_minimal()

ggplot(valores_p, aes(x, p_by_Sp))+
  geom_line(color='indianred', linewidth=1)+
  labs(
    title="p según Sp",
    x = "Se",
    y = "p"
  )+
  theme_minimal()

ggplot(valores_p, aes(x, p_by_tita))+
  geom_line(color='indianred', linewidth=1)+
  labs(
    title="p según tita",
    x = "Se",
    y = "p"
  )+
  theme_minimal()


###########################################
# solución 2
###########################################


valores_p = data.frame('x'=c(), 'p'=c(), variable=c())

for (x in x_s){
  p_by_Se = p(x, Sp, tita)
  p_by_Sp = p(Se, x, tita)
  p_by_tita = p(Se, Sp, x)
  
  valores_p = rbind(valores_p, list('x'=x, 'p'=p_by_Se, 'variable'='Se'),
                    list('x'=x, 'p'=p_by_Sp, 'variable'='Sp'),
                    list('x'=x, 'p'=p_by_tita, 'variable'='tita'))
}

valores_p$variable = as.factor(valores_p$variable)

ggplot(valores_p, aes(x, p, color=variable))+
  geom_line(linewidth=1)+
  scale_color_manual(values = c("red", "blue", 'green'), 
                     labels = c("Se", "Sp", 'Tita'))+
  labs(
    title="p según las variables",
    x = "valor de la variable",
    y = "p",
    color='Parámetro variable'
  )+
  theme_minimal()

