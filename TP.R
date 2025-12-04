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
N = 500 # cantidad de simulaciones que vamos a hacer
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

coverturas_graph = coverturas[coverturas$N <= 100, ]

plot = ggplot(coverturas_graph, aes(y = N, x=inicio, xend=fin, color=cubre, frame=n))+
  geom_segment(aes(yend=N), linewidth=1)+
  geom_vline(xintercept=tita, linetype='dashed', color="black")+
  scale_color_manual(values = c("red", "blue"), labels = c("No cubre", "Cubre"))+
  labs(
    title="Intervalos de confianza para tita, \nde las primeras 100 simulaciones",
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

# los valores fijos
Se = 0.9
Sp = 0.95
tita = 0.25

# valores de la variable
x_s = seq(0.01, 0.99, length.out = 100)

p = function(Se, Sp, tita) {
  return(Se * tita + (1-Sp)*(1-tita))
}

valores_p = data.frame('x'=c(), 'p'=c(), variable=c())

# calculo p y lo guardo
for (x in x_s){
  p_by_Se = p(x, Sp, tita)
  p_by_Sp = p(Se, x, tita)
  p_by_tita = p(Se, Sp, x)
  
  valores_p = rbind(valores_p, list('x'=x, 'p'=p_by_Se, 'variable'='Se'),
                    list('x'=x, 'p'=p_by_Sp, 'variable'='Sp'),
                    list('x'=x, 'p'=p_by_tita, 'variable'='tita'))
}

valores_p$variable = as.factor(valores_p$variable)


# grafíco
plot = ggplot(valores_p, aes(x, p, color=variable))+
  geom_line(linewidth=1)+
  scale_color_manual(values = c("#00CD66", "tomato", 'skyblue2'), 
                     labels = c("Se", "Sp", 'Tita'))+
  labs(
    title="p según las variables",
    x = "valor de la variable",
    y = "p",
    color='Parámetro variable'
  )+
  theme_minimal()

ggplotly(plot)


################################################
##                Parte 2
################################################
# 6. Grafíque el ECM en función de n y compárelo con el ECM del test perfecto

tita = 0.25
Se = 0.9
Sp = 0.95

ECM_imp = function(Se, Sp, tita, n){
  p = Se*tita+(1-Sp)*(1-tita)
  return(p*(1-p)/n/(Se+Sp-1)**2)
}

ECM_perf = function(tita, n){
  return(tita*(1-tita)/n)
}

valores_ECM = data.frame(n=c(), valor=c(), ECM=c())

for (n in 1:300){
  valores_ECM = rbind(valores_ECM, 
                      list(
                        "n"=n,
                        "valor"=ECM_imp(Se, Sp, tita, n),
                        "ECM" = "ECM imperfecto"
                      ),
                      list(
                        "n"=n,
                        "valor"=ECM_perf(tita, n),
                        "ECM" = "ECM perfecto"
                      ))
}

valores_ECM$ECM = as.factor(valores_ECM$ECM)

ggplot(valores_ECM, aes(n, valor, color=ECM))+
  geom_line(linewidth=1)+
  scale_color_manual(values = c("tomato", "#00CD66"), 
                     labels = c("Imperfecto", "Perfecto"))+
  labs(
    title="Valor del ECM según n",
    x = "n",
    y = "ECM",
    color='Estimador'
  )+
  theme_minimal()


##########################################
# 7. Realice simulaciones para comparar los valores teóricos hallados 
# en el item 5 con los simulados.

tita = 0.25

#sesgo, varianza, ecm

sesgo_real = function(t_vals, Se=0.9, Sp=0.95){
  return(abs((mean(t_vals-1+Sp)/(Se+Sp-1)) - tita))
}

varianza_real = function(t_vals, Se=0.9, Sp=0.95){
  return(var((t_vals-1+Sp)/(Se+Sp-1)))
}

sesgo_teorico = 0

varianza_teorica = function(n, Se=0.9, Sp=0.95, tita=0.25){
  p = Se*tita+(1-Sp)*(1-tita)
  return(p*(1-p)/n/(Se+Sp-1)**2)
}

datos = data.frame(n=c(), valor=c(), tipo=c())

for (n in 1:500){
  t_vals = rbinom(n, 1, tita)
  datos = rbind(datos,
                list(
                  n=n,
                  valor=sesgo_real(t_vals),
                  tipo='Sesgo Real'
                ),
                list(
                  n=n,
                  valor=sesgo_teorico,
                  tipo='Sesgo Teórico'
                ),
                list(
                  n=n,
                  valor=varianza_real(t_vals),
                  tipo='Varianza Real'
                ),
                list(
                  n=n,
                  valor=varianza_teorica(n),
                  tipo='Varianza Teorica'
                ))
}

datos$tipo = as.factor(datos$tipo)

plot = ggplot(datos, aes(n, valor, color=tipo))+
  geom_line(linewidth=1)+
  theme_minimal()

ggplotly(plot)
