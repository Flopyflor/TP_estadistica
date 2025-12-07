# Trabajo práctico de estadística
# integrantes: 
#    Franco Vanotti
#    Emiliano Torres
#    Florencia Allami

library(ggplot2)
library(plotly)

set.seed(80)


################################################
##                Parte 1
################################################

################################################
# 5 - Suponga θ = 0.25. Evalúe coberturas empíricas mediante simulación Monte Carlo.
# Observe qué ocurre para distintos valores de n.

tita = 0.25
N = 500 # cantidad de simulaciones que vamos a hacer
n_s = c(10, 50, 100, 200, 500) # cantidad de muestras que vamos a tomar

calculo_intervalo = function(tita_hat, n){
  z = qnorm(0.025) # tomado por izquierda
  varianza = sqrt(tita_hat*(1-tita_hat))
  return(list(
    'inicio'= tita_hat + z*varianza/sqrt(n),
    'fin' = tita_hat - z*varianza/sqrt(n)
  ))
}

coverturas <- data.frame(N=c(), n=c(), titahat=c(), inicio=c(), fin=c(), cubre=c())

# hacemos las simulaciones
for (n in n_s){ 
  for (i in 1:N){
    datos = rbinom(n, 1, tita) # simulamos datos
    tita_hat = mean(datos) # estimamos tita
    
    intervalo = calculo_intervalo(tita_hat, n)
    inicio = intervalo$inicio
    fin = intervalo$fin
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
##                Parte 2.1
################################################

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
valores = seq(0.01, 0.99, length.out = 100)

calcular_p = function(Se, Sp, tita) {
  return(Se * tita + (1-Sp)*(1-tita))
}

valores_p = data.frame('x'=c(), 'p'=c(), variable=c())

# calculo p y lo guardo
for (variable in valores){
  p_by_Se = calcular_p(variable, Sp, tita)
  p_by_Sp = calcular_p(Se, variable, tita)
  p_by_tita = calcular_p(Se, Sp, variable)
  
  valores_p = rbind(valores_p, list('x'=variable, 'p'=p_by_Se, 'variable'='Se'),
                    list('x'=variable, 'p'=p_by_Sp, 'variable'='Sp'),
                    list('x'=variable, 'p'=p_by_tita, 'variable'='tita'))
}

valores_p$variable = as.factor(valores_p$variable)


# grafíco
plot = ggplot(valores_p, aes(x, p, color=variable))+
  geom_line(linewidth=1)+
  scale_color_manual(values = c("#00CD66", "tomato", 'skyblue2'), 
                     labels = c("Se", "Sp", "Tita"))+
  labs(
    title="p según las variables",
    x = "Valor de la variable",
    y = "p",
    color='Parámetro variable'
  )+
  theme_minimal()

plot = ggplotly(plot)

plot <- style(
  plot,
  name = "Tita", # Set the name for the first trace (which is 'red')
  traces = 3
)

plot



################################################
# 6. Grafíque el ECM en función de n y compárelo con el ECM del test perfecto

tita = 0.25
Se = 0.9
Sp = 0.95

ECM_imp = function(Se, Sp, tita, n){
  p = calcular_p(Se,Sp,tita)
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


################################################
# 7. Realice simulaciones para comparar los valores teóricos hallados 
# en el item 5 con los simulados.

tita = 0.25

#sesgo, varianza, ecm

sesgo_real = function(t_vals, Se=0.9, Sp=0.95){
  return(((mean(t_vals)+Sp-1)/(Se+Sp-1)) - tita) 
}

varianza_real = function(t_vals,n ,Se=0.9, Sp=0.95){
  return(var(t_vals)/(n*(Se+Sp-1)**2)) 
}


sesgo_teorico = 0

varianza_teorica = function(n, Se=0.9, Sp=0.95, tita=0.25){
  p = calcular_p(Se,Sp,tita)
  return(p*(1-p)/n/(Se+Sp-1)**2)
}

datos = data.frame(n=c(), valor=c(), Tipo=c())

for (n in 1:500){
  t_vals = rbinom(n, 1, calcular_p(Se,Sp,tita)) 
  datos = rbind(datos,
                list(
                  n=n,
                  valor=sesgo_real(t_vals),
                  Tipo='Sesgo Real'
                ),
                list(
                  n=n,
                  valor=sesgo_teorico,
                  Tipo='Sesgo Teórico'
                ),
                list(
                  n=n,
                  valor=varianza_real(t_vals,n),
                  Tipo='Varianza Real'
                ),
                list(
                  n=n,
                  valor=varianza_teorica(n),
                  Tipo='Varianza Teórica'
                ))
}

datos$tipo = as.factor(datos$Tipo)

datos_sesgo = datos[datos$Tipo == 'Sesgo Real' | datos$Tipo == 'Sesgo Teórico', ]
datos_var = datos[datos$Tipo == 'Varianza Real' | datos$Tipo == 'Varianza Teórica', ]

plot = ggplot(datos_sesgo, aes(n, valor, color=Tipo))+
  geom_line(linewidth=1)+
  scale_color_manual(values=c('tomato', 'darkolivegreen3'),
                     labels=c('Real', 'Teórica'))+
  labs(
    title= 'Comparación del Sesgo Real con el Teórico',
    color = 'Sesgo'
  )+
  theme_minimal()

ggplotly(plot)

plot = ggplot(datos_var, aes(n, valor, color=Tipo))+
  geom_line(linewidth=1)+
  scale_color_manual(values=c('orchid', 'skyblue'),
                     labels=c('Real', 'Teórica'))+
  labs(
    title= 'Comparación de la Varianza Real con la Teórica',
    color = 'Varianza'
  )+
  theme_minimal()

ggplotly(plot)


##########################################
#8. Para θ = 0.25, Se = 0.9 y Sp = 0.95, construya muestras bootstrap para observar la distribuci´on del estimador de
#momentos de θ cuando n = 10. ¿Qué observa?

# definiciones
tita = 0.25
N_rep = 1000
Se=0.9
Sp= 0.95
n = 10

p = calcular_p(Se,Sp,tita)

calcular_tita_mom = function(t_vals,Se=0.9,Sp=0.95){
  return((mean(t_vals)+Sp-1)/(Se+Sp-1))
}

# calculo el titahat que voy a usar para crear mis muestras bootstrap
muestra_original = rbinom(n, 1, p)
titahat = calcular_tita_mom(muestra_original)
phat = calcular_p(Se, Sp, titahat)

# inicializo el acumulador
muestras_bootstrap=rep(NaN,N_rep)

# creo mis muestras bootstrap, y calculo los estimadores
for (i in 1:N_rep){
  t_vals= rbinom(n, 1, phat)
  muestras_bootstrap[i]=calcular_tita_mom(t_vals)
} 

media_b = mean(muestras_bootstrap)
sd_b = sd(muestras_bootstrap)

muestras_bootstrap = as.data.frame(muestras_bootstrap)

ggplot(muestras_bootstrap, aes(x = muestras_bootstrap))+
  geom_histogram(aes(y=after_stat(density)), bins=10,
                 fill='lightblue', color='black')+
  stat_function(
    fun = dnorm,
    args = list(mean = media_b, sd = sd_b),
    color = "red",
    linewidth = 1,
    linetype = "dashed"
  ) +
  labs(
    title = "Distribución Bootstrap con Curva Normal Superpuesta",
    x = "Valor del estimador Bootstrap",
    y = "Densidad"
  )+
  theme_minimal()


#Ajustamos una normal porque el promedio se distribuye como una normal 
#y sumar y dividir por ctes solo modifica la forma de la normal

#los límitesd de la normal están dados por default por el máximo y mínimo
#de los resultados

################################################
#                 Parte 2.2
################################################

################################################
#9. Construya intervalos de confianza bootstrap percentil de θ basado en hat_θ_MoM. Para ello, realice simulaciones para
#θ = 0.25, Se = 0.9 y Sp = 0.95 y distintos valores de n.

# vamos a solo definir la función, corremos esta y la del 10 en el 11.


calcular_intervalo_bootstrap_percentil =
  function(tita_hat, n, Se=0.9, Sp=0.95, N_rep=1000){
    
    muestras_bootstrap=rep(NaN,N_rep)
    p_hat = calcular_p(Se,Sp,tita_hat)
    
    for (i in 1:N_rep){
      t_vals= rbinom(n,1,p_hat) #muestra bootstrap
      
      muestras_bootstrap[i]=calcular_tita_mom(t_vals) #estimador bootstrap
    }
  
  return(list(
    'inicio'= quantile(muestras_bootstrap,0.025),
    'fin' = quantile(muestras_bootstrap,0.975)
  ))
}


######################################
#10. Construya intervalos de confianza de nivel asintótico 0.95 para θ basado 
# en θMoMhat.

calcular_intervalo_asintotico=
  function(tita_hat, n, Se=0.9, Sp=0.95, alfa=0.05, N_rep=1000){
    z_alfa=qnorm(alfa/2,lower.tail=FALSE) #cuantil a derecha
    
    muestras_bootstrap=rep(NaN,N_rep)
    p_hat = calcular_p(Se,Sp,tita_hat)
    
    for (i in 1:N_rep){
      t_vals= rbinom(n,1,p_hat) #muestra bootstrap
      
      muestras_bootstrap[i]=calcular_tita_mom(t_vals) #estimador bootstrap
    }
    
    sd_bootstrap = 
      sqrt(mean((muestras_bootstrap-mean(muestras_bootstrap))**2))
    
    return(list(
      'inicio'= tita_hat-z_alfa*sd_bootstrap,
      'fin' = tita_hat+z_alfa*sd_bootstrap
    ))
  }


####################################
#11. Con simulaciones, compare coberturas y longitudes promedio de los 
#intevalos de confianza de los items anteriores.

#constantes
tita=0.25
Se=0.9
Sp=0.95
N_rep = 500 # si lo aumentás mucho más empieza a tardar en correr

p =  calcular_p(Se, Sp, tita)

n_s = c(10, 50, 100, 200, 500) # cantidad de muestras que vamos a tomar

#acumulador
coverturas = data.frame(N=c(),n=c(), inicio=c(),final=c(),
                        cubre=c(), tipo=c(), longitud=c(), tita_generador=c())

for (n in n_s){
  for (i in 1:N_rep){
    
    #obtengo un estimador original
    muestra_original = rbinom(n, 1, p)
    tita_hat = calcular_tita_mom(muestra_original)
    
    #Intervalo Boostrap percentil
    intervalo_percentil = calcular_intervalo_bootstrap_percentil(tita_hat, n)
    inicio = unname(intervalo_percentil$inicio)
    fin = unname(intervalo_percentil$fin)
    cubre= inicio <= tita && tita <= fin
    
    longitud = fin - inicio
    
    coverturas = rbind(coverturas, 
                       list('N'=i, 'n'=n, 'inicio'=inicio, 
                            'fin'=fin, 'cubre'=cubre, 'tipo'='percentil', 
                            'longitud'=longitud, tita_generador=tita_hat))
    
    #Intervalo asintotico tita mom
    intervalo_asintotico = calcular_intervalo_asintotico(tita_hat, n)
    inicio = unname(intervalo_asintotico$inicio)
    fin = unname(intervalo_asintotico$fin)
    cubre = inicio <= tita && tita <= fin
    
    longitud = fin - inicio
    
    coverturas = rbind(coverturas, 
                       list('N'=i, 'n'=n, 'inicio'=inicio, 
                            'fin'=fin, 'cubre'=cubre, 'tipo'='asintotico', 
                            'longitud'=longitud, tita_generador=tita_hat))
  }
}

coverturas$tipo = as.factor(coverturas$tipo)


for (n in n_s){
  
  porcentaje_covertura_percentil = 
    sum(coverturas[coverturas$n == n & coverturas$tipo=='percentil', ]$cubre)/
    N_rep*100
  
  print(paste0('El porcentaje de covertura con ', n, ' muestras en ', N_rep,
               ' repeticiones para los intervalos bootrstrap percentil es de ',
               porcentaje_covertura_percentil, '%'))
  
  
  porcentaje_covertura_asintotico = 
    sum(coverturas[coverturas$n == n & coverturas$tipo=='asintotico', ]$cubre)/
    N_rep*100
  
  print(paste0('El porcentaje de covertura con ', n, ' muestras en ', N_rep,  
               ' repeticiones para los intervalos asintóticos basados en tita mom es de ',
               porcentaje_covertura_asintotico, '%'))
  
  # Longitudes promedio
  long_prom_percentil =
    mean(coverturas[coverturas$n == n & coverturas$tipo=='percentil', ]$longitud)
  
  long_prom_asintotico =
    mean(coverturas[coverturas$n == n & coverturas$tipo=='asintotico', ]$longitud)
  
  print(paste0(
    'Longitud promedio bootstrap (n=', n, '): ', long_prom_percentil
  ))
  
  print(paste0(
    'Longitud promedio asintótico (n=', n, '): ', long_prom_asintotico
  ))
  
  
  print("\n")
  
}

View(coverturas)



## vamos a filtrar los casos de longitud 0 y solo mostrar algunos casos

coverturas_graph_percentil = 
  coverturas[coverturas$N <= 100 & coverturas$longitud > 0 & coverturas$tipo == 'percentil', ]

coverturas_graph_asintotico = 
  coverturas[coverturas$N <= 100 & coverturas$longitud > 0 & coverturas$tipo == 'asintotico', ]

# grafico percentil
plot = ggplot(coverturas_graph_percentil, aes(x= inicio, xend=fin, y= N, color=cubre, frame=n))+
  geom_segment(aes(yend=N))+
  geom_vline(xintercept=tita, linetype='dashed', color="black")+
  labs(
    title='Covertura del intervalo Bootstrap Percentil',
    x='Intervalo',
    y='Simulación',
    color='Cubre?'
  )+
  theme_minimal()

ggplotly(plot)

#grafico asintotico
plot = ggplot(coverturas_graph_asintotico,
              aes(x= inicio, xend=fin, y= N, color=cubre, frame=n))+
  geom_segment(aes(yend=N))+
  geom_vline(xintercept=tita, linetype='dashed', color="black")+
  labs(
    title='Covertura del intervalo Bootstrap asintótico',
    x='Intervalo',
    y='Simulación',
    color='Cubre?'
  )+
  theme_minimal()

ggplotly(plot)



################################################
##                Parte 2.3
################################################

####################################
# 13: truncado


# Función que simula una única muestra y devuelve el estimador truncado
simular_una_muestra <- function(n, theta, Se, Sp) {
  
  # Generar estados verdaderos (enfermo/no enfermo)
  verdaderos <- rbinom(n, 1, theta)
  
  # Generar resultado del test imperfecto
  test <- ifelse(
    verdaderos == 1,
    rbinom(n, 1, Se),       # enfermo -> test positivo con Se
    rbinom(n, 1, 1 - Sp)   # sano -> test falso positivo con (1 - Sp)
  )
  
  # 3) Calcular el estimador de momentos
  media_test <- mean(test)
  theta_momentos <- (media_test - 1 + Sp) / (Se + Sp - 1)
  
  # 4) Truncar al intervalo [0, 1]
  theta_truncado <- min(max(theta_momentos, 0), 1)
  
  return(theta_truncado)
}


# Función que repite la simulación muchas veces
simular_estimador <- function(num_simulaciones, n, theta, Se, Sp) {
  resultados <- replicate(
    num_simulaciones,
    simular_una_muestra(n, theta, Se, Sp)
  )
  return(resultados)
}

# Función que calcula métricas (media, sesgo, varianza, ECM)
calcular_metricas <- function(resultados, theta_verdadero) {
  media <- mean(resultados)
  varianza <- var(resultados)
  sesgo <- media - theta_verdadero
  ecm <- sesgo^2 + varianza
  
  return(list(
    media = media,
    sesgo = sesgo,
    varianza = varianza,
    ecm = ecm
  ))
}

# CORRER LAS SIMULACIONES

theta_verdadero <- 0.25
Se <- 0.9
Sp <- 0.95
num_simulaciones <- 100
num_muestras <- 100

metricas = data.frame(media=c(), sesgo=c(), varianza=c(), ecm=c(), n=c(), N=c())

for (n in c(10, 100, 1000)){
  for (i in 1:num_simulaciones){
    resultados = simular_estimador(num_muestras, n, theta_verdadero, Se, Sp)
    metricas_simulacion = calcular_metricas(resultados, theta_verdadero)
    metricas = rbind(metricas, list(
      media=metricas_simulacion$media,
      sesgo=metricas_simulacion$sesgo,
      varianza=metricas_simulacion$varianza,
      ecm=metricas_simulacion$ecm,
      n=n, N=i
      ))
    
  }
}

# Gráfico

plot = ggplot(metricas, aes(x=N, y=media, frame=n))+
  geom_point(color='skyblue')+
  geom_hline(yintercept=theta_verdadero, linetype='dashed', color='black')+
  labs(
    title='Sesgo del calculo de tita en distintas simulaciones',
    x='Simulación',
    y='Media de tita'
  )+
  theme_minimal()

ggplotly(plot)

plot = ggplot(metricas, aes(x=N, y=varianza, frame=n))+
  geom_point(color='tomato')+
  geom_hline(yintercept=0, linetype='dashed', color='black')+
  labs(
    title='Varianza del calculo de tita en distintas simulaciones',
    x='Simulación',
    y='Varianza de tita'
  )+
  theme_minimal()

ggplotly(plot)

plot = ggplot(metricas, aes(x=N, y=ecm, frame=n))+
  geom_point(color='darkolivegreen2')+
  geom_hline(yintercept=0, linetype='dashed', color='black')+
  labs(
    title='ECM del calculo de tita en distintas simulaciones',
    x='Simulación',
    y='ECM'
  )+
  theme_minimal()

ggplotly(plot)

# Notese la diferencia de escala entre el gráfico del sesgo y el de la varianza



