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

Coberturas <- data.frame(N=c(), n=c(), titahat=c(), inicio=c(), fin=c(), cubre=c())

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
    Coberturas = rbind(Coberturas, 
                       list('N'=i, 'n'=n, 'titahat'=tita_hat, 'inicio'=inicio, 
                            'fin'=fin, 'cubre'=cubre))
    
  }
}

# graficamos

Coberturas_graph = Coberturas[Coberturas$N <= 100, ]

plot = ggplot(Coberturas_graph, aes(y = N, x=inicio, xend=fin, color=cubre, frame=n))+
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

# imprimimos por consola el porcentaje de Cobertura
for (n in n_s){
  porcentaje_Cobertura = sum(Coberturas[Coberturas$n == n, ]$cubre)/N*100
  print(paste0('El porcentaje de Cobertura con ', n, ' muestras en ', N, 
               ' simulaciones es de ', porcentaje_Cobertura, '%'))
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
coberturas = data.frame(N=c(),n=c(), inicio=c(),final=c(),
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
    
    coberturas = rbind(coberturas, 
                       list('N'=i, 'n'=n, 'inicio'=inicio, 
                            'fin'=fin, 'cubre'=cubre, 'tipo'='percentil', 
                            'longitud'=longitud, tita_generador=tita_hat))
    
    #Intervalo asintotico tita mom
    intervalo_asintotico = calcular_intervalo_asintotico(tita_hat, n)
    inicio = unname(intervalo_asintotico$inicio)
    fin = unname(intervalo_asintotico$fin)
    cubre = inicio <= tita && tita <= fin
    
    longitud = fin - inicio
    
    coberturas = rbind(coberturas, 
                       list('N'=i, 'n'=n, 'inicio'=inicio, 
                            'fin'=fin, 'cubre'=cubre, 'tipo'='asintotico', 
                            'longitud'=longitud, tita_generador=tita_hat))
  }
}

coberturas$tipo = as.factor(coberturas$tipo)


for (n in n_s){
  
  porcentaje_cobertura_percentil = 
    sum(coberturas[coberturas$n == n & coberturas$tipo=='percentil', ]$cubre)/
    N_rep*100
  
  print(paste0('El porcentaje de cobertura con ', n, ' muestras en ', N_rep,
               ' repeticiones para los intervalos bootrstrap percentil es de ',
               porcentaje_cobertura_percentil, '%'))
  
  
  porcentaje_cobertura_asintotico = 
    sum(coberturas[coberturas$n == n & coberturas$tipo=='asintotico', ]$cubre)/
    N_rep*100
  
  print(paste0('El porcentaje de cobertura con ', n, ' muestras en ', N_rep,  
               ' repeticiones para los intervalos asintóticos basados en tita mom es de ',
               porcentaje_cobertura_asintotico, '%'))
  
  # Longitudes promedio
  long_prom_percentil =
    mean(coberturas[coberturas$n == n & coberturas$tipo=='percentil', ]$longitud)
  
  long_prom_asintotico =
    mean(coberturas[coberturas$n == n & coberturas$tipo=='asintotico', ]$longitud)
  
  print(paste0(
    'Longitud promedio bootstrap (n=', n, '): ', long_prom_percentil
  ))
  
  print(paste0(
    'Longitud promedio asintótico (n=', n, '): ', long_prom_asintotico
  ))
  
  
  print("\n")
  
}


## vamos a filtrar los casos de longitud 0 y solo mostrar algunos casos

coberturas_graph_percentil = 
  coberturas[coberturas$N <= 100 & coberturas$longitud > 0 & coberturas$tipo == 'percentil', ]

coberturas_graph_asintotico = 
  coberturas[coberturas$N <= 100 & coberturas$longitud > 0 & coberturas$tipo == 'asintotico', ]

# grafico percentil
plot = ggplot(coberturas_graph_percentil, aes(x= inicio, xend=fin, y= N, color=cubre, frame=n))+
  geom_segment(aes(yend=N))+
  geom_vline(xintercept=tita, linetype='dashed', color="black")+
  labs(
    title='Cobertura del intervalo Bootstrap Percentil',
    x='Intervalo',
    y='Simulación',
    color='Cubre?'
  )+
  theme_minimal()

ggplotly(plot)

#grafico asintotico
plot = ggplot(coberturas_graph_asintotico,
              aes(x= inicio, xend=fin, y= N, color=cubre, frame=n))+
  geom_segment(aes(yend=N))+
  geom_vline(xintercept=tita, linetype='dashed', color="black")+
  labs(
    title='Cobertura del intervalo Bootstrap asintótico',
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
# 12: titas fuera del intervalo [0, 1]
sum(coberturas$tita_generador < 0 | coberturas$tita_generador > 1)/2

# hubo 24 casos en los que el tita generador dio fuera del intervalo, en las 500 simulaciones
# (divido por dos porque cada tita generador aparece dos veces, una para el
# asintótico y otra para el percentil de la misma simulación)

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
num_simulaciones <- 500 # si cambiamos estos números hay que cambiarlo tmb en el latex
num_muestras <- 100
p <- calcular_p(Se, Sp, theta_verdadero)

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

## calculando la distribución asintótica
N = 1000
n = 1000

#muestra
tita_hats = simular_estimador(N, n, theta_verdadero, Se, Sp)
tita_hats = data.frame(tita_hat = tita_hats)

# vemos que es normal
tita_hats$asintotico=(tita_hats$tita_hat-theta_verdadero)*sqrt(N)

ggplot(tita_hats, aes(sample = asintotico)) +
  stat_qq() +
  stat_qq_line(color = "red")+
  theme_minimal()+
  labs(
    title="QQ-Norm de la distribucion asintotica de los estimadores de tita",
    x='Cuantiles Teóricos',
    y='Cuantiles Reales'
  )



#luego, vemos la distribución y superponemos una normal
sd_tita_hat = sd(tita_hats$tita_hat*sqrt(N))
mean_tita_hat = mean((tita_hats$tita_hat-theta_verdadero)*sqrt(N))

ggplot(tita_hats, aes(x=asintotico))+
  geom_histogram(aes(y=after_stat(density)), bins = 16,
                     fill='pink', color='black')+
  stat_function(
  fun = dnorm,
  args = list(mean = mean_tita_hat, sd_tita_hat),
  color = "royalblue",
  linewidth = 1,
  linetype = "dashed"
)+
  labs(
    title = "Distribución asintótica del estimador truncado",
    x = "Valor del estimador Bootstrap",
    y = "Densidad"
  )+
  theme_minimal()

sd_tita_hat
mean_tita_hat
################################################
##                Parte 3.1
################################################

## 2. Aplique a un caso ficticio con npre = npost = 100, Se = 0.9, Sp = 0.95, θpre = 0.2 
#y θpost = 0.15 y α = 0.05. ¿Qué ocurre si achicamos los tamaños de muestra?

calcular_t_obs = function(muestra_post, muestra_prev, Se=0.9, Sp=0.95){
  n=length(muestra_post)
  X_raya=mean(muestra_post-muestra_prev)/(Se+Sp-1)
  #le sumamos un epsilon para prevenir divisiones por 0
  var_X=(var(muestra_post)+var(muestra_prev))/((Se+Sp-1)**2)+0.000000001
  return(sqrt(n)*abs(X_raya)/sqrt(var_X))
}

ns=c(10,20,50,70,100)
Se=0.9
Sp=0.95
theta_post=0.15
theta_prev=0.20
alpha=0.05


z_alpha=qnorm(1-alpha/2)
p_prev=calcular_p(Se,Sp,theta_prev)
p_post=calcular_p(Se,Sp,theta_post)

for (n in ns){
  muestra_prev=rbinom(n,1,p_prev)
  muestra_post=rbinom(n,1,p_post)
  t_obs=calcular_t_obs(muestra_post,muestra_prev,Se,Sp)
  print(paste0("El t observado es ",t_obs))
  if (t_obs>=z_alpha){
   print(paste0("Tenemos suficiente evidencia estadistica para rechazar H0 con nivel ",alpha," con ",n," muestras"))
  }
  else{
    print(paste0("NO tenemos suficiente evidencia estadistica para rechazar H0 con nivel ",alpha," con ",n," muestras"))
  }
}
##############################
## 3. Construya instervalos de confianza de nivel asintótico 0.95 para ∆.

ns=c(10,20,50,70,100,200,500)
Se=0.9
Sp=0.95
theta_post=0.15
theta_prev=0.20
alpha=0.05
N_rep = 500


z_alpha=qnorm(1-alpha/2)
p_prev=calcular_p(Se,Sp,theta_prev)
p_post=calcular_p(Se,Sp,theta_post)


invtervalo_asintotico_test=
  function(muestra_post, muestra_prev, Se=0.9, Sp=0.95, alpha=0.05){
    z_alpha=qnorm(1-alpha/2)
    n=length(muestra_post)
    X_raya=mean(muestra_post-muestra_prev)/(Se+Sp-1)
    #le sumamos un epsilon para prevenir divisiones por 0
    var_X=(var(muestra_post)+var(muestra_prev))/((Se+Sp-1)**2)+0.000000001
    return(list(
      'inicio'= X_raya - z_alpha*sqrt(var_X)/sqrt(n),
      'fin' = X_raya + z_alpha*sqrt(var_X)/sqrt(n)
    ))
    
  
  }

#Miremos su cobertura
coberturas <- data.frame(N=c(), n=c(), inicio=c(), fin=c(), cubre=c())
valor_real=theta_post-theta_prev


# hacemos las simulaciones
for (n in ns){ 
  for (i in 1:N_rep){
    muestra_prev=rbinom(n,1,p_prev)
    muestra_post=rbinom(n,1,p_post)
    intervalo = invtervalo_asintotico_test(muestra_post,muestra_prev)
    inicio = intervalo$inicio
    fin = intervalo$fin
    cubre = inicio <= valor_real && valor_real <= fin
    
    # guardamos los datos
    coberturas = rbind(coberturas, 
                       list('N'=i, 'n'=n, 'inicio'=inicio, 
                            'fin'=fin, 'cubre'=cubre))
    
  }
}

# graficamos

coberturas_graph = coberturas[coberturas$N <= 100, ]

plot = ggplot(coberturas_graph, aes(y = N, x=inicio, xend=fin, color=cubre, frame=n))+
  geom_segment(aes(yend=N), linewidth=1)+
  geom_vline(xintercept=valor_real, linetype='dashed', color="black")+
  scale_color_manual(values = c("tomato", "royalblue"), labels = c("No cubre", "Cubre"))+
  labs(
    title="Intervalos de confianza para delta, \nde las primeras 100 simulaciones",
    x = "Valor de delta",
    y = "Número de simulación",
    color="Cubre?"
  )+
  theme_minimal()+
  theme(axis.text.y = element_blank())

ggplotly(plot)

for (n in ns){
  porcentaje_cobertura = sum(coberturas[coberturas$n == n, ]$cubre)/N_rep*100
  print(paste0('El porcentaje de cobertura con ', n, ' muestras en ', N_rep, 
               ' simulaciones es de ', porcentaje_cobertura, '%'))
}
################################
#4.Fijado el tamaño de muestras npre y npost, calcule el nivel empírico del test, es decir, 
#genere N rep muestras con los tamaños establecidos y calcule la proporción de 
#rechazos a nivel 0.05. Utilice tamaños de muesras pequeños y grandes.

calcular_t_obs=function(muestra_post,muestra_prev,Se=0.9,Sp=0.95){
  n=length(muestra_post)
  X_raya=mean(muestra_post-muestra_prev)/(Se+Sp-1)
  #le sumamos un epsilon para prevenir divisiones por 0
  var_X=(var(muestra_post)+var(muestra_prev))/((Se+Sp-1)**2)+0.000000001
  return(sqrt(n)*abs(X_raya)/sqrt(var_X))
}

ns=c(10,20,50,70,100,200,300,400,500,600,700,800,900,1000)
Se=0.9
Sp=0.95
#tomamos theta post = theta prev ya que el nivel se ve bajo H0
theta_post=0.20
theta_prev=0.20 
alpha=0.05


z_alpha=qnorm(1-alpha/2)
p_prev=calcular_p(Se,Sp,theta_prev)
p_post=calcular_p(Se,Sp,theta_post)

N_rep=1000
niveles=rep(NaN,length(ns))
j=1
for (n in ns){
  
  rechazos=0
  
  for (i in 1:N_rep){
    muestra_prev=rbinom(n,1,p_prev)
    muestra_post=rbinom(n,1,p_post)
    t_obs=calcular_t_obs(muestra_post,muestra_prev,Se,Sp)
    if (t_obs>=z_alpha){
      rechazos=rechazos+1
    }
  }
  niveles[j]=rechazos/N_rep
  j=j+1
  print(paste0('El nivel empírico del test con ', n, ' muestras en ', N_rep, 
               ' simulaciones es de ',rechazos/N_rep))
}


datos = data.frame(n = ns, nivel=niveles)

plot = ggplot(datos, aes(n, nivel))+
  geom_line(linewidth = 1, color='seagreen')+
  geom_point(color='seagreen')+
  geom_hline(yintercept=0.05, linetype="dashed")+
  theme_minimal()+
  labs(
    title='Nivel empírico del test según n',
    y='Nivel del test',
    x='Valor de n'
  )

ggplotly(plot)


