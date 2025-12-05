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

##########################################
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

datos = data.frame(n=c(), valor=c(), tipo=c())

for (n in 1:500){
  t_vals = rbinom(n, 1, calcular_p(Se,Sp,tita)) 
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
                  valor=varianza_real(t_vals,n),
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


##########################################
#Para θ = 0.25, Se = 0.9 y Sp = 0.95, construya muestras bootstrap para observar la distribuci´on del estimador de
#momentos de θ cuando n = 10. ¿Qué observa?

tita = 0.25
N_rep = 1000
Se=0.9
Sp= 0.95

p=calcular_p(Se,Sp,tita)

calcular_tita_mom= function(t_vals,Se=0.9,Sp=0.95){
  
  return((mean(t_vals)+Sp-1)/(Se+Sp-1))
}

muestras_bootstrap=rep(NaN,N_rep)

for (i in 1:N_rep){
  t_vals= rbinom(10,1,p) #n=10
  muestras_bootstrap[i]=calcular_tita_mom(t_vals)
} 
x=seq(min(muestras_bootstrap),max(muestras_bootstrap),length.out =500)
hist(muestras_bootstrap,freq=FALSE,main="Histograma de muestras bootstrap hat_tita_MOM")
lines(x,dnorm(x,mean(muestras_bootstrap),sd(muestras_bootstrap)),lwd=2)
#Ajustamos una normal porque el promedio se distribuye como una normal 
#y sumar y dividir por ctes solo modifica la forma de la normal


#faltan observaciones entre 0.3 y 0.4 porque el n=10 es muy chico
#El estimador solo puede tomar 11 valores distintos, debido a que mean(t_vals)
#Solo toma 11 valores distintos
tita_mom_values=rep(NaN,11)
j=1
for (i in 0:10){
  tita_mom_values[j+i]=(i/10+Sp-1)/(Se+Sp-1)
}

tita_mom_values #como vemos ninguno entre 0.3 y 0,4

#####################################
#9. Construya intervalos de confianza bootstrap percentil de θ basado en hat_θ_MoM. Para ello, realice simulaciones para
#θ = 0.25, Se = 0.9 y Sp = 0.95 y distintos valores de n.

N_rep=500
tita=0.25
Se=0.9
Sp=0.95

#donde vamos a almacenar los intervalos
inicios_percentil=c()
finales_percentil=c()
muestras_bootstrap=rep(NaN,N_rep)

calcular_intervalo_bootstrap_percentil=function(n,Se=0.9,Sp=0.95,tita=0.25,N_rep=10){
  muestras_bootstrap=rep(NaN,N_rep)
  p=calcular_p(Se,Sp,tita)
  for (i in 1:N_rep){
    t_vals= rbinom(n,1,p)
    
    muestras_bootstrap[i]=calcular_tita_mom(t_vals)
  }
  
  return(list(
    'inicio'= quantile(muestras_bootstrap,0.05),
    'fin' = quantile(muestras_bootstrap,0.95)
  ))
}



######################################
#10. Construya intervalos de confianza de nivel asint´otico 0.95 para θ basado en ˆθMoM.

calcular_intervalo_tita_mom=function(n,Se=0.9,Sp=0.95,tita=0.25,alfa=0.05){
  z_alfa=qnorm(alfa/2,lower.tail=FALSE) #cuantil a derecha
  
  p=calcular_p(Se,Sp,tita)
  
  t_vals=rbinom(n,1,p)
  
  tita_mom= calcular_tita_mom(t_vals)
  
  desvio_standar= sqrt((p*(1-p))/((Se+Sp-1)**2))
  
  return(list(
    'inicio'= tita_mom-z_alfa*desvio_standar/sqrt(n),
    'fin' = tita_mom+z_alfa*desvio_standar/sqrt(n)
  ))
}

####################################
#11. Con simulaciones, compare coberturas y longitudes promedio de los intevalos de confianza de los items anteriores.

n_s = c(10, 50, 100, 200, 500) # cantidad de muestras que vamos a tomar
tita=0.25
Se=0.9
Sp=0.95
N_rep
coverturas = data.frame(N=c(),n=c(), inicio=c(),final=c(),cubre=c(), tipo=c())
for (n in n_s){
  for (i in 1:N_rep){
    
    #Intervalo Boostrap percentil
    intervalo_percentil = calcular_intervalo_bootstrap_percentil(n)
    inicio = intervalo_percentil$inicio
    fin = intervalo_percentil$fin
    cubre= inicio <= tita && tita <= fin
    
    longitud = fin - inicio
    
    coverturas = rbind(coverturas, 
                       list('N'=i, 'n'=n, 'inicio'=inicio, 
                            'fin'=fin, 'cubre'=cubre, 'tipo'='percentil', 'longitud'=longitud))
    
    #Intervalo asintotico tita mom
    intervalo_tita_mom = calcular_intervalo_tita_mom(n)
    inicio = intervalo_tita_mom$inicio
    fin = intervalo_tita_mom$fin
    cubre = inicio <= tita && tita <= fin
    
    longitud = fin - inicio
    
    coverturas = rbind(coverturas, 
                       list('N'=i, 'n'=n, 'inicio'=inicio, 
                            'fin'=fin, 'cubre'=cubre, 'tipo'='tita_mom', 'longitud'=longitud))
  }
  }


for (n in n_s){
  
  porcentaje_covertura_percentil = sum(coverturas[coverturas$n == n & coverturas$tipo=='percentil', ]$cubre)/N_rep*100
  print(paste0('El porcentaje de covertura con ', n, ' muestras en ', N_rep, 'repeticiones para los intervalos bootrstrap percentil', 
               ' simulaciones es de ', porcentaje_covertura_percentil, '%'))
  
  
  porcentaje_covertura_tita_mom = sum(coverturas[coverturas$n == n & coverturas$tipo=='tita_mom', ]$cubre)/N_rep*100
  print(paste0('El porcentaje de covertura con ', n, ' muestras en ', N_rep,  'repeticiones para los intervalos basados en tita mom',
               ' simulaciones es de ', porcentaje_covertura_tita_mom, '%'))
  
  # Longitudes promedio
  long_prom_boot =
    mean(coverturas[coverturas$n == n & coverturas$tipo=='percentil', ]$longitud)
  
  long_prom_mom =
    mean(coverturas[coverturas$n == n & coverturas$tipo=='tita_mom', ]$longitud)
  
  print(paste0(
    'Longitud promedio bootstrap (n=', n, '): ', long_prom_boot
  ))
  
  print(paste0(
    'Longitud promedio MOM (n=', n, '): ', long_prom_mom
  ))
  
  
  cat("\n")
  
}

View(coverturas)






