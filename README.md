# DesarrolloPreguntasForLoop.R
#1.-Cargue las bases de datos incoporando en cada una de ellas la variable “tamanio”,
#donde indique de que tamaño es la empresa de ese país.
rm(list = ls())
Datos_Chile <- 0
  Datos_Colombia <- 0
Datos_Peru <- 0
Ingreso_Chile <- 0 
Ingreso_Colombia <- 0
Ingreso_Peru <- 0

Tamaños <- c("grandes","medianas","pequena", "micro")
Paises <- c("chile","colombia","peru")
Data <- NULL
Direccion <- "C://Users//Usuario//Desktop//excel//"

for (Tamaño in Tamaños) {
  for (Pais in Paises) {
 Direccion_Actual <- paste0(Direccion,Tamaño,"_",Pais,".csv")
 Datos_Actual <- read.csv(Direccion_Actual,sep = ";",dec = ",")
 #1.-
 Datos_Actual$tamanio <- Tamaño
 
 ##Hago una correccion en el nombre de la 5ta columna porque los datos no cuadran
 
 colnames(Datos_Actual)[5] <- "porcentaje_mujeres"

 ##3.-En esta parte del ciclo contare cuantos datos tiene cada pais:
 if (Pais=="chile"){ 
   Datos_Actual$Variable <- Datos_Actual$tasa_interes *0.1
   Datos_Chile <- Datos_Chile + nrow(Datos_Actual)
   Ingreso_Chile <- Ingreso_Chile + sum(Datos_Actual$ingresos)
 }
 if (Pais=="colombia"){ 
   Datos_Actual$Variable <- Datos_Actual$tasa_interes / 10
   Datos_Colombia <- Datos_Colombia + nrow(Datos_Actual)
   Ingreso_Colombia <- Ingreso_Colombia +  sum(Datos_Actual$ingresos)
 }
 if (Pais=="peru"){ 
   Datos_Actual$Variable <- Datos_Actual$tasa_interes + 0.3
   Datos_Peru <- Datos_Peru + nrow(Datos_Actual)
   Ingreso_Peru <- Ingreso_Peru + sum(Datos_Actual$ingresos)
 }
 Data <- rbind(Data,Datos_Actual)
  }
}

#2.-Reuna todas las bases en una sola y defina de qué tipología (tipo de datos) son cada
#una de las variables que se encuentran en la data.
#columna 1.-"fecha"<- string.
#Columna 2.-"pais"<- string.
#Columna 3.-"Ingreso"<- double.
#columna 4.-"costos"<- double.
#Columna 5.-"porcentaje_Mujeres"<- double.
#Columna 6.-"exportaciones<- double.
#columna 7.-"importaciones"<- double.
#Columna 8.-"endeudamiento"<- double.
#Columna 9.-"morosidad"<- double.
#columna 10.-"reservas"<- double.
#Columna 11.-"spread"<- double.
#Columna 12.-"tasa_interes<- double.
#columna 13.-"tamanio"<- string.


#3.-Determine a través del uso de condicionales y/o for cuántas obervaciones tiene Peru
#versus Chile.
if( Datos_Peru > Datos_Chile){
  print(paste("Los datos de peru son", Datos_Peru," y los datos de chile son ", Datos_Chile
              ," Por lo tanto los datos de peru son mas que los de chile." )) 
}else
if( Datos_Peru < Datos_Chile){
  print(paste("Los datos de peru son", Datos_Peru," y los datos de chile son ", Datos_Chile
              ," Por lo tanto los datos de peru son menos que los de chile." )) 
}else
if( Datos_Peru = Datos_Chile){
  print(paste("Los datos de peru son", Datos_Peru," y los datos de chile son ", Datos_Chile
              ," Por lo tanto los datos de peru son iguales a los de chile." )) 
}


#4.- Determine a través del uso de condicionales y/o for ¿cuál es el país con mayor
#ingresos de explotación para los años que considera la muestra.

Ingreso_Chile
Ingreso_Colombia
Ingreso_Peru

if(Ingreso_Chile > Ingreso_Colombia && Ingreso_Chile > Ingreso_Peru){
  print(paste("el ingreso de chile es ", Ingreso_Chile ," y es mayor a los de peru con 
              ",Ingreso_Peru ," y colombia con ", Ingreso_Colombia ," respectivamente "))
}else
if(Ingreso_Colombia > Ingreso_Chile && Ingreso_Colombia > Ingreso_Peru){
  print(paste("el ingreso de colombia es ", Ingreso_Colombia ," y es mayor a los de peru con 
              ",Ingreso_Peru ," y chile con ", Ingreso_Chile ," respectivamente "))
}else
if(Ingreso_Peru > Ingreso_Chile && Ingreso_Peru > Ingreso_Colombia){
  print(paste("el ingreso de peru es ", Ingreso_Peru ," y es mayor a los de Colombia con 
              ",Ingreso_Colombia ," y chile con ", Ingreso_Chile ," respectivamente "))
}


#5.-Genere una variable(columna) , donde si el país es Chile multiplique la tasa de interes
#por 0,1, cuando sea Peru le sume 0,3 y, y finalmente si es Colombia divida por 10 (2

#la columna inventada por mi , donde hice el calculo se llama "variable".
#porfavor revisar el codigo principal de la linea 29 a la 43.
Data$Variable



#6.-Reemplace en la columna exportaciones con 1 cuando es mayor a 2,1, con un 2
#cuando es menor 2,1y un 3 cuando es igual a 2,1, redondee al primer decimal la
#variable                                                                                   ptos).
Data$exportaciones <- ifelse( round( Data$exportaciones , digits = 1) > 2.1 , 1 , 
                      ifelse(round( Data$exportaciones , digits = 1) < 2.1 , 2 , 
                      ifelse(round( Data$exportaciones , digits = 1) == 2.1 , 3 , 0))) 

#BONUS TRACK
#7.- Gráfique algunas variables seleccionadas, las cuales puedan responder a una
#pregunta que se haga con respecto a los datos.

#en las grandes empresas de chile,¿podemos afirmar que existe una relacion entre la 
#morosidad y el porcentaje de mujeres? 

Data_chile <- subset(Data, pais == "chile" & tamanio == "grandes")

plot( Data_chile$morosidad ~ Data_chile$porcentaje_mujeres , type="p" )
abline(lm(Data_chile$morosidad ~ Data_chile$porcentaje_mujeres, data = Data_chile), col = "blue")

#podemos apreciar en el grafico que la nube de datos muestra un descenso;
#con los datos resultantes, se realiza una regresion lineal para determinar
#si efectivamente los datos descienden,
#la recta  que representa a los datos (linea azul en el grafico) tiene una pendiente
#negativa,por lo que podemos decir que a mayor porcentaje de mujeres en las empresas
#grandes chilenas, es menor la morosidad.
#por lo tanto si existe una relacion entre el porcentaje de mujeres y la morosidad.

