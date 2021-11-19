---
Tarea 12 de noviembre
---

#Cargamos los datos
setwd("/Users/Moy/Desktop")
data <- read.csv("Alumnos.csv")
#1) Las estudiantes mujeres tienen, en promedio, un promedio en la carrera de 9. Supongan una desviación estándar poblacional de 0.5.
gamma <- 0.95
alfa <- 1-gamma
sd.1 <- 0.5
mu0.1 <- 9
#Hipótesis estadísticas
#H0: mu1 mujeres == 9 (Igual a 9)
#H1: mu1 mujeres != 9 (Diferente de 9)
#Como tengo un contraste de hipótesis con igual y diferentes, será una prueba de dos colas.
#Como conozco la sigma, usaré una prueba de Z para una muestra
#Calculamos t

M.prom <- data$prom[data$sexo == "Mujer"] 
t.1 <- (mean(M.prom)-mu0.1)/(sd.1/sqrt(length(M.prom)))

#Calculamos las zonas de rechazo

ZR.Inf <- qnorm(alfa/2)
ZR.Sup <- qnorm(1-alfa/2)

#Graficamos las zonas de rechazo y vemos donde cae t
region.4.a=seq(ZR.Inf,ZR.Sup,0.01)  # Intervalo a sombrear
xNR <- c(ZR.Inf,region.4.a,ZR.Sup)  # Base de los polígonos que crean el efecto "sombra"
yNR <- c(0,dnorm(region.4.a),0)     # Altura de los polígonos sombreados
xR.Inf <- c(-5,seq(-5,ZR.Inf,length.out = 100),ZR.Inf)
yR.Inf <- c(0,dnorm(seq(-5,ZR.Inf,length.out = 100)),0)
xR.Sup <- c(ZR.Sup,seq(ZR.Sup, 5,length.out = 100),5)
yR.Sup <- c(0,dnorm(seq(ZR.Sup, 5,length.out = 100)),0)

plot(x= seq(-5,5,length.out= length(region.4.a)),
     y = dnorm(seq(-5,5,length.out = length(region.4.a))), type = "l", bty = "l", main = "Ejercicio 4. a.", ylab = "Probabilidad", xlab = "Z") 
polygon(xNR, yNR, col="blue")
polygon(xR.Inf, yR.Inf, col="red")
polygon(xR.Sup, yR.Sup, col="red")
text(x = c(0,-3.5,3.5), y = c(0.2,0.1,0.1), labels = c("Zona de \n no rechazo", "Zona de rechazo","Zona de rechazo"), col = c("white",1,1))
text(x = -4.5, y = 0.03, labels ="t.1", cex = 3, col = "purple")

#Como se puede ver, t.1 cae lejísimos de la zona de no rechazo, por lo tanto rechazamos H0 y concluimos que el promedio de las mujeres es diferente de 9 y además es mucho menor a este porque el valor de t.1 cayó en el lado de los negativos.
#2) Les estudiantes con pareja (edo.civil) tienen, en promedio, una edad mayor a 21 años. Supongan una desviación estándar poblacional de 1 año.

sd.2 <- 1
#Separamos los datos
edad.pareja <- data$edad[data$edo.civil == "Con"]
mu0.2 <- 21
#Hipótesis estadísticas
#H0: edad promedio de alumnos con pareja <= 21 (Menor o igual 21)
#H1: edad promedio de alumnos con pareja > 21 años(Mayor a 21 años)
#Como tengo un contraste de hipótesis mayor que y menor que, será una prueba de una cola.
#Como conozco la sigma, usaré una prueba de Z para una muestra
#Calculamos t

t.2 <- (mean(edad.pareja)-mu0.2)/(sd.2/sqrt(length(edad.pareja)))

ZR.Inf.2 <- -5
ZR.Sup.2 <- qnorm(1-alfa)
#Graficamos las zonas de rechazo y vemos donde cae t
region=seq(ZR.Inf.2,ZR.Sup.2,0.01)  # Intervalo a sombrear
xNR <- c(ZR.Inf.2,region,ZR.Sup.2)  # Base de los polígonos que crean el efecto "sombra"
yNR <- c(0,dnorm(region),0)     # Altura de los polígonos sombreados
xR.Sup <- c(ZR.Sup.2,seq(ZR.Sup.2, 5,length.out = 100),5)
yR.Sup <- c(0,dnorm(seq(ZR.Sup.2, 5,length.out = 100)),0)

plot(x= seq(-5,5,length.out= length(region.4.a)),
     y = dnorm(seq(-5,5,length.out = length(region.4.a))), type = "l", bty = "l", main = "Ejercicio 4. a.", ylab = "Probabilidad", xlab = "Z") 
polygon(xNR, yNR, col="blue")
polygon(xR.Sup, yR.Sup, col="red")
text(x = c(0,3.5), y = c(0.2,0.1), labels = c("Zona de \n no rechazo","Zona de rechazo"), col = c("white",1))
text(x = t.2, y = 0.03, labels ="←", cex = 2, col = "purple", srt = 90)
text(x = t.2, y = 0.06, labels ="t.1", cex = 2, col = "purple")
#3) Les estudiantes de laboratorio tienen, en promedio, un peso mayor a les estudiantes de campo. Supongan una desviación estándar poblacional para ambos conjuntos de estudiantes de 2 kg.
#4) Los estudiantes hombres tienen, en promedio, una estatura mayor que las estudiantes mujeres. Supongan una desviación estándar poblacional para hombres de 0.15 m y para mujeres de 0.13 m.

#Función que no arroja una gráfica de distribución de probabilidad que está dividida en zonas de rechazo y nor echazo de la hipótesis nula y que coloca al estadístico de prueba t en el sitio que le corresponde. Puede ser para pruebas de una o dos poblaciones.

plot.norm.test <- function (data, data2 = NULL, mu0 = NULL, gamma = 0.95, sigma = NULL, sigma2 = NULL, colas = "ambas", main = "", ylab ="", xlab ="", col.NR ="blue", col.R = "red", col.t = "black") {
	media <- mean(data)
	alfa = 1-gamma
	if(is.null(data2)) {
		est.prueb <- (mean(data)-mu0)/(sigma/sqrt(length(data)))
		if(colas == "izquierda"){
			ZR.Inf <- -5
			ZR.Sup <- qnorm(alfa)
			zona.NR=seq(ZR.Sup,5,length.out = 100)
			xNR <- c(ZR.Sup,zona.NR,5)
			yNR <- c(0,dnorm(zona.NR),0)
			xR <- c(-5,seq(-5,ZR.Sup,length.out = 100),ZR.Sup)
			yR <- c(0,dnorm(seq(-5,ZR.Sup,length.out = 100)),0)
			plot(x= seq(-5,5,length.out= length(zona.NR)), y = dnorm(seq(-5,5,length.out = length(zona.NR))), type = "l", bty = "l", main = main, ylab = ylab, xlab = xlab)
			polygon(xNR, yNR, col= col.NR)
			polygon(xR, yR, col= col.R)
			text(x = c(0,-3.5), y = c(0.2,0.1), labels = c("Zona de \n no rechazo", "Zona de rechazo"), col = c("white",1))
			if(est.prueb < -5){
				text(x = -4.5, y = 0.06, labels ="t", cex = 2, col = "black")
				text(x = -4.5, y = 0.03, labels ="<-", cex = 2, col = "black", srt = 90)
			}else {
				text(x = est.prueb, y = 0.06, labels ="t", cex = 2, col = "black")
				text(x = est.prueb, y = 0.03, labels ="<-", cex = 2, col = "black", srt = 90)
 			}
		}else if (colas == "derecha"){
			ZR.Inf <- qnorm(1-alfa)
			ZR.Sup <- 5
			zona.NR=seq(-5,ZR.Inf,length.out = 100)
			xNR <- c(-5,zona.NR,ZR.Inf)
			yNR <- c(0,dnorm(zona.NR),0)
			xR <- c(ZR.Inf,seq(ZR.Inf, 5,length.out = 100),5)
			yR <- c(0,dnorm(seq(ZR.Inf, 5,length.out = 100)),0)
			plot(x= seq(-5,5,length.out= length(zona.NR)), y = dnorm(seq(-5,5,length.out = length(zona.NR))), type = "l", bty = "l", main = main, ylab = ylab, xlab = xlab)
			polygon(xNR, yNR, col= col.NR)
			polygon(xR, yR, col= col.R)
			text(x = c(0,3.5), y = c(0.2,0.1), labels = c("Zona de \n no rechazo", "Zona de rechazo"), col = c("white",1))
			if(est.prueb > 5){
				text(x = 4.5, y = 0.06, labels ="t", cex = 2, col = "black")
				text(x = 4.5, y = 0.03, labels ="<-", cex = 2, col = "black", srt = 90)
			} else {
				text(x = est.prueb, y = 0.06, labels ="t", cex = 2, col = "black")
				text(x = est.prueb, y = 0.03, labels ="<-", cex = 2, col = "black", srt = 90)
 			}
		}else if (colas == "ambas"){
			ZR.Inf <- qnorm(alfa/2)
			ZR.Sup <- qnorm(1-alfa/2)
			zona.NR=seq(ZR.Inf,ZR.Sup,length.out = 100)
			xNR <- c(ZR.Inf,zona.NR,ZR.Sup)
			yNR <- c(0,dnorm(zona.NR),0)
			xR.Inf <- c(-5,seq(-5,ZR.Inf,length.out = 100),ZR.Inf)
			yR.Inf <- c(0,dnorm(seq(-5,ZR.Inf,length.out = 100)),0)
			xR.Sup <- c(ZR.Sup,seq(ZR.Sup, 5,length.out = 100),5)
			yR.Sup <- c(0,dnorm(seq(ZR.Sup, 5,length.out = 100)),0)	
			plot(x= seq(-5,5,length.out= length(zona.NR)), y = dnorm(seq(-5,5,length.out = length(zona.NR))), type = "l", bty = "l", main = main, ylab = ylab, xlab = xlab)
			polygon(xNR, yNR, col= col.NR)
			polygon(xR.Inf, yR.Inf, col= col.R)
			polygon(xR.Sup, yR.Sup, col= col.R)
			text(x = c(0,-3.5,3.5), y = c(0.2,0.1,0.1), labels = c("Zona de \n no rechazo", "Zona de rechazo","Zona de rechazo"), col = c("white",1,1))
			if(est.prueb < -5){
				text(x = -4.5, y = 0.06, labels ="t", cex = 2, col = col.t)
				text(x = -4.5, y = 0.03, labels ="<-", cex = 2, col = col.t, srt = 90)
			}else if(est.prueb > 5){
				text(x = 4.5, y = 0.06, labels ="t", cex = 2, col = col.t)
				text(x = 4.5, y = 0.03, labels ="<-", cex = 2, col = col.t, srt = 90)
			}else {
				text(x = est.prueb, y = 0.06, labels ="t", cex = 2, col = col.t)
				text(x = est.prueb, y = 0.03, labels ="<-", cex = 2, col = col.t, srt = 90)
 			}
		}
	} else {
		est.prueb <- (mean(data)-mean(data2))/sqrt((sigma/length(data))+(sigma2/length(data2)))
			if(colas == "izquierda"){
			ZR.Inf <- -5
			ZR.Sup <- qnorm(alfa)
			zona.NR=seq(ZR.Sup,5,length.out = 100)
			xNR <- c(ZR.Sup,zona.NR,5)
			yNR <- c(0,dnorm(zona.NR),0)
			xR <- c(-5,seq(-5,ZR.Sup,length.out = 100),ZR.Sup)
			yR <- c(0,dnorm(seq(-5,ZR.Sup,length.out = 100)),0)
			plot(x= seq(-5,5,length.out= length(zona.NR)), y = dnorm(seq(-5,5,length.out = length(zona.NR))), type = "l", bty = "l", main = main, ylab = ylab, xlab = xlab)
			polygon(xNR, yNR, col= col.NR)
			polygon(xR, yR, col= col.R)
			text(x = c(0,-3.5), y = c(0.2,0.1), labels = c("Zona de \n no rechazo", "Zona de rechazo"), col = c("white",1))
			if(est.prueb < -5){
				text(x = -4.5, y = 0.06, labels ="t", cex = 2, col = "black")
				text(x = -4.5, y = 0.03, labels ="<-", cex = 2, col = "black", srt = 90)
			}else {
				text(x = est.prueb, y = 0.06, labels ="t", cex = 2, col = "black")
				text(x = est.prueb, y = 0.03, labels ="<-", cex = 2, col = "black", srt = 90)
 			}
		}else if (colas == "derecha"){
			ZR.Inf <- qnorm(1-alfa)
			ZR.Sup <- 5
			zona.NR=seq(-5,ZR.Inf,length.out = 100)
			xNR <- c(-5,zona.NR,ZR.Inf)
			yNR <- c(0,dnorm(zona.NR),0)
			xR <- c(ZR.Inf,seq(ZR.Inf, 5,length.out = 100),5)
			yR <- c(0,dnorm(seq(ZR.Inf, 5,length.out = 100)),0)
			plot(x= seq(-5,5,length.out= length(zona.NR)), y = dnorm(seq(-5,5,length.out = length(zona.NR))), type = "l", bty = "l", main = main, ylab = ylab, xlab = xlab)
			polygon(xNR, yNR, col= col.NR)
			polygon(xR, yR, col= col.R)
			text(x = c(0,3.5), y = c(0.2,0.1), labels = c("Zona de \n no rechazo", "Zona de rechazo"), col = c("white",1))
			if(est.prueb > 5){
				text(x = 4.5, y = 0.06, labels ="t", cex = 2, col = "black")
				text(x = 4.5, y = 0.03, labels ="<-", cex = 2, col = "black", srt = 90)
			} else {
				text(x = est.prueb, y = 0.06, labels ="t", cex = 2, col = "black")
				text(x = est.prueb, y = 0.03, labels ="<-", cex = 2, col = "black", srt = 90)
 			}
		}else if (colas == "ambas"){
			ZR.Inf <- qnorm(alfa/2)
			ZR.Sup <- qnorm(1-alfa/2)
			zona.NR=seq(ZR.Inf,ZR.Sup,length.out = 100)
			xNR <- c(ZR.Inf,zona.NR,ZR.Sup)
			yNR <- c(0,dnorm(zona.NR),0)
			xR.Inf <- c(-5,seq(-5,ZR.Inf,length.out = 100),ZR.Inf)
			yR.Inf <- c(0,dnorm(seq(-5,ZR.Inf,length.out = 100)),0)
			xR.Sup <- c(ZR.Sup,seq(ZR.Sup, 5,length.out = 100),5)
			yR.Sup <- c(0,dnorm(seq(ZR.Sup, 5,length.out = 100)),0)	
			plot(x= seq(-5,5,length.out= length(zona.NR)), y = dnorm(seq(-5,5,length.out = length(zona.NR))), type = "l", bty = "l", main = main, ylab = ylab, xlab = xlab)
			polygon(xNR, yNR, col= col.NR)
			polygon(xR.Inf, yR.Inf, col= col.R)
			polygon(xR.Sup, yR.Sup, col= col.R)
			text(x = c(0,-3.5,3.5), y = c(0.2,0.1,0.1), labels = c("Zona de \n no rechazo", "Zona de rechazo","Zona de rechazo"), col = c("white",1,1))
			if(est.prueb < -5){
				text(x = -4.5, y = 0.06, labels ="t", cex = 2, col = col.t)
				text(x = -4.5, y = 0.03, labels ="<-", cex = 2, col = col.t, srt = 90)
			}else if(est.prueb > 5){
				text(x = 4.5, y = 0.06, labels ="t", cex = 2, col = col.t)
				text(x = 4.5, y = 0.03, labels ="<-", cex = 2, col = col.t, srt = 90)
			}else {
				text(x = est.prueb, y = 0.06, labels ="t", cex = 2, col = col.t)
				text(x = est.prueb, y = 0.03, labels ="<-", cex = 2, col = col.t, srt = 90)
 			}
		}
	}
} 