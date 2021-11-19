#Voy a hacer una prueba de t con una base precargada para ver que pasa
mod1 <- lm(trees$Volume~trees$Height*trees$Volume)
summary(mod1)
plot(trees$Volume~trees$Height*trees$Volume)
#Que loco, sí me hio la grafica pero la guardó en otro archivo directamente. Mejor usaré R studio para hacer estas cosas
