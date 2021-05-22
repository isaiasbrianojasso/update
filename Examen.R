# --------------------------------------------------------------
# Examen Parte 2
# Datos 2
# Jose Isaias Briano Jasso
# Script en R
# --------------------------------------------------------------

datos<-c(116,136,126,118,137,125,139,128,134,128,119,136,116,125,136,135,120,140,120,123,115,139,138,136,136,134,131,135,130,125,123,127,135,131,130,120,137,117,115,137,138,120,120,132,120,130,125,126,130,123,138,129,124,134,119,123,120,140,138,116,127,122,123,137,139,115,121,140,116,133,130,139,131,132,125,127,120,120,130,130,116,135,130,136,130,132,133,115,133,124,127,136,115,134,122,133,130,134,136,138)
#View(datos)
#attach(datos)
#plot(x,datos)
ultimo<-tail(datos,32)
hist(ultimo)
curve(dnorm(x,3,2),add=TRUE)
k=nclass.Sturges(datos)
rango=ceiling(max(datos)-min(ultimo))
amplitud=round(rango/k,1) #amplitud del intervalo
rango.corregido=abs(round(amplitud*k-rango,1))
lim.inf=numeric(k)
lim.inf[1]=min(ultimo)-(rango.corregido/2)
for(i in 2:k)
{
  lim.inf[i]=lim.inf[i-1]+amplitud
}
lim.inf=round(lim.inf,2)

lim.sup=numeric(k) #creamos un vector numérico de cero que se rellenara con los límites superiores de la clase
lim.sup[1]=lim.inf[1]+amplitud
#Ahora establecemos los distintos valores de la clase superior
for(i in 2:k)
{
  lim.sup[i]=lim.sup[i-1]+amplitud
}

lim.sup=round(lim.sup,2)

MC=numeric(k) #creamos un vector numérico de cero que se rellenará con la marca de clase
for(i in 1:k)
{
  MC[i]=sum(lim.inf[i],lim.sup[i])/2
}
MC=round(MC,2)

frecuencia=numeric(k)#creamos un vector numérico de cero que se rellenará con la frecuencia absoluta
for(i in 1:k)
{
  frecuencia[i]=length(x[x>=lim.inf[i] & x<lim.sup[i]])
}
f.acumulada=cumsum(frecuencia)

f.relativa=numeric(k)

for(i in 1:k)
{
  f.relativa[i]=frecuencia[i]/length(x)
}

f.relativa=round(f.relativa,2)

f.rel.acum=cumsum(f.relativa)

tabla=data.frame(lim.inf,lim.sup,MC,frecuencia,f.acumulada,f.relativa,f.rel.acum)

print(tabla)

windows()

par(mfrow=c(1,2))
histograma=barplot(tabla$frecuencia,space=0,font=1,
          col.main="darkorange3",
          main="Histograma y poligono de frecuencia",
          xlab="Marca de clase",
          ylab="Frecuencia",
          names.arg=tabla$MC,
          col = heat.colors(8))

lines(x=histograma,y=tabla$frecuencia,col="blue")
points(x=histograma,y=tabla$frecuencia,col="darksalmon")

## Segunda Gráfica

plot(tabla$MC,tabla$f.rel.acum,
     col="blue",
     xlab="datos de la variable",
     ylab="Frecuencias Relativas acumulada",
     axes = TRUE)

lines(tabla$MC,tabla$f.rel.acum,col="green")

title(main="Ojiva de la distribución",col.main="darksalmon")

binom.test( x = 100, # 
+             n = 32, # 
+             p = 0.05, 
+             alternative = "less", # en relación a la H.alternativa
+             conf.level = 0.01)
