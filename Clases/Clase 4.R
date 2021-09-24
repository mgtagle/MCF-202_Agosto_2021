# Clase 4

cr <- read.csv("Clases/cedro-rojo.csv", header = TRUE)

# Revisión gráfica y de normalidad de los datos
boxplot(cr$diametro)
boxplot(cr$altura)

shapiro.test(cr$diametro) # Variable no normal
shapiro.test(cr$altura)

# Transformar la variable diametro para normalizar

cr$dia.log <- log(cr$diametro + 1)

shapiro.test(cr$dia.log)
hist(cr$dia.log)


# Transformar por raíz cuadrada

cr$dia_al2 <- sqrt(cr$diametro + 0.5)
shapiro.test(cr$dia_al2)
hist(cr$dia_al2)


cor.test(cr$diametro, cr$altura)


# Crear un subset (subconjuto) con diámestros menores a 16

cr.16 <- subset(cr, cr$diametro <16)
boxplot(cr.16$diametro)
shapiro.test(cr.16$diametro)

cor.test(cr.16$diametro, cr.16$altura)

plot(cr.16$diametro, cr.16$altura)


# Revisar datos de CANOPY

canopy <- read.csv("Clases/canopy.csv", header=T)

# Realizar un subset solo con los datos de CBE

cbe <- subset(canopy, canopy$Forest =="CBE")

plot(cbe$Cnpy, cbe$LAI4, col= "red", xlab="Apertura del dosel",
     ylab="LAI")

cor.test(cbe$Cnpy, cbe$GLI)

plot(cbe$Cnpy, cbe$GLI, col= "red", xlab="Apertura del dosel",
     ylab="GLI", pch=19)

