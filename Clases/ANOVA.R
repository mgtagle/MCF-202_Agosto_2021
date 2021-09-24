# Anova: Análisis de varianza

arena <- c(6, 10, 8, 6, 14, 17, 9, 11, 7, 11)
arcilla <- c(17, 15, 3, 11, 14, 12, 12, 8, 10, 13)
limo <- c(13, 16, 9, 12, 15, 16, 17, 13, 18, 14)

y.prod <- c(arena, arcilla, limo)
y.prod

suelo <- gl(3, 10,30, labels = c("arena", "arcilla", "limo"))

produc <- data.frame(suelo, y.prod)

produc


# tapply función

tapply(produc$y.prod, produc$suelo, length)
tapply(produc$y.prod, produc$suelo, mean)

tapply(produc$y.prod, produc$suelo, var)

# revisar si existen diferencias entre las varianzas de los tres tratamientos.

bartlett.test(produc$y.prod, produc$suelo)

fligner.test(produc$y.prod, produc$suelo)

boxplot(produc$y.prod  ~ produc$suelo, ylab="Productividad ton/ha", xlab="Tipo de suelo",
        col="green")

# ANova en R

prod.aov <- aov(produc$y.prod ~ produc$suelo)
summary(prod.aov)

#  Suma de cuadrados del total SSTotal

SST <- sum((produc$y.prod - mean(produc$y.prod))^2)
SST

arena-mean(arena)
arcilla-mean(arcilla)
limo - mean(limo)


arena.sum <- sum((arena-mean(arena))^2)
arcilla.sum <- sum((arcilla-mean(arcilla))^2)
limo.sum <- sum((limo-mean(limo))^2)

SSE <- sum(arena.sum + arcilla.sum + limo.sum)
SSE


# SSTratamiento = diferencia de SST-SSE

SST-SSE

Ftab <- qf(0.95, 2, 27)
Ftab

probF <- 1-pf(4.24, 2,27)
probF


# Diferencias significativas entre los tratamientos
# Tukey Test

TukeyHSD(prod.aov, conf.level = 0.95)

plot(TukeyHSD(prod.aov))
