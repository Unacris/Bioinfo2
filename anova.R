#### anova
#Se importar de txt la base de datos cell 
`datos2` #vase de datos que se va a utiliza

View(`datos2`) # ver datos para ver que la tabla este correcta 

# esto es para las clases y visualizar datos de los grupos por 
#separado 

attach(datos2)
names(datos2)
attach(datos12)
names(datos12)

# ahora se utiliza  la tabla para probar un anailis de anova 

summary(datos2)
par(mfrow = c(1, 1))
boxplot(datos2)
# esto se hace para ver el resumen del archivo para ver estadisticas
# basicas de los datos informacion de los datos 


# importar la libreria a utilizar  para poder hacer diagramas 
# de cajas para comparar medias 

install.packages("sjPlot")

library("tidyverse")

install.packages("tidyverse")

library("sjPlot")
install.packages("lne4")

library("lne4")

install.packages("stats")

library("stats")

# boxplot para ver de cells de acuerdo a smoker para ver los 
#de smoker con las medias podeos ver los cuartiles de los grupos 
# de smoker, se puede ver que las medias son diferentes a simple vista 



# se va a hacer con la independiente de acuerdo a la variable categorica
# nos muestra lo cuadrados y los grados de liberdad que tiene y el error estandar que presenta 


aov(altura ~ l_pierna)
altura1 <- as.numeric(datos2$altura)
altura1
altura1 <- as.numeric(datos2$l_pierna)
altura1
#ahora para el nivel de significancia  se crea una variable y se le va a poner
# con respecto a cell smoker y se le hace el resumen ( summary) para el nivel de significancia
# el nivel de sinificancia  este es <2e-16 *** que quiere decir que las medias son diferentes

anova1 = aov(datos2$altura ~ datos2$l_pierna)
anova1
summary(anova1)
plot(anova1)

# Aplicar el test de Tukey prueba pos hoc

resultado_tukey <- TukeyHSD(aov(datos12$altura ~ datos12$l_pierna))



# Mostrar los resultados
print(resultado_tukey)


datosaltura <- as.numeric(datos2$altura)

anova2 <- aov(altura ~ l_pierna + l_brazo + a_espalda, data = datos2)
anova2
summary(anova2)



## peuba dunnet post hot para anovas de mas vias 
install.packages("multcomp")

library(multcomp)

# Paso 3: Realizar la prueba de Dunnett
comparaciones <- glht(anova2, linfct = mcp(grupo = "Dunnett"))
resultado_dunnett <- summary(comparaciones)

# Paso 4: Obtener los valores ajustados y p-valores
ajustados <- confint(comparaciones)
p_valores <- pvalues(comparaciones)

# Paso 5: Mostrar los resultados
print(resultado_dunnett)
print(ajustados)
print(p_valores)


