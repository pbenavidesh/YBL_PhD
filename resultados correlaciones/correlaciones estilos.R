# Correlaciones Yermein estilos

# Carga paqueterías
library(ggpubr)
library(xtable)

setwd(paste("C:/Users/behep/OneDrive - Seiton de México/",
            "PBH Personal/y&p/Yermik/Yermein's PhD",sep = ""))

# Carga datos
datos <- read.csv("base estilos crianza.csv")

#
estilos <- datos[,1:5]
vars <- datos[,6:37]

# Matriz de Correlación de todas las variables
correlacion <- round(cor(datos),2)
correlacion[upper.tri(correlacion)] <- ""
correlacion <- as.data.frame(correlacion)
# print(xtable(correlacion))
corstars(datos)
write.csv(corstars(datos),"prueba.csv")

# Prueba de correlación de estilos vs. las demás
prueba_cor <- c()
correlaciones <- data.frame(matrix(ncol = 3, nrow = length(estilos)*
                                     length(vars)))
noms <- c("Variables","Corr", "P_val")
colnames(correlaciones) <- noms
n <- 1
for (i in 1:length(estilos)){
  for (j in 1:length(vars)){
    prueba_cor[[n]]<-cor.test(vars[,j],estilos[,i],
                              method = "pearson")
    correlaciones$Variables[n] <- paste(names(estilos[i]),
                                        names(vars[j]),sep = "-")
    correlaciones$Corr[n] <- prueba_cor[[n]]$estimate
    correlaciones$P_val[n] <- prueba_cor[[j]]$p.value
    n <- n+1
    
  }
}

ggscatter(datos, x = "RC.emp.A.1", y = "EPPPCP..AC", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "RC emp A", ylab = "EPPPCP AC")


write.csv(correlaciones,file = "correlaciones_estilos.csv")

reg <- lm(datos$EPPPCP..AC ~ datos$RC.emp.A.1)
