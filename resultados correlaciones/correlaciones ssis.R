# Correlaciones Yermein SSIS

# Carga paqueterías
library(ggpubr)

setwd(paste("C:/Users/behep/OneDrive - Seiton de México/",
          "PBH Personal/y&p/Yermik/Yermein's PhD",sep = ""))

# Carga datos
datos <- read.csv(paste("C:/Users/behep/Downloads/",
"base datos entrenamiento emocion.csv",sep = ""))

#
ssis <- datos[,1:13]
vars <- datos[,14:45]

# Matriz de Correlación de todas las variables
correlacion <- cor(datos)

# Prueba de correlación de ssis vs. las demás
prueba_cor <- c()
correlaciones <- data.frame(matrix(ncol = 3, nrow = length(ssis)*
                                     length(vars)))
noms <- c("Variables","Corr", "P_val")
colnames(correlaciones) <- noms
n <- 1
for (i in 1:length(ssis)){
  for (j in 1:length(vars)){
    prueba_cor[[n]]<-cor.test(vars[,j],ssis[,i],
                              method = "pearson")
    correlaciones$Variables[n] <- paste(names(ssis[i]),
                                names(vars[j]),sep = "-")
    correlaciones$Corr[n] <- prueba_cor[[n]]$estimate
    correlaciones$P_val[n] <- prueba_cor[[j]]$p.value
    n <- n+1
    
  }
}

ggscatter(datos, x = "RC.emp.A.1", y = "SSIS.1", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "RC emp A", ylab = "SSIS")


# write.csv(correlaciones,file = "correlaciones.csv")

reg <- lm(datos$SSIS.1 ~ datos$RC.emp.A.1)
