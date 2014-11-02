source("../1_funcoes.R")
dados = read.csv("setores_faixa_temporal.csv")

# Utiliza os setores em comum entre todos os anos
intersecao_setores = subset(dados,dados$Anos==2008)$Setores
for(ano in unique(dados$Anos)){
  intersecao_setores = intersect(subset(dados,dados$Anos==ano)$Setores,intersecao_setores)
}
# Aceita apenas os setores em comum
dados = dados[dados$Setores %in% intersecao_setores,]

# plot(main="11 Setores por ano",dados$Volatilidade * dados$Coeficiente_B,xlab="Índices",ylab="Volatilidade * Coeficiente B",col="red",ylim=c(0,2))

#treino = dados[1:(nrow(dados)*0.5),]
treino = dados[dados$Anos %in% 2008:2010,]
plot_comportamento_volatilidade_B(treino)
plot(main="11 Setores por ano",treino$Volatilidade * treino$Coeficiente_B,xlab="Índices",ylab="Volatilidade * Coeficiente B",col="red",ylim=c(0,2))

dados = treino
k=3
dados = dados[,c("Volatilidade","Coeficiente_B")]

km = kmeans (x = dados, centers = k)
dados$cluster = km$cluster

plot(main= paste(length(intersecao_setores)," setores entre 2008 e 2010\nPara K = ",k,sep =""),dados$Coeficiente_B~dados$Volatilidade,xlab="Volatility",ylab="Coefficient B", col = km$cluster,pch = 20, cex = 0.9)
points(km$centers[,2]~km$centers[,1],col=1:k, pch = 8,lwd=3)