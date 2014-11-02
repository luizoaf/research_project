source("../../1_funcoes.R")
library("epiR")
library("ggplot2")
arquivos = dir(pattern=".csv")
todos_janelamentos = data.frame()
for(arquivo in arquivos){
  #   arquivo = arquivos[1]
  janela = strsplit(arquivo,"_")[[1]][4]
  #   janela = strsplit(arquivo,"_")[[1]][5]
  dados = read.table(arquivo,head=T)
  dados$janela = janela
  todos_janelamentos = rbind(todos_janelamentos,dados[,c("coeficiente_B","sse","volatilidade","janela")])
}

dados = todos_janelamentos
dados$janela = as.numeric(dados$janela)
coeficiente.de.confianca = .95
ic.estados = aggregate(dados$sse,list(dados$janela),FUN=intervalos_confianca)
medias<- with(dados,aggregate(dados$sse,list(dados$janela),FUN = mean))
df<-data.frame(janela = ic.estados$Group.1,Min = ic.estados$x[,1],Max = ic.estados$x[,2], SSE = medias$x)
# write.table(df,"intervalos_de_confianca_media_SSE_por_janela.csv",sep=",",row.names=F)
ggplot(df, aes(x=janela,y=SSE))+ylim(0,0.25) + 
  geom_errorbar(aes(ymin = Min, ymax = Max),width=.001) 

