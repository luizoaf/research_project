source("../1_funcoes.R")
library("epiR")
library("ggplot2")

dados = read.csv("combinacao_janelamento_30_incrementos_0_01_0_28_e_de_49_codigos_de_acoes.csv")

# coeficiente.de.confianca = .95
# ic.estados = aggregate(dados$sse,list(dados$incremento),FUN=intervalos_confianca)
# medias<- with(dados,aggregate(dados$sse,list(dados$incremento),FUN = mean))
# df<-data.frame(Incremento = ic.estados$Group.1,Min = ic.estados$x[,1],Max = ic.estados$x[,2], SSE = medias$x)
# # write.table(df,"intervalos_de_confianca_media_SSE_por_incremento.csv",sep=",",row.names=F)
# ggplot(df, aes(x=Incremento,y=SSE)) + 
#    geom_errorbar(aes(ymin = Min, ymax = Max),width=.003) 
#+xlim(0, 0.2)
# # xlim(0, 0.3)
#   
# 
# #TAMANHO MEDIA
# 
# ic.estados = aggregate(dados$tamanho_serie,list(dados$incremento),FUN=intervalos_confianca)
# medias<- with(dados,aggregate(dados$tamanho_serie,list(dados$incremento),FUN = mean))
# df<-data.frame(Incremento = ic.estados$Group.1,Min = ic.estados$x[,1],Max = ic.estados$x[,2], tamanho_serie = medias$x)
# df$Min = as.integer(df$Min)
# df$Max = as.integer(df$Max)
# df$tamanho_serie = as.integer(df$tamanho_serie)
# # write.table(df,"intervalos_de_confianca_media_tamanho_serie_por_incremento.csv",sep=",",row.names=F)
# ggplot(df, aes(x=Incremento,y=tamanho_serie)) + 
#   geom_errorbar(aes(ymin = Min, ymax = Max),width=.003)
#+xlim(0, 0.2)
# 
# 
# 
# #TAMANHO MEDIANA
# 
# ic.estados = aggregate(dados$tamanho_serie,list(dados$incremento),FUN=intervalos_confianca)
# medias<- with(dados,aggregate(dados$tamanho_serie,list(dados$incremento),FUN = median))
# df<-data.frame(Incremento = ic.estados$Group.1,Min = ic.estados$x[,1],Max = ic.estados$x[,2], tamanho_serie = medias$x)
# df$Min = as.integer(df$Min)
# df$Max = as.integer(df$Max)
# df$tamanho_serie = as.integer(df$tamanho_serie)
# # write.table(df,"intervalos_de_confianca_media_tamanho_serie_por_incremento.csv",sep=",",row.names=F)
# ggplot(df, aes(x=Incremento,y=tamanho_serie)) + 
#   geom_errorbar(aes(ymin = Min, ymax = Max),width=.003) 
# #+xlim(0, 0.2)
# 
# dados = read.csv("EVEN3.SA 40 combinacao_janelamento_30_incrementos_0_01_0_28_todas_acoes.csv")
# dados = read.csv("27_acoes_B_volatilidade.csv")
# dados = subset(dados, dados$sse<0.01)
# write.table(dados,"27_acoes_B_volatilidade.csv",sep=",",row.names=F)

plot_B_volatilidade = function(acao,incremento_fdp){
  acoes_incrementos = dados[dados$codigos_acao==acao & dados$incremento==incremento_fdp,]
  plot(main=paste("Incremento:",incremento_fdp,"\nAcao:",acao),ylim=c(0,5),acoes_incrementos$B_vezes_volatilidade,ylab="B X volatilidade")
  intercept = regressao.simples(1:(length(acoes_incrementos$B_vezes_volatilidade)),(acoes_incrementos$B_vezes_volatilidade))[1]
  slope = regressao.simples(1:(length(acoes_incrementos$B_vezes_volatilidade)),(acoes_incrementos$B_vezes_volatilidade))[2]
  abline(a=intercept,b =slope,col=2)
}

plot_B_volatilidade_todas_acoes = function(incremento_fdp){
  acoes_incrementos = dados[dados$incremento==incremento_fdp,]
  plot(main=paste("Incremento:",incremento_fdp),ylim=c(0,5),acoes_incrementos$B_vezes_volatilidade,ylab="B X volatilidade")
  intercept = regressao.simples(1:(length(acoes_incrementos$B_vezes_volatilidade)),(acoes_incrementos$B_vezes_volatilidade))[1]
  slope = regressao.simples(1:(length(acoes_incrementos$B_vezes_volatilidade)),(acoes_incrementos$B_vezes_volatilidade))[2]
  abline(a=intercept,b =slope,col=2)
}
plot_B_volatilidade_todos_incrementos_e_acoes = function(dados){
  acoes_incrementos = dados
  plot(main="Todas as ações(menos ABEV3.SA, ALLL3.SA, CSNA3.SA, BRAP4.SA)\n e incrementos",ylim=c(0,5),acoes_incrementos$B_vezes_volatilidade,ylab="B X volatilidade")
  intercept = regressao.simples(1:(length(acoes_incrementos$B_vezes_volatilidade)),(acoes_incrementos$B_vezes_volatilidade))[1]
  slope = regressao.simples(1:(length(acoes_incrementos$B_vezes_volatilidade)),(acoes_incrementos$B_vezes_volatilidade))[2]
  abline(a=intercept,b =slope,col=2)
}

# plot_B_volatilidade_todos_incrementos_e_acoes()
# # plot_B_volatilidade_todas_acoes(0.17)
# par(mfrow=c(4,7))
# acoes = as.character(unique(dados$codigos_acao))
# incrementos = seq(from=0.01,to=0.26,by=0.01)
# for(incremento_fdp in incrementos){
# #   plot_B_volatilidade(dados$codigos_acao[1],0.01)
# #   plot_B_volatilidade_todas_acoes(incrementos[i])
# #   dados$incremento = as.numeric(dados$incremento)
# #   incremento_fdp = incrementos[7]
#   acoes_incrementos = dados[dados$incremento==0.07,]
#   plot(main=paste("Incremento:",incremento_fdp),xlim=c(0,2000),ylim=c(0,5),acoes_incrementos$B_vezes_volatilidade,ylab="B X volatilidade")
#   intercept = regressao.simples(1:(length(acoes_incrementos$B_vezes_volatilidade)),(acoes_incrementos$B_vezes_volatilidade))[1]
#   slope = regressao.simples(1:(length(acoes_incrementos$B_vezes_volatilidade)),(acoes_incrementos$B_vezes_volatilidade))[2]
#   abline(a=intercept,b =slope,col=2)
# }
# 
# 
# plot(dados$B_vezes_volatilidade[dados$grupo_janelamento==1],ylim=c(0,3.5))
# intercept = regressao.simples(1:(length(dados$B_vezes_volatilidade)),(dados$B_vezes_volatilidade))[1]
# slope = regressao.simples(1:(length(dados$B_vezes_volatilidade)),(dados$B_vezes_volatilidade))[2]
# abline(a=intercept,b =slope,col=2)
# 
# 
# plot((dados$coeficiente_B[order(dados$coeficiente_B,decreasing=T)]),xlab="Volatility",ylab="Coefficient B",ylim=c(0, 10),col="red")
# points((dados$volatilidade[order(dados$volatilidade,decreasing=F)]),xlab="Indice",ylab="Volatility",col="blue")


# 
# plot((dados$coeficiente_B[order(dados$coeficiente_B,decreasing=T)]),xlab="Volatility",ylab="Coefficient B",ylim=c(0, 10),col="red")
# points((dados$volatilidade[order(dados$volatilidade,decreasing=F)]),xlab="Indice",ylab="Volatility",col="blue")
# 
# plot(dados$coeficiente_B[order(dados$coeficiente_B,decreasing=T)])
# pontos_estranhos_B = dados[order(dados$coeficiente_B,decreasing=T)[1:500],]
pontos_estranhos_B = dados[order(dados$coeficiente_B,decreasing=T),]
pontos_estranhos_B = dados[dados$coeficiente_B>as.numeric(quantile(dados$coeficiente_B,.99)),]
# boxplot(main="Coeficiente B",dados$coeficiente_B)
acoes_B_outlier = as.character(unique(pontos_estranhos_B$codigos_acao))
# ABEV3.SA ALLL3.SA CSNA3.SA BRAP4.SA
colunas=setdiff(dados$codigos_acao,acoes_B_outlier)
# dados_sem_outlier_em_B = subset(dados,setdiff(as.character(dados$codigos_acao),colunas))
dados_sem_outlier_em_B = dados[dados$codigos_acao[dados$codigos_acao %in%colunas],]
plot_B_volatilidade_todos_incrementos_e_acoes(dados_sem_outlier_em_B)
# serie_retornos = read.csv("serie_retornos_normalizado.csv")
plot(serie_retornos$ABEV3,type="l")