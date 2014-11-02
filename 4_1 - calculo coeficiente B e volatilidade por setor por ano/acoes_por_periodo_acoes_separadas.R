# tamanho_janelamento = 30
source("../1_funcoes.R")

dados = read.csv(file="papeis_da_ibovespa_2008_a_2014_2.csv")
dados$datas  = as.Date(dados$datas)
dados$datas = format( dados$datas,"%Y")

faixa_temporal = unique(dados$datas)

serie_retornos_normalizado = cria_tabela_serie_retornos_de_todas_as_acoes(dados[,2:ncol(dados)])
serie_retornos_normalizado = janelamento(serie_retornos_normalizado,tamanho_janelamento) 

for(coluna_acao in 1:length(names(serie_retornos_normalizado))){
  grupo_coeficiente_B = data.frame()
  for(ano in as.numeric(faixa_temporal)){
    for(grupo in unique(serie_retornos_normalizado$grupo)){
      
      serie = serie_retornos_normalizado[serie_retornos_normalizado$grupo==grupo,coluna_acao]
      source("3_define_coeficiente_B.R")
      grupo_coeficiente_B = rbind(grupo_coeficiente_B,c(coeficiente_B,sse,volatilidade,ano))
      colnames(grupo_coeficiente_B) = c("coeficiente_B","sse","volatilidade","ano")
      print(grupo_coeficiente_B)
    }
  }
  nome_acao = names(serie_retornos_normalizado)[coluna_acao]
  grupo_coeficiente_B = cbind(data.frame(acao=nome_acao),grupo_coeficiente_B)
  grupo_coeficiente_B$b_volatilidade = grupo_coeficiente_B$coeficiente_B*grupo_coeficiente_B$volatilidade
  write.table(grupo_coeficiente_B,paste(nome_acao,"_b_volatilidade_janelemanto_",tamanho_janelamento,"_dias_por_acoes_no_ano.csv",sep=""),dec=".",row.names=F)
}












# 
# 
# i=1
# colunas = c()
# faixa_temporal_das_acoes = c()
# # tamanho_janelamento=90
# for(ano in faixa_temporal){
#   faixa_temporal_das_acoes = c()
#   #     ano = faixa_temporal[1]
#   serie_retornos_normalizado = cria_tabela_serie_retornos_de_todas_as_acoes(dados[,2:ncol(dados)])
#   #   tamanho_janelamento=30
#   serie_retornos_normalizado = janelamento(serie_retornos_normalizado,tamanho_janelamento) 
#   #   t = janelamento(serie_retornos_normalizado,30)
#   #   length(names(serie_retornos_normalizado))
#   for(coluna_acao in 1:length(names(serie_retornos_normalizado))){
#     grupo_coeficiente_B = data.frame()
#     #           coluna_acao = 1
#     for(grupo in unique(serie_retornos_normalizado$grupo)){
#       #       grupo = 1
#       
#       colunas[i] = names(serie_retornos_normalizado)[coluna_acao]
#       faixa_temporal_das_acoes[i] = ano
#       serie = serie_retornos_normalizado[serie_retornos_normalizado$grupo==grupo,coluna_acao]
#       source("3_define_coeficiente_B.R")
#       grupo_coeficiente_B = rbind(grupo_coeficiente_B,c(coeficiente_B,sse,volatilidade))
#       colnames(grupo_coeficiente_B) = c("coeficiente_B","sse","volatilidade")
#       print(faixa_temporal_das_acoes)
#       print(grupo_coeficiente_B)
#       i= i+1
#       
#     }
#     #     }
#     #     grupo_coeficiente_B = cbind(faixa_temporal_das_acoes,grupo_coeficiente_B)
#     #     grupo_coeficiente_B = cbind(data.frame(setores=colunas),grupo_coeficiente_B)
#     #     grupo_coeficiente_B$b_volatilidade = grupo_coeficiente_B$coeficiente_B*grupo_coeficiente_B$volatilidade
#     #     write.table(grupo_coeficiente_B,paste(names(serie_retornos_normalizado)[coluna_acao],"_b_volatilidade_janelemanto_",tamanho_janelamento,"_dias_por_acoes_no_ano.csv",sep=""),dec=".",row.names=F)
#     # write.table(grupo_coeficiente_B,paste(names(serie_retornos_normalizado)[i],grupo_janelamento,"combinacao_janelamento_30_incrementos_0_01_0_28_todas_acoes.csv"),row.names=F,sep=",")
#   }
#   
# }
# grupo_coeficiente_B = cbind(faixa_temporal_das_acoes,grupo_coeficiente_B)
# grupo_coeficiente_B = cbind(data.frame(setores=colunas),grupo_coeficiente_B)
# grupo_coeficiente_B$b_volatilidade = grupo_coeficiente_B$coeficiente_B*grupo_coeficiente_B$volatilidade
# write.table(grupo_coeficiente_B,paste("b_volatilidade_janelemanto_",tamanho_janelamento,"_dias_por_acoes_no_ano.csv",sep=""),dec=".",row.names=F)
# 
# 
# plot(grupo_coeficiente_B$b_volatilidade,ylim=c(0,2.5),ylab="Volatility x Coefficient B",xlab="Indices")
# intercept = regressao.simples(1:(length(grupo_coeficiente_B$b_volatilidade)),(grupo_coeficiente_B$b_volatilidade))[1]
# slope = regressao.simples(1:(length(grupo_coeficiente_B$b_volatilidade)),(grupo_coeficiente_B$b_volatilidade))[2]
# abline(a=intercept,b =slope,col=2)
# 
# 
# 
# 
# plot((grupo_coeficiente_B$coeficiente_B[order(grupo_coeficiente_B$coeficiente_B,decreasing=T)]),xlab="Volatility",ylab="Coefficient B",ylim=c(0, 10),col="red")
# points((grupo_coeficiente_B$volatilidade[order(grupo_coeficiente_B$volatilidade,decreasing=F)]),xlab="Indice",ylab="Volatility",col="blue")
# 
# com_b_menor_que_1.5 = grupo_coeficiente_B[grupo_coeficiente_B$coeficiente_B<1.5,]
# plot(com_b_menor_que_1.5$coeficiente_B~com_b_menor_que_1.5$volatilidade,xlab="Volatility",ylab="Coefficient B")
# 
# plot(grupo_coeficiente_B$coeficiente_B~grupo_coeficiente_B$volatilidade,xlab="Volatility",ylab="Coefficient B")
# 
# 
# retorna_cluster = function(faixa_temporal){
#   dados = subset(com_b_menor_que_1.5,com_b_menor_que_1.5$faixa_temporal_das_acoes==faixa_temporal)
#   k=2
#   #   iter = 100000
#   dados = dados[,c(3,5)]
#   
#   km = kmeans (x = dados, centers = k)
#   dados$cluster = km$cluster
#   
#   plot(main= paste("Para K = ",k,sep =""),dados$coeficiente_B~dados$volatilidade,xlab="Volatility",ylab="Coefficient B", col = km$cluster,pch = 20, cex = 0.9)
#   points(km$centers[,1]~km$centers[,2],col=3, pch = 8,lwd=3)
#   # points(km$centers,col=1:k, pch = 8,lwd=3)
#   
#   #   dados = dados[order(dados$cluster),]
#   # dados
#   volatilidade_primeiro_cluster = dados$volatilidade[dados$cluster==1][1]
#   volatilidade_segundo_cluster = dados$volatilidade[dados$cluster==2][1]
#   teste_volatilidade_cluster_1= ifelse(volatilidade_primeiro_cluster >volatilidade_segundo_cluster,1,0)
#   teste_volatilidade_cluster_2= ifelse(volatilidade_primeiro_cluster <volatilidade_segundo_cluster,1,0)
#   #   padronizar_cluster$cluster_padronizado = ifelse(teste_volatilidade== 1,teste_volatilidade
#   cluster_padronizado = ifelse(dados$cluster==1, teste_volatilidade_cluster_1,teste_volatilidade_cluster_2)
#   
#   return(cluster_padronizado)
# }
# faixa_temporal = unique(dados$datas)
# setores_faixa_temporal = data.frame()
# for(periodo in faixa_temporal){
#   #   periodo = faixa_temporal[2]
#   #   periodo = "2008"
#   padronizar_cluster = retorna_cluster(periodo)
#   
#   #   volatilidade_primeiro_cluster = padronizar_cluster$volatilidade[padronizar_cluster$cluster==1][1]
#   #   volatilidade_segundo_cluster = padronizar_cluster$volatilidade[padronizar_cluster$cluster==2][1]
#   #   teste_volatilidade_cluster_1= ifelse(volatilidade_primeiro_cluster >volatilidade_segundo_cluster,1,0)
#   #   teste_volatilidade_cluster_2= ifelse(volatilidade_primeiro_cluster <volatilidade_segundo_cluster,1,0)
#   # #   padronizar_cluster$cluster_padronizado = ifelse(teste_volatilidade== 1,teste_volatilidade
#   #   cluster_padronizado = ifelse(padronizar_cluster$cluster==1, teste_volatilidade_cluster_1,teste_volatilidade_cluster_2)
#   
#   #   padronizar_cluster$cluster_padronizado = ifelse(volatilidade_primeiro_cluster <volatilidade_segundo_cluster,0,1)
#   setor_faixa_temporal = cbind(data.frame(cluster = retorna_cluster(periodo)),com_b_menor_que_1.5[com_b_menor_que_1.5$faixa_temporal_das_acoes==periodo,c("setores","coeficiente_B","volatilidade","faixa_temporal_das_acoes" )])
#   setores_faixa_temporal = rbind(setores_faixa_temporal,setor_faixa_temporal)
# }
# # setores_faixa_temporal
# names(setores_faixa_temporal)
# 
# setores_faixa_temporal_estruturado = data.frame(Setores = setores_faixa_temporal$setores,Anos=setores_faixa_temporal$faixa_temporal_das_acoes,Volatilidade=setores_faixa_temporal$volatilidade,Coeficiente_B=setores_faixa_temporal$coeficiente_B,Cor=setores_faixa_temporal$cluster)
# 
# # write.table(setores_faixa_temporal_estruturado,"setores_faixa_temporal.csv",row.names=F,sep=",")