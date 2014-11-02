source("../1_funcoes.R")
# papeis_da_ibovespa_2007_2012
# dados = read.csv(file="papeis_da_ibovespa_2007_2012.csv")
dados = read.csv(file="papeis_da_ibovespa_2008_a_2014_2.csv")
dados$datas  = as.Date(dados$datas)
dados$datas = ifelse(as.numeric(format( dados$datas,"%m")) <7,paste(format( dados$datas,"%Y"),".1",sep=""),paste(format( dados$datas,"%Y"),".2",sep=""))

df_setores = read.csv("setores.csv")

correcao_coluna_setores = function(){
  setores_atual = ""
  setores = as.character(df_setores$Setor)
  correcao_setores = c()
  j_correcao_setores = 1
  for(i in 2:length(setores)){
    setor = setores[i]
    if(setor ==""){
      correcao_setores[j_correcao_setores] =setores_atual 
      j_correcao_setores = j_correcao_setores+1
    }else{
      setores_atual = setor
      correcao_setores[j_correcao_setores] =setores_atual 
      j_correcao_setores = j_correcao_setores+1
      
    }
  }
  return(correcao_setores)
}

df_setores_codigo_acao = data.frame(codigo = df_setores$Código[2:nrow(df_setores)],acao = df_setores$Ação[2:nrow(df_setores)],setores = correcao_coluna_setores())

relacao_setores_acoes = function(dados,periodo){
  periodo_acoes = dados[dados$datas==periodo,]
  papeis = names(periodo_acoes)[2:ncol(periodo_acoes)]
  codigo_acoes = substr(papeis, 0, nchar(papeis)-3) # remocao do ".SA"
  df_codigo_menos_acoes = data.frame(codigo = codigo_acoes)
  relacao_setores_acoes_menos_acoes = merge(x=df_setores_codigo_acao,y=df_codigo_menos_acoes,by="codigo",all=FALSE)
  return(relacao_setores_acoes_menos_acoes)
}
acoes_por_setores_por_periodo = function(dados,periodo){
  relacao_setores_acoes_menos_acoes = relacao_setores_acoes(dados,periodo)
  quantidade_acoes_por_setor_menos_acoes = aggregate(relacao_setores_acoes_menos_acoes$setores,list(relacao_setores_acoes_menos_acoes$setores),FUN=length)
  colnames(quantidade_acoes_por_setor_menos_acoes) = c("Setor","Quantidade_de_Acoes_pesquisadas")
  quantidade_acoes_por_setor_menos_acoes = quantidade_acoes_por_setor_menos_acoes[order(quantidade_acoes_por_setor_menos_acoes$Quantidade_de_Acoes,decreasing=T),]
  
  # Todas acoes
  codigos =as.character(df_setores$Código[2:length(df_setores$Código)])
  df_codigo = data.frame(codigo = codigos)
  relacao_setores_acoes = merge(x=df_setores_codigo_acao,y=df_codigo,by="codigo",all=FALSE)
  quantidade_acoes_por_setor = aggregate(relacao_setores_acoes$setores,list(relacao_setores_acoes$setores),FUN=length)
  colnames(quantidade_acoes_por_setor) = c("Setor","Quantidade_de_Acoes_todas_acoes")
  quantidade_acoes_por_setor = quantidade_acoes_por_setor[order(quantidade_acoes_por_setor$Quantidade_de_Acoes,decreasing=T),]
  
  # merge dos setores do periodo com o de todos os setores com todas as acoes possiveis
  setores = merge(quantidade_acoes_por_setor,quantidade_acoes_por_setor_menos_acoes,by="Setor",all=TRUE)
  setores[is.na(setores)] = 0
  setores$porcentagem = 100*(setores$Quantidade_de_Acoes_pesquisadas/setores$Quantidade_de_Acoes_todas_acoes)
  setores = setores[order(setores$porcentagem,decreasing=T),]
  return(setores)
}
# write.table(quantidade_acoes_por_setor_menos_acoes,"quantidade_acoes_por_setor_49_acoes.csv",sep=",",row.names=F)

setores_100_porcento_por_periodo = function(periodo){
  setores_100_porcento = acoes_por_setores_por_periodo(dados,periodo) 
  setores_100_porcento = setores_100_porcento$Setor[setores_100_porcento$porcentagem ==100]
  setores_100_porcento = as.character(setores_100_porcento)
  return(setores_100_porcento)
}
dado_semestre_retorna_media_serie_retornos_por_setor = function(semestre){
  semestre_acoes = subset(dados,dados$datas==semestre)
  serie_retornos_por_semestre = cria_tabela_serie_retornos_de_todas_as_acoes(semestre_acoes)
  setores_media_acoes = data.frame(1)
  setores_100_porcento = setores_100_porcento_por_periodo(semestre)
  for( setor in setores_100_porcento){
    relacao_setores_acoes_menos_acoes = relacao_setores_acoes(dados,semestre)
    acoes_do_setor = paste(relacao_setores_acoes_menos_acoes$codigo[relacao_setores_acoes_menos_acoes$setores==setor],".SA",sep="")
    if(length(acoes_do_setor)!=1){ # nao ira calcular a media quando tiver apenas 1 acao
      medias_por_setor = apply(serie_retornos_por_semestre[,acoes_do_setor],MARGIN=1,FUN=mean)
    }else{
      medias_por_setor = serie_retornos_por_semestre[,acoes_do_setor]
    }
    setores_media_acoes = cbind(setores_media_acoes,medias_por_setor)
  }
  setores_media_acoes = setores_media_acoes[,-1]
  head(setores_media_acoes)
  colnames(setores_media_acoes) = setores_100_porcento_por_periodo(semestre)
  return(setores_media_acoes)
}

semestres = unique(dados$datas)
# setores_2008_1 = dado_semestre_retorna_media_serie_retornos_por_setor("2008.1")
# setores_2008_2 = dado_semestre_retorna_media_serie_retornos_por_setor("2008.2")
# setores_2009_1 = dado_semestre_retorna_media_serie_retornos_por_setor("2009.1")
# setores_2009_2 = dado_semestre_retorna_media_serie_retornos_por_setor("2009.2")
# setores_2010_1 = dado_semestre_retorna_media_serie_retornos_por_setor("2010.1")
# setores_2010_2 = dado_semestre_retorna_media_serie_retornos_por_setor("2010.2")
# setores_2011_1 = dado_semestre_retorna_media_serie_retornos_por_setor("2011.1")
# setores_2011_2 = dado_semestre_retorna_media_serie_retornos_por_setor("2011.2")
# setores_2012_1 = dado_semestre_retorna_media_serie_retornos_por_setor("2012.1")
# setores_2012_2 = dado_semestre_retorna_media_serie_retornos_por_setor("2012.2")
# setores_2013_1 = dado_semestre_retorna_media_serie_retornos_por_setor("2013.1")
# setores_2013_2 = dado_semestre_retorna_media_serie_retornos_por_setor("2013.2")
# setores_2014_1 = dado_semestre_retorna_media_serie_retornos_por_setor("2014.1")

grupo_coeficiente_B = data.frame()
# incremento_fdp = seq(from=0.01,to=0.1,by=0.01) 
# incremento_fdp = 0.05
i=1
colunas = c()
semestres_por_setores = c()
for(semestre in semestres){
  serie_retornos_normalizado = dado_semestre_retorna_media_serie_retornos_por_setor(semestre)
  
  for(coluna in 1:length(names(serie_retornos_normalizado))){
    colunas[i] = names(serie_retornos_normalizado)[coluna]
    semestres_por_setores[i] = semestre
    serie = serie_retornos_normalizado[,coluna]
    source("3_define_coeficiente_B.R")
    grupo_coeficiente_B = rbind(grupo_coeficiente_B,c(coeficiente_B,sse,volatilidade,tamanho_serie))
    colnames(grupo_coeficiente_B) = c("coeficiente_B","sse","volatilidade","tamanho_serie")
    print(semestres_por_setores)
    print(grupo_coeficiente_B)
    i= i+1
    #     }
    
    # write.table(grupo_coeficiente_B,paste(names(serie_retornos_normalizado)[i],grupo_janelamento,"combinacao_janelamento_30_incrementos_0_01_0_28_todas_acoes.csv"),row.names=F,sep=",")
  }
}
grupo_coeficiente_B = cbind(semestres_por_setores,grupo_coeficiente_B)
grupo_coeficiente_B = cbind(colunas,grupo_coeficiente_B)
grupo_coeficiente_B$b_volatilidade = grupo_coeficiente_B$coeficiente_B*grupo_coeficiente_B$volatilidade



plot(grupo_coeficiente_B$b_volatilidade,ylim=c(0,2.5),ylab="Volatility x Coefficient B",xlab="Indices")
intercept = regressao.simples(1:(length(grupo_coeficiente_B$b_volatilidade)),(grupo_coeficiente_B$b_volatilidade))[1]
slope = regressao.simples(1:(length(grupo_coeficiente_B$b_volatilidade)),(grupo_coeficiente_B$b_volatilidade))[2]
abline(a=intercept,b =slope,col=2)




plot((grupo_coeficiente_B$coeficiente_B[order(grupo_coeficiente_B$coeficiente_B,decreasing=T)]),xlab="Volatility",ylab="Coefficient B",ylim=c(0, 10),col="red")
points((grupo_coeficiente_B$volatilidade[order(grupo_coeficiente_B$volatilidade,decreasing=F)]),xlab="Indice",ylab="Volatility",col="blue")

teste = grupo_coeficiente_B[grupo_coeficiente_B$coeficiente_B<2,]
plot(teste$coeficiente_B~teste$volatilidade,xlab="Volatility",ylab="Coefficient B")

plot(grupo_coeficiente_B$coeficiente_B~grupo_coeficiente_B$volatilidade,xlab="Volatility",ylab="Coefficient B")



dados = subset(teste,teste$semestres_por_setores=="2014.1")
k=2
iter = 45
dados = dados[,c(3,5)]

km = kmeans (x = dados, centers = k, iter.max = iter)
dados$cluster = km$cluster

plot(main= paste("Para K = ",k,sep =""),dados$coeficiente_B~dados$volatilidade,xlab="Volatility",ylab="Coefficient B", col = km$cluster,pch = 20, cex = 0.9)
points(km$centers[,1]~km$centers[,2],col=3, pch = 8,lwd=3)
# points(km$centers,col=1:k, pch = 8,lwd=3)

dados = dados[order(dados$cluster),]


