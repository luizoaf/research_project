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
acoes_por_setores_por_periodo(dados,"2008.1")
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

setores_2008_1 = dado_semestre_retorna_media_serie_retornos_por_setor("2008.1")
setores_2008_2 = dado_semestre_retorna_media_serie_retornos_por_setor("2008.2")

# head(setores_media_acoes)
# by.data.frame(dados[,2:ncol(dados)],list(dados$datas),FUN=mean)
for( setor in setores_100_porcento){
  setor=setores_100_porcento[1]
  acoes_do_setor = paste(relacao_setores_acoes_menos_acoes$codigo[relacao_setores_acoes_menos_acoes$setores==setor],".SA",sep="")
  medias_por_setor = apply(dados[,acoes_do_setor],MARGIN=1,FUN=mean)
}
# serie_retornos_normalizado = cria_tabela_serie_retornos_de_todas_as_acoes(dados[,2:ncol(dados)])