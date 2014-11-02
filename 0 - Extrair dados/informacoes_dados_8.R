source("../1_funcoes.R")
# papeis_da_ibovespa_2007_2012
# dados = read.csv(file="papeis_da_ibovespa_2007_2012.csv")
dados = read.csv(file="papeis_da_ibovespa_2008_a_2014_2.csv")
dados$datas  = as.Date(dados$datas)

qnt_dias_ano = aggregate(dados$datas,list(format( dados$datas,"%Y")),FUN=length)
colnames(qnt_dias_ano) = c("Ano","qnt_dias")
qnt_dias_ano


# Agrupamento por semestre e calculando a média por setor


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

# Agrupamento dos setores para todas as acoes que compoem a IBOVESPA


df_setores_codigo_acao = data.frame(codigo = df_setores$Código[2:nrow(df_setores)],acao = df_setores$Ação[2:nrow(df_setores)],setores = correcao_coluna_setores())



codigos =as.character(df_setores$Código[2:length(df_setores$Código)])
df_codigo = data.frame(codigo = codigos)
relacao_setores_acoes = merge(x=df_setores_codigo_acao,y=df_codigo,by="codigo",all=FALSE)
quantidade_acoes_por_setor = aggregate(relacao_setores_acoes$setores,list(relacao_setores_acoes$setores),FUN=length)
colnames(quantidade_acoes_por_setor) = c("Setor","Quantidade_de_Acoes_todas_acoes")
quantidade_acoes_por_setor = quantidade_acoes_por_setor[order(quantidade_acoes_por_setor$Quantidade_de_Acoes,decreasing=T),]


# Agrupamento dos setores para apenas as acoes que estão sendo analisadas

df_setores_codigo_acao = data.frame(codigo = df_setores$Código[2:nrow(df_setores)],acao = df_setores$Ação[2:nrow(df_setores)],setores = correcao_coluna_setores())

# ações observadas
papeis = names(dados)[2:ncol(dados)]
codigo_acoes = substr(papeis, 0, nchar(papeis)-3) # remocao do ".SA"
df_codigo_menos_acoes = data.frame(codigo = codigo_acoes)
relacao_setores_acoes_menos_acoes = merge(x=df_setores_codigo_acao,y=df_codigo_menos_acoes,by="codigo",all=FALSE)
quantidade_acoes_por_setor_menos_acoes = aggregate(relacao_setores_acoes_menos_acoes$setores,list(relacao_setores_acoes_menos_acoes$setores),FUN=length)
colnames(quantidade_acoes_por_setor_menos_acoes) = c("Setor","Quantidade_de_Acoes_pesquisadas")
quantidade_acoes_por_setor_menos_acoes = quantidade_acoes_por_setor_menos_acoes[order(quantidade_acoes_por_setor_menos_acoes$Quantidade_de_Acoes,decreasing=T),]

# write.table(quantidade_acoes_por_setor_menos_acoes,"quantidade_acoes_por_setor_49_acoes.csv",sep=",",row.names=F)


setores = merge(quantidade_acoes_por_setor,quantidade_acoes_por_setor_menos_acoes,by="Setor",all=TRUE)
setores[is.na(setores)] = 0
setores$porcentagem = 100*(setores$Quantidade_de_Acoes_pesquisadas/setores$Quantidade_de_Acoes_todas_acoes)
setores = setores[order(setores$porcentagem,decreasing=T),]
# 
# setor = as.character(relacao_setores_acoes$setores[1])
# acoes = names(dados)[2:ncol(dados)]
# codigo_acoes = substr(acoes, 0, nchar(acoes)-3)
# df_acoes_por_setor = data.frame(1)
# acao = codigo_acoes[1]
# for( acao in codigo_acoes){
#   df_acoes_por_setor = cbind(df_acoes_por_setor,dados[,paste(eval(acao),".SA",sep="")])
# }
# df_acoes_por_setor[,-1]
setores_100_porcento = as.character(setores$Setor[setores$porcentagem ==100])
acoes_do_setor = c("datas",paste(relacao_setores_acoes_menos_acoes$codigo[relacao_setores_acoes_menos_acoes$setores==setores_100_porcento[1]],".SA",sep=""))
# dados[1:5,acoes_do_setor]
# dados_por_setor = dados[1:5,acoes_do_setor]
# # media_janelamento_por_setor = aggregate(dados_por_setor[,2:ncol(dados_por_setor)],list(format( dados_por_setor$datas,"%Y")),FUN=mean)
# # colnames(media_janelamento_por_setor) = c("ano",acoes_do_setor[2:length(acoes_do_setor)])
# media_janelamento_por_setor = apply(dados_por_setor[,2:ncol(dados_por_setor)],MARGIN=1,FUN=mean)
# # df_media_setor = data.frame( ano = format( dados_por_setor$datas,"%Y"),media_setor=media_janelamento_por_setor)


# criacao da inclusao de semestre nas datas
dados$datas = ifelse(as.numeric(format( dados$datas,"%m")) <7,paste(format( dados$datas,"%Y"),".1",sep=""),paste(format( dados$datas,"%Y"),".2",sep=""))
# media_setor = function(dados){
#   return(apply(dados[,2:ncol(dados)],MARGIN=1,FUN=mean))
# }
# aggregate(dados[,2:ncol(dados)],list(dados$datas),FUN=mean)

semestres = unique(dados$datas)
semestre_2008_1 = subset(dados,dados$datas=="2008.1")
serie_retornos_por_semestre = cria_tabela_serie_retornos_de_todas_as_acoes(semestre_2008_1)
# setor = setores_100_porcento[1]
# acoes_do_setor = paste(relacao_setores_acoes_menos_acoes$codigo[relacao_setores_acoes_menos_acoes$setores==setor],".SA",sep="")
# medias_por_setor = apply(serie_retornos_por_semestre[,acoes_do_setor],MARGIN=1,FUN=mean)
setores_media_acoes = data.frame(1)
for( setor in setores_100_porcento){
#   setor = setores_100_porcento[3]
  acoes_do_setor = paste(relacao_setores_acoes_menos_acoes$codigo[relacao_setores_acoes_menos_acoes$setores==setor],".SA",sep="")
  if(length(acoes_do_setor)!=1){
    medias_por_setor = apply(serie_retornos_por_semestre[,acoes_do_setor],MARGIN=1,FUN=mean)
  }else{
    medias_por_setor = serie_retornos_por_semestre[,acoes_do_setor]
  }
  setores_media_acoes = cbind(setores_media_acoes,medias_por_setor)
  head(setores_media_acoes)
}
setores_media_acoes = setores_media_acoes[,-1]
colnames(setores_media_acoes) = setores_100_porcento
head(setores_media_acoes)
# by.data.frame(dados[,2:ncol(dados)],list(dados$datas),FUN=mean)
for( setor in setores_100_porcento){
  setor=setores_100_porcento[1]
  acoes_do_setor = paste(relacao_setores_acoes_menos_acoes$codigo[relacao_setores_acoes_menos_acoes$setores==setor],".SA",sep="")
  medias_por_setor = apply(dados[,acoes_do_setor],MARGIN=1,FUN=mean)
}
# serie_retornos_normalizado = cria_tabela_serie_retornos_de_todas_as_acoes(dados[,2:ncol(dados)])