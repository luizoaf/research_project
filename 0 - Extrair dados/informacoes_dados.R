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
merge_df = merge(x=df_setores_codigo_acao,y=df_codigo,by="codigo",all=FALSE)
quantidade_acoes_por_setor = aggregate(merge_df$setores,list(merge_df$setores),FUN=length)
colnames(quantidade_acoes_por_setor) = c("Setor","Quantidade_de_Acoes_todas_acoes")
quantidade_acoes_por_setor = quantidade_acoes_por_setor[order(quantidade_acoes_por_setor$Quantidade_de_Acoes,decreasing=T),]


# Agrupamento dos setores para apenas as acoes que estão sendo analisadas

df_setores_codigo_acao = data.frame(codigo = df_setores$Código[2:nrow(df_setores)],acao = df_setores$Ação[2:nrow(df_setores)],setores = correcao_coluna_setores())

# ações observadas
papeis = names(dados)[2:ncol(dados)]
codigo_acoes = substr(papeis, 0, nchar(papeis)-3)  # remocao do ".SA"
df_codigo_menos_acoes = data.frame(codigo = codigo_acoes)
merge_df_menos_acoes = merge(x=df_setores_codigo_acao,y=df_codigo_menos_acoes,by="codigo",all=FALSE)
quantidade_acoes_por_setor_menos_acoes = aggregate(merge_df_menos_acoes$setores,list(merge_df_menos_acoes$setores),FUN=length)
colnames(quantidade_acoes_por_setor_menos_acoes) = c("Setor","Quantidade_de_Acoes_49_acoes")
quantidade_acoes_por_setor_menos_acoes = quantidade_acoes_por_setor_menos_acoes[order(quantidade_acoes_por_setor_menos_acoes$Quantidade_de_Acoes,decreasing=T),]

# write.table(quantidade_acoes_por_setor_menos_acoes,"quantidade_acoes_por_setor_49_acoes.csv",sep=",",row.names=F)


setores = merge(quantidade_acoes_por_setor,quantidade_acoes_por_setor_menos_acoes,by="Setor",all=TRUE)
setores[is.na(setores)] = 0
setores$porcentagem = 100*(setores$Quantidade_de_Acoes_49_acoes/setores$Quantidade_de_Acoes_todas_acoes)
setores = setores[order(setores$porcentagem,decreasing=T),]




# serie_retornos_normalizado = cria_tabela_serie_retornos_de_todas_as_acoes(dados[,2:ncol(dados)])