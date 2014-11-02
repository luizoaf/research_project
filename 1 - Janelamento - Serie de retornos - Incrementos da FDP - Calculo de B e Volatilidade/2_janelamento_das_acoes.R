source("../1_funcoes.R")
dados = read.csv("papeis_da_ibovespa_2007_2012.csv",sep=";")
serie_retornos_normalizado = cria_tabela_serie_retornos_de_todas_as_acoes(dados)
serie_retornos_normalizado = janelamento(serie_retornos_normalizado,30)

grupo_coeficiente_B = data.frame()
incremento_fdp = seq(from=0.01,to=0.28,by=0.01) 
for(i in 1:length(names(serie_retornos_normalizado))){
  for(grupo_janelamento in unique(serie_retornos_normalizado$grupo)){
    for(incremento in incremento_fdp){
      
      serie = serie_retornos_normalizado[serie_retornos_normalizado$grupo == grupo_janelamento, codigo_acao]
      source("3_define_coeficiente_B.R")
      grupo_coeficiente_B = rbind(grupo_coeficiente_B,c(grupo_janelamento,coeficiente_B,sse,volatilidade,incremento,tamanho_serie))
      colnames(grupo_coeficiente_B) = c("grupo_janelamento","coeficiente_B","sse","volatilidade","incremento","tamanho_serie")
    }
    # write.table(grupo_coeficiente_B,paste(names(serie_retornos_normalizado)[i],grupo_janelamento,"combinacao_janelamento_30_incrementos_0_01_0_28_todas_acoes.csv"),row.names=F,sep=",")
  }
}


acoes_investigadas = names(dados)[2:ncol(dados)] # remocao da coluna data
acoes=c()
acao = 1
for(i in 1:nrow(grupo_coeficiente_B)){
  if(!(grupo_coeficiente_B$grupo_janelamento[i]==40 & grupo_coeficiente_B$incremento[i]==0.28)){
    acoes[i] = acoes_investigadas[acao]
  }else{
    acoes[i] = acoes_investigadas[acao]
    acao = acao+1
  }
}
grupo_coeficiente_B$codigos_acao = acoes
grupo_coeficiente_B$B_vezes_volatilidade = grupo_coeficiente_B$coeficiente_B*grupo_coeficiente_B$volatilidade
serie_retornos_normalizado
write.table(serie_retornos_normalizado,"serie_retornos_normalizado.csv",row.names=F,sep=",")
write.table(grupo_coeficiente_B,"combinacao_janelamento_30_incrementos_0_01_0_28_e_de_49_codigos_de_acoes.csv",row.names=F,sep=",")