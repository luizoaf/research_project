valor_incremento_B = 0.01
vetor_incremento_coeficiente_B = seq(from=0.05,to=10,by=valor_incremento_B)
coeficiente_B_e_erros = data.frame()
iteracao = 1
for(B in vetor_incremento_coeficiente_B){
  #   coeficiente_B = vetor_incremento_coeficiente_B[1]
  coeficiente_B = B
  source("4_erro_coeficiente_B.R")  
  coeficiente_B_e_erros = rbind(coeficiente_B_e_erros,c(coeficiente_B, sse, rmse,volatilidade,tamanho_serie))
  colnames(coeficiente_B_e_erros) = c("coeficiente_B","sse", "rmse","volatilidade","tamanho_serie")
  #   print(paste("Incremento: " ,incremento,"Iteracao: ",iteracao," SSE: ",sse," Coeficiente B: ", coeficiente_B," tamanho serie: ",tamanho_serie))
  
  # Quando o erro começar a aumentar, pare, pois teremos o melhor coeficiente B.
  if(iteracao > 1){
    if(sse > coeficiente_B_e_erros$sse[iteracao-1]){
      break
    }
  }
  iteracao = iteracao + 1
}

# Coeficientes B,tamanho_serie,volatilidade do menor SSE encontrado
coeficiente_B = as.double(as.character(coeficiente_B_e_erros$coeficiente_B[which.min(coeficiente_B_e_erros$sse)]))
sse =  as.double(as.character(coeficiente_B_e_erros$sse[which.min(coeficiente_B_e_erros$sse)]))
tamanho_serie =  as.double(as.character(coeficiente_B_e_erros$tamanho_serie[which.min(coeficiente_B_e_erros$sse)]))
volatilidade =  as.double(as.character(coeficiente_B_e_erros$volatilidade[which.min(coeficiente_B_e_erros$sse)]))

eixo_x_frequencias = funcao_distribuicao_probabilidade(serie,incremento)$valor_serie_retorno_eixo_x
exponencial = resultado_funcao_exponencial(eixo_x_frequencias)
plot_previsao_com_B_e_exponencial(eixo_x_frequencias,exponencial,incremento)

