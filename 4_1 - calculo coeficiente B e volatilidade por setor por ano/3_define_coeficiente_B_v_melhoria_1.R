# valor_incremento_B = 0.01
# vetor_incremento_coeficiente_B = seq(from=0.05,to=20,by=valor_incremento_B)
# coeficiente_B_e_erros = data.frame()
# iteracao = 1
# for(B in vetor_incremento_coeficiente_B){
#   #   coeficiente_B = vetor_incremento_coeficiente_B[1]
#   coeficiente_B = B
#   source("4_erro_coeficiente_B.R")  
#   coeficiente_B_e_erros = rbind(coeficiente_B_e_erros,c(coeficiente_B, sse, rmse,volatilidade,tamanho_serie))
#   colnames(coeficiente_B_e_erros) = c("coeficiente_B","sse", "rmse","volatilidade","tamanho_serie")
#   #   print(paste("Incremento: " ,incremento,"Iteracao: ",iteracao," SSE: ",sse," Coeficiente B: ", coeficiente_B," tamanho serie: ",tamanho_serie))
#   
#   # Quando o erro começar a aumentar, pare, pois teremos o melhor coeficiente B.
#   if(iteracao > 1){
#     if(sse > coeficiente_B_e_erros$sse[iteracao-1]){
#       break
#     }
#   }
#   iteracao = iteracao + 1
# }
# ########### calculo do B mais rapido ############# 
i_erros = 1
incremento = c(2,1,0.5,0.05,0.01,0.005)
i_incremento = 1
coeficiente_B = 0.01 #tenho sse, comparo o sse atual com o antigo
i_erros =1
incremento_atual = incremento[1]
coeficiente_B_e_erros = data.frame()
posicoes_fitting = c()
i_posicao_fitting= 1
while(T){
  source("4_erro_coeficiente_B.R")  
  plot_previsao_com_B_e_exponencial(eixo_x_frequencias,exponencial)
  coeficiente_B_e_erros = rbind(coeficiente_B_e_erros,c(coeficiente_B, sse, rmse,volatilidade,tamanho_serie))
  coeficiente_B_e_erros
  colnames(coeficiente_B_e_erros) = c("coeficiente_B","sse", "rmse","volatilidade","tamanho_serie")
  posicao_fitting =  sum(exponencial-serie_retornos_frequencia) < 0  #negativo(TRUE), a exponencial estara abaixo da serie de retornos
  posicao_exponencial = exponencial-serie_retornos_frequencia
  posicao_fitting = length(posicao_exponencial[posicao_exponencial<0])/length(posicao_exponencial) # porcentagem dos negativos
  posicoes_fitting[i_posicao_fitting] =  posicao_fitting
  posicao_fitting
#   posso estar com erro baixo mas a exponencial toda em cima da serie de retornos
  #     if(posicao_fitting > 0.5){
  if(i_erros > 1 && (sse > coeficiente_B_e_erros$sse[i_erros-1] && posicao_fitting >0.5)){
    coeficiente_B = coeficiente_B - ifelse(is.na(incremento_atual),0,incremento_atual)
    i_incremento = i_incremento + 1
    incremento_atual = incremento[i_incremento]
    coeficiente_B = coeficiente_B + ifelse(is.na(incremento_atual),0,incremento_atual)
  }else{
    coeficiente_B = coeficiente_B + ifelse(is.na(incremento_atual),0,incremento_atual)
    coeficiente_B
  }
  print(paste("B: ",coeficiente_B,", posicao_fitting: ",posicao_fitting," sse:",sse))
  if(is.na(incremento_atual)){
    break
  }
  
  i_erros = i_erros + 1
  i_posicao_fitting = i_posicao_fitting + 1
  
}



# Coeficientes B,tamanho_serie,volatilidade do menor SSE encontrado
coeficiente_B = as.double(as.character(coeficiente_B_e_erros$coeficiente_B[which.min(coeficiente_B_e_erros$sse)]))
sse =  as.double(as.character(coeficiente_B_e_erros$sse[which.min(coeficiente_B_e_erros$sse)]))
tamanho_serie =  as.double(as.character(coeficiente_B_e_erros$tamanho_serie[which.min(coeficiente_B_e_erros$sse)]))
volatilidade =  as.double(as.character(coeficiente_B_e_erros$volatilidade[which.min(coeficiente_B_e_erros$sse)]))

eixo_x_frequencias = funcao_distribuicao_probabilidade(serie)$valor_serie_retorno_eixo_x
exponencial = resultado_funcao_exponencial(eixo_x_frequencias)
# plot_previsao_com_B_e_exponencial(eixo_x_frequencias,exponencial)

