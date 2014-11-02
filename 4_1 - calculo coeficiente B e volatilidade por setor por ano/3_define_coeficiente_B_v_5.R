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
incremento = c(2,1,0.5,0.1,0.05,0.1,0.01,0.005)
# 0.25,0.125,0.0625,0.03125,0.015625,0.0078125,0.00390625,0.001953125,0.0009765625,0.0004882812)
i_incremento = 1
coeficiente_B = 0.01 #tenho sse, comparo o sse atual com o antigo
i_erros =1
incremento_atual = incremento[1]
coeficiente_B_e_erros = data.frame()
posicoes_fitting = c()
i_posicao_fitting= 1
transicao = F
while(T){
  source("4_erro_coeficiente_B.R")  
  plot_previsao_com_B_e_exponencial(eixo_x_frequencias,exponencial)
  coeficiente_B_e_erros = rbind(coeficiente_B_e_erros,c(coeficiente_B, sse, rmse,volatilidade,tamanho_serie))
  colnames(coeficiente_B_e_erros) = c("coeficiente_B","sse", "rmse","volatilidade","tamanho_serie")
  print(coeficiente_B_e_erros)
  #virou o lado
  if(i_erros>2 && coeficiente_B_e_erros$sse[i_erros-1] >= coeficiente_B_e_erros$sse[i_erros]){
    incremento_atual = incremento[i_incremento]
    if(transicao){
      coeficiente_B = coeficiente_B - ifelse(is.na(incremento_atual),0,incremento_atual)
    }else{
      coeficiente_B = coeficiente_B + ifelse(is.na(incremento_atual),0,incremento_atual)
    }
  }else if(i_erros>2 && coeficiente_B_e_erros$sse[i_erros-1] < coeficiente_B_e_erros$sse[i_erros]){
    i_incremento = i_incremento + 1
    transicao = !transicao
    incremento_atual = incremento[i_incremento]
    if(transicao){
      
      coeficiente_B = coeficiente_B - ifelse(is.na(incremento_atual),0,incremento_atual)
    }else{
      coeficiente_B = coeficiente_B + ifelse(is.na(incremento_atual),0,incremento_atual)
    }
  }
  if(i_erros==1){
    incremento_atual = incremento[i_incremento]
    coeficiente_B = coeficiente_B + ifelse(is.na(incremento_atual),0,incremento_atual)
  }
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

