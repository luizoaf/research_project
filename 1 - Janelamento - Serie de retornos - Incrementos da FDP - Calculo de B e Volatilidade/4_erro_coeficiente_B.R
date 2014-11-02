serie_retornos_frequencia = funcao_distribuicao_probabilidade(serie,incremento)$frequencia_eixo_y
eixo_x_frequencias = funcao_distribuicao_probabilidade(serie,incremento)$valor_serie_retorno_eixo_x
exponencial = resultado_funcao_exponencial(eixo_x_frequencias)
volatilidade = calcula_volatilidade(serie)

sse = sum(( exponencial-serie_retornos_frequencia)^2)
tamanho_serie = length(serie_retornos_frequencia)
rmse = sqrt(mean((exponencial-serie_retornos_frequencia)^2))


