setores_faixa_temporal = read.csv("por_semestre_b_volatilidade_incremento_sse.csv")
# dados = read.csv("setores_faixa_temporal.csv")
intersect(subset(dados,dados$Anos==2014)$Setores,subset(dados,dados$Anos==2008)$Setores)