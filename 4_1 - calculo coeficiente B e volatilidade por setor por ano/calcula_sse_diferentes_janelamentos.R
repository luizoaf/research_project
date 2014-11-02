# janelamentos_avaliados = c(seq(10,90,by=10))
# janelamentos_avaliados = c(seq(130,240,by=10))
janelamentos_avaliados = c(seq(120,180,by=30))
# janelamentos_avaliados=90
for(tamanho_janelamento in janelamentos_avaliados){
  source("media_acoes_por_setor_100_porcento_com_janelamento_30_dias_v4.R")
}