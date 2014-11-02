dados = read.csv("papeis_da_ibovespa_2007_2012.csv",sep=";")

par(mfrow=c(4,7))
for(i in 2:26){
  plot(main=names(dados)[i],dados[,i],type="l")
}
# plot(dados$ABEV3,type="l")
# plot(dados$ALLL3.SA,type="l")
# plot(dados$CSNA3.SA,type="l")
# plot(dados$BRAP4.SA,type="l")
# # > 98 %
# plot(dados$HGTX3.SA,type="l")
# plot(dados$RENT3.SA,type="l")
# # > 97.8%
# plot(dados$POMO4.SA,type="l")
# # "ABEV3.SA" "ALLL3.SA" "BBAS3.SA" "BRAP4.SA" "BRKM5.SA" "CESP6.SA" "CMIG4.SA" "CRUZ3.SA" "CSAN3.SA" "CSNA3.SA" "ELPL4.SA" "HGTX3.SA"
# # [13] "ITUB4.SA" "POMO4.SA" "RENT3.SA"
# plot(dados$POMO4.SA,type="l")