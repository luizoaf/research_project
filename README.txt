Funcionamento dos scripts

1. Extração da base de dados, 2008 até agosto de 2014
2. cria_tabela_serie_retornos_de_todas_as_acoes
	2.1 Elimina NA
	2.2 Elimina o vetor das datas
	2.3 Inverte a ordem da base para ficar da data mais recente para a mais antiga
	2.4  |(log(valor_acao_dia) - log(valor_acao_dia+1))| / #Da data mais antiga para o mais recente
              sd( do vetor de toda a diferenciação feita no numerador)
3. janelamento(serie_retornos_normalizado,30)