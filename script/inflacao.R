


library(magrittr, include.only = '%>%')



#' SÉRIE HISTÓRICA DO IPCA CHEIO -------------------------------------------------------------------
ipca_ts <- sidrar::get_sidra(api = '/t/1737/n1/all/v/63,69,2266/p/all/d/v63%202,v69%202,v2266%2013') %>%

  janitor::clean_names()



#' - Número Índice
num_indice <- ipca_ts %>%

  dplyr::filter(variavel == 'IPCA - Número-índice (base: dezembro de 1993 = 100)') %>%

  dplyr::transmute(data = zoo::as.yearmon(mes_codigo, format = '%Y %m'),

                   valor = round(valor, 6)) %>%

  dplyr::filter(data >= zoo::as.yearmon(1995, 01)) %>%

  tibble::view()



#' - Variação Mensal
var_mensal <- ipca_ts %>%

  dplyr::filter(variavel == 'IPCA - Variação mensal') %>%

  dplyr::transmute(data = zoo::as.yearmon(mes_codigo, format = '%Y %m'),

                   valor = round(valor, 6)) %>%

  dplyr::filter(data >= zoo::as.yearmon(1995, 01)) %>%

  tibble::view()



#' - Acumulado Anual
acum_ano <- ipca_ts %>%

  dplyr::filter(variavel == 'IPCA - Variação acumulada no ano') %>%

  dplyr::transmute(data = zoo::as.yearmon(mes_codigo, format = '%Y %m'),

                   valor = round(valor, 6)) %>%

  dplyr::mutate(mes = lubridate::month(data)) %>%

  dplyr::filter(mes == 12) %>%

  dplyr::filter(data >= zoo::as.yearmon(1995, 01)) %>%

  dplyr::mutate(data = lubridate::year(data)) %>%

  dplyr::select(data, valor) %>%

  tibble::view()



#' SÉRIE HISTÓRICA DO IPCA GRUPOS ------------------------------------------------------------------
grupo_ts00_06 <- sidrar::get_sidra(api = '/t/655/n1/all/v/all/p/all/c315/7170,7445,7486,7558,7625,7660,7712,7766,7786/d/v63%202') %>%

  janitor::clean_names()



grupo_ts07_11 <- sidrar::get_sidra(api = '/t/2938/n1/all/v/all/p/all/c315/7170,7445,7486,7558,7625,7660,7712,7766,7786/d/v63%202,v66%204,v69%202') %>%

  janitor::clean_names()



grupo_ts12_19 <- sidrar::get_sidra(api = '/t/1419/n1/all/v/63,66,69/p/all/c315/7170,7445,7486,7558,7625,7660,7712,7766,7786/d/v63%202,v66%204,v69%202') %>%

  janitor::clean_names()



grupo_ts20_21 <- sidrar::get_sidra(api = '/t/7060/n1/all/v/63,66,69/p/all/c315/7170,7445,7486,7558,7625,7660,7712,7766,7786/d/v63%202,v66%204,v69%202') %>%

  janitor::clean_names()



var_infla_01 <- grupo_ts00_06 %>%

  dplyr::select(mes_codigo, valor, geral_grupo_subgrupo_item_e_subitem) %>%

  dplyr::rename('data' = mes_codigo,

                'valor' = valor,

                'grupos' = geral_grupo_subgrupo_item_e_subitem) %>%

  dplyr::mutate(grupos = dplyr::case_when(grupos == '1.Alimentação e bebidas' ~ 'Alimentação e bebidas',

                                          grupos == '2.Habitação' ~ 'Habitação',

                                          grupos == '3.Artigos de residência' ~ 'Artigos de residência',

                                          grupos == '4.Vestuário' ~ 'Vestuário',

                                          grupos == '5.Transportes' ~ 'Transportes',

                                          grupos == '6.Saúde e cuidados pessoais' ~ 'Saúde e cuidados pessoais',

                                          grupos == '7.Despesas pessoais' ~ 'Despesas pessoais',

                                          grupos == '8.Educação' ~ 'Educação',

                                          grupos == '9.Comunicação' ~ 'Comunicação'),

                data = zoo::as.yearmon(data, format = '%Y %m'),

                valor = round(valor, 6))



var_infla_02 <- grupo_ts07_11 %>%

  dplyr::filter(variavel == 'IPCA - Variação mensal') %>%

  dplyr::select(mes_codigo, valor, geral_grupo_subgrupo_item_e_subitem) %>%

  dplyr::rename('data' = mes_codigo,

                'valor' = valor,

                'grupos' = geral_grupo_subgrupo_item_e_subitem) %>%

  dplyr::mutate(grupos = dplyr::case_when(grupos == '1.Alimentação e bebidas' ~ 'Alimentação e bebidas',

                                          grupos == '2.Habitação' ~ 'Habitação',

                                          grupos == '3.Artigos de residência' ~ 'Artigos de residência',

                                          grupos == '4.Vestuário' ~ 'Vestuário',

                                          grupos == '5.Transportes' ~ 'Transportes',

                                          grupos == '6.Saúde e cuidados pessoais' ~ 'Saúde e cuidados pessoais',

                                          grupos == '7.Despesas pessoais' ~ 'Despesas pessoais',

                                          grupos == '8.Educação' ~ 'Educação',

                                          grupos == '9.Comunicação' ~ 'Comunicação'),

                data = zoo::as.yearmon(data, format = '%Y %m'),

                valor = round(valor, 6))



var_infla_03 <- grupo_ts12_19 %>%

  dplyr::filter(variavel == 'IPCA - Variação mensal') %>%

  dplyr::select(mes_codigo, valor, geral_grupo_subgrupo_item_e_subitem) %>%

  dplyr::rename('data' = mes_codigo,

                'valor' = valor,

                'grupos' = geral_grupo_subgrupo_item_e_subitem) %>%

  dplyr::mutate(grupos = dplyr::case_when(grupos == '1.Alimentação e bebidas' ~ 'Alimentação e bebidas',

                                          grupos == '2.Habitação' ~ 'Habitação',

                                          grupos == '3.Artigos de residência' ~ 'Artigos de residência',

                                          grupos == '4.Vestuário' ~ 'Vestuário',

                                          grupos == '5.Transportes' ~ 'Transportes',

                                          grupos == '6.Saúde e cuidados pessoais' ~ 'Saúde e cuidados pessoais',

                                          grupos == '7.Despesas pessoais' ~ 'Despesas pessoais',

                                          grupos == '8.Educação' ~ 'Educação',

                                          grupos == '9.Comunicação' ~ 'Comunicação'),

                data = zoo::as.yearmon(data, format = '%Y %m'),

                valor = round(valor, 6))



var_infla_04 <- grupo_ts20_21 %>%

  dplyr::filter(variavel == 'IPCA - Variação mensal') %>%

  dplyr::select(mes_codigo, valor, geral_grupo_subgrupo_item_e_subitem) %>%

  dplyr::rename('data' = mes_codigo,

                'valor' = valor,

                'grupos' = geral_grupo_subgrupo_item_e_subitem) %>%

  dplyr::mutate(grupos = dplyr::case_when(grupos == '1.Alimentação e bebidas' ~ 'Alimentação e bebidas',

                                          grupos == '2.Habitação' ~ 'Habitação',

                                          grupos == '3.Artigos de residência' ~ 'Artigos de residência',

                                          grupos == '4.Vestuário' ~ 'Vestuário',

                                          grupos == '5.Transportes' ~ 'Transportes',

                                          grupos == '6.Saúde e cuidados pessoais' ~ 'Saúde e cuidados pessoais',

                                          grupos == '7.Despesas pessoais' ~ 'Despesas pessoais',

                                          grupos == '8.Educação' ~ 'Educação',

                                          grupos == '9.Comunicação' ~ 'Comunicação'),

                data = zoo::as.yearmon(data, format = '%Y %m'),

                valor = round(valor, 6))



var_mensal <- rbind(var_infla_01,

                    var_infla_02,

                    var_infla_03,

                    var_infla_04)



acm_infla_02 <- grupo_ts07_11 %>%

  dplyr::filter(variavel == 'IPCA - Variação acumulada no ano') %>%

  dplyr::select(mes_codigo, valor, geral_grupo_subgrupo_item_e_subitem) %>%

  dplyr::rename('data' = mes_codigo,

                'valor' = valor,

                'grupos' = geral_grupo_subgrupo_item_e_subitem) %>%

  dplyr::mutate(grupos = dplyr::case_when(grupos == '1.Alimentação e bebidas' ~ 'Alimentação e bebidas',

                                          grupos == '2.Habitação' ~ 'Habitação',

                                          grupos == '3.Artigos de residência' ~ 'Artigos de residência',

                                          grupos == '4.Vestuário' ~ 'Vestuário',

                                          grupos == '5.Transportes' ~ 'Transportes',

                                          grupos == '6.Saúde e cuidados pessoais' ~ 'Saúde e cuidados pessoais',

                                          grupos == '7.Despesas pessoais' ~ 'Despesas pessoais',

                                          grupos == '8.Educação' ~ 'Educação',

                                          grupos == '9.Comunicação' ~ 'Comunicação'),

                data = zoo::as.yearmon(data, format = '%Y %m'),

                valor = round(valor, 6)) %>%

  dplyr::mutate(mes = lubridate::month(data)) %>%

  dplyr::mutate(data = lubridate::year(data)) %>%

  dplyr::filter(mes == 12) %>%

  dplyr::select(data, valor, grupos)



acm_infla_03 <- grupo_ts12_19 %>%

  dplyr::filter(variavel == 'IPCA - Variação acumulada no ano') %>%

  dplyr::select(mes_codigo, valor, geral_grupo_subgrupo_item_e_subitem) %>%

  dplyr::rename('data' = mes_codigo,

                'valor' = valor,

                'grupos' = geral_grupo_subgrupo_item_e_subitem) %>%

  dplyr::mutate(grupos = dplyr::case_when(grupos == '1.Alimentação e bebidas' ~ 'Alimentação e bebidas',

                                          grupos == '2.Habitação' ~ 'Habitação',

                                          grupos == '3.Artigos de residência' ~ 'Artigos de residência',

                                          grupos == '4.Vestuário' ~ 'Vestuário',

                                          grupos == '5.Transportes' ~ 'Transportes',

                                          grupos == '6.Saúde e cuidados pessoais' ~ 'Saúde e cuidados pessoais',

                                          grupos == '7.Despesas pessoais' ~ 'Despesas pessoais',

                                          grupos == '8.Educação' ~ 'Educação',

                                          grupos == '9.Comunicação' ~ 'Comunicação'),

                data = zoo::as.yearmon(data, format = '%Y %m'),

                valor = round(valor, 6)) %>%

  dplyr::mutate(mes = lubridate::month(data)) %>%

  dplyr::mutate(data = lubridate::year(data)) %>%

  dplyr::filter(mes == 12) %>%

  dplyr::select(data, valor, grupos)




acm_infla_04 <- grupo_ts20_21 %>%

  dplyr::filter(variavel == 'IPCA - Variação acumulada no ano') %>%

  dplyr::select(mes_codigo, valor, geral_grupo_subgrupo_item_e_subitem) %>%

  dplyr::rename('data' = mes_codigo,

                'valor' = valor,

                'grupos' = geral_grupo_subgrupo_item_e_subitem) %>%

  dplyr::mutate(grupos = dplyr::case_when(grupos == '1.Alimentação e bebidas' ~ 'Alimentação e bebidas',

                                          grupos == '2.Habitação' ~ 'Habitação',

                                          grupos == '3.Artigos de residência' ~ 'Artigos de residência',

                                          grupos == '4.Vestuário' ~ 'Vestuário',

                                          grupos == '5.Transportes' ~ 'Transportes',

                                          grupos == '6.Saúde e cuidados pessoais' ~ 'Saúde e cuidados pessoais',

                                          grupos == '7.Despesas pessoais' ~ 'Despesas pessoais',

                                          grupos == '8.Educação' ~ 'Educação',

                                          grupos == '9.Comunicação' ~ 'Comunicação'),

                data = zoo::as.yearmon(data, format = '%Y %m'),

                valor = round(valor, 6)) %>%

  dplyr::mutate(mes = lubridate::month(data)) %>%

  dplyr::mutate(data = lubridate::year(data)) %>%

  dplyr::filter(mes == 12) %>%

  dplyr::select(data, valor, grupos)




acm_mensal <- rbind(acm_infla_02,

                    acm_infla_03,

                    acm_infla_04)



pesos_atuais <- grupo_ts20_21 %>%

  dplyr::filter(variavel == 'IPCA - Peso mensal',

                mes_codigo == 202104) %>%

  dplyr::select(mes_codigo, valor, geral_grupo_subgrupo_item_e_subitem) %>%

  dplyr::rename('data' = mes_codigo,

                'valor' = valor,

                'grupos' = geral_grupo_subgrupo_item_e_subitem) %>%

  dplyr::mutate(grupos = dplyr::case_when(grupos == '1.Alimentação e bebidas' ~ 'Alimentação e bebidas',

                                          grupos == '2.Habitação' ~ 'Habitação',

                                          grupos == '3.Artigos de residência' ~ 'Artigos de residência',

                                          grupos == '4.Vestuário' ~ 'Vestuário',

                                          grupos == '5.Transportes' ~ 'Transportes',

                                          grupos == '6.Saúde e cuidados pessoais' ~ 'Saúde e cuidados pessoais',

                                          grupos == '7.Despesas pessoais' ~ 'Despesas pessoais',

                                          grupos == '8.Educação' ~ 'Educação',

                                          grupos == '9.Comunicação' ~ 'Comunicação'),

                valor = round(valor, 2))













