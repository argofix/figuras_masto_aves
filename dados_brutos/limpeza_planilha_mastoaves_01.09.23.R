
## Script de limpeza de dados do Programa Monitora aplicado na planilha de 
## mastoaves gerada por Elildo na reunião de acompanhamento do relatório do
## componente florestal realizada em 01/09/2023.



# install.packages("janitor") 
# install.packages("tidyverse")


# Carregando pacotes

library(janitor) 
library(tidyverse)


# Lendo os dados brutos da planilha validada COMOB - passada pelo Elido, criando uma nova planilha chamada "base_masto_aves"


base_masto_aves <- read_csv2("planilha_bruta_elildo/monitora_para_excel_2023-09-01.csv")



# Inspecionando e ajustando os nomes das colunas na nova planilha - base_masto_aves


# Inspecionando os nomes das colunas na planilha carregada


colnames(base_masto_aves) 


# Simplificando os nomes das colunas com a função clean_names() do pacote Janitor


base_masto_aves <- base_masto_aves %>% 
  clean_names()


# Inspecionando os nomes alterados das colunas


colnames(base_masto_aves)  


# Encurtando osnomes de algumas colunas


base_masto_aves <- base_masto_aves %>% 
  dplyr::rename(
      estacao = "estacao_do_ano",
      h_inicio = "hora_inicio",
      h_fim = "hora_fim",
      tempo_censo = "tempo_de_censo",
      velocidade = "velocidade_km_h",
      cond_clim = "condicao_climatica_ensolarado_nublado_e_chuvoso",
      observadores = "nome_dos_observadores",
      n_guia = "numero_do_animal_no_guia",
      especie = "especies",
      class_taxon_val = "clasificacao_taxonomica_validada",
      h_avistamento = "hora_avistamento",
      cont_tot_par = "contagem_total_ou_parcial",
      problema_amostragem = "teve_problema_durante_a_amostragem"
)


# Inspecionando os nomes encurtados das colunas


colnames(base_masto_aves)  


# Visualizando a planilha após os ajustes dos nomes das colunas


View(base_masto_aves) 


# Procurando por erros nos registros em todas as colunas da planilha e fazendo as correções


# Definindo uma função de visualização do registros das colunas para busca de erros

registros_unicos_coluna <- function(df, nome_col) {
  resultado <- {{df}} %>%
    distinct({{nome_col}}) %>%
    arrange({{nome_col}}) %>%
    print(n=Inf)
  
  return(View(resultado))
}



# Checando registros da coluna cnuc

registros_unicos_coluna(base_masto_aves, cnuc)



# Checando registros da coluna uc 

registros_unicos_coluna(base_masto_aves, uc)

# Correção dos nomes das UCs (Os nomes serão corrigidos conforme o CNUC/Decreto de criação)

base_masto_aves <- base_masto_aves %>%  
  mutate(
    uc = case_when(
      uc == "RESEX Riozinho da Liberdade" ~ "Resex Riozinho da Liberdade",
      uc == "Esec Jari" ~ "Esec do Jari",
      uc == "Esec Maracá" ~ "Esec de Maracá",
      uc == "Esec Pirapitinga" ~ "Esec de Pirapitinga",
      uc == "Esec da Terra do Meio/Resex Iriri" ~ "Esec da Terra do Meio",
      uc == "Flona do Tapajós" ~ "Flona de Tapajós",
      uc == "Rebio Gurupi" ~ "Rebio do Gurupi",
      uc == "Rebio Tapirapé" ~ "Rebio do Tapirapé",
      uc == "Rebio de Uatumã" ~ "Rebio do Uatumã",
      uc == "Resex Alto Tarauacá" ~ "Resex do Alto Tarauacá",
      uc == "Resex Cazumbá-Iracema" ~ "Resex do Cazumbá-Iracema",
      uc == "Resex Lago do Capanã Grande" ~ "Resex do Lago do Capanã Grande",
      uc == "Resex Riozinho do Anfrisio" ~ "Resex Riozinho do Anfrízio",
      uc == "Resex Tapajós Arapiuns" ~ "Resex Tapajós-Arapiuns",
      uc == "Resex Verde Para Sempre" ~ "Resex Verde para Sempre",
      uc == "Resex do Rio Cautário" ~ "Resex do Rio do Cautário",
      uc == "Parna dos Campos Amazonicos" ~ "Parna dos Campos Amazônicos",
      TRUE ~ uc))
     



# Visualizar nomes das UCs corrigidos

registros_unicos_coluna(base_masto_aves, uc)


# Checagem dos nomes das UC x número CNUC, para ver se há repetições

lista_nomes_cnuc <- base_masto_aves %>%
  distinct(cnuc, uc) %>% 
  print(n=Inf) %>% 
  View()




# Checando registros da coluna cnuc

registros_unicos_coluna(base_masto_aves, cnuc)




# # Checando registros da coluna ea

registros_unicos_coluna(base_masto_aves, ea)



# # Checando registros da coluna nome_ea

registros_unicos_coluna(base_masto_aves, nome_ea)

# Corrigindo os nomes errados

base_masto_aves <- base_masto_aves %>%
  mutate(
    nome_ea = case_when(
      nome_ea == "TIaracá" ~ "Tiaracá",
      nome_ea == "tiaracá" ~ "Tiaracá",
      nome_ea == "Trilha do carrapato" ~ "Trilha do Carrapato",
      nome_ea == "KAAPORI" ~ "Kaapori",
      nome_ea == "Manoel Teofilo" ~ "Manoel Teófilo",
      nome_ea == "Boa Esperança/ São Francisco" ~ "Boa Esperança/São Francisco",
      nome_ea == "2ª acampamento" ~ "2º Acampamento",
      nome_ea == "do Zé" ~ "Do Zé",
      TRUE ~ nome_ea))


# Visualizando os nome corrigidos

registros_unicos_coluna(base_masto_aves, nome_ea)



# Checando registros da coluna esforco

registros_unicos_coluna(base_masto_aves, esforco)



# Checando registros da coluna data

registros_unicos_coluna(base_masto_aves, data)



# Checando registros da coluna ano

registros_unicos_coluna(base_masto_aves, ano)



# Checando registros da coluna estacao Ajustar Erros!!

registros_unicos_coluna(base_masto_aves, estacao)

base_masto_aves <- base_masto_aves %>%
  mutate(
    estacao = case_when(
      estacao == "Chuvosa" ~ "chuvosa",
      estacao == "Seca" ~ "seca",
      estacao == "final de chuva" ~ "chuvosa",
      estacao == "Transição seca/chuva" ~ "seca/chuvosa", # foi mantida essa categoria como transição entre a estação seca e chuvosa
      TRUE ~ estacao))

registros_unicos_coluna(base_masto_aves, estacao)

# Checando registros da coluna h_inicio 

registros_unicos_coluna(base_masto_aves, h_inicio)



# Checando registros da coluna h_fim

registros_unicos_coluna(base_masto_aves, h_fim)



# Checando registros da coluna tempo_censo

registros_unicos_coluna(base_masto_aves, tempo_censo)



# Checando registros da coluna velocidade 
# Dois registros estranhos: 25.5 e 27.3 - Recomendo checar durante análises

registros_unicos_coluna(base_masto_aves, velocidade)



# Checando registros da coluna cond_clim

registros_unicos_coluna(base_masto_aves, cond_clim)

base_masto_aves <- base_masto_aves %>%
  mutate(
    cond_clim = case_when(
      cond_clim == "Chuvoso" ~ "chuvoso",
      TRUE ~ cond_clim))

registros_unicos_coluna(base_masto_aves, cond_clim)

# Checando registros da coluna obsrevadores - falta padronização nos registros

registros_unicos_coluna(base_masto_aves, observadores)



# Checando registros da coluna n_guia

registros_unicos_coluna(base_masto_aves, n_guia)



# Checando registros da coluna classe

registros_unicos_coluna(base_masto_aves, classe)



# Checando registros da coluna ordem

registros_unicos_coluna(base_masto_aves, ordem)



# Checando registros da coluna familia

registros_unicos_coluna(base_masto_aves, familia)



# Checando registros da coluna genero

registros_unicos_coluna(base_masto_aves, genero)



# Checando registros da coluna espécie

registros_unicos_coluna(base_masto_aves, especie)

base_masto_aves <- base_masto_aves %>%  
  mutate(
    especie = case_when(
      especie == "Aotus Nigriceps" ~ "Aotus nigriceps",
      especie == "Cebus Kaapori" ~ "Cebus kaapori",
      especie == "Callicebus Cinerascens" ~ "Callicebus cinerascens",
      especie == "Cebuella pygmaea niveiventris" ~ "Cebuella pygmaea",
      especie == "Eira Barbara" ~ "Eira barbara",
      especie == "Crypturellus Undulatus" ~ "Crypturellus undulatus",
      especie == "Galea sp" ~ "Galea sp.",
      especie == "MYoprocta acouchy" ~ "Myoprocta acouchy",
      especie == "Mico Argentatus" ~ "Mico argentatus",
      especie == "Mico Rondoni" ~ "Mico rondoni",
      especie == "Penelope Obscura" ~ "Penelope obscura",
      especie == "Saimiri Ustus" ~ "Saimiri ustus",
      especie == "Sapajus Libidinosus" ~ "Sapajus libidinosus",
      especie == "crypturellus soui" ~ "Crypturellus soui",
      especie == "crypturellus undulatus" ~ "Crypturellus undulatus",
      especie == "Aburria sp." ~ "Aburria sp",
      especie == "Alouatta sp." ~ "Alouatta sp",
      especie == "Ateles sp." ~ "Ateles sp",
      especie == "Callicebus sp." ~ "Callicebus sp",
      especie == "Callithrix sp." ~ "Callithirix sp",
      especie == "Cavia sp." ~ "Cavia sp",
      especie == "Cebus sp." ~ "Cebus sp",
      especie == "Chiropotes sp." ~ "Chiropotes sp",
      especie == "Coendou sp." ~ "Coendou sp",
      especie == "Crax sp." ~ "Crax sp",
      especie == "Crypturellus sp." ~ "Crypturellus sp",
      especie == "Dasypus sp." ~ "Dasypus sp",
      especie == "Galictis." ~ "Galictis",
      especie == "Guerlinguetus sp." ~ "Guerlinguetus sp",
      especie == "Mazama sp." ~ "Mazama sp",
      especie == "Mico sp." ~ "Mico sp",
      especie == "Microsciurus sp." ~ "Microsciurus sp",
      especie == "Myoprocta sp." ~ "Myoprocta sp",
      especie == "Nothura sp." ~ "Nothura sp",
      especie == "Pauxi sp." ~ "Pauxi sp",
      especie == "Penelope sp." ~ "Penelope sp",
      especie == "Odontophorus sp." ~ "Odontophorus sp",
      especie == "Ortalis sp." ~ "Ortalis sp",
      especie == "Pithecia sp." ~ "Pithecia sp",
      especie == "Rhynchotus sp." ~ "Rhynchotus sp",
      especie == "Saguinus sp." ~ "Saguinus sp",
      especie == "Saimiri sp." ~ "Saimiri sp",
      especie == "Sapajus sp." ~ "Sapajus sp",
      especie == "Tapirus sp." ~ "Tapirus sp",
      especie == "Tinamus sp." ~ "Tinamus sp",
      especie == "Urosciurus sp." ~ "Urosciurus sp",
      TRUE ~ especie))

registros_unicos_coluna(base_masto_aves, especie)


# Checando registros da coluna nive_taxon

registros_unicos_coluna(base_masto_aves, nivel_taxon)



# Checando registros da coluna taxon_validado

registros_unicos_coluna(base_masto_aves, taxon_validado)



# Checando registros da coluna class_taxon_val

registros_unicos_coluna(base_masto_aves, class_taxon_val)



# Checando registros da coluna h_avistamento

registros_unicos_coluna(base_masto_aves, h_avistamento)



# Checando registros da coluna n_animais

registros_unicos_coluna(base_masto_aves, n_animais)



# Checando registros da coluna cont_tot_par

registros_unicos_coluna(base_masto_aves, cont_tot_par)

base_masto_aves <- base_masto_aves %>%  
  mutate(
    cont_tot_par = case_when(
      cont_tot_par == "Parcial" ~ "parcial",
      cont_tot_par == "Total" ~ "total",
      TRUE ~ cont_tot_par))

registros_unicos_coluna(base_masto_aves, cont_tot_par)



# Checando registros da coluna distancia (# sugiro conferir valores!!)

registros_unicos_coluna(base_masto_aves, distancia)



# Checando registros da coluna plaqueta

registros_unicos_coluna(base_masto_aves, plaqueta)



# Checando registros da coluna problema_amostragem

registros_unicos_coluna(base_masto_aves, problema_amostragem)

base_masto_aves <- base_masto_aves %>%  
  mutate(
    problema_amostragem = case_when(
      problema_amostragem == "Sim" ~ "sim",
      problema_amostragem == "Não" ~ "não",
      TRUE ~ problema_amostragem))

registros_unicos_coluna(base_masto_aves, problema_amostragem)



# Visualizar e salvar a planilha corrigida ("limpa") em arquivo no formato .csv
# dentro da pasta "planilha_limpa"

View(base_masto_aves)

write.csv(base_masto_aves, "planilha_limpa/base_masto_aves_limpa_elido_arlindo_01.09.2023.csv", row.names = FALSE)

#base_masto_aves_limpa <- read_csv("planilha_limpa/base_masto_aves_limpa_elido_arlindo_01.09.2023.csv")

                                
                                  ########### ---- ############


# Observações:

# 1) Não foram "corrigidos" (padronizados) os registros das seguintes colunas: coletores, 
# observacoes

# 2) Os nomes e números das UCs já foram corrigidos de acordo com o CNUC

# 3) A planilha contem uma coluna "populacao"... creio que essa coluna não é da planilha original, 
# tendo sido criada pelo Elildo.





