
# aprendendo Regex com report do Estoca Peidos Entregues ------------------
 library(wordcloud2)

d <- read_csv("data-raw/Estoca_pedidos.csv") %>%
    janitor::clean_names()

ped <- d %>%
    mutate( Qtde        = str_extract(string = items, pattern = "^[0-9]+") %>% as.numeric(),
               PRD         = str_extract(string = items, pattern = "[0-9A-Z]{6,16}"),
               gtin        = str_extract(string = items, pattern = "[0-9]{13}"),
               pedido      = str_extract(string = items, pattern = "(?<=\\()(.*)(?=\\) )"),
               pedidos_b   = str_extract(string = items, pattern = "\\(.*\\)"),
               descricao   = str_extract(string = items, pattern = ".*\\)"),
               descricao_2 = str_extract(string = items, pattern = "(?<=-\\s\\s)(.*)(?=\\()"),
               gtin_n = if_else( condition = is.na(gtin),
                                 true = PRD,
                                 false = gtin))

#  WordClound Cliente
ped %>%
select(cliente) %>%
    mutate( nome = str_extract( string = cliente,
                                pattern = "[^ ]+") %>%
                str_to_upper()) %>%
    count(separador) %>%
    wordcloud2(shape = 'star')

#  WordClound Cliente
ped %>%
    select(descricao_2) %>%
    mutate( nome = str_extract( string = descricao_2,
                                pattern = "[^ ]+") %>%
                str_to_upper()) %>%
    count(nome, sort = T) %>%
    wordcloud2(shape = 'star', fontWeight = 885, backgroundColor = "#0B134A")


# empacotador -------------------------------------------------------------

ped %>%
    select(empacotador, med_tempo_empacotamento_segundos) %>%
    ggplot(aes( x = empacotador, y = med_tempo_empacotamento_segundos)) +
    geom_boxplot() +
    scale_y_log10() +
    coord_flip() +
    geom_hline(yintercept = 2.5)

ped %>%
    select(empacotador, med_tempo_empacotamento_segundos) %>%
    count(empacotador, wt = med_tempo_empacotamento_segundos, sort = T)


# Separador ---------------------------------------------------------------

ped %>%
    select(separador, med_tempo_separacao_segundos) %>%
    mutate( separador = fct_reorder(separador, -med_tempo_separacao_segundos) ) %>%
    ggplot(aes( x = separador, y = med_tempo_separacao_segundos)) +
    geom_boxplot() +
    scale_y_log10() +
    coord_flip() +
    geom_hline(yintercept = 50, linetype = "longdash")+
    geom_hline(yintercept = 500, linetype = "longdash") +
    annotate("text", x = 'luigi.paixao@olistpax.com', y = 350, label = "Melhor Separador")

ped %>%
    select(separador, med_tempo_separacao_segundos) %>%
    count(separador, sort = T)



dados::dados_gapminder %>%
    filter(ano == max(ano)) %>%
    slice_max(n = 10, order_by = pib_per_capita)


ped %>%
    select(endereco_de_entrega) %>%
    mutate( estado = str_extract(string =endereco_de_entrega,
                                 pattern = "[A-Z]{2}$")) %>%
    count(estado, sort = T)



# modelo enviado no Telegram ( extrair espaço dia 20/09/2020) -------------


 str_extract( string = "Joel Rocha de Souza Maria Rocha",
                     pattern = "^([^ ]+ ){0,1}[^ ]+")


# PDF ---------------------------------------------------------------------

library(pdftools)
library(dplyr)

b <- pdftools::pdf_text(pdf = "data-raw/Dados Demais - Thomas Davenport.pdf")

w <- b %>% tidytext::unnest_tokens()

corpus
