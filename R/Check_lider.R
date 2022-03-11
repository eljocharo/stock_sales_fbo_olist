
library(MetBrewer); library(tidyverse)


lider <- read_csv( "data-raw/360.csv") %>% janitor::clean_names()


lider %>%
    pivot_longer( cols = c(5:14),
                  names_to = "perguntas") %>%
    select(-c(1:2)) %>%
    filter( value %in% c( "Não", "Sim") ) %>%
    count(quem_e_seu_lider_imediato, perguntas, value ) %>%
    pivot_wider( names_from = value,
                 values_from = n,
                 values_fill = 0) %>%
    mutate( total =  Sim + Não,
            pct = Não / total * 100,
            perguntas = forcats::fct_reorder( .f = perguntas,.x =  pct)) %>%
ggplot( aes( x = pct, y = perguntas )) +
    geom_col(aes(fill = perguntas)) +
    facet_wrap(~quem_e_seu_lider_imediato)+
    scale_fill_manual( values = met.brewer("Isfahan1", 8)) +
    theme_minimal() +
    theme(legend.position="none")


# fa
lider %>%
    pivot_longer( cols = c(5:14),
                  names_to = "perguntas") %>%
    select(-c(1:2)) %>%
    filter( value %in% c( "Não", "Sim") ) %>%
    count(quem_e_seu_lider_imediato, perguntas, value ) %>%
    pivot_wider( names_from = value,
                 values_from = n,
                 values_fill = 0) %>%
    mutate( total =  Sim + Não,
            pct = Sim / total,
            pct_txt = scales::percent(pct, accuracy = 1),
            perguntas = forcats::fct_reorder( .f = perguntas,.x =  Sim)) %>%
    ggplot( aes( x = Sim, y = perguntas )) +
    geom_col(aes(fill = perguntas ), color = "black") +
    labs( title = "Feedback operacional Líder",
          x = "quantidade de respostas sim") +
    geom_text( aes(label = pct_txt), hjust = 1, color="white") +
    facet_wrap(~quem_e_seu_lider_imediato)+
    scale_fill_manual( values = met.brewer("Isfahan1", 8)) +
    theme_minimal() +
    theme(legend.position="none")

