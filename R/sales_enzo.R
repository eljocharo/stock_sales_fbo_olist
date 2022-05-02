
# library
library(tidyverse)
library(tidyquant)
library(lubridate)
library(gt)
# Imports -----------------------------------------------------------------
# Import faturamento Enzo
sales_mes_atual <-
    data.table::fread("data-raw/Enzo/sales/2022/2022-04.csv", encoding = "UTF-8") %>%
    janitor::clean_names() %>% as_tibble()
# Import data faturamento Q1-2022
Q1_2022 <- read_rds("data-raw/Enzo/sales/2022/Q1_2022.rds")
# Consolidação 2022
sales <- sales_mes_atual %>% bind_rows(Q1_2022)
# ETL Faturamento Enzo
sales_etl <- sales |>
    mutate( mes = lubridate::month(data_fat, label = T, abbr = T),
            mes_ =  lubridate::floor_date( data_fat, unit = "month"),
            gtin_char = as.character(product_gtin),
            cnpj_short = stringr::str_sub(cnpj,1,8),
            dia_s = lubridate::wday(data_fat, label = TRUE, abbr = TRUE),
            week = lubridate::week(data_fat))
# Limpando as variaveis
rm(sales_mes_atual); rm(sales_others); rm(sales);rm(Q1_2022)

# Import Estoca_G
  estoca_g <- readr::read_csv(file = "data-raw/Estoca_G/2022-04-30_Estoca_G.csv") %>%
     janitor::clean_names() %>% filter( !nome_da_loja %in%
                                            c('Olist', 'Olist 2', 'Outlet Olist', 'H2O Purificadores (Olist)') ) %>%
     mutate( seller_type =
                 case_when(
                     nome_da_loja == "Olist (Protheus)" ~ "1P-Barueri",
                     TRUE~ "3P"))
# Import CSV da query do Enzo (Stock)
 stk_enzo <- data.table::fread("data-raw/Enzo/stock/2022-05-02_stock.csv") %>%
     janitor::clean_names() %>% as_tibble()

# Import data (Isamara Sellers)
 id_sheet <- "https://docs.google.com/spreadsheets/d/1WCK0TkoeHODmjPyq73vZoxqmexkq70QECL953dG0FYw/edit#gid=0"
# Import google seller Id ISA
 isa_seller <-  googlesheets4::read_sheet(id_sheet) %>%
     janitor::clean_names()
 rm(id_sheet)
# tbl apoio
 tbl <- 'https://docs.google.com/spreadsheets/d/15C60TQ5t6gViblOEU4slHXGhUIYjAt_vtO14xFIk1Tk/edit#gid=0'
 tbl_data <- googlesheets4::read_sheet(ss = tbl)
 rm(tbl)
#  gerando a lista unica de seller ISA
 seller_isamara_unique <-  isa_seller %>%
     select("seller_id_fbo" = seller_id) %>%
     bind_rows(
         isa_seller %>%
             select(seller_id_fbo)
     )   %>%  distinct()
# Resuno
 stk %>%
     filter( seller_id %in% seller_isamara_unique$seller_id_fbo) %>%
     filter(quantity != 0) %>%
     group_by(seller_type) %>%
     summarise( seller = n_distinct(seller_id),
                stock  = sum(quantity ),
                gmv = sum(value_stk),
                sku = n_distinct(sku))
estoca_g %>%
     group_by(seller_type) %>%
     filter(disponivel > 0, seller_type == '3P') %>%
     summarise( seller = n_distinct(nome_da_loja),
                stock  = sum(no_estoque ),
                sku = n_distinct(sku))

# Dados Stock Enzo que não está no estoca
 stk %>%
     filter( seller_id %in% seller_rel_isa$seller_id_fbo) %>%
     filter(quantity > 0,seller_type == '3P_FBO' ) %>%
 anti_join( estoca_g %>%
     filter(disponivel > 0, seller_type == '3P') , by = "sku")

# Sellers ISA que não está na query
 isa_seller %>%
     anti_join( stk, by = c("seller_id_fbo"="seller_id"))

# Touch Point Werner( Estoca ) --------------------------------------------

 # Estoca
 estoca_g %>%
     group_by(nome_da_loja) %>%
     add_tally( wt = disponivel, name = "disponivel_total") %>%
     ungroup() %>%
     group_by(seller_type) %>%
     # filter( nome_da_loja != 'Olist (Protheus)') %>%
     summarise(  Sellers = n_distinct(nome_da_loja[disponivel > 0 ]),
                 Sellers_Without_Stock = n_distinct(nome_da_loja[disponivel_total ==0]),
                 SKUs = n_distinct(sku[disponivel > 0]),
                 SKU_Stock_Out = n_distinct(sku[disponivel== 0 & bloqueado== 0]),
                 Unit_stock_disp = sum(disponivel) ) |>
     ungroup() %>% gt::gt()

 # Marcos
 marcos_etl %>%
     group_by(seller_type)%>%
     summarise(  Valor = sum(stock_value),
                 SKU_liquido_7d = n_distinct( product_sku[sku_items_until_7d > 0]),
                 Sellers_liquido_7d =  n_distinct( seller_id[sku_items_until_7d > 0])
     )
 # Query de stock do Enzo
 bStocOUt <- estoca_g %>%
     filter(disponivel ==0, seller_type =='3P') %>%
     left_join( sales_etl %>% select(seller_id, sku ) %>% distinct()) %>%
     select(-c("items_number", "no_estoque", "bloqueado", "seller_type", "reservado"))

 tbl_sales_stock_out <- sales_etl %>%
     filter(sku %in% bStocOUt$sku) %>%
     select( data_fat, seller_id,sku, gmv_brl) %>%
     group_by(seller_id,sku) %>%
     summarise(gmv = sum(gmv_brl),.groups = "drop",
               data_min_sales = min(data_fat),
               data_max_sales = max(data_fat)) %>%
     left_join( sales_etl %>% select(seller_id, brand) %>% distinct())

 tbl_sales_stock_out %>%
     write.csv("2022-04-29_Stock_OUt_FBO.csv")

 # Valor R$ Stock Enzo
 stk %>%
     filter( gtin %in% estoca_g$codigo_de_barras) %>%
    group_by(seller_type) %>%
     summarise(Valor = sum(value_stk)
     )

# Sales (Day, Month, Comparation)  ----------------------------------------------------

 # Diario Sales
 sales_etl %>%
     filter(seller_type == '3P', data_fat>='2022-04-01',
     ) %>%
     group_by(data_fat, dia_s ) %>%
     summarise( Qtde_sales_Unit = sum(qtde_uni),
                GMV = sum(gmv_brl),
                seller_liq = n_distinct(seller_id)) %>%
     ungroup() %>%
     mutate( gmv_t = scales::dollar(GMV,  scale = 1e-3,
                                    prefix = "", suffix = "k", accuracy = 1)
             ,label_text = str_c( Qtde_sales_Unit, "\n", gmv_t, " GMV")
             ,data_fat_ = data_fat %>% as.character()
     ) %>%
     writexl::write_xlsx("Fat.diario.xlsx")

 ggplot(aes( x = data_fat_, y = Qtde_sales_Unit), .groups = data_fat_) +
     geom_col(fill = "#0C29D0", color = "black") +
     geom_text(aes(label = label_text),size = 3, vjust = 1, color = "white") +
     labs( title = "Faturamento 3P FBO - abr.22",
           subtitle = "",
           x = "",
           y = "Sales Unit")+
     theme_bw( )+
     theme(axis.text.x = element_text(size=10, angle = 90))

 # mensal
 fat_olist <- sales_etl |>
     # filter( mes != "2022-04-01") |>
       group_by( mes, seller_type) |>
       summarise(faturamento = sum(gmv_brl),
                 sku = n_distinct(sku),
                 pedidos = n_distinct(ord),
                 sellers = n_distinct(seller_id),
                 .groups = "drop") |>
    mutate( gmv_t = scales::dollar(faturamento, scale = 1e-6,  prefix = ""))

    fat_olist |>
     ggplot(aes(mes, faturamento, fill = seller_type )) +
        geom_col(color = "black") +
        scale_fill_manual( values = c("#364EF7", "#0A1F9C", "#0B134A", "#312F4F")) +
        expand_limits( y =  seq(50, 500, by = 50)) +
        scale_y_continuous(labels = scales::dollar_format(scale = 1e-6)) +
        geom_text(aes(label= gmv_t), vjust = 1, color = "white") +
        theme_tq() +
        labs( title = "Faturamento Olist 2022", x = "", y = "GMV R$") +
         facet_wrap( ~seller_type, scales = "free_y") +
        theme( legend.position = "none")

# mes atual vs mes anterior

    sales_etl %>%
        filter( mes %in% c("mar","abr")) %>%
        mutate(dia = lubridate::day(data_fat)) %>%
        group_by(seller_type, mes, dia) %>%
        summarise( fat_dia = sum(gmv_brl)) %>%
        mutate( fat_acc = cumsum( fat_dia)) %>%
        ungroup() %>%
        mutate(gmv_t = scales::dollar(fat_acc, scale = 1e-6,  prefix = "")) %>%
        ggplot(aes(x = dia, y = fat_acc, group = mes, colour = mes)) +
        geom_line( size = 2) +
        scale_color_manual(values=c("#9498B8", "#0C29D0")) +
        scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, prefix = "", suffix = "M" )) +
        ggrepel::geom_text_repel(aes(label= gmv_t)) +
        facet_wrap(~seller_type, scales = "free_y") +
        geom_vline(xintercept = 30, colour = "red", linetype = "dashed", size = 1) +
        theme_tq() +
        labs( title = "Faturamento acumulado Olist",
              x = "dia do mês", y = "GMV R$",
              subtitle =  "mar vs abr",
              caption = "dia 24 - linha vertical vermelha") +
        theme(legend.position = c(.35, 1.07),
              legend.direction = "horizontal",
              legend.text = element_text(size = 15),
              legend.title = element_text(color = "white"))


# Média movel pedidos -----------------------------------------------------

    m_v <-    sales_etl |>
        filter(data_fat >= '2022-02-21' & data_fat<= "2022-04-10") |>
        mutate(week = floor_date(data_fat, unit = "week"),
               week_year = lubridate::isoweek(data_fat)) |>
        group_by( week_year, seller_type) |>
        summarise( pedido = n_distinct(ord),
                   unit = sum(qtde_uni),
                   mv_last_7 = round(unit/7, 0) ) |>
        ungroup()

    m_v |>
        clipr::write_clip()

    m_v |>
        filter(seller_type %in% c("1P_BARUERI", "3P")) |>
        ggplot(aes( x = week_year  , y = mv_last_7, group=seller_type, colour=seller_type   ) )+
        geom_line() +
        geom_text(aes(label = mv_last_7))

    m_v |>
        ggplot(aes(x = week_year, y = mv_last_7)) +
        geom_col() +
        facet_wrap(~seller_type, scales = "free_y")

# Faturamento BIG Geral -------------------------------------------------------

    sales_2021 <- read_rds("data-raw/Enzo/sales/2021/sales_2021.rds")

    sales_2021 <-   sales_2021  |>
        mutate( mes = lubridate::month(data_fat, label = T, abbr = T),
                mes_ =  lubridate::floor_date( data_fat, unit = "month"),
                gtin_char = as.character(product_gtin),
                cnpj_short = stringr::str_sub(cnpj,1,8),
                dia_s = lubridate::wday(data_fat, label = TRUE, abbr = TRUE),
                week = lubridate::week(data_fat))

    big_sales <- sales_2021 %>% bind_rows( sales_etl)

    rm(sales_2021); rm(sales_etl)

    # Adicionar o trimestre
    big_sales <-  big_sales %>%
        mutate(qtr = quarter(data_fat, with_year = T),
               YM = str_sub(string = data_fat, start = 1, end = 7))

   mes_fat <-  big_sales %>%
        group_by(YM, mes) %>%
        summarise(gmv = sum(gmv_brl), .groups = "drop")


    options(scipen = 99999999999)
   mes_fat %>%
       mutate(
               year = str_sub( YM, 1,4),
               gmv = gmv %>% scales::dollar( scale = 1e-6, prefix = "")) %>%
       pivot_wider(names_from =  mes,
                   values_from =  gmv,
                   -YM) %>% writexl::write_xlsx("fat_mensal.xlsx")

# Curva ABC Stock---------------------------------------------------------------

 sales_abc <-  sales_etl %>%
       group_by(mes, gtin_char) %>%
       summarise( gmv = sum(gmv_brl),
                  unit  = sum(qtde_uni),
                  price = min(preco_brl),
                  .groups = "drop")

  # filtrando o mes para o ABC
   sales_abc_etl <- sales_abc %>%
       filter(mes == 'mar') %>%
       arrange(desc(gmv)) %>%
       mutate( pct_gmv_1_acc = cumsum(gmv),
               gtin_1 = 1:n(),
               pct_gtin_1 = gtin_1 / n(),
               curva_abc = case_when(
                   pct_gtin_1  <= 0.05  ~ "A",
                   pct_gtin_1  <= 0.25 ~ "B",
                   pct_gtin_1  <= 0.50 ~ "C",
                   TRUE ~ "D" )
               )
   # resultado abc
   sales_abc_etl %>%
       group_by( curva_abc ) %>%
   summarise( n_gtin = n_distinct(gtin_char),
              gmv = sum(gmv),
              .groups = "drop") %>%
       mutate( pct_gtin= (n_gtin / sum(n_gtin)) %>% scales::percent(),
               pct_gmv= (gmv / sum(gmv)) %>% scales::percent())

   # Comparar com gtin Estoca

   estoca_g %>%
       left_join( sales_abc_etl, by = c("codigo_de_barras"="gtin_char")) %>%
       mutate( gmv_stock_FBO = disponivel * price) %>%
       filter( seller_type == '3P', disponivel >0) %>%
       group_by(curva_abc) %>%
       summarise(gtin_FBO = n(),
                 gmv_stock_FBO = sum(gmv_stock_FBO)) %>%
       replace_na( list( curva_abc = "No_sales", gmv_stock_FBO = 0) ) %>%
       mutate( pct_gtin_FBO = gtin_FBO / sum(gtin_FBO),
               pct_gtin_FBO = pct_gtin_FBO %>% scales::percent(accuracy = 1)) %>%
       relocate( pct_gtin_FBO, .after = gtin_FBO )


# TOP 10 (Seller, Categorias, SKU)  --------------------------------------------------
# Seller
   top_sellers   <-
        sales_etl %>%
        filter( mes == "abr",
                seller_type %in% c("3P", "Not_FBO")) %>%
        group_by(seller_type, brand) %>%
        summarise( gmv = sum(gmv_brl), .groups = "drop") %>%
        group_by(seller_type) %>%
        arrange( desc(gmv ), .by_group = T) %>%
        mutate(brand = as_factor(brand) %>% fct_lump(n = 10, w = gmv)) %>%
        group_by(seller_type, brand) %>%
        summarise( gmv = sum(gmv), .groups = "drop") %>%
        mutate( index = 1:n(),
                brand = fct_reorder(brand, gmv),
                brand = brand %>% fct_relevel("Other", after = 0),
                gmv_t = scales::dollar(gmv, scale = 1e-3,  prefix = "", suffix = "k", accuracy = .1)) %>%
       group_by(seller_type) %>%
        add_tally( wt = gmv, name = "total_gmv") %>%
            ungroup() %>%
        mutate( pct_total = total_gmv / (max(total_gmv) + min(total_gmv)),
                pct_total_t = scales::percent(pct_total, accuracy = 0.1),
                total_gmv_t = scales::dollar(total_gmv, scale = 1e-6,  prefix = "", suffix = "M", accuracy = .1)) %>%
            rowwise() %>%
            mutate( pct_seller = gmv / total_gmv,
                    pct_seller_t = scales::percent(pct_seller, accuracy = 0.1),
                    seller_type_t = str_c( seller_type, " GMV total ", total_gmv_t, " ( ", pct_total_t, " )"),
                    gmv_t = str_c( gmv_t, " (", pct_seller_t, ")"))
# PLOT
    top_sellers %>%
        ggplot(aes( y = brand, x = gmv, fill = seller_type)) +
        geom_col(color = "black") +
        geom_text( aes(label = gmv_t), hjust = 1, color = "white") +
        facet_wrap( ~seller_type, scales = "free") +
        scale_x_log10(labels = scales::dollar_format(scale = 1e-3, prefix = "", suffix = "k" )) +
        scale_fill_manual( values = c("#364EF7", "#0A1F9C"))+
        theme_tq() +
        labs(title = "Top 10 Seller GMV abr-22", y = "") +
        facet_wrap( ~seller_type_t, scales = "free") +
        theme(legend.position = "none" )

# Categorias
        sales_etl %>%
        filter( mes == "abr") %>%
           count(seller_type, categoria, sort = T, wt = gmv_brl, name = "gmv" ) %>%
            mutate(categoria = as_factor(categoria) %>% fct_lump(n = 10, w = gmv)) %>%
            mutate( index = 1:n(),
                    categoria = fct_reorder(categoria, gmv),
                    categoria = categoria %>% fct_relevel("Other", after = 0)) %>%
            count(seller_type,categoria, sort = T, wt = gmv, name = "gmv" ) %>%
            ungroup() %>%
            ggplot(aes( y = categoria, x = gmv, fill = seller_type)) +
            geom_col(color = "black") +
            scale_fill_manual( values = c("#364EF7", "#0A1F9C", "#0B134A", "#312F4F"))+
            scale_x_continuous(labels = scales::dollar_format(scale = 1e-3, prefix = "", suffix = "k" )) +
            theme_tq() +
            labs(title = "Top 10 Categorias GMV") +
            facet_wrap( ~seller_type, scales = "free") +
            theme(legend.position = "top",
                  legend.justification='left',
                  legend.direction='horizontal',
                  legend.title = element_blank() )
# SKUs
     top_sku   <-
        sales_etl %>%
        filter( mes == "abr") %>%
        mutate( desc_etl =  str_extract( descricao , pattern = "^([^ ]+ ){0,2}[^ ]+"),
                desc_etl  = str_c(desc_etl, "(", sku, ")" )) %>%
        group_by(seller_type, desc_etl) %>%
        summarise( gmv = sum(gmv_brl),
                   peças = sum(qtde_uni),
                   .groups = "drop") %>%
        mutate( desc_etl =  str_c( desc_etl," ",peças," unit")) %>%
        group_by(seller_type) %>%
        arrange( desc(gmv ), .by_group = T) %>%
        mutate(desc_etl = as_factor(desc_etl) %>% fct_lump(n = 10, w = gmv, other_level ="Outros")) %>%
        group_by(seller_type, desc_etl) %>%
        summarise( gmv = sum(gmv), .groups = "drop") %>%
        mutate( index = 1:n(),
                desc_etl = fct_reorder(desc_etl, gmv),
                desc_etl = desc_etl %>% fct_relevel("Outros", after = 0),
                gmv_t = scales::dollar(gmv, scale = 1e-3,  prefix = "", suffix = "k")) %>%
        group_by(seller_type) %>%
        add_tally( wt = gmv, name = "total_gmv") %>%
        ungroup() %>%
        add_tally(  wt = gmv )

     top_sku <-
        top_sku %>%
        mutate( pct_total = total_gmv / n,
                pct_total_t = scales::percent(pct_total),
                total_gmv_t = scales::dollar(total_gmv, scale = 1e-6,  prefix = "", suffix = "M")) %>%
        mutate( pct_seller = gmv / total_gmv,
                pct_seller_t = scales::percent(pct_seller, accuracy = 0.1),
                seller_type_t = str_c( seller_type, " GMV total ", total_gmv_t, " ( ", pct_total_t, " )"),
                gmv_sku = scales::dollar(gmv, scale = 1e-3,  prefix = "", suffix = "k"),
                gmv_t = str_c( gmv_sku , " (", pct_seller_t, ")")) %>% view()
 # PLOT
    top_sku %>%
        ggplot(aes( y = desc_etl, x = gmv, fill = seller_type)) +
        geom_col() +
        geom_text( aes(label = gmv_t), hjust = 1, color = "white") +
        facet_wrap( ~seller_type, scales = "free") +
        scale_x_log10(labels = scales::dollar_format(scale = 1e-3, prefix = "", suffix = "k" )) +
        scale_fill_manual( values = c("#364EF7", "#0A1F9C", "#0B134A", "#ABC6FF"))+
        theme_tq() +
        labs(title = "Top 10 SKUs GMV abr-22", y = "") +
        facet_wrap( ~seller_type_t, scales = "free") +
        theme(legend.position = "none" )

# Box Plot valor de vendas ------------------------------------------------

    sales_etl |>
        filter( mes == "mar") |>
        mutate( seller_type = fct_reorder(seller_type, -preco_brl)) %>%
         ggplot(aes(x = "", y = preco_brl)) +
        # geom_violin() +
        geom_boxplot(fill = "#ABC6FF") +
        scale_y_continuous(limits = c(0, 3200), n.breaks = 30) +
        # expand_limits( y =  seq(50, 500, by = 50)) +
        # scale_y_log10(labels = scales::dollar_format( prefix = "", suffix = "" )) +
            facet_wrap(~seller_type, ncol = 4) +
        theme_tq() +
        stat_summary( fun = mean, geom = "point", color="#6A00A3") +
        geom_hline( yintercept = 100, linetype = "solid", color = "#E64C2D", size = .5 ) +
        labs( x = "")

    # Tabela da distribuição dos itens vendidos no olist
    sales_etl |>
        filter( mes == "abr") %>%
        select(seller_type, preco_brl) %>%
        group_by(seller_type) %>%
        summarise( min = min(preco_brl),
                       q1 = quantile(preco_brl, probs = .25),
                   mediana = quantile(preco_brl, probs = .50),
                   q3 = quantile(preco_brl, probs = .75),
                   q99 = quantile(preco_brl, probs = .99),
                   media = mean(preco_brl),
                   sd = sd(preco_brl),
                   max = max(preco_brl))


    sales_etl |>
        filter( data_fat    == "2022-04-18", seller_type=="3P" ) %>%
        summarise(c = n_distinct(ord))


# Analise de vendas por preço e gtin duplicado ----------------------------

    sales_etl |>
        filter(seller_type %in% c("3P", "Not_FBO"), mes == 'abr' ) |>
        group_by( seller_type) |>
    summarise( unidade = sum(qtde_uni) ,
               pedidos = n_distinct(ord),
               sku = n_distinct(sku),
               gtins = n_distinct(gtin_char),
               seller = n_distinct(seller_id),
               gmv = sum(gmv_brl))

    sales_c <- sales_etl |>
        filter( gtin_char %in% estoca_g$codigo_de_barras,
                seller_type %in% c("3P", "Not_FBO") )

   sku_venda_dup <-  sales_etl |>
        group_by(seller_type, gtin_char) |>
        summarise( n_seller = n_distinct(seller_id), .groups = "drop") |>
       filter(n_seller >0) |>
        pivot_wider( names_from = seller_type,
                     values_from = n_seller,
                     values_fill = 0) |>
        filter( Not_FBO !=0, `3P` !=0)

   gtin_final <-   sales_etl |>
      filter( gtin_char %in% sku_venda_dup$gtin_char) |>
      group_by( seller_type, gtin_char) |>
      summarise( value_min = min(preco_brl),
                 value_avg = median(preco_brl),
                 sd_price = sd(preco_brl),
                 desconto_avg = mean(desconto)) |>
       ungroup() |>
       arrange(gtin_char) |>
       group_by( gtin_char) |>
       mutate( n = 1:n() ) |>
       ungroup() |>
       pivot_wider( names_from = seller_type,
                    values_from = value_avg,
                    values_fill = 0,
                    -c(n, value_min, sd_price, desconto_avg) ) |>
       mutate( Quem_vence = ifelse( `3P` == Not_FBO, yes = "Empate",
            no = ifelse( `3P` < Not_FBO, yes = "3P_vence", "Not_FBO_vence"))
       )

  gtin_final |>
       count(Quem_vence, name = "n_SKU") |>
       mutate(Quem_vence = forcats::fct_reorder( Quem_vence  ,n_SKU ) ) |>
       ggplot( aes(y = Quem_vence, x = n_SKU)) +
       geom_col( color = "black", fill= "#0A1F9C") +
       geom_text( aes(label = n_SKU), hjust=1, size = 10, color="white") +
       labs( title = "SKUs FBO vs Not_FBO Price",
           subtitle = "Sales mar.22",
           x = "Qtde SKU´s") +
      theme_bw()

      # box plot price
       sales_etl |>
           filter( gtin_char %in% sku_venda_dup$gtin_char) |>
           group_by( seller_type, gtin_char) |>
           summarise( value_min = min(preco_brl)) |>
           ungroup() |>
           arrange(gtin_char) |>
           group_by( gtin_char) |>
           mutate( n = 1:n() ) |>
           ungroup() |>
           ggplot( aes(y = value_min)) +
           geom_boxplot() +
           scale_y_log10() +
           facet_wrap(~ seller_type)

       # Box plot comparation avg
       sales_etl |>
           filter( gtin_char %in% sku_venda_dup$gtin_char) |>
           group_by( seller_type, gtin_char) |>
           summarise( value_min = min(preco_brl),
                      value_avg = mean(preco_brl),
                      sd_price = sd(preco_brl),
                      desconto_avg = mean(desconto),
                      .groups = "drop") |>
           ggplot( aes(y = desconto_avg)) +
           geom_boxplot() +
           scale_y_log10() +
           facet_wrap(~ seller_type)

       sales_etl  |>
        filter(gtin_char %in% gtin_final$gtin_char) |>
            select( -c(channel_slug, telefone_seller, height:weight, mes_ ) ) |> writexl::write_xlsx("Sales_duplicado_safra.xlsx")
            group_by(seller_type) |>
            summarise( SKU = n_distinct(gtin_char),
                  Unit = sum(qtde_uni),
                  Pedidos = n_distinct(ord),
                  GMV  = sum(gmv_brl)) |>
           kableExtra::kbl(caption = " Sales same Gtins (3P/NotFBO)") |>
           kableExtra::kable_classic( html_font = "Cambria", full_width = F)
       # Analise por pedido
       sales_etl |>
           filter(gtin_char %in% gtin_final$gtin_char) |>
           group_by( seller_type, ord) |>
           summarise( avg_price_gmv = mean(gmv_brl),
                      .groups = "drop") |>

           ggplot( aes( y = avg_price_gmv)) +
           geom_boxplot() +
           scale_y_log10() +
           facet_wrap(~seller_type)

       sales_etl |>
           filter(gtin_char %in% gtin_final$gtin_char) |>
           ggplot( aes( y = preco_brl)) +
           geom_boxplot() +
           scale_y_log10() +
           facet_wrap(~seller_type)

   sales_etl |>
      filter( gtin_char =='2993500246109') |>
      select(seller_type, preco_brl)

  # Resumo
  sales %>%
      group_by(mes_, mes, seller_type) %>%

      summarise( SKU = n_distinct(sku),
                 unidades = sum(qtde_uni),
                 Sellers = n_distinct(seller_id),
                 GMV = sum(gmv_brl), .groups = "drop") %>%
      arrange(mes_)
      writexl::write_xlsx("data_fat_olist.xlsx")

  sales |>
      filter(str_detect(seller_type, pattern = "1P_SERRA")) |>
      top_n(wt = preco_brl, n = 10) |>
      select(descricao, gmv_brl, seller_type,
             qtde_uni, preco_brl, sku, frete_brl, desconto)

# unit
  unit <- sales |>
      filter( mes == "mar") |>
      mutate(
          categorized_note = cut(preco_brl, breaks = c(0, 80, 300, Inf),
                                 labels = c("low", "medium", "high"),
                                 right = FALSE) ) |>
      group_by( seller_type) |>
      count(categorized_note, wt = gmv_brl) |>
      mutate( pct = n / sum(n),
              pct_t = scales::percent(pct, accuracy = 1)) |>

      ggplot(aes(x = categorized_note, y = n)) +
      geom_col() +
      geom_label( aes(label = pct_t)) +
      facet_wrap(~seller_type, scales = "free_y")

  gmv <- sales |>
      filter( mes == "mar") |>
      mutate(
          categorized_note = cut(preco_brl, breaks = c(0, 80, 300, Inf),
                                 labels = c("low", "medium", "high"),
                                 right = FALSE) ) |>
      group_by( seller_type) |>
      count(categorized_note, wt = qtde_uni) |>
      mutate( pct = n / sum(n),
              pct_t = scales::percent(pct, accuracy = 1)) |>
     # gmv
      ggplot(aes(x = categorized_note, y = n)) +
      geom_col() +
      geom_label( aes(label = pct_t)) +
      facet_wrap(~seller_type, scales = "free_y")

# Curva ABC ---------------------------------------------------------------
    sales %>%
        filter (mes == "mar") %>%
        group_by( seller_type) %>%
        summarise( sku = n_distinct(sku),
                   pedido = n_distinct(ord),
                   unit = sum(qtde_uni),
                   gmv = sum(gmv_brl),
                   date_min = min(data_fat),
                   date_max = max(data_fat),
                   count_date = n_distinct(data_fat),
                   .groups = "drop"  ) %>%
        mutate( avd_order = pedido / count_date,
                avg_unit = unit  / count_date,
                avd_gmv = gmv / count_date )

   c_abc <-  sales %>%
        filter (mes == "mar", seller_type =='transp') %>%
        group_by( brand, sku) %>%
        summarise( gmv_total_sku = sum(gmv_brl), .groups = "drop") %>%
        arrange(desc(gmv_total_sku)) %>%
        add_count( name = "sku_total") %>%
        mutate(rnum = row_number(),
               acc_pct_sku = rnum / sku_total,
                curva_abc= case_when(
                    acc_pct_sku <= .2 ~ "A",
                    acc_pct_sku <= .5 ~ "B",
                    TRUE ~ "C" ) )

   c_abc |>
        group_by(curva_abc) %>%
        summarise( sku = n(),
                   gmv = sum(gmv_total_sku  ), .groups = "drop") %>%
        mutate( pct_gmv_abc = gmv / sum(gmv),
                pct_sku_abc = sku / sum(sku))

   c_abc  %>% gt::gt() %>% gt::tab_header("Curva ABC - PAX - mar.22") %>%
       fmt_currency( columns = gmv, currency = "BRL", scale_by = 1e-6) %>%
       fmt_percent( columns = contains("pct"), decimals = 0 ) %>%
       fmt_number( columns = sku, sep_mark = ".", decimals = 0)


   top_seller <- sales |> inner_join( c_abc, by = "sku" ) |>
       group_by(brand.x) |>
       summarise( gmv = sum(gmv_brl),
                  sku = n_distinct(sku),
                  orders = n_distinct(ord),
                  .groups = "drop") |>
       arrange( desc(gmv)) |>
       slice(1:30) |>
       janitor::adorn_totals()

   sum(top_seller$gmv)

# Douglas faturamento FBO vs Store (CNPJ) ---------------------------------

   sellers <- data.table::fread("data-raw/Enzo/Sellers Ativos - FBO - Sellers.csv",
               encoding = "UTF-8") %>%
               janitor::clean_names() %>%
               as_tibble()

   sellers <- sellers |>
       mutate( c = stringr::str_sub(cnpj,1,10),
               cn = stringr::str_remove(c, pattern = "[\\.+]+"),
               cn = stringr::str_remove(cn, pattern = "\\.+"))

   sellers |>
       anti_join( sales_etl, by = c("cn"="cnpj_short"))

   sales_etl |>
       filter( seller_type %in% c("3P", "Not_FBO")) |>
       group_by(cnpj_short, seller_type) |>
       summarise( gmv = sum(gmv_brl)
                  ,pedidos = n_distinct(ord)
                  ,.groups = "drop") |>
       pivot_wider( names_from = seller_type,
                    values_from = c(gmv, pedidos),
                    values_fill = 0) |>
       filter(  gmv_Not_FBO  > 0 & gmv_3P  > 0) |>
       writexl::write_xlsx("Sales_Douglas.xlsx")

# FBO e NOT_FBO por Gtins (Comparation Sales)-----------------------------------------

   # filtro de codigo barras que temos no FBO Estoca WMS
   sales_itens_fbo  <-
       sales_etl %>%
       filter( gtin_char %in% estoca_g$codigo_de_barras)

   # Comparativo semanal FBO e not FBO
    sales_itens_fbo %>%
        filter(data_fat <= "2022-04-30" & data_fat >= "2022-04-01",
               seller_type %in% c("3P","Not_FBO") ) %>%
       select(gtin_char, qtde_uni, gmv_brl,seller_id, seller_type, mes, week) %>%
       group_by(seller_type, week ) %>%
       summarise(GMV = sum(gmv_brl) ) %>%
       ungroup() %>%
       pivot_wider( names_from = seller_type  ,
                    values_from = GMV,
                    values_fill = 0) %>%
        janitor::adorn_totals() %>%
         gt::gt() %>%
        fmt_currency(columns =c("3P", "Not_FBO"), scale_by = 1e-3,
                     suffixing = "k", currency = "BRL" ) %>%
        gt::tab_header( "Comparativo vendas Unit FBO vs Not_FBO",
                        subtitle = "Somente os gtins que temos em FBO")

   gtins_sales_fbo_not_fbo <- sales_etl %>%
       filter(mes == "abr",
              data_fat  >='2022-04-20',
              gtin_char %in% estoca_g$codigo_de_barras,
              !str_detect( seller_type, "1P")) %>%
       select(gtin_char, qtde_uni, seller_id, seller_type, mes) %>%
       group_by(seller_type, gtin_char ) %>%
       summarise(unit_sales_abr = sum(qtde_uni) ) %>%
       ungroup() %>%
       pivot_wider( names_from = seller_type,
                    values_from = unit_sales_abr,
                    values_fill = 0)

   gtins_sales_fbo_not_fbo_b <- gtins_sales_fbo_not_fbo %>%
       rowwise() %>%
       mutate(
             Quem_Vende_mais = case_when(
           `3P` == Not_FBO ~'Empate',
           `3P` > Not_FBO ~'3P_ganha',
           `3P` < Not_FBO ~'Not_FBO_ganha',
           TRUE ~'ver'
       )) %>%
       ungroup()

   gtins_sales_fbo_not_fbo_b %>%
       count(Quem_Vende_mais, name = "qtde Gtins") %>%
       gt::gt() %>%
       gt::tab_header("Sales FBO por Gtins 3P vs Not_FBO",
                      subtitle = "Report 20 à 30/04")

   estoca_g %>%
       filter( seller_type == '3P') %>%
     left_join( gtins_sales_fbo_not_fbo_b, by = c("codigo_de_barras"="gtin_char")) %>%
       group_by(Quem_Vende_mais) %>%
       summarise( Gtins_FBO_geral = n_distinct(codigo_de_barras ),
                  Gtins_FBO_com_estoque = n_distinct(codigo_de_barras[disponivel >0] ),
                  .groups = "drop") %>%
       replace_na(list(Quem_Vende_mais = "Gtins Sem vendas" ) ) %>%
       janitor::adorn_totals() %>%
       gt::gt() %>%
       gt::tab_header("Sales FBO por Gtins 3P vs Not_FBO",
                      subtitle = "Report 20 à 30/04")

   estoca_g %>%
       filter( seller_type == '3P') %>%
       left_join( gtins_sales_fbo_not_fbo_b, by = c("codigo_de_barras"="gtin_char")) %>%
       replace_na(list( `3P` = 0, Not_FBO = 0, Quem_Vende_mais = 'Sem_Vendas_geral')) %>%
       rename( 'FBO_3P_sales'=`3P`, 'Not_FBO_sales'= Not_FBO, 'Status_Vendas_desde_20_04_2022'=Quem_Vende_mais) %>%
       select(-items_number , -seller_type, -reservado, -no_estoque, -bloqueado, "Saldo_Estoque" =disponivel  ) %>%
       relocate(Status_Vendas_desde_20_04_2022, .after = nome_da_loja) %>%
       filter(FBO_3P_sales  == max(FBO_3P_sales ))

   # Detalhe por orders
  gtins_safra <-  gtins_sales_fbo_not_fbo_b  %>%
      left_join( sales_etl %>% filter(data_fat  >='2022-04-20') , by = 'gtin_char') %>%
       select(-product_gtin,-width ,-height,-length , -weight ,-mes , -cnpj_short,-mes_  ) %>%
       rename( 'FBO_3P_sales'=`3P`, 'Not_FBO_sales'= Not_FBO, 'Status_Vendas_desde_20_04_2022'=Quem_Vende_mais) %>%
       select(-FBO_3P_sales, -Not_FBO_sales ) %>%
      filter(!str_detect( seller_type, "1P") ) %>%
      left_join( estoca_g %>% select(sku, "Stock_disponivel"=disponivel)) %>%
      writexl::write_xlsx("Sales_Comparativo_FBO.xlsx")

    estoca_tem_stk <-  estoca_g %>% filter(disponivel>0)

   stock_out <-
       estoca_g %>%
       filter(disponivel  == 0,
              seller_type=='3P')

   sales_etl %>%
       filter(sku %in% stock_out$sku, seller_type=='3P' ) %>%
       count(mes, wt = gmv_brl)

   stock_out$sku

   estoca_g %>%
       filter( codigo_de_barras == '6015000407401')

# Yas pool ----------------------------------------------------------------
sales_top <- sales_etl %>%
    filter( seller_type =="3P", mes == 'abr') %>%
    group_by(seller_id, brand, sku, descricao) %>%
    summarise( unit_sales_abr = sum(qtde_uni),
               preco_brl = max(preco_brl),
               gmv_brl_abr = sum(gmv_brl)) %>%
    ungroup() %>%
    left_join( estoca_g %>% select(sku, "Stock_disponivel"=disponivel)) %>%
    filter(Stock_disponivel > 9) %>%
    arrange(desc(gmv_brl_abr)) %>%
    slice(1:100)

sales_top %>%  writexl::write_xlsx("TOP_sales.xlsx")
sales_yas
estoca_g %>%
    filter(seller_type =='3P') %>%
    select(sku, codigo_de_barras, disponivel) %>%
    filter( disponivel >=5) %>% clipr::write_clip()

sales_etl %>%  select(sku, preco_brl) %>%
    group_by(sku) %>%
    summarise( price = min(preco_brl))
# YAS pool comparativo
  sales_etl %>%
    filter(data_fat == '2022-04-28',
           sku %in% tbl_data$sku) %>%
    group_by(data_fat) %>%
    summarise(unit = sum(qtde_uni),
              sku = n_distinct(sku),
              pedidos = n_distinct(ord),
              gmv = sum(gmv_brl)) %>% gt::gt() %>%
    gt::tab_header("Itens vendidos 20/04 ( lista Yas Pool)")

sales_etl %>%
    filter(data_fat == '2022-04-28',
           seller_type == '3P'|seller_type=='1P_BARUERI') %>%
    group_by(data_fat) %>%
    summarise(unit = sum(qtde_uni),
              sku = n_distinct(sku),
              pedidos = n_distinct(ord),
              gmv = sum(gmv_brl)) %>% gt::gt() %>%
    gt::tab_header("FATURAMENTO")
# Maior lado --------------------------------------------------------------
sales_etl <-  sales %>%
    filter( mes == "mar") %>%
    pivot_longer( cols = c(height:length),
                  names_to = "lado"
    ) %>%
    group_by(ord) %>%
    filter( value == max(value)) %>%
    ungroup() %>%
    select( -lado) %>% distinct() |>
    mutate(lado_range =
               case_when(
                   value <16 ~ "P15",
                   value <31 ~ "P30",
                   value <46 ~ "P45",
                   value <61 ~ "P60",
                   TRUE  ~ ">60"
               ))

sales_etl %>%
    # group_by(seller_type) |>
    count(lado_range, sort = T) %>%
    mutate( pct = n / sum(n)) |>
    ggplot(aes(  x = lado_range, y = pct)) +
    geom_col( color="black", fill="red") +
    facet_wrap(~ seller_type) +
    theme_bw()

