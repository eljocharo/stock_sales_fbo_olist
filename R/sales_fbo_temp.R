

library(tidyverse); library(tidyquant); library(gt)

    sales <- data.table::fread("data-raw/sales/2022-03-28_sales_enzo.csv", encoding = "UTF-8") %>%
        janitor::clean_names() %>% as_tibble() |>
        mutate( mes = lubridate::month(data_fat, label = T, abbr = T),
                mes_ =  lubridate::floor_date( data_fat, unit = "month"),
                gtin_char = as.character(product_gtin))

    options(scipen = 9999)
    sales |>
        filter(seller_type == "transp") |>
        # group_by(data_fat) |>
        summarise( gmv = sum(gmv_brl), .groups = "drop") |>
        ggplot(aes( x = data_fat, y = gmv)) +
        geom_line()

# Analise de vendas por preço e gtin duplicado ----------------------------

    sales |>
        filter(seller_type %in% c("3P", "transp") ) |>
        group_by( seller_type) |>
    summarise( unidade = sum(qtde_uni) ,
               pedidos = n_distinct(ord),
               sku = n_distinct(sku),
               gtins = n_distinct(gtin_char),
               seller = n_distinct(seller_id),
               gmv = sum(gmv_brl))


    sales_etl <- sales |>
        filter( gtin_char %in% estoca_g$codigo_de_barras,
                seller_type %in% c("3P", "transp") )

   sku_venda_dup <-  sales_etl |>
        group_by(seller_type, gtin_char) |>
        summarise( n_seller = n_distinct(seller_id), .groups = "drop") |>
       filter(n_seller >0) |>
        pivot_wider( names_from = seller_type,
                     values_from = n_seller,
                     values_fill = 0) |>
        filter( transp !=0, `3P` !=0)


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
       mutate( Quem_vence = ifelse( `3P` == transp, yes = "Empate",
            no = ifelse( `3P` < transp, yes = "3P_vence", "Not_FBO_vence"))
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

  unit / gmv

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
      geom_col( color="black", fill="blue") +
      facet_wrap(~ seller_type) +
      theme_bw()


# TOP sellers -------------------------------------------------------------

  sales %>%
      filter(seller_type == "transp", mes=="mar") %>%
      count( categoria, wt = gmv_brl, sort = T) %>%
      slice(1:30) %>% view()

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
