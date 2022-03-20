

library(tidyverse); library(tidyquant); library(gt)

    sales <- data.table::fread("data-raw/sales/out21_mar22.csv") %>% janitor::clean_names() %>% as_tibble()





  sales <-   sales %>%
        mutate( mes = lubridate::month(data_fat, label = T, abbr = T),
                mes_ =  lubridate::floor_date( data_fat, unit = "month"))

  # Resumo
  sales %>%

      group_by(mes_, mes, seller_type) %>%

      summarise( SKU = n_distinct(sku),
                 unidades = sum(qtde_uni),
                 Sellers = n_distinct(seller_id),
                 GMV = sum(gmv_brl), .groups = "drop") %>%
      arrange(mes_) %>%
      writexl::write_xlsx("data_fat_olist.xlsx")


# Maior lado --------------------------------------------------------------

  sales_etl <-  sales %>%
      filter( mes == "mar") %>%
      pivot_longer( cols = c(height:length),
                    names_to = "lado"
      ) %>%
      group_by(ord) %>%
      filter( value == max(value)) %>%
      ungroup() %>%
      select(-maior_lado, -lado) %>% distinct()

  sales_etl %>%
      mutate(lado_range =
                 case_when(
                     value <16 ~ "P15",
                     value <31 ~ "P30",
                     value <46 ~ "P45",
                     value <61 ~ "P60",
                     TRUE  ~ ">60"
                 )) %>%
      count(lado_range, sort = T) %>%
      mutate( pct = n / sum(n))


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
        group_by( sku) %>%
        summarise( gmv_total_sku = sum(gmv_brl), .groups = "drop") %>%
        arrange(desc(gmv_total_sku)) %>%
        add_count( name = "sku_total") %>%
        mutate(rnum = row_number(),
               acc_pct_sku = rnum / sku_total,
                curva_abc= case_when(
                    acc_pct_sku <= .2 ~ "A",
                    acc_pct_sku <= .5 ~ "B",
                    TRUE ~ "C" ) ) %>%
        group_by(curva_abc) %>%
        summarise( sku = n(),
                   gmv = sum(gmv_total_sku  ), .groups = "drop") %>%
        mutate( pct_gmv_abc = gmv / sum(gmv),
                pct_sku_abc = sku / sum(sku))

   c_abc  %>% gt::gt() %>% gt::tab_header("Curva ABC - PAX - mar.22") %>%
       fmt_currency( columns = gmv, currency = "BRL", scale_by = 1e-6) %>%
       fmt_percent( columns = contains("pct"), decimals = 0 ) %>%
       fmt_number( columns = sku, sep_mark = ".", decimals = 0)

