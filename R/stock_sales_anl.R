
library(tidyverse); library(tidyquant); library(gt);library(kableExtra)

# Import -----------------------------------------------------------------------

# data_sellers <- 'https://docs.google.com/spreadsheets/d/1WCK0TkoeHODmjPyq73vZoxqmexkq70QECL953dG0FYw/edit#gid=0'
# estoca_location <- '../../../../joel.souza/Downloads/Estoca(2022.02.01).csv'
# cad_seller <-  googlesheets4::read_sheet(sf,col_types = "nDccccc") %>% janitor::clean_names()
# estoca <- readr::read_csv(stk) %>% janitor::clean_names() %>% filter( nome_da_loja != 'Olist')



mr <- "data-raw/2022-03-29_sales_marcos.csv"

marcos <- readr::read_csv(mr) %>% janitor::clean_names()

rm(mr)


estoca_group <- "data-raw/2022-03-29_Estoca_G.csv"


estoca_g <- readr::read_csv(estoca_group) %>%
    janitor::clean_names() %>% filter( !nome_da_loja %in%
    c('Olist', 'Olist 2', 'Outlet Olist', 'H2O Purificadores (Olist)') ) %>%
    mutate( seller_type =
                case_when(
                    nome_da_loja == "Olist (Protheus)" ~ "1P-Barueri",
                    TRUE~ "3P"))

rm(estoca_group)

estoca_g |>
    filter(seller_type == '3P') |>
    select(nome_da_loja
           , nome_do_item
           , sku:codigo_de_barras
           , disponivel
           ) |> clipr::write_clip()

# Resumo Estoca
estoca_g %>%
    group_by(seller_type) %>%
    summarise(across(
        .cols = where(is.character),
        .fns = n_distinct ),
        across(
            .cols = where(is.numeric),
            .fns = sum ) ) %>%
       mutate( vendas_7d = round(vendas / 7, 0),
                  dias_stock = round(disponivel / vendas_7d, 0)
               ) %>%
    select( seller_type, nome_da_loja, sku, "stock_disponivel" = disponivel, vendas_7d,
            dias_stock, no_estoque, bloqueado, reservado , codigo_de_barras, vendas ) %>%
    janitor::adorn_totals() |>
    kbl( caption = "Report Estoque G resumo") |>
    kable_classic_2("hover", full_width = F, html_font = )

# ETL_marcos --------------------------------------------------------------

marcos_etl <- marcos %>%
    filter( !brand %in% c('galpao1pJadonly','Olist')) %>%
    replace_na( list( sku_items = 0,
                      avg_sku_items_day = 0,
                      stock = 0)
    ) %>%
    mutate( avg_gmv_day =  (sku_items * price) / 30,
            dias_cobertura_gmv = stock_value / avg_gmv_day) %>%
    replace_na( list(
        dias_cobertura_gmv = 0,
        avg_gmv_day = 0 )
    ) %>%
    mutate(
        dias_stock_gmv = case_when(
            stock == 0 ~ "StockOut",
            sku_items == 0 ~ "WithoutSales",
            dias_cobertura_gmv <= 5 ~ "00to05",
            dias_cobertura_gmv <= 15 ~ "06to15",
            dias_cobertura_gmv <= 30 ~ "16to30",
            dias_cobertura_gmv <= 45 ~ "31to45",
            dias_cobertura_gmv <= 60 ~ "46to60",
            dias_cobertura_gmv <= 90 ~ "61to90",
            dias_cobertura_gmv <= 120 ~ "91to120",
            dias_cobertura_gmv <= 180 ~ "121to180" ,
            TRUE ~ "over_180"),
        dias_stock_gmv = factor(dias_stock_gmv,
                                levels = c("00to05","06to15","16to30","31to45","46to60","61to90",
                                           "91to120","121to180","over_180","WithoutSales","StockOut")
        ) )

# vendas macro ------------------------------------------------------------

# Gerar report do Marcos somente olist
olist_stk <- marcos_etl %>% filter(brand %in% c('Olist Hub SP', 'Hub Vitória') )

# gerar somente filtros com report do estoque (sem olist)

third_party <- marcos_etl %>%
    filter(product_sku %in%
               estoca_g$sku, brand != 'Olist')

stk_etl <- olist_stk %>%
    bind_rows( third_party) %>%
    mutate(
        seller_type = case_when(
            brand == "Hub Vitória" ~ "1P-Serra",
            brand == "Olist Hub SP" ~ "1P-Barueri",
            TRUE ~ "3P"
        )
    )
    # filter( product_sku != 'PRDF0VEEPPMMNTXJ')

stk_etl %>%
    filter( seller_type =='1P-Serra', stock >0) %>% select(stock) %>%
    count(stock, wt = stock)

stk_etl %>%
    filter( seller_type =='1P-Serra', stock >0) %>% select(stock) %>% sum

# Sellers Ativos Douglas --------------------------------------------------

third_party %>%
    select(seller_id, brand, stock_update_at) %>%
    group_by(seller_id, brand) %>%
    summarise( min_date = min(stock_update_at)) %>% clipr::write_clip()


stk_etl %>%
    mutate( tipe_sales = ifelse( dias_stock_gmv=="WithoutSales", "WhithoutSales", "Others")) %>%
    group_by(  tipe_sales,seller_type) %>%
    summarise(valor_stock = sum( stock_value), .groups = "drop") %>%
    mutate( pct = valor_stock / sum(valor_stock))

stk_etl %>%
    slice_max(n = 3, order_by = price) %>%
    select(product_sku, product_name, stock, price, stock_value)
# Touch Point Werner------------------------------------------------

estoca_g %>%
    group_by(nome_da_loja) %>%
    add_tally( wt = disponivel, name = "disponivel_total") %>%
    ungroup() %>%
    group_by(seller_type) %>%
    # filter( nome_da_loja != 'Olist (Protheus)') %>%
    summarise(  Seller_3P = n_distinct(nome_da_loja[disponivel > 0 ]),
                Sellers_Stock_Out = n_distinct(nome_da_loja[disponivel_total ==0]),
                Seller_liq_Sales_7d = n_distinct(nome_da_loja[vendas>0]),SKU_total = n_distinct(sku),
                SKU_liquido_last7d = n_distinct( sku[vendas>0]),
                SKU_Disponivel = n_distinct(sku[disponivel > 0]),
                SKU_Stock_Out = n_distinct(sku[disponivel== 0 & bloqueado== 0]),
                SKU_bloqueado = n_distinct(sku[disponivel== 0 & bloqueado>=1]),
                Unit_stock_disp = sum(disponivel),

                Sales_last_7d_avg = round(sum(vendas) / 7, 0),
                .groups = "drop") %>%
    left_join(
        stk_etl %>%
            select(stock, product_sku, stock_value, seller_type, sku_items) %>%
            group_by(seller_type)%>%
            summarise(  Valor = sum(stock_value),
                        SKU_liquido_30d = n_distinct( product_sku[sku_items>0])
            ) %>%
            filter(seller_type != '1P-Serra')
    , by = "seller_type") %>% janitor::adorn_totals() %>%
    gt::gt() %>%
    gt::tab_header("Touch Point FBO - Barueri") %>%
    fmt_currency(columns = Valor, currency = "BRL")

# Seller Stock out
estoca_g %>%
    group_by(nome_da_loja) %>%
    add_tally( wt = disponivel, name = "disponivel_total") %>%
    ungroup() %>%
    filter(disponivel_total==0) %>% distinct(nome_da_loja)

# Table SKU Stock Out
# sku_stock_out_data <-  estoca_g %>%
#    filter( nome_da_loja != 'Olist (Protheus)',
#            ) %>%
#   # disponivel ==0, bloqueado ==0
#    select(sku)
# stk_etl %>%
#   filter( product_sku %in% sku_stock_out_data$sku) %>%
#   select(seller_id, brand, product_category,
#          product_sku, product_gtin, product_name, stock) %>%
#   arrange(seller_id) %>% clipr::write_clip()

# Liquidos


third_party %>%
    group_by(seller_id) %>%
    add_tally(wt = stock, name = "stock_seller_total") %>%
    ungroup() %>%
    # filter( stock_seller_total==0) %>% select(brand) %>% distinct() %>% pull() %>% clipr::write_clip()
    summarise( seller_liq = n_distinct( seller_id[sku_items > 0]),
               Sellers_Whithout_stock = n_distinct(seller_id[stock_seller_total == 0]),
               SKU_liquidos = n_distinct( product_sku[sku_items > 0])
    )

sku_sellers_out <- third_party %>%
    group_by(seller_id) %>%
    add_tally(wt = stock, name = "stock_seller_total")

stk_etl %>%
    group_by(seller_type) %>%
    summarise(  Seller = n_distinct(seller_id[stock>0]),
                SKU_total         = n_distinct( product_sku[stock>0]),
                SKU_liquidos       = n_distinct(product_sku[sku_items>0]),
                Stock_unit        = sum(stock),
                Stock_GMV         = sum(stock_value),
                Vendas_bruto_unit   = round(sum(sku_items, na.rm = T),0),
                avg_vendas_day_unit     = round(sum(sku_items, na.rm = T)/30,0),
                GMV_bruto     = sum(sum(price*sku_items, na.rm = T)),
                avg_GMV_day       = sum(price*sku_items, na.rm = T)/30
    ) %>%
    janitor::adorn_totals() %>%
    gt() %>%
    gt::tab_header( title = "Resumo Vendas FBO (Marcos)",  subtitle = "Last 30d") %>%
    gt::fmt_number(columns = contains("unit"),  dec_mark = ",", sep_mark = ".",
                   decimals = 0) %>%
    gt::fmt_number(columns = contains("GMV"),  dec_mark = ",", sep_mark = ".",
                   decimals = 3, scale_by = 1e-6) %>%
    tab_footnote(
        footnote = str_glue("Report {lubridate::today()}"),
        locations = cells_column_labels(
            columns = seller_type)
    ) %>%
    tab_footnote(
        footnote = "em milh?es",
        locations = cells_column_labels(
            columns = c(Stock_GMV, GMV_bruto, avg_GMV_day )
        )
    )
# Estoque por tipo (3P vs 1P) Estoca
estoca_g %>%
    count( seller_type, wt = disponivel, name = "stock") %>%
    janitor::adorn_percentages(denominator = "col") %>%
    janitor::adorn_pct_formatting(digits = 0) %>%
    janitor::adorn_ns(position = "front")

# TOP Sales  --------------------------------------------------------
# base olist

#  TABLE SALES UNIT
dt_unit <-  stk_etl %>%
    select(seller_type, product_gtin,
           brand, product_sku,
           stock, sku_items,
           product_name, price) %>%
    group_by(seller_type) %>%
    dplyr::add_count( wt = sku_items, name = "total_unit") %>%
    ungroup() %>%
    add_tally(wt = sku_items, name = "geral_unit" ) %>%
    group_by(seller_type) %>%
    arrange(desc(sku_items),.by_group = T) %>%
    slice(1:10) %>%
    add_tally( wt = sku_items, name = "total_unit_top10") %>%
    mutate( gmv = price * sku_items,
            gmv_t = scales::dollar(gmv, scale = 1e-3, suffix = "k", prefix = "", accuracy = 0.1),
            product_name_= stringr::str_extract(product_name, "[^ ]+ ?[^ ]+ ?[^ ]+"),
            product_name_ = str_c( product_name_, " (R$ ", gmv_t, ")" ),
            product_name_ = product_name_ %>% fct_reorder(sku_items ),
            pct_top_10_unit = total_unit_top10 / geral_unit,
            pct_top_10_unit = scales::percent(pct_top_10_unit),
            seller_type_ = str_c( seller_type, " (Total Unit ", total_unit, " | Top10 ", pct_top_10_unit, ")")) %>%
    ungroup()

#  TABLE SALES GMV
dt_gmv <-  stk_etl %>%
    select(seller_type, product_gtin,
           brand, product_sku,
           stock, sku_items,
           product_name, price) %>%
    mutate( gmv = price * sku_items) %>%
    group_by( seller_type) %>%
    dplyr::add_count( wt = gmv, name = "t_gmv") %>%
    ungroup() %>%
    add_tally(wt = gmv, name = "geral_gmv" ) %>%
    group_by(seller_type) %>%
    arrange(desc(gmv),.by_group = T) %>%
    slice(1:10) %>%
    add_tally(wt = gmv, name = "total_gmv_top10" ) %>%
    ungroup() %>%
    mutate( gmv_t = scales::dollar(gmv, scale = 1e-3, suffix = "k", prefix = "", accuracy = 0.01),
            gmv_text_total = scales::dollar(t_gmv, scale = 1e-6, suffix = "M", prefix = "", accuracy = 0.01),
            seller_txt = stringr::str_extract(brand, "[^ ]+"),
            product_name_= stringr::str_extract(product_name, "[^ ]+ ?[^ ]+ ?[^ ]+"),
            product_name_ = str_c( product_name_,"-",seller_txt, " (unit ", sku_items, ")" ),
            product_name_ = product_name_ %>% fct_reorder(gmv ),
            pct_top_10_gmv = total_gmv_top10 / t_gmv,
            pct_top_10_gmv = scales::percent(pct_top_10_gmv, accuracy = 1),
            seller_type_ = str_c( seller_type, " (Total GMV ", gmv_text_total, " | Top10 ",pct_top_10_gmv, ")") )

# Plot SALES
#  Sales Unit

total_unit <- max(dt_unit$geral_unit) %>%
    scales::number(decimal.mark = ",", big.mark = "." )

plot_items <- dt_unit %>%
    ggplot(aes( x = sku_items,  y = product_name_)) +
    geom_col( color ="black", fill= "#0A1F9C") +
    labs( title = "FBO Top 10 Sales Items/Unit",
          subtitle = str_glue("Total - last 30d - {total_unit} "),
          x = "",
          y= "",
          caption = str_glue("Report {lubridate::today()}" ) ) +
    geom_text(aes(label = sku_items),
              hjust = 1,
              size  = 4,
              color = "white") +
    facet_wrap( ~ seller_type_, scales = "free") +
    theme_tq() +
    theme( axis.text.x = element_blank())

# Sales GMV

# GMV TOTAL
total_gmv <- max(dt_gmv$geral_gmv) %>%
    scales::dollar( scale = 1e-6,
                    suffix = "M", prefix = "", accuracy = 0.01)
# PLOT
plot_gmv <- dt_gmv %>%
    ggplot(aes( x = gmv,  y = product_name_)) +
    geom_col( color ="black", fill= "#0A1F9C") +
    labs( title = "FBO Top 10 Sales GMV",
          subtitle = str_glue("Total - last 30d - {total_gmv}"),
          x = "",
          y= "",
          caption = str_glue("Report {lubridate::today()}" ) ) +
    geom_text(aes(label = gmv_t),
              hjust = 1,
              size  = 4,
              color = "white") +
    facet_wrap( ~seller_type_, scales = "free_y") +
    scale_x_log10() +
    theme_tq() +
    theme( axis.text.x = element_blank())

plot_items
plot_gmv

# salvando o grafico Items
ggsave( dpi = 400,
        filename = str_glue("sales_unit_{lubridate::today()}.png"),
        device = png, plot = plot_items)

# salvando o grafico GMV
ggsave( dpi = 400,
        filename = str_glue("sales_gmv_{lubridate::today()}.png"),
        device = png, plot = plot_gmv)

#  Category Sales

stk_etl %>%
    filter( seller_type == '1P-Barueri') %>%
    mutate( valor = price *sku_items) %>%
    group_by( product_category) %>%
    summarise( sales_unit = sum( sku_items),
               stock = sum(stock)) %>%
    arrange(desc(sales_unit)) %>%
    mutate( pct_sales = sales_unit / sum(sales_unit),
            pct_stock = stock / sum(stock),
            pct_sales  = scales::percent(pct_sales,  accuracy = .1),
            pct_stock  = scales::percent(pct_stock,  accuracy = .1))

# Estoca top vendas por seller
estoca_g  %>%
    filter( vendas >0,
            nome_da_loja != 'Olist (Protheus)') %>%
    group_by(nome_da_loja) %>%
    summarise( vendas_last_7d = sum(vendas)/7, .groups = "drop") %>%
    select(nome_da_loja, vendas_last_7d) %>%
    mutate(nome_da_loja = fct_reorder(nome_da_loja,vendas_last_7d)) %>%
    ggplot(aes(x=vendas_last_7d, y = nome_da_loja)) +
    geom_col() +
    geom_text( aes( label =round(vendas_last_7d,0 ) ), hjust = -0.1) +
    theme_classic()

# Table Cobertura Stock - GMV ---------------------------------------------

stk_etl %>%
    select( seller_type, product_sku, product_name, stock, sku_items_until_7d,
            sku_items_until_15d, sku_items_until_30d, sku_items, avg_sku_items_day,
            dias_cobertura, price, stock_value, avg_gmv_day, dias_stock_gmv) %>%

    count(seller_type, dias_stock_gmv, wt = stock_value ) %>%
    tidyr::pivot_wider( names_from = seller_type,
                        values_from = n,
                        values_fill = 0) %>%
    mutate( pct_SP = `1P-Barueri` / sum(`1P-Barueri`),
            pct_ES = `1P-Serra` / sum(`1P-Serra`),
            pct_3P = `3P` / sum(`3P`)) %>%
    janitor::adorn_totals() %>%
    mutate( `1P-Barueri` = scales::dollar(`1P-Barueri`,
                                          prefix = "", decimal.mark = ",",
                                          big.mark = ".",
                                          scale = 1e-3,
                                          accuracy = 0.1,
                                          suffix = "k"),
            `1P-Serra` = scales::dollar(`1P-Serra`,
                                        prefix = "", decimal.mark = ",",
                                        big.mark = ".",
                                        scale = 1e-3,
                                        accuracy = 0.1,
                                        suffix = "k"),
            `3P` = scales::dollar(`3P`,
                                  prefix = "", decimal.mark = ",",
                                  big.mark = ".",
                                  scale = 1e-3,
                                  accuracy = 0.1,
                                  suffix = "k"),
            pct_SP = scales::percent(pct_SP, accuracy = 0.1),
            pct_ES = scales::percent(pct_ES, accuracy = 0.1),
            pct_3P = scales::percent(pct_3P, accuracy = 0.1)) %>%
    gt() %>% gt::tab_header( title = "Dias de Cubertura por GMV")

# Table Cobertura Stock - SKU ---------------------------------------------

stk_etl %>%
    select( seller_type, product_sku, product_name, stock, sku_items_until_7d,
            sku_items_until_15d, sku_items_until_30d, sku_items, avg_sku_items_day,
            dias_cobertura, price, stock_value, avg_gmv_day, dias_stock_gmv) %>%

    group_by(seller_type, dias_stock_gmv) %>%
    summarise( SKU = n_distinct(product_sku), .groups = "drop") %>%

    tidyr::pivot_wider( names_from = seller_type,
                        values_from = SKU,
                        values_fill = 0) %>%
    mutate( pct_SP = `1P-Barueri` / sum(`1P-Barueri`),
            pct_ES = `1P-Serra` / sum(`1P-Serra`),
            pct_3P = `3P` / sum(`3P`)) %>%
    janitor::adorn_totals() %>%
    mutate(
        pct_SP = scales::percent(pct_SP, accuracy = 0.1),
        pct_ES = scales::percent(pct_ES, accuracy = 0.1),
        pct_3P = scales::percent(pct_3P, accuracy = 0.1)
    ) %>% gt() %>%
    gt::tab_header( title = "Dias de Cubertura por SKU")

# 3P Table general --------------------------------------------------------

tabela_sales_sellers <-  stk_etl %>%
    filter( !str_detect(seller_type, "1P"), seller_id !='4424144f-7b1a-4c5b-b1e0-f87c3bc05f9a') %>%
    mutate( sku_gmv = price * sku_items) %>%
    group_by(brand) %>%
    summarise(Unit_Stock = sum(stock),
              SKU_Total = n_distinct(product_sku),
              SKU_Stock = n_distinct(product_sku[stock>0]),
              SKU_Without_Sales = n_distinct(product_sku[sku_items==0 & stock>0] ),
              SKU_Stock_Out = n_distinct(product_sku[stock==0]),
              GMV_Stock = sum( stock_value),
              Sales_until_30d = sum(sku_items),
              GMV_until_30d = sum( sku_gmv),
              GMV_dia_avg =((GMV_until_30d / 30)),
              dias_cobertura_gmv = round(GMV_Stock / GMV_dia_avg,0),
              gtins_quotes = sum(gtins_quotes),
              channels = sum(channels),
              channels_stt_published = sum(channels_stt_published),
              channels_stt_ready = sum(channels_stt_ready),
              days_last_order = max(days_last_order),
              days_first_stock = max(days_first_stock),
              days_last_stock = max(days_last_stock),
              last_order_created_at = max(last_order_created_at),
              stock_created_at = max(stock_created_at),
              stock_update_at = max(stock_update_at) ) %>%
    mutate(  dias_cobertura_gmv = ifelse( dias_cobertura_gmv == Inf, 0 , dias_cobertura_gmv) ) %>%
    mutate(
        dias_stock_gmv = case_when(
            Unit_Stock  == 0 ~ "StockOut",
            Sales_until_30d == 0 ~ "WithoutSales",
            dias_cobertura_gmv <= 5 ~ "00to05",
            dias_cobertura_gmv <= 15 ~ "06to15",
            dias_cobertura_gmv <= 30 ~ "16to30",
            dias_cobertura_gmv <= 45 ~ "31to45",
            dias_cobertura_gmv <= 60 ~ "46to60",
            dias_cobertura_gmv <= 90 ~ "61to90",
            dias_cobertura_gmv <= 120 ~ "91to120",
            dias_cobertura_gmv <= 180 ~ "121to180" ,
            TRUE ~ "over_180")
    ) %>%
    janitor::adorn_totals( ) %>%
    mutate( brand = str_to_title(brand)) %>% as_tibble()

total_tabela_sales_seller <-
    tabela_sales_sellers %>%
    mutate( dias_stock_gmv = factor(dias_stock_gmv,
                                    levels = c("00to05","06to15","16to30","31to45","46to60","61to90",
                                               "91to120","121to180","over_180","WithoutSales","StockOut") )) %>%
    count(brand,dias_stock_gmv) %>%
    filter(dias_stock_gmv != "-") %>%
    arrange(dias_stock_gmv) %>%
    pivot_wider( names_from = dias_stock_gmv,
                 values_from = n,
                 values_fill = NA) %>%
    left_join( tabela_sales_sellers %>% select(1:channels_stt_ready), by = "brand") %>%
    summarise( across( where(is.numeric), ~sum(., na.rm = T)) ) %>%
    mutate( brand = "Total",
            dias_cobertura_gmv = round(GMV_Stock / GMV_dia_avg, 0) ) %>%
    relocate( brand, .before = 1)

tabela_sales_sellers %>%
    mutate( dias_stock_gmv = factor(dias_stock_gmv,
                                    levels = c("00to05","06to15","16to30","31to45","46to60","61to90",
                                               "91to120","121to180","over_180","WithoutSales","StockOut") )) %>%
    count(brand,dias_stock_gmv) %>%
    filter(dias_stock_gmv != "-") %>%
    arrange(dias_stock_gmv) %>%
    pivot_wider( names_from = dias_stock_gmv,
                 values_from = n,
                 values_fill = NA) %>%
    left_join( tabela_sales_sellers %>% select(1:channels_stt_ready), by = "brand") %>%
    bind_rows(total_tabela_sales_seller) %>%
    # writexl::write_xlsx("Sales_FBO_last_30d.xlsx")
    gt() %>%
    gt::tab_header("Table Sellers Dias Stock") %>%
    # data_color(
    #     columns = vars(`00to05`:`31to45`),
    #     colors = scales::col_numeric(
    #         palette = c(
    #             "#0C29D0"),
    #         domain = NULL)
    # ) %>%
    #
    data_color(
        columns = vars(`06to15`:`31to45`),
        colors = scales::col_numeric(
            palette = c(
                "#0C29D0"),
            domain = NULL)
    ) %>%
    data_color(
        columns = vars(`61to90`:`121to180`),
        colors = scales::col_numeric(
            palette = c(
                "#6A00A3"),
            domain = NULL)
    ) %>%
    data_color(
        columns = vars(over_180),
        colors = scales::col_numeric(
            palette = c(
                "#EDAD00"),
            domain = NULL)
    ) %>%
    data_color(
        columns = vars(`WithoutSales`),
        colors = scales::col_numeric(
            palette = c(
                "#E64C2D"),
            domain = NULL)
    ) %>%
    data_color(
        columns = vars(StockOut),
        colors = scales::col_numeric(
            palette = c(
                "#0B134A"),
            domain = NULL)
    ) %>%
    gt::fmt_number( columns = starts_with("GMV"),
                    locale = "pt_BR", scale_by = 1e-3, suffixing = "k")

# tabela_sales_sellers %>% writexl::write_xlsx("Sales_last30d_FBO_2022-03-02.xlsx")


# Sales 3P FBO table Javi -------------------------------------------------

stk_etl %>%
    filter( seller_type == "3P") %>%
    select( seller_id,brand, product_sku, product_name, "Stock_Unit"=stock, "Dias_Stock_Status"=dias_stock_gmv,
            "SKU_Unit_Sales_LAST30d" = sku_items, avg_sku_items_day,
            dias_cobertura, price, stock_value, avg_gmv_day) %>%
    writexl::write_xlsx("3p_FBO_SALES_LAST30_2022-03-02.xlsx")

# Table resumo de vendas por dias de Stock
stk_etl %>%
    filter( seller_type == "3P") %>%
    group_by( "Dias_Stock"=dias_stock_gmv ) %>%
    summarise(SKU = n_distinct( product_sku),
              Unit = sum(stock),
              Stock_value = sum(stock_value),
              gtins_quotes = sum(gtins_quotes)) %>%  gt() %>%
    gt::tab_header("Stock 3P Status - 2022-03-02") %>%
    fmt_number( columns = vars(SKU, Unit, gtins_quotes), decimals = 0, sep_mark = "." ) %>%
    fmt_number( columns = vars(Stock_value), decimals = 2, scale_by = 1e-3, suffixing = "k")

#  Ver Seller Stock Dias de Stock
stk_etl %>%
    select( brand,  product_sku, product_name, stock, sku_items,dias_cobertura_gmv, stock_value, price) %>%
    filter(str_detect(brand, "ALTAS") )

# WEB APP vs Estoca ------------------------------------------------

third_party %>% select(seller_id, brand, product_sku  , product_name, "saldo_webapp"=stock) %>%
    full_join(
        estoca_g %>%
            filter( str_detect(nome_da_loja, "(FBO)")) %>%
            select(sku, "saldo_estoca" = disponivel), by = c("product_sku"="sku")
    ) %>%
    replace_na( list( saldo_webapp= 0, saldo_estoca=0)) %>%
    mutate( dif = saldo_estoca - saldo_webapp,
            status =  case_when(
                dif == 0 ~ "OK",
                dif > 0 ~ "sobra",
                dif < 0 ~ "falta") ) %>%
    filter( status != "OK", saldo_webapp == 0) %>% arrange(desc(dif)) %>%
    filter(dif>0) |>
    select( -status) |> clipr::write_clip()

 # gt() %>%  gt::tab_header("")
# select( product_sku) %>% pull() %>% clipr::write_clip()


marcos %>%
    filter( product_sku == 'PRD3DR1QJB4AM9TT') %>% select(stock)
# Barcode Duple -----------------------------------------

estoca_g %>%
    filter(disponivel>0) %>%
    janitor::get_dupes(codigo_de_barras) %>%
    select(nome_da_loja, nome_do_item, codigo_de_barras,sku, disponivel) %>%
    arrange(codigo_de_barras) %>%
    gt::gt() %>%
    gt::tab_header( "Codigo barras SKU duplicados")

# Box Plot Unit Stock -----------------------------------------------------

g <- estoca_g %>%
    filter( no_estoque >0, nome_da_loja != 'Olist (Protheus)') %>%
    ggplot( aes(x = "unit", y = no_estoque)) +
    geom_boxplot() +
    facet_wrap(~ nome_da_loja, scales = "free_y") +
    theme_tq()

plotly::ggplotly(g)

# stat_summary(fun.y = mean, geom = "point",colour = "yellow", size=4) +
# stat_summary(fun.data = meanFunction, geom ="text", color = "red", size = 2, vjust = 1.3) +
# scale_y_continuous( limits = c(1,1000))

# ESTOCA POR ADDRESS ------------------------------------------------------

estoca_address <- 'data-raw/2022-03-25_Estoca_ads.csv'

estoca_add <- readr::read_csv(estoca_address) %>%
    janitor::clean_names() %>% filter( !nome_da_loja %in%
 c('Olist', 'Olist 2', 'Outlet Olist', 'H2O Purificadores (Olist)') ) %>%
    mutate( seller_type =
            case_when( nome_da_loja == "Olist (Protheus)" ~ "1P", TRUE~ "3P"))

estoca_add_etl <- estoca_add |>
    rename("seller"    =nome_da_loja,
           "produto"   =nome_do_item,
           "barcode"   =codigo_de_barras,
           "no_estoque"=cd_olist_barueri_no_estoque,
           "reservado" =cd_olist_barueri_reservado,
           "disponivel"=cd_olist_barueri_disponivel,
           "vendas"    =cd_olist_barueri_vendas,
           "dias_stock"=cd_olist_barueri_dias_de_estoque) |>
    tidyr::separate( col = local, sep = "-",
        into = c("area", "rua", "predio", "nivel", "apto"), remove = F)  |>
    mutate(
        rua =  as.numeric(rua),
        predio =  as.numeric(predio),
        apto =  as.numeric(apto))


# Valorizando Stock Address

estoca_add %>%
    summarise(Sellers = n_distinct( nome_da_loja))

# Stock Nivea
estoca_add_etl  |>
    filter(rua == 28, between( x = predio, left = 20, right = 38)) %>%
    count( wt = disponivel)

estoca_add_etl %>%
    count(area, nivel, wt = disponivel, sort = T) %>%
    pivot_wider( names_from = nivel, values_from = n, values_fill = 0)

estoca_add_etl %>%
    filter(area =="P1", nivel=="F") %>% select(local, disponivel, sku, produto) %>%
    distinct() %>% pull() %>% clipr::write_clip()

# Resumo stock sem 3P
estoca_add_etl %>%
    # filter( seller != 'Olist (Protheus)') %>%
    group_by(area ) %>%
    summarise(
        sku = n_distinct(sku),
        unit = sum(no_estoque),
        # valor = sum(valor_total, na.rm = T),
        seller = n_distinct(seller),
        .groups = "drop") %>%
    arrange(desc(sku)) %>%
    janitor::adorn_totals() %>% as_tibble()

#  resumo geral area
estoca_add_etl %>%
    group_by(area) %>%
    summarise( Cliente = n_distinct(seller),
               SKU = n_distinct(sku),
               Unit = sum(no_estoque),
               # Valor_BRL = sum(valor_total, na.rm = T),
               Address = n_distinct(local)) %>%
    mutate( pct_valor = Valor_BRL / sum(Valor_BRL, na.rm = T),
            pct_unit = Unit  / sum(Unit ),
            pct_address = Address    / sum(Address   ),
            pct_SKU = SKU  / sum(SKU ),
    ) %>%
    select( area, Cliente, Unit, pct_unit, SKU, pct_SKU, Valor_BRL, pct_valor,Address, pct_address) %>%
    arrange(desc(Unit)) %>%
    bind_rows(
        estoca_add_etl %>%
            summarise( area = "Total",
                       Cliente = n_distinct(seller),
                       Valor_BRL = sum(valor_total, na.rm = T),
                       Unit = sum(no_estoque),
                       Address = n_distinct(local),
                       SKU = n_distinct(sku) ) %>%
            mutate( pct_valor = Valor_BRL / sum(Valor_BRL),
                    pct_unit = Unit  / sum(Unit ),
                    pct_address = Address    / sum(Address   ),
                    pct_SKU = SKU  / sum(SKU ),
            ) %>%
            select( area, Cliente, Unit, pct_unit, SKU, pct_SKU, Valor_BRL, pct_valor,Address, pct_address) ) %>%
    gt::gt() %>%
    gt::fmt_currency( columns = c(Valor_BRL),
                      locale = "pt_BR", currency = "BRL",
                      decimals = 0, incl_space = T) %>%
    gt::fmt_number( columns = c(Cliente, Unit, Address, SKU),
                    locale ="pt_BR", decimals = 0  ) %>%
    gt::fmt_percent( columns = starts_with("pct"), decimals = 0)

estoca_add_etl %>%
    select(area,local, sku, no_estoque) %>%
    group_by( area) %>%
    summarise( unit = sum(no_estoque),
               sku = n_distinct(sku)) %>%
    ungroup() %>%
    mutate( avg_unit_sku = round(unit / sku, digits = 0)) %>%
    arrange(desc(sku)) %>%
    gt::gt()

# analise SKU per Address

estoca_add_etl %>%
    filter( area %in% c("B1", "E1", "R1", "R2", "P1", "P2")) %>%
    count(area, local, sort = T) %>%
    filter(n>=4)

# ver
estoca_g %>%
    group_by(nome_da_loja) %>%
    nest() %>%
    map( ~quantile(.) )

# pulverizaÃ§Ã£o address

p_ads <-
    estoca_add_etl %>%
    group_by(area,sku, produto) %>%
    summarise( ads = n_distinct(local)) %>%

    ggplot(aes(x = "", y = ads)) +
    geom_boxplot() +
    facet_wrap(~ area)

plotly::ggplotly(p_ads)

estoca_add_etl %>%
    group_by(area,sku, produto) %>%
    summarise( ads = n_distinct(local), .groups = "drop") %>%
    filter( area == "R1", ads >=2) %>% view()

estoca_add_etl %>%
    filter(rua == "028")

b_alto_sku <- estoca_add_etl |>
    filter(area == "P2") |>
    distinct(sku) |>

    anti_join(

estoca_add_etl |>
    filter(area %in% c("P1", "E1", "R1", "B1", "BR", "R2") ) |>
    distinct(sku)
                       )

estoca_add_etl |>
    filter( sku %in% b_alto_sku$sku, nivel =="A") |>
    # select( local, no_estoque, produto, sku, nivel)
    select( sku) |> pull() |> clipr::write_clip()

# Vendas geral Olist-------------------------------------------------------

library(tidyverse)

base_sales <- readxl::read_xlsx("../../joel.souza/Downloads/data.xlsx") %>% as_tibble() %>%
    janitor::clean_names()

base_sales %>%
    filter( seller_product_sku == 'PRD24FFY12O58QWU')


base_dup_gtin <-  base_sales %>%
    add_count(gtin, name = "gtin_count") %>%
    filter( gtin_count >1)

base_dup_fbo <-   base_dup_gtin %>%
    select(gtin, seller_flag, gtin_count) %>%
    pivot_wider( names_from = seller_flag,
                 values_from = gtin_count,
                 values_fn = sum,
                 values_fill = 0) %>%
    filter(FBO_3P_Seller >0, `Not in FBO`>0)


WS_base_sku <- third_party %>%
    filter(dias_stock_gmv == 'WithoutSales') %>%
    select(product_gtin )

third_party %>% writexl::write_xlsx("Dados_FBO_Stock.xlsx")
    filter(seller_id == "54973a3e-3543-4ac3-a26a-f17ba56f26d3") %>%
    select(brand, product_sku, product_name,stock, stock_value) %>%
    janitor::adorn_totals() %>% gt::gt() %>%
    gt::tab_header( "seller 54973a3e-3543-4ac3-a26a-f17ba56f26d3")


WS_base_sku %>% pull() %>% clipr::write_clip()

base_sales %>%
    filter( gtin %in% base_dup_fbo$gtin, seller_flag != 'FBO_3P_Seller_no_config') %>%
    select(seller_id, seller_product_sku, seller_responsible,stock, gmv, seller_flag) %>%
    pivot_wider( names_from = seller_flag,
                 values_from = gmv,
                 values_fill = 0) %>%
    filter( seller_product_sku %in% WS_base_sku$product_gtin)

# Estoca pedidos ----------------------------------------------------------

library(tidyverse)
bs <- readr::read_csv(file = "../../joel.souza/Downloads/Estoca (1).csv") %>% as_tibble() %>%
    janitor::clean_names() %>% select(-c(1, 3:origem, "marketplace_id", "x16" ))
bs_etl <- bs %>%
    mutate( qtde = stringr::str_extract( items, pattern = "^[0-9]+") %>% as.numeric(),
            descricao = stringr::str_extract( items, pattern = "[A-Z][a-z].*(?=\\()|[A-Z].(?=\\()"),
            SKU = stringr::str_extract( items, pattern = "(?<=\\)\\s).*(?=\\s)" ),
            pedido = stringr::str_extract( items, pattern = "[A-Z][0-9]{3}\\.[A-Z][0-9]{3}"),
            scount = str_count(items),
            pedido = str_sub( items, start = scount-6, end = scount),
            pedido0 = str_sub( number, start = 1, end = 6),
            descricao = str_to_title(descricao)) %>%
    tidyr::separate( col = SKU, into = c("SKU", "gtin", "out"), sep = "\\/") %>%
    select(-items, -out, -scount, -number)

bs_etl %>%
    count(descricao, wt = qtde, sort = T) %>% janitor::adorn_totals() %>% gt::gt()



