
library(tidyverse)
library(wordcloud2)

# Import -----------------------------------------------------------------------



googlesheets4::gs4_auth(email = "joel.rocha.souza@gmail.com")

dt <- 'https://docs.google.com/spreadsheets/d/1Xa7ei_ga6hV10dVGnprHy3tmVLkgZ_zfIinA_1YW0O8/edit#gid=0'

dados <-  googlesheets4::read_sheet(dt,col_types = "cn" )

dados


 wordcloud2::wordcloud2(dados, size = .6,
                        backgroundColor = "white",
                        shape = "circle")

 wordcloud2::demoFreq |> as_tibble()

