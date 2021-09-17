#PROGRAMANDO IA COM R - Exercicios IMDB

imdb <- readr::read_rds("imdb.rds")
install.packages('dplyr')
library('dplyr')

#1
filmes_ordenados <- imdb %>% select(ano,receita) %>% 
                        arrange(ano,desc(receita))

#teste <- imdb %>% select(receita) %>% arrange(desc(receita))

#2
filmes_colunas <- imdb %>% 
                   select(titulo,orcamento) %>% 
                   arrange(desc(orcamento))
#3
filmes_pb <- imdb %>% filter(cor=="Black and White")

#4
curtos_legais <- imdb %>% filter(duracao <= 90 & nota_imdb > 8.5) 

#5
imdb_prejuizo <- imdb %>% mutate(prejuizo=orcamento-receita) %>%
                           filter(prejuizo<0) %>% 
                           arrange(prejuizo)


#6
imdb_novas_colunas <- imdb %>%
                        mutate(
                          lucro = receita - orcamento,
                          lucro_medio = mean(lucro,na.rm = TRUE),
                          lucro_relativo = (lucro - lucro_medio)/lucro_medio,
                          houve_lucro = ifelse(lucro > 0, "sim", "não")
                        )

#Replace
imdb_tratado <- imdb_novas_colunas %>% 
                  select(lucro) %>% filter(is.na(lucro)) %>%
                  mutate(lucro=replace(lucro,is.na(lucro),17258230))

mean(imdb_novas_colunas$lucro,na.rm = TRUE)

