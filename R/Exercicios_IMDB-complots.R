#PROGRAMANDO IA COM R - Exercicios IMDB
library("tidyverse")

imdb <- readr::read_rds("fiap/desafios-fiap/R/imdb.rds")


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

#Join

#1
imdb_nota_media <- imdb %>% select(diretor,nota_imdb) %>% 
  group_by(diretor) %>%
  summarise(nota_media=mean(nota_imdb))

#2
imdb_join <- left_join(imdb_nota_media,imdb,by="diretor")

# ifelse(is.na(bla), median(bla, na.rm = TRUE, bla)


library("ggplot2")

ggplot(data=mtcars)
plot(mtcars$mpg, mtcars$qsec, pch=42)

imdb %>% ggplot() + geom_point(mapping = aes(x=nota_imdb, y=orcamento), color='blue')


top5 <- imdb_novas_colunas %>% group_by(ator_1) %>%
  summarise(total=n()) %>% 
  arrange(desc(total)) %>% 
  head 
top5

imdb_novas_colunas %>% filter(ator_1 %in% top5$ator_1) %>% 
  ggplot() + geom_boxplot(aes(y=lucro))


group_by(diretor) %>% 
  filter(n() >= 5) %>%
  

imdb %>% 
  filter(!is.na(diretor)) %>%
  group_by(diretor) %>% 
  filter(n() >= 15) %>% 
  mutate(lucro = receita - orcamento) %>% 
  ggplot() +
  geom_boxplot(aes(x = diretor, y = lucro))



