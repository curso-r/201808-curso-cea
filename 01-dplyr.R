# Pacotes -----------------------------------------------------------------

library(tidyverse)

# Pipe (%>%) --------------------------------------------------------------

# Receita de bolo sem pipe. Tente entender o que é preciso fazer.

esfrie(
  asse(
    coloque(
      bata(
        acrescente(
          recipiente(
            rep(
              "farinha", 
              2
            ), 
            "água", "fermento", "leite", "óleo"
          ), 
          "farinha", até = "macio"
        ), 
        duração = "3min"
      ), 
      lugar = "forma", tipo = "grande", untada = TRUE
    ), 
    duração = "50min"
  ), 
  "geladeira", "20min"
)

# Veja como o código acima pode ser reescrito utilizando-se o pipe. Agora realmente se parece com uma receita de bolo.

recipiente(rep("farinha", 2), "água", "fermento", "leite", "óleo") %>%
  acrescente("farinha", até = "macio") %>%
  bata(duração = "3min") %>%
  coloque(lugar = "forma", tipo = "grande", untada = TRUE) %>%
  asse(duração = "50min") %>%
  esfrie("geladeira", "20min")

# Base de dados -----------------------------------------------------------

imdb <- read_rds("data/imdb.rds")

# exercício 1
# Leia as bases que estão na pasta data.



# filter ------------------------------------------------------------------

# exemplo 1
imdb %>% filter(nota_imdb > 9)

# exemplo 2
filmes_bons <- imdb %>% filter(nota_imdb > 8 & nota_imdb < 9)
filmes_bons

# exemplo 3
filmes_bons <- filmes_bons %>% filter(orcamento < 1000000)
filmes_bons

# exemplo 4
# comparações com o R

1 == 1
"a" == "b"

sqrt(2) ^ 2 == 2
1 / 49 * 49 == 1

near(sqrt(2) ^ 2,  2)
near(1 / 49 * 49, 1)

bons <- imdb %>% filter(nota_imdb > 9)
bons$ano
bons$ano == 2010
bons$ano > 2010
bons$ano <= 2010

x <- c(2010, 2011, 2012)

bons$ano > x
bons$receita > bons$orcamento

# exercício 1
# Criar uma variável chamada `filmes_baratos` com filmes com orçamento menor do 
# que 1 milhão de dólares.


# exemplo 5
# operadores lógicos

imdb %>% filter(ano > 2010 & nota_imdb > 8.5) %>% View()
imdb %>% filter(orcamento < 100000 & receita > 1000000) %>% View()

imdb %>% filter(receita > orcamento)
imdb %>% filter(receita > orcamento + 500000000) %>% View()
imdb %>% filter(receita > orcamento + 500000000 | nota_imdb > 9) %>% View()

imdb %>% filter(ano > 2010)
imdb %>% filter(!ano > 2010)
imdb %>% filter(!receita > orcamento) %>% View()

# exercício 2
# Criar um objeto chamado bons_baratos com filmes que tiveram nota no imdb 
# maior do que 8.5 e um orcamento menor do que 1 milhão de dólares.

# exercício 3
# Criar um objeto chamado curtos_legais com filmes de até 1h30 e nota no imdb
# maior do que 8.5.

# exercício 4
# Criar um objeto antigo_colorido com filmes de antes de 1940 que são 
# coloridos. Crie também um objeto antigo_bw com filmes antigos que não são coloridos.

# exercício 5
# Criar um objeto ww com filmes do Wes Anderson e do Woody Allen.

# exemplo 6
# %in%

pitts <- imdb %>% filter(ator_1 %in% c('Angelina Jolie Pitt', "Brad Pitt"))
pitts <- imdb %>% filter(
  ator_1 %in% c('Angelina Jolie Pitt', "Brad Pitt"),
  ator_2 %in% c('Angelina Jolie Pitt', "Brad Pitt")
  )

# exercicio 6
# Refaça o exercício 5 usando o %in%.

# exemplo 7
# NA

NA > 5

10 == NA

NA + 10

NA / 2

NA == NA

# Seja x a idade de Maria. Não sabemos a idade de Maria:
x <- NA

# Seja y a idade de João. Não sabemos a idade de João:
y <- NA

# Maria e João têm a mesma idade?
x == y
#> [1] NA
# Não sabemos.

is.na(x)

df <- tibble(x = c(1, NA, 3))
# filter(df, x > 1)
# filter(df, is.na(x))
df %>% filter(x > 1)
df %>% filter(is.na(x))

imdb %>% filter(is.na(orcamento))

# exercício 7
# Identifique os filmes que não possuem informação tanto de receita quanto de orcamento
# e salve em um objeto com nome sem_info.

# exemplo 8
# str_detect

imdb %>% filter(str_detect(generos, "Action"))
imdb %>% filter(generos == "Action")

# exercício 8
# Salve em um objeto os filmes de Ação e Comédia com nota no imdb maior do que 8.

# arrange -----------------------------------------------------------------

# exemplo 1

imdb %>% arrange(orcamento) %>% View()

# exemplo 2

imdb %>% arrange(desc(orcamento)) %>% View()

# exemplo 3

imdb %>% arrange(desc(ano), titulo) %>% View()

# exercício 1
# Ordene os filmes em ordem decrescente de lucro por ano e salve em um objeto chamado
# filmes_ordenados

# exemplo 4
# NA

df <- tibble(x = c(NA, 1, 2), y = c(1, NA, 2))

df %>% arrange(x)
df %>% arrange(y)
df %>% arrange(x, y)
df %>% arrange(!is.na(x), x)

# exemplo 5

imdb %>% 
  filter(ano == 2010) %>% 
  arrange(desc(orcamento)) %>%
  View()

# exercício 2 
# Ordene por ordem decrescente do orçamento os filmes de um diretor a sua escolha.
# Salve o resultado em um objeto chamado diretor_ordenado


# select ------------------------------------------------------------------

# exemplo 1

imdb %>% select(titulo, ano, orcamento)

# exemplo 2 

imdb %>% select(starts_with("ator"))

# exemplo 3

imdb %>% select(-starts_with("ator"), -titulo)

# exercício 1
# Crie uma tabela com apenas as colunas titulo, diretor, e orcamento. Salve em um
# objeto chamado imdb_simples.

# exercício 2
# Remova as colunas ator_1, ator_2 e ator_3 de três formas diferentes. Salve em um
# objeto chamado imdb_sem_ator.

# exercício 3
# Crie uma tabela apenas com filmes do Woody Allen e as colunas titulo e ano
# ordenada por ano.

# mutate ------------------------------------------------------------------

# exemplo 1

imdb %>% mutate(
  duracao = duracao/60,
  duracao_horas = duracao
  )

# exemplo 2

imdb %>% mutate(duracao_horas = duracao/60)

# exercício 1
# Crie uma variável chamada lucro. Salve em um objeto chamado imdb_lucro.

# exercicio 2
# Modifique a variável lucro para ficar na escala de milhões de dólares.

# exercício 3
# Filtre apenas os filmes com prejuízo maior do que 3 milhões de dólares. 
# Deixe essa tabela ordenada com o maior prejuízo primeiro. Salve o resultado em 
# um objeto chamado filmes_prejuizo.

# exemplo 3
# gêneros

# install.packages("gender")
library(gender)

gender(c("Madison", "Hillary"), years = 1930, method = "ssa")
gender(c("Madison", "Hillary"), years = 2010, method = "ssa")
gender(NA, years = 1920)

obter_genero <- function(nome, ano) {
  
  if (is.na(nome) | is.na(ano)) {
    return(NA_character_)
  }
  
  ano_min <- ano - 60
  ano_max <- ano - 30
   
  if (ano_min < 1880) {
    ano_min <- 1880
  }
  
  genero <- gender(nome, years = c(ano_min, ano_max), method = "ssa")$gender
  
  if(length(genero) == 0) {
    genero <- NA_character_
  }
  
  genero
}

obter_genero("Madison", 1930)
obter_genero(NA, NA)

# demora +- 10 min.
imdb_generos <- imdb %>%
  select(diretor, ano) %>%
  mutate(
    diretor_primeiro_nome = str_extract(diretor, ".* ") %>% str_trim(),
    diretor_sobrenome = str_extract(diretor, " .*") %>% str_trim(),
    genero = map2_chr(diretor_primeiro_nome, ano, obter_genero)
    )

# saveRDS(imdb_generos, "data/imdb_generos.rds")
imdb_generos <- read_rds("data/imdb_generos.rds")

# https://github.com/meirelesff/genderBR

# summarise ---------------------------------------------------------------

# exemplo 1

imdb %>% summarise(media_orcamento = mean(orcamento, na.rm = TRUE))

# exemplo 2

imdb %>% summarise(
  media_orcamento = mean(orcamento, na.rm=TRUE),
  mediana_orcamento = median(orcamento, na.rm = TRUE)
)

# exemplo 3

imdb %>% summarise(
  media_orcamento = mean(orcamento, na.rm=TRUE),
  mediana_orcamento = median(orcamento, na.rm = TRUE),
  qtd = n(),
  qtd_diretores = n_distinct(diretor)
)

# exemplo 4

imdb_generos %>%
  summarise(n_diretora = sum(genero == "female", na.rm = TRUE))

# exercício 1
# Use o `summarise` para calcular a proporção de filmes com diretoras.

# exercício 2
# Calcule a duração média e mediana dos filmes da base.

# exercício 3
# Calcule o lucro médio dos filmes com duracao < 60 minutos. E o lucro médio dos filmes com
# mais de 2 horas.

# group_by + summarise ----------------------------------------------------

# exemplo 1

imdb %>% group_by(ano)

# exemplo 2

imdb %>% 
  group_by(ano) %>% 
  summarise(qtd_filmes = n()) %>%
  View()

# exemplo 3

imdb %>% 
  group_by(diretor) %>% 
  summarise(qtd_filmes = n())

# exercício 1
# Crie uma tabela com apenas o nome dos diretores com mais de 10 filmes.

# exercício 2
# Crie uma tabela com a receita média e mediana dos filmes por ano.

# exercício 3
# Crie uma tabela com a nota média do imdb dos filmes por tipo de classificacao.

# exemplo 4

imdb %>%
  filter(str_detect(generos, "Action"), !is.na(diretor)) %>%
  group_by(diretor) %>%
  summarise(qtd_filmes = n()) %>%
  arrange(desc(qtd_filmes))

# exemplo 5

imdb %>% 
  filter(ator_1 %in% c("Brad Pitt", "Angelina Jolie Pitt")) %>%
  group_by(ator_1) %>%
  summarise(
    lucro = mean(receita - orcamento), 
    orcamento = mean(orcamento), 
    receita = mean(receita), 
    qtd = n()
  )

# left join ---------------------------------------------------------------

imdb_generos <- read_rds("data/imdb_generos.rds")

# exemplo 1

imdb_generos2 <- imdb %>%
  left_join(imdb_generos, by = c("titulo", "diretor", "ano"))

# exemplo 2

depara_cores <- tibble(
  cor = c("Color", "Black and White"),
  cor2 = c("colorido", "pretoEbranco")
)

imdb_cor <- left_join(imdb, depara_cores, by = c("cor"))

# exemplo 3


imdb_generos3 <- imdb %>%
  left_join(imdb_generos, by = c("diretor", "ano"))



# exercicio 1
# Calcule a média dos orçamentos e receitas para filmes feitos por
# genero do diretor.

imdb_generos <- read_rds("data/imdb_generos.rds")


# gather ------------------------------------------------------------------

# exemplo 1

imdb_gather <- imdb %>% gather("importancia_ator", "nome_ator", starts_with("ator"))

# spread ------------------------------------------------------------------

# exemplo 1

ano_genero <- imdb_generos2 %>%
  group_by(ano, genero) %>%
  summarise(orcamento = mean(orcamento, na.rm = TRUE))

ano_genero %>%
  spread(genero, orcamento) %>% View()
























