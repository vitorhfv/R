#PURR

library(purr)


install.packages('purr')


soma_um <- function(x) { x + 1 }


obj <- 10:15

for (i in seq_along(obj)) {
  obj[i] <- soma_um(obj[i])
}


obj


obj <- 10:15

map_dbl(obj, soma_um)


soma_n <- function(x, n = 1) { x + n }

obj <- 10:15

map_dbl(obj, ~soma_n(.x, 2))

soma_ambos <- function(x, y) { x + y }
obj_1 <- 10:15
obj_2 <- 20:25
for (i in seq_along(obj_1)) {
  obj_1[i] <- soma_ambos(obj_1[i], obj_2[i])
}
obj_1


obj_1 <- 10:15
obj_2 <- 20:25
map2_dbl(obj_1, obj_2, ~.x+.y)

obj <- 10:20
reduce(obj, ~.x+.y)

###########################################

filme         |       ano |     elenco        | nota_imdb | imdb_classificacao
Clube da Luta |     1999  |   Brad Pitt       |  8        |   ALTO
A vida        |     2002  |   Orlando Bloom   |  6        |   MEDIO
Nova Jersei   |     2009  |   Sam Worthington |  4        |   BAIXO


