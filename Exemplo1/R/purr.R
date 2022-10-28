library(tidyverse)
library(purrr)

l <- list (
  um_numero = 123,
  um_vetor = c(TRUE, FALSE, TRUE),
  uma_string = 'abc',
  uma_lista = list (1,2,3)
)

c <- l [4]
d <- l [[4]]

c
d
