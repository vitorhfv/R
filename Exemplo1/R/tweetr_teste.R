library(rtweet)
library(tidyverse)
library(chron)
ceni<-
  rtweet::search_tweets(q="calleri",
                        n=200000,
                        include_rts = FALSE)




ceni


ceni1 <- ceni %>%
  select(created_at, id)



ceni2 %>%
  mutate (contagem = map(data3, ~sum(id)))





ceni2<-ceni %>%
  mutate(data2 = ymd_hms (created_at),
         data3 = as_date(data2)
         ) %>%
  select(data3, id)




ceni3 <- ceni2 %>%
  group_by(data3) %>%
  summarise(count = n())

ceni3 %>%
  ggplot (aes(
    x= data3, y= count
  ))+
  geom_line()


pintou_clima2 <- pintou_clima %>%
  select(created_at, id_str)

pintou_clima3 <- pintou_clima2 %>%
  mutate(data2 = ymd_hms (created_at),
         data3 = as_date(data2)
         ) %>%
  select(data3, id_str)

pintou_clima4 <- pintou_clima3 %>%
  group_by(data3) %>%
  summarise(count = n())



pintou_clima4 %>%
  ggplot (aes(
  x= data3, y = count
  ))+
  geom_line()

