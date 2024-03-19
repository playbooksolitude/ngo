#24-0319 tue

#
library(tidyverse)
library(googlesheets4)
library(showtext)
showtext_auto()
library(bbplot)
library(wordcloud)

#
read_sheet("https://docs.google.com/spreadsheets/d/1tOaOmknCBi5U8RFEDJuuld5ZNY6EV1xAyNPvuon_nyw/edit#gid=1295458859", 
           sheet = "all_responses_copy") -> jrs_1csv

#
jrs_1csv |> 
  select(5:19) |> 
  slice(-c(1,2)) -> jrs_2slice

#
jrs_2slice |> names()
jrs_2slice |> 
  rename(소개 = 1,
         역할 = 2,
         뉴스레터_구독 = 4,
         거주지역 = 5,
         구성원수 = 14,
         정기모임횟수 = 15) -> jrs_3rename
jrs_3rename |> view()

#jrs_3rename
jrs_3rename
jrs_3rename |> names()
jrs_3rename |> select(9) |> view()
jrs_3rename$`운영자 역할을 하고 있는(했던) A커뮤니티는 어떤 목적, 내용, 방식으로 운영되고 있는지 간략하게 설명해주세요. `

jrs_3rename |> 
  count(소개)
jrs_3rename |> 
  filter(소개 == "위 어느 항목에도 해당되지 않는다. ")

jrs_3rename$소개
jrs_3rename |> str()
jrs_3rename |> is.na(역할)
is.na(jrs_3rename$역할)
jrs_3rename
jrs_3rename |> 
  filter(is.na(역할)) |> 
  select(1:2) |> print(n = Inf)

jrs_3rename |> 
  filter(!is.na(역할)) |> 
  drop_na(연령대) |> 
  count(역할,연령대,성별) |> 
  ggplot(aes(x = 연령대, y = 성별, fill = n)) +
  geom_tile(show.legend = F) +
  geom_text(aes(label = n), size = 10) +
  facet_wrap(.~역할) +
  bbc_style() +
  scale_fill_gradient(low = "#ebf4f5", high = "red")

#
jrs_3rename$역할

jrs_3rename |> 
  filter(!is.na(역할)) |> 
  drop_na(구성원수) |> 
  count(구성원수,정기모임횟수) -> jrs_3rename_1factor

  as.factor(jrs_3rename_1factor$구성원수) -> 
  jrs_3rename_1factor$구성원수
  
  jrs_3rename_1factor$구성원수 |> 
    fct_relevel(
      c("~ 5명","6~10명",
        "11~20명", "21~30명", "31명 ~")) -> 
    jrs_3rename_1factor$구성원수
  
jrs_3rename_1factor$정기모임횟수 |> as.factor() |> 
  fct_relevel(c("주 1회","격주 1회", "월 1회", 
                "기타")) -> jrs_3rename_1factor$정기모임횟수
  
jrs_3rename_1factor |> 
  ggplot(aes(x = 구성원수, y = 정기모임횟수, fill = n)) +
  geom_tile(show.legend = F) +
  geom_text(aes(label = n), size = 10) +
  #facet_wrap(.~역할) +
  bbc_style() +
  scale_fill_gradient(low = "#ebf4f5", high = "red") +
  ggtitle(label = "정기모임횟수 * 구성원수")
  
# konlp ----
jrs_3rename |> select(9) |> drop_na() -> jrs_3rename_2konlp


jrs_3rename_2konlp
jrs_3rename_2konlp |> 
  rename(설명 = 1) -> jrs_3rename_2konlp

extractNoun(jrs_3rename_2konlp$설명)

jrs_3rename_2konlp |> 
  unnest_tokens(input = 설명,
                output = words,
                token = extractNoun) |> 
  count(words, sort = T) -> jrs_3rename_3konlpresult
  
  
jrs_3rename_3konlpresult |> 
  count(words) |> 
  with(wordcloud(words, n, 
                 max.words = 40,
                 min.freq = 10,
                 random.order = F,
                 random.color = F,
                 colors = brewer.pal(8, "Dark2"),
                 scale = c(4,.5)))

wordcloud(words = jrs_3rename_4konlpresult$words,
          freq = jrs_3rename_4konlpresult$n,
          max.words = 40,
          min.freq = 5,
          random.order = F,
          random.color = F,
          colors = brewer.pal(8, "Dark2"),
          scale = c(4,.6))

jrs_3rename_3konlpresult |> 
  filter(str_count(jrs_3rename_3konlpresult$words > 1))

jrs_3rename_3konlpresult |> 
  filter(
    str_count(jrs_3rename_3konlpresult$words) > 1) -> 
  jrs_3rename_4konlpresult
