library(tidyverse)
install.packages('readxl')
library(readxl)
library(tidytext)
install.packages('tm')
library(tm)


love_words <- read_csv("love_words.csv")
colnames(love_words)[2] = "words"

love_words <- read_excel('love_words.xlsx',
                      sheet = 1, col_names = FALSE)
colnames(love_words)[1] <- "words"
love_stemmed_words <- stemDocument(love_words$words)

drug_words <- read_excel('drug_words.xlsx',
                         sheet = 1, col_names = FALSE)
colnames(drug_words)[1] <- "words"
drug_stemmed_words <- stemDocument(drug_words$words)

rural_words <- read_excel('rural_words.xlsx',
                         sheet = 1, col_names = FALSE)
colnames(rural_words)[1] <- "words"
rural_stemmed_words <- stemDocument(rural_words$words)

money_words <- read_excel('money_words.xlsx',
                         sheet = 1, col_names = FALSE)
colnames(money_words)[1] <- "words"
money_stemmed_words <- stemDocument(money_words$words)

religion_words <- read_excel('religion_words.xlsx',
                         sheet = 1, col_names = FALSE)
colnames(religion_words)[1] <- "words"
religion_stemmed_words <- stemDocument(religion_words$words)

urban_words <- read_excel('urban_words.xlsx',
                             sheet = 1, col_names = FALSE)
colnames(urban_words)[1] <- "words"
urban_stemmed_words <- stemDocument(urban_words$words)

sad_words <- read_excel('sad_words.xlsx',
                             sheet = 1, col_names = FALSE)
colnames(sad_words)[1] <- "words"
sad_stemmed_words <- stemDocument(sad_words$words)

profane_words <- read_excel('profane_words.xlsx',
                        sheet = 1, col_names = FALSE)
colnames(profane_words)[1] <- "words"
profane_stemmed_words <- stemDocument(profane_words$words)


write.csv(love_stemmed_words, "love_words.csv")
write.csv(rural_stemmed_words, "rural_words.csv")
write.csv(money_stemmed_words, "money_words.csv")
write.csv(drug_stemmed_words, "drug_words.csv")
write.csv(religion_stemmed_words, "religion_words.csv")
write.csv(urban_stemmed_words, "urban_words.csv")
write.csv(profane_stemmed_words, "profane_words.csv")
write.csv(sad_stemmed_words, "sad_words.csv")


billboard_data <- read_csv('billboard_lyrics_1964-2015.csv')

billboard_data <- billboard_data %>%
  filter(!is.na(Lyrics))


every_word <- billboard_data %>%
  unnest_tokens("word", "Lyrics")

#word_data <- every_word

every_word <- every_word %>%
  mutate(religion_word = 0) %>%
  mutate(urban_word = 0) %>%
  mutate(sad_word = 0) %>%
  mutate(profane_word = 0) %>%
  filter(!(word %in% stop_words$word))
  
  
every_word$word <- stemDocument(every_word$word)
#View(every_word)
#sample <- every_word %>%
 # head(1000)

#install.packages('SnowballC')
#library(SnowballC)

#love_stem <- wordStem(love_words$word, language="porter")
#every_word_stem <- wordStem(every_word$word, language="porter")

#every_word$word_stem <- every_word_stem[0]
#View(every_word_stem)

for (i in 1:nrow(every_word)){
  #if(every_word[i,6] %in% love_stemmed_words){
   # every_word[i,7] = 1
  #}
  if(every_word[i,6] %in% religion_stemmed_words){
    every_word[i,7] = 1
  }
  if(every_word[i,6] %in% urban_stemmed_words){
    every_word[i,8] = 1
  }
  if(every_word[i,6] %in% sad_stemmed_words){
    every_word[i,9] = 1
  }
  if(every_word[i,6] %in% profane_stemmed_words){
    every_word[i,10] = 1
  }
}

rural_counts <- every_word %>%
  group_by(Year, Rank, Song, Artist) %>%
  summarize(rural_count = sum(rural_word),
            num_words = sum(n())) %>%
  mutate(normalized_rural = rural_count/num_words)

rural_1 <- rural_counts %>%
  select(normalized_rural)

rural_scaled <- scale(rural_1$normalized_rural)

rural_counts$st_dev_rural <- rural_scaled

word_data$rural_count <- rural_counts$rural_count
word_data$normalized_rural <- rural_counts$normalized_rural
word_data$st_dev_rural <- rural_counts$st_dev_rural

love_counts <- every_word %>%
  group_by(Year, Rank, Song, Artist) %>%
  summarize(love_count = sum(love_word),
            num_words = sum(n())) %>%
  mutate(normalized_love = love_count/num_words)

every_word$love_count <- love_counts$love_count
every_word$normalized_love <- love_counts$normalized_love
every_word$st_dev_love <- love_counts$st_dev_love

love_1 <- love_counts %>%
  select(normalized_love)

love_scaled <- scale(love_1$normalized_love)

love_counts$st_dev_love <- love_scaled

love_counts %>%
  ggplot(aes(x=st_dev_love, y=Rank)) +
  geom_point()

love_counts %>%
  group_by(Year)%>%
  summarize(mean_sd = mean(st_dev_love))%>%
  ggplot(aes(x=Year, y=mean_sd))+
  geom_line()

#Drugs
drug_counts <- every_word %>%
  group_by(Year, Rank, Song, Artist) %>%
  summarize(drug_count = sum(drug_word),
            num_words = sum(n())) %>%
  mutate(normalized_drug = drug_count/num_words)

drug_1 <- drug_counts %>%
  select(normalized_drug)

drug_scaled <- scale(drug_1$normalized_drug)

drug_counts$st_dev_drug <- drug_scaled

word_data$drug_count <- drug_counts$drug_count
word_data$normalized_drug <- drug_counts$normalized_drug
word_data$st_dev_drug <- drug_counts$st_dev_drug


every_word <- every_word %>%
  mutate(song_year = paste0(Song,", ",Year))
  
#every_word_gathered <- every_word %>%
  #select(st_dev_love,st_dev_drug,song_year,Year)%>%
 # gather(key="Year",
  #       value="Standard Deviations",
   #      -factor("Year"))
  
  
View(every_word_gathered)

#Money
money_counts <- every_word %>%
  group_by(Year, Rank, Song, Artist) %>%
  summarize(money_count = sum(money_word),
            num_words = sum(n())) %>%
  mutate(normalized_money = money_count/num_words)

money_1 <- money_counts %>%
  select(normalized_money)

money_scaled <- scale(money_1$normalized_money)

money_counts$st_dev_money <- money_scaled

word_data$money_count <- money_counts$money_count
word_data$normalized_money <- money_counts$normalized_money
word_data$st_dev_money <- money_counts$st_dev_money

#religion
religion_counts <- every_word %>%
  group_by(Year, Rank, Song, Artist) %>%
  summarize(religion_count = sum(religion_word),
            num_words = sum(n())) %>%
  mutate(normalized_religion = religion_count/num_words)

religion_1 <- religion_counts %>%
  select(normalized_religion)

religion_scaled <- scale(religion_1$normalized_religion)

religion_counts$st_dev_religion <- religion_scaled

word_data$religion_count <- religion_counts$religion_count
word_data$normalized_religion <- religion_counts$normalized_religion
word_data$st_dev_religion <- religion_counts$st_dev_religion

#urban
urban_counts <- every_word %>%
  group_by(Year, Rank, Song, Artist) %>%
  summarize(urban_count = sum(urban_word),
            num_words = sum(n())) %>%
  mutate(normalized_urban = urban_count/num_words)

urban_1 <- urban_counts %>%
  select(normalized_urban)

urban_scaled <- scale(urban_1$normalized_urban)

urban_counts$st_dev_urban <- urban_scaled

word_data$urban_count <- urban_counts$urban_count
word_data$normalized_urban <- urban_counts$normalized_urban
word_data$st_dev_urban <- urban_counts$st_dev_urban

#sad
sad_counts <- every_word %>%
  group_by(Year, Rank, Song, Artist) %>%
  summarize(sad_count = sum(sad_word),
            num_words = sum(n())) %>%
  mutate(normalized_sad = sad_count/num_words)

sad_1 <- sad_counts %>%
  select(normalized_sad)

sad_scaled <- scale(sad_1$normalized_sad)

sad_counts$st_dev_sad <- sad_scaled

word_data$sad_count <- sad_counts$sad_count
word_data$normalized_sad <- sad_counts$normalized_sad
word_data$st_dev_sad <- sad_counts$st_dev_sad

#profane
profane_counts <- every_word %>%
  group_by(Year, Rank, Song, Artist) %>%
  summarize(profane_count = sum(profane_word),
            num_words = sum(n())) %>%
  mutate(normalized_profane = profane_count/num_words)

profane_1 <- profane_counts %>%
  select(normalized_profane)

profane_scaled <- scale(profane_1$normalized_profane)

profane_counts$st_dev_profane <- profane_scaled

word_data$profane_count <- profane_counts$profane_count
word_data$normalized_profane <- profane_counts$normalized_profane
word_data$st_dev_profane <- profane_counts$st_dev_profane

#drug_counts %>%
 # ggplot(aes(x=st_dev_drug, y=Rank)) +
  #geom_point()


std_drugs <- drug_counts %>%
  group_by(Year)%>%
  summarize(mean_sd = mean(st_dev_drug))

std_love <- love_counts %>%
  group_by(Year)%>%
  summarize(mean_sd = mean(st_dev_love))

std_rural <- rural_counts %>%
  group_by(Year)%>%
  summarize(mean_sd = mean(st_dev_rural))

std_money <- money_counts %>%
  filter(st_dev_money<5)%>%
  group_by(Year)%>%
  summarize(mean_sd = mean(st_dev_money))

std_religion <- religion_counts %>%
  filter(st_dev_religion<5)%>%
  group_by(Year)%>%
  summarize(mean_sd = mean(st_dev_religion))

std_urban <- urban_counts %>%
  filter(st_dev_urban<5)%>%
  group_by(Year)%>%
  summarize(mean_sd = mean(st_dev_urban))

std_profane <- profane_counts %>%
  filter(st_dev_profane<5)%>%
  group_by(Year)%>%
  summarize(mean_sd = mean(st_dev_profane))

money_word_data <- word_data %>%
  filter(st_dev_money<5)

ggplot() +
  #geom_line(data=std_love, aes(x=Year, y=mean_sd, color="Love")) +
  #geom_line(data=std_drugs, aes(x=Year, y=mean_sd, color = "Drugs"))+
  geom_line(data=std_money, aes(x=Year, y=mean_sd, color = "money"))
  #geom_point(data=money_word_data,aes(x=Year, y=st_dev_religion),size=1,alpha=0.25)+
  #geom_smooth(method = "lm")
  #geom_line(data=std_money, aes(x=Year, y=mean_sd, color = "Money"))+


plot_love<- love_counts %>%
  group_by(Year)%>%
  summarize(mean_sd = mean(st_dev_love))%>%
  ggplot(aes(x=Year, y=mean_sd))+
  geom_line()

plot_love %>% plot_drug

#
word_data %>%
  filter(st_dev_drug > 1) %>%
  ggplot()+
  geom_point(aes(y=st_dev_drug, x=Rank))

#predict what the song is about

word_data %>%
  ggplot()+
  geom_point(aes(x=Year, y=st_dev_drug),size=1,alpha=0.5)

drugs_big <- word_data %>%
  filter(st_dev_drug > 5)
###########################


url <- "https://genius.com/lil-pump-gucci-gang-lyrics"

text <- url %>%
                   read_html() %>%
                   html_nodes("p") %>%
                   html_text()

lyrics <- text[[1]]
lyrics_replace <- str_replace_all(lyrics,"\\n"," ")
lyrics_letters <- str_replace_all(lyrics_replace,"[:punct:]"," ")
lyrics_lower <- str_to_lower(lyrics_letters)
lyrics_list <- strsplit(lyrics_lower, " ")
lyrics_vector <- as.vector(unlist(lyrics_list, use.names=FALSE))
#lyrics_df <- data.frame(lyrics_vector)
#names(lyrics_vector) <- 1:length(lyrics_vector)
#mylist2 <- lyrics_vector[which(!sapply(lyrics_vector, is.null))]
lyrics_vector <- lyrics_vector[lyrics_vector != ""]

lyrics_stemmed <- stemDocument(lyrics_vector)
lyrics_df <- data.frame(lyrics_stemmed)
colnames(lyrics_df)[1] <- "words"

#lyrics_df$words <- stemDocument(lyrics_df$words)

lyrics_df <- lyrics_df %>%
  mutate(drug_words = 0) %>%
  mutate(money_words = 0) %>%
  mutate(love_words = 0) %>%
  mutate(rural_words = 0)

for (i in 1:nrow(lyrics_df)){
  #if(every_word[i,6] %in% love_stemmed_words){
  # every_word[i,7] = 1
  #}
  if(lyrics_df[i,1] %in% drug_stemmed_words){
    lyrics_df[i,2] = 1
  }
  if(lyrics_df[i,1] %in% money_stemmed_words){
    lyrics_df[i,3] = 1
  }
  if(lyrics_df[i,1] %in% love_stemmed_words){
    lyrics_df[i,4] = 1
  }
  if(lyrics_df[i,1] %in% rural_stemmed_words){
    lyrics_df[i,5] = 1
  }
}



rural_count = sum(lyrics_df$rural_words)
money_count = sum(lyrics_df$money_words)
drug_count = sum(lyrics_df$drug_words)
love_count = sum(lyrics_df$love_words)
num_words = nrow(lyrics_df)

normalized_rural = rural_count/num_words
normalized_money = money_count/num_words
normalized_drug = drug_count/num_words
normalized_love = love_count/num_words

#paste0(money_count)


proportions <- c(normalized_rural, normalized_money, normalized_drug, normalized_love)
proportions_df <- data.frame(proportions)

proportions_df <- proportions_df %>%
  mutate(Theme = c("rural","money","drug","love"))

gg <- ggplot()+
  geom_bar(data=proportions_df, aes(x=Theme, y=proportions), stat="identity")
gg
