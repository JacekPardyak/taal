library(vwr)
library(markovchain)
library(tidyverse)
library(zip)
data <- read.csv("data_in/nl_NL.csv",
                 header = FALSE,
                 encoding = "ISO-8859-1",
                 stringsAsFactors = F)
# change encoding
Encoding(data$V1)  <- "UTF-8"

# skip duplicates
data <- data.frame(V1 = data[!duplicated(data$V1),], stringsAsFactors = F) # 135254 rows

# skip too short and too long words
vec <- sapply(data$V1, function(x) nchar(x))
data <- data.frame(V1 = data[vec > 3 & vec < 15,], stringsAsFactors = F) # 123481 rows

# skip words under different alphabet
alphabet = c('a', 'b', 'c', 'd', 'e' , 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'ĳ', 'z',
             'ä', 'ï', 'ü', 'ö' , 'ë', 'á', 'ó', 'é', 'è' )
is_proper <- function(x){
  tmp <- unlist(strsplit(x, ""))
  sum(sapply(tmp, function(x) x %in% alphabet)) == length(tmp)
}

vec <- sapply(data$V1, function(x) is_proper(x))
data <- data.frame(V1 = data[vec,], stringsAsFactors = F) # 94752 rows

# coding check
'crèche' %in% data$V1

str_to_tokens = function(str_in, n){
  str_in_ls = unlist(strsplit(str_in, " "))
  str_out = str_in
  for (i in c(1:(n-1))) {
    str_out_ls = unlist(strsplit(str_out, " "))
    str_out_ls = str_out_ls[-length(str_out_ls)]
    str_in_ls <- str_in_ls[-1]
    str_out = paste(str_out_ls, str_in_ls, sep ="", collapse = " ")
  }
  str_out
}

tokens_to_str = function(str_in, n){
  str_in_ls = unlist(strsplit(str_in, " "))
  for (i in 2:length(str_in_ls)) {
    str_in_ls[i] <- substring(str_in_ls[i], n)
  }
  str_out = paste(str_in_ls, collapse = "")
  str_out
}

# use dutch.words
data <- data.frame(V1 = dutch.words, stringsAsFactors = F)


# prepare n-grams
data$gram_1 <- sapply(data$V1, function(x) {paste(unlist(strsplit(x, "")), collapse = " ")})
data$gram_2 <- sapply(data$gram_1, function(x) {str_to_tokens(str_in = x, n = 2)})
data$gram_3 <- sapply(data$gram_1, function(x) {str_to_tokens(str_in = x, n = 3)})

# train and save models
grams = c("gram_1", "gram_2", "gram_3")
for (gram in grams[2:2]) {
  list<- unlist(strsplit(paste(data[,gram], collapse = " \t "), " "))
  mcFit <- markovchainFit(data = list)
  states           <- mcFit$estimate@states
  transitionMatrix <- mcFit$estimate@transitionMatrix
  fileConn <- file(paste("./models/mcFit_", gram, "_states.txt", sep = ""))
  writeLines(states, fileConn)
  close(fileConn)
  fileConn <- file(paste("./models/mcFit_", gram, "_transitionMatrix.txt", sep = ""))
  writeLines(as.character(transitionMatrix), fileConn)
  close(fileConn)
  
}

zip_files = list.files(path = "./models/", pattern = ".txt$", full.names = T)
zip(zipfile = "/models/models.zip", files = zip_files)

# --------------------------------------------------------------------
# construct models from files
temp <- tempfile()
download.file("https://jacekpardyak.github.io/Kaggle/dutch_models.zip",temp)
# 1 - gram
states <- read_lines(unz(temp, "mcFit_gram_1_states.txt"))
transitionMatrix = read_lines(unz(temp, "mcFit_gram_1_transitionMatrix.txt"))
transitionMatrix <- matrix(data = as.numeric(transitionMatrix),
                           byrow = FALSE,
                           nrow = length(states),
                           dimnames = list(states, states))
mcFit_1 <- new("markovchain", 
                   states = states, 
                   byrow = TRUE,
                   transitionMatrix = transitionMatrix,
                   name = "dutch-1-gram")

# 2 - gram
states <- read_lines(unz(temp, "mcFit_gram_2_states.txt"))
transitionMatrix = read_lines(unz(temp, "mcFit_gram_2_transitionMatrix.txt"))
transitionMatrix <- matrix(data = as.numeric(transitionMatrix),
                           byrow = FALSE,
                           nrow = length(states),
                           dimnames = list(states, states))
mcFit_2 <- new("markovchain", 
               states = states, 
               byrow = TRUE,
               transitionMatrix = transitionMatrix,
               name = "dutch-2-gram")
# 3 - gram
states <- read_lines(unz(temp, "mcFit_gram_3_states.txt"))
transitionMatrix = read_lines(unz(temp, "mcFit_gram_3_transitionMatrix.txt"))
transitionMatrix <- matrix(data = as.numeric(transitionMatrix),
                           byrow = FALSE,
                           nrow = length(states),
                           dimnames = list(states, states))
mcFit_3 <- new("markovchain", 
               states = states, 
               byrow = TRUE,
               transitionMatrix = transitionMatrix,
               name = "dutch-3-gram")
unlink(temp)

# ------------------------------------------------------------------------------
# Evaluate models

predict(mcFit$estimate, newdata = "\t", n.ahead = 5)
predict(mcFit_2, newdata = "\t", n.ahead = 5)
predict(mcFit_3, newdata = "\t", n.ahead = 5)

# pseudo words
# 1 - gram
df_out_1 <- 
  markovchainSequence(n=100,
                      markovchain = mcFit_1,
                      include=TRUE,
                      t0="\t") %>%
  paste(collapse = " ")  
df_out_1 <- gsub(pattern = "\t", replacement = "\n", x = df_out_1) %>% textConnection() %>%
  read.csv(header = F, stringsAsFactors = F) 
df_out_1$V1 <- sapply(df_out_1$V1, function(x) {gsub(" ", "", x)})
df_out_1                    
                      
# 2 - gram
df_out_2 <- 
  markovchainSequence(n=100,
                      markovchain = mcFit_2,
                      include=TRUE,
                      t0="\t") %>%
  paste(collapse = " ")  
df_out_2 <- gsub(pattern = "\t", replacement = "\n", x = df_out_2) %>% textConnection() %>%
  read.csv(header = F, stringsAsFactors = F)  %>%
  mutate(V1 = trimws(V1))
df_out_2$V1 <- sapply(df_out_2$V1, function(x) {tokens_to_str(str_in = x, n = 2)})
df_out_2

# 3 - gram
df_out_3 <- 
  markovchainSequence(n=100,
                      markovchain = mcFit_3,
                      include=TRUE,
                      t0="\t") %>%
  paste(collapse = " ")  
df_out_3 <- gsub(pattern = "\t", replacement = "\n", x = df_out_3) %>% textConnection() %>%
  read.csv(header = F, stringsAsFactors = F)  %>%
  mutate(V1 = trimws(V1))
df_out_3$V1 <- sapply(df_out_3$V1, function(x) {tokens_to_str(str_in = x, n = 3)})
df_out_3


# -------------------------------------------
# Evaluate the result
pred_3 = 'gesaus'
pred_3 %in% dutch.words
#levenshtein.distance(pred_3, sample(dutch.words, 20))
levenshtein.neighbors(pred_3, dutch.words)[1:2]

df_out_3 <- 
  markovchainSequence(n=100,
                      markovchain=mcFit_3$estimate,
                      include=TRUE,
                      t0="\n") %>%
  paste(collapse = " ") %>% textConnection() %>%
  read.csv(header = F, stringsAsFactors = F) %>%
  mutate(V1 = trimws(V1))
df_out_3$V1 <- sapply(df_out_3$V1, function(x) {tokens_to_str(str_in = x, n = 3)})

pred_3 <- df_out_3$V1[4]
pred_3 <- 'crèche'
pred_3 <- levenshtein.neighbors(pred_3, dutch.words)[[2]][[2]]
# check if in dictionary
pred_3 %in% dutch.words
levenshtein.distance(pred_3, sample(dutch.words, 20))
levenshtein.neighbors(pred_3, dutch.words)[1:2]



# set up a mock experiment: English stimuli are words, Basque stimuli are nonwords
experiment <- data.frame(stimulus=c(sample(english.words,500),
                                  sample(basque.words,500)),
                       type=factor(rep(c('Word','Nonword'),each=500),levels=c('Word','Nonword')))


# randomize the trials
experiment<-experiment[sample(1:1000,1000),]
# run the ldknn algorithm
results<-ldknn(experiment$stimulus,experiment$type,'Word')
print(results)
plot(results)

# ----------

?ldknn



levenshtein.distance('jacek', c("Jacek", "jaceke", "ola", "jacek"))

data(serbian_latin.words)

levenshtein.damerau.neighbors('jacek', dutch.words)[1:2]

levenshtein.neighbors('botstels', dutch.words)[1:2]

old20('huis', dutch.words, method="levenshtein", parallel = FALSE)

ald(dutch.words[1:10], dutch.words, 20)
old20(dutch.words[1:10], dutch.words)


write_lines(dutch.words, "./models/dutch-words.txt")



dir('./models/')
# [1] "cats.csv" "test.csv" "txt.txt" 
zip(zipfile = 'model.zip', files = './models/mcFit_3_transitionMatrix.txt')

?zip

files = list.files("./models/", full.names = T)
zip("./models/models.zip", files = files)

