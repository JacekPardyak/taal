library(vwr)
library(markovchain)
#library(tidyverse)
library(readr)
data <- read.csv("data_in/nl_NL.csv",
                 header = FALSE,
                 encoding = "ISO-8859-1", stringsAsFactors = F)
# Functions for n - grams
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


# prepare n-grams
data$gram_1 <- sapply(data$V1, function(x) {paste(unlist(strsplit(x, "")), collapse = " ")})
data$gram_2 <- sapply(data$gram_1, function(x) {str_to_tokens(str_in = x, n = 2)})
data$gram_3 <- sapply(data$gram_1, function(x) {str_to_tokens(str_in = x, n = 3)})

# train and save models
grams = c("gram_1", "gram_2", "gram_3")
for (gram in grams) {
  list<- unlist(strsplit(paste(data[,gram], collapse = " \t "), " "))
  mcFit <- markovchainFit(data = list)
  states           <- mcFit$estimate@states
  transitionMatrix <- mcFit$estimate@transitionMatrix
  write_lines(states, 
              paste("./models/mcFit_", gram, "_states.txt", sep = ""))
  write_lines(transitionMatrix, 
              paste("./models/mcFit_", gram, "_transitionMatrix.txt", sep = ""))
}

# --------------------------------------------------------------------
# construct models from files
# 1 - gram
states = read_lines("./models/mcFit_1_states.txt")
states

states = read_lines("https://jacekpardyak.github.io/Kaggle/models/mcFit_1_states.txt", 
                    locale = locale(encoding = "UTF-8"))
transitionMatrix = read_lines("./models/mcFit_1_transitionMatrix.txt")
#transitionMatrix = read_lines("./models/mcFit_1_transitionMatrix.zip")
temp <- tempfile()
download.file("https://jacekpardyak.github.io/Kaggle/models/mcFit_1_transitionMatrix.zip",temp)
transitionMatrix <- read_lines((unz(temp, "mcFit_1_transitionMatrix.txt")))
unlink(temp)

transitionMatrix <- matrix(data = as.numeric(transitionMatrix),
                           byrow = FALSE,
                           nrow = length(states),
                         dimnames = list(states, states))

mcFit_1_exp <- new("markovchain", 
                   states = states, 
                   byrow = TRUE,
                   transitionMatrix = transitionMatrix,
                   name = "dutch-1-gram")
# 2 - gram
#states = read_lines("./models/mcFit_2_states.txt")
states = read_lines("https://jacekpardyak.github.io/Kaggle/models/mcFit_2_states.txt")
#transitionMatrix = read_lines("./models/mcFit_2_transitionMatrix.txt")
#transitionMatrix = read_lines("./models/mcFit_2_transitionMatrix.zip")
temp <- tempfile()
download.file("https://jacekpardyak.github.io/Kaggle/models/mcFit_2_transitionMatrix.zip",temp)
transitionMatrix <- read_lines((unz(temp, "mcFit_2_transitionMatrix.txt")))
unlink(temp)

transitionMatrix <- matrix(data = as.numeric(transitionMatrix),
                           byrow = FALSE,
                           nrow = length(states),
                           dimnames = list(states, states))

mcFit_2_exp <- new("markovchain", 
                   states = states, 
                   byrow = TRUE,
                   transitionMatrix = transitionMatrix,
                   name = "dutch-2-gram")

# 3 - gram
#states = read_lines("./models/mcFit_3_states.txt")
states = read_lines("https://jacekpardyak.github.io/Kaggle/models/mcFit_3_states.txt")
#transitionMatrix = read_lines("./models/mcFit_3_transitionMatrix.txt")
#transitionMatrix = read_lines("./models/mcFit_3_transitionMatrix.zip")
temp <- tempfile()
download.file("https://jacekpardyak.github.io/Kaggle/models/mcFit_3_transitionMatrix.zip",temp)
transitionMatrix <- read_lines((unz(temp, "mcFit_3_transitionMatrix.txt")))
unlink(temp)

transitionMatrix <- matrix(data = as.numeric(transitionMatrix),
                           byrow = FALSE,
                           nrow = length(states),
                           dimnames = list(states, states))

mcFit_3_exp <- new("markovchain", 
                   states = states, 
                   byrow = TRUE,
                   transitionMatrix = transitionMatrix,
                   name = "dutch-3-gram")



predict(mcFit_1_exp, newdata = "\t", n.ahead = 5)
predict(mcFit_2_exp, newdata = "\t", n.ahead = 5)
predict(mcFit_3_exp, newdata = "\t", n.ahead = 5)

# pseudo words
# 1 - gram
df_out_1 <- 
  markovchainSequence(n=100,
                      markovchain = mcFit_1_exp,
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
                      markovchain = mcFit_2_exp,
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
                      markovchain = mcFit_3_exp,
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
pred_3 = df_out_3$V1[16]
pred_3 %in% dutch.words
levenshtein.distance(pred_3, sample(dutch.words, 20))
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

