
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

# n > 1 and < length(str_in)
str_to_tokens(str_in = "J a c e k" , n = 2)
str_to_tokens(str_in = "J a c e k" , n = 3)


tokens_to_str = function(str_in, n){
  str_in_ls = unlist(strsplit(str_in, " "))
  str_in_ls
  
  for (i in 2:length(str_in_ls)) {
    str_in_ls[i] <- substring(str_in_ls[i], n)
  }
  
  str_out = paste(str_in_ls, collapse = "")
  str_out
}

tokens_to_str(str_in = "Ja ac ce ek", n = 2)
tokens_to_str(str_in = "Jac ace cek", n = 3)
  
