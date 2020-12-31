base_url <- '~/GitHub/kul-multivariate-a2/src/'
load(paste0(base_url,"confusion.Rdata"))
library(dplyr)
library(qdapRegex)
library(stringr)

headers <- c('signal','alph', 'nchar', 'dots', 'lines', 's_dot', 's_line')


v_signal <- colnames(confusion)
v_signal <- str_replace(v_signal, " ", "")
v_signal <- str_replace(v_signal, "- -", "--")
v_signal <- str_replace(v_signal, "· -", "·-")
v_signal <- str_replace(v_signal, "· ·", "··")
v_signal <- str_replace(v_signal, "- ·", "-·")


another_matrix <- data.frame(v_signal) %>% mutate(alph = alphabet, 
                                                  size = nchar(trimws(v_signal)),
                                                  ndots =  str_count(v_signal, "·"),
                                                  nlines = str_count(v_signal, "-"),
                                                  sig_s_length = (str_count(v_signal, "·") * 0.05) + 
                                                    (str_count(v_signal, "-") * 0.15) ,
                                                  t_s_length = (str_count(v_signal, "·") * 0.05) + 
                                                    (str_count(v_signal, "-") * 0.15) +  
                                                    ((nchar(trimws(v_signal)) - 1 ) * 0.05), 
                                                  dots_s_length = (str_count(v_signal, "·") * 0.05),
                                                  lines_s_length = (str_count(v_signal, "-") * 0.15) 
                                                  ) %>% print()



