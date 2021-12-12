text = c("budi makan nasi", "Rudi makan nasi goreng", "ayam bakar enak")
tokenizer = text_tokenizer(num_words = 3)
# keep the most frequent 3 words

tokenizer %>% 
  fit_text_tokenizer(text)

tokenizer$word_index
tokenizer$word_counts
tokenizer$index_word

# menjadikan tfidf
tokenizer %>% 
  texts_to_matrix(text, mode = "tfidf")

# create sequence
seq = tokenizer %>% 
  texts_to_sequences(text)
seq

# pad sequence
seq_pad = pad_sequences(seq, maxlen = 7, padding = "post") 
#post ada didepan
seq_pad


