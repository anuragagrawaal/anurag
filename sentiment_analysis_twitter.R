consumer_key <- 'LL9nOfKsdDePHYxg4RNEYtUBG'
consumer_secret_key <- 'bBFzUnvrxfPZ81fkHLJ3KmeZk10Lun3SIZJJ2ul9HlsOL7RAAc'
access_token <- '1014832302172573696-QpvruIkqjpAKGb1OY2QmYhNSeg5qrR'
access_secret_token <- 'QPKzbqi4FsifgPuYvG5HEl8qGdMy8LLsL69iCTmT4s7bH'
twitteR::setup_twitter_oauth(consumer_key = consumer_key, consumer_secret = consumer_secret_key, access_token = access_token, access_secret = access_secret_token)
td = twitteR::searchTwitter(searchString = '#realDonaldTrump')
tdf = twitteR::twListToDF(td)
td_list = data_frame(txt = list(tdf$text))
sample_sentiments = data_frame(txt = tdf$text)
u1 = unnest_tokens(tbl = sample_sentiments, output = sent, input = txt)
l1 = get_sentiments(lexicon = 'afinn')
i1 = inner_join(x = u1, y = l1, by = c('sent' = 'word'))

freq_1 <- i1 %>%
  count(sent, score, sort = TRUE)