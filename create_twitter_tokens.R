# create twitter tokens

require(httr)

#options(httr_oauth_cache = FALSE)

myapp1 <- oauth_app(appname = "my_dissertation",
                    key = "zbIzYseIu7qP76mIJ1xKRFuAi",
                    secret = "tXt7Htd1V5BzKCPdTfnMt2gM4ouo0mYCq2lz5537gIob1Nlrst")
twitter_token1 <- oauth1.0_token(oauth_endpoints("twitter"), myapp1)
saveRDS(twitter_token1, file = "tokens/twitter_token1")

myapp2 <- oauth_app(appname = "quantnerd",
                    key = "ITvIOtCEJ99Wp2T5eNGMa3gX1",
                    secret = "hHnWih5xGAc6SnJqxdBQa4c4u9Be7plHOoZqvZ7obKcvBZgONh")
twitter_token2 <- oauth1.0_token(oauth_endpoints("twitter"), myapp2)
saveRDS(twitter_token2, file = "tokens/twitter_token2")

myapp3 <- oauth_app(appname = "mwk",
                    key = "9vXa03YA09CAm5DBVmem2CMiW",
                    secret = "x8djCoqhtDOnk0NROixuxpbrheXiQ0ykZ5nz1FTVxCutiNjo2q")
twitter_token3 <- oauth1.0_token(oauth_endpoints("twitter"), myapp3)
saveRDS(twitter_token3, file = "tokens/twitter_token3")

myapp4 <- oauth_app(appname = "quant[n]er[d]",
                    key = "biatmYRqBmzsEDXIxIanQvzFP",
                    secret = "XnjCILTviEmiFDpdKwGEvIlD8fEkaBcSLU5JZY8cOq18vcdRP9")
twitter_token4 <- oauth1.0_token(oauth_endpoints("twitter"), myapp4)
saveRDS(twitter_token4, file = "tokens/twitter_token4")

myapp5 <- oauth_app(appname = "my_disseration_project",
                    key = "9aGPIVi6ZphO8Mkvnv9ai6P43",
                    secret = "EYs39ywsVqPQQPRRZnPklsqqQogHsvPayNZTPip85uJVAfnNa3")
twitter_token5 <- oauth1.0_token(oauth_endpoints("twitter"), myapp5)
saveRDS(twitter_token5, file = "tokens/twitter_token5")

myapp6 <- oauth_app(appname = "mwk_python_API",
                    key = "3T4IL7rzgHXyphvcjQQYTZRcQ",
                    secret = "vLNrKe7uhCMStZ8SMCto6aTKMWHBq8ifJSHAICJdlTFoPnCKuZ")
twitter_token6 <- oauth1.0_token(oauth_endpoints("twitter"), myapp6)
saveRDS(twitter_token6, file = "tokens/twitter_token6")

myapp7 <- oauth_app(appname = "selective_exposure",
                    key = "uUus88Hb1RypQ4roGKJXNDnfW",
                    secret = "Y7XlTqouGNvej7EHV8GcMWcnRlNNXM1RwM81g4C5lEtAysBIKE")
twitter_token7 <- oauth1.0_token(oauth_endpoints("twitter"), myapp7)
saveRDS(twitter_token7, file = "tokens/twitter_token7")

myapp8 <- oauth_app(appname = "political_polarization",
                    key = "6mNkRk7MDlyMutaH3jX1mANoS",
                    secret = "E5xba5yD3tpkTDqkvui8QEeiMSYnEFca2Dzk5KQaT4diYCVZoT")
twitter_token8 <- oauth1.0_token(oauth_endpoints("twitter"), myapp8)
saveRDS(twitter_token8, file = "tokens/twitter_token8")


myapp9 <- oauth_app(appname = "nonpartisans_tune_out",
                    key = "xxsiQ7YVI7l9gImSQO0nqn6qN",
                    secret = "zLkHnzgL4WvHJvhETRsZTxf9kZ92cOIMFqR6ggTBl9koIXmvns")
twitter_token9 <- oauth1.0_token(oauth_endpoints("twitter"), myapp9)
saveRDS(twitter_token9, file = "tokens/twitter_token9")

tokens <- c(twitter_token1, twitter_token2, twitter_token3,
            twitter_token4, twitter_token5, twitter_token6,
            twitter_token7, twitter_token8, twitter_token9)

rm(myapp1, myapp2, myapp3,
   myapp4, myapp5, myapp6,
   myapp7, myapp8, myapp9,
   twitter_token1, twitter_token2, twitter_token3,
   twitter_token4, twitter_token5, twitter_token6,
   twitter_token7, twitter_token8, twitter_token9)

