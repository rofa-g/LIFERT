## NA and zero vectors ##

# Lengths of vectors in months corresponding to years in object name

len_40y <- 481
len_35y <- 421
len_34y <- 409
len_33y <- 397
len_32y <- 385
len_31y <- 373
len_30y <- 361
len_29y <- 349
len_28y <- 337
len_27y <- 325
len_26y <- 313
len_25y <- 301
len_21y <- 253
len_20y <- 241
len_15y <- 181
len_10y <- 121
len_8y <- 97
len_5y <- 61
len_3y <- 37



# Number of NA's to add to vectors to fill out columns

NA_35y <- len_40y - len_35y
NA_34y <- len_40y - len_34y
NA_33y <- len_40y - len_33y
NA_32y <- len_40y - len_32y
NA_31y <- len_40y - len_31y
NA_30y <- len_40y - len_30y
NA_29y <- len_40y - len_29y
NA_28y <- len_40y - len_28y
NA_27y <- len_40y - len_27y
NA_26y <- len_40y - len_26y
NA_25y <- len_40y - len_25y
NA_21y <- len_40y - len_21y
NA_20y <- len_40y - len_20y
NA_15y <- len_40y - len_15y
NA_10y <- len_40y - len_10y
NA_8y  <- len_40y - len_8y
NA_5y  <- len_40y - len_5y
NA_3y  <- len_40y - len_3y
NA_20m <- len_40y - 20

# Zero vector used in while loops

zero_vector <- rep(0, length.out = len_40y)
