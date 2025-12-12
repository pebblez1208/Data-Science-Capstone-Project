library(data.table)

nGramTable <- fread('predictionTable.csv')

###Predictive Model###
next_pred <- function(rawStr) {
    ## [A] Simplify input if necessary
    simplStr <- gsub('[[:punct:]]|[[:digit:]]', "", tolower(rawStr))
    # strsplit by all white spaces
    simplStr <- unlist(strsplit(simplStr, "\\s+"))
    
    ## [B] Extract last 6 words for query
    if (length(simplStr) > 6) {
        simplStr <- simplStr[(length(simplStr)-5):length(simplStr)] #make query length 6
        filtStr <- paste(simplStr, collapse = " ") #combine back to sentence
    } else {
        filtStr <- paste(simplStr, collapse = " ") #combine back to sentence
    }
    
    ## [C] Predicts the most likely word
    predText <- nGramTable[match(filtStr, nGramTable$query), ]$predict
    if (is.na(predText) == F) {
        #hit with 7 gram
        finalText <- predText
    } else {
        #no hits
        filtStr <- paste(simplStr[2:length(simplStr)], collapse = " ") #remove 1st word
        predText <- nGramTable[match(filtStr, nGramTable$query), ]$predict
        if (is.na(predText) == F) {
            #hit with 6 gram
            finalText <- predText
        } else {
            #no hits
            filtStr <- paste(simplStr[3:length(simplStr)], collapse = " ") #remove 2nd word
            predText <- nGramTable[match(filtStr, nGramTable$query), ]$predict
            if (is.na(predText) == F) {
                #hit with 5 gram
                finalText <- predText
            } else {
                #no hits
                filtStr <- paste(simplStr[4:length(simplStr)], collapse = " ") #remove 3rd word
                predText <- nGramTable[match(filtStr, nGramTable$query), ]$predict
                if (is.na(predText) == F) {
                    #hit with 4 gram
                    finalText <- predText
                } else {
                    #no hits
                    filtStr <- paste(simplStr[5:length(simplStr)], collapse = " ") #remove 4th word
                    predText <- nGramTable[match(filtStr, nGramTable$query), ]$predict
                    if (is.na(predText) == F) {
                        #hit with 3 gram
                        finalText <- predText
                    } else {
                        #no hits
                        filtStr <- paste(simplStr[6:length(simplStr)], collapse = " ") #remove 5th word (one word left)
                        predText <- nGramTable[match(filtStr, nGramTable$query), ]$predict
                        if (is.na(predText) == F) {
                            #hit with 2 gram
                            finalText <- predText
                        } else {
                            #no hits
                            finalText <- 'cannot predict' #most common word
                        }
                    }
                }
            }
        }  
    }
    return(finalText)
}
