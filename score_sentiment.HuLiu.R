score_sentiment.HuLiu = function(sentences, huliu.dict, .progress='none')
{
  require(plyr)
  require(stringr)
  # we got a vector of sentences. plyr will handle a list or a vector as an "l" for us
  # we want a simple array of scores back, so we use "l" + "a" + "ply" = laply:
  scores = laply(sentences, function(sentence, huliu.dict) {
    
    # negation handling
    #   not
    sentence = gsub('not ', 'not', sentence, ignore.case = TRUE)
    sentence = gsub("n\\'t ", ' not', sentence, ignore.case = TRUE)
    sentence = gsub("dont ", 'do not', sentence, ignore.case = TRUE)
    
    #  neither ... nor ...
    sentence = gsub('neither ', 'not', sentence, ignore.case = TRUE)
    sentence = gsub(' nor ', ' not', sentence, ignore.case = TRUE)
    
    #  no sense of ...
    sentence = gsub(' no sense of ', ' not', sentence, ignore.case = TRUE)
    sentence = gsub(' no ', ' not', sentence, ignore.case = TRUE)
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', ' ', sentence)
    sentence = gsub('[[:cntrl:]]', ' ', sentence)
    sentence = gsub('\\d+', '', sentence)
    
    # and convert to lower case:
    sentence = tolower(sentence)
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, huliu.dict$pos)
    neg.matches = match(words, huliu.dict$neg)
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    if (sum(pos.matches) + sum(neg.matches)!=0){
      score = (sum(pos.matches) - sum(neg.matches))/(sum(pos.matches) + sum(neg.matches))
    }else{
      score = 0
    }
    
    return(score)
  }, huliu.dict, .progress=.progress )
  scores.df = data.frame(score.HuLiu=scores, text=sentences)
  return(scores.df)
}