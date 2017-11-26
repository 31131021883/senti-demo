source('check.wordForm.R')
demo2.swn = function(sentences, swn.dict, .progress='text')
{
  require(plyr)
  require(stringr)
  # we got a vector of sentences. plyr will handle a list or a vector as an "l" for us
  # we want a simple array of scores back, so we use "l" + "a" + "ply" = laply:
  scores = laply(sentences, function(sentence, swn.dict) {
    
    score = 0.0
    # negation handling
    #  not: already included in swn [00024073]
    #  neither ... : already included in swn
    #  no sense of ... : already included in swn
    #  n't => not
    sentence = gsub("n\\'t ", ' not ', sentence, ignore.case = TRUE)
    sentence = gsub("dont ", 'do not ', sentence, ignore.case = TRUE)
    
    ## attention: swn includes term of many words and special characters e.g: 
    ##            heavy-footed, right-hand, surgeon's_knot,
    ##            sense_of_humor, sense_of_shame, smart_as_a_whip, too_large, ... etc
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', ' ', sentence)
    sentence = gsub('[[:cntrl:]]', ' ', sentence)
    sentence = gsub('\\d+', '', sentence)
    
    # and convert to lower case:
    sentence = tolower(sentence)
    
    ## split into words. str_split is in the stringr package
    words = unlist(str_split(sentence, '\\s+'))
    
    score.list = laply(words, function(word, swn.dict){
      
      s = 0.0
      if(word!=""&&!is.null(word)){
        s = swn.dict[[word]]
        if (is.null(s)){
          s = check.wordForm(word, swn.dict)
        }
      }
      if (is.null(s)){
        s = 0.0 
      }
      
      if (s>0){
        s = 1
      }else if (s<0){
        s = -1
      }
      
      return(s)
    }, swn.dict)
    
    sum = sum((score.list!=0))
    if (sum!=0){
      score = sum(score.list)/sum
    }else{
      score = 0
    }
    
    return(score)
  }, swn.dict, .progress=.progress )
  scores.df = data.frame(score.swn=scores, text=sentences)
  return(scores.df)
}