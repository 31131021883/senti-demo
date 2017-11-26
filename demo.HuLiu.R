source('score_sentiment.HuLiu.R')
#source("draft.swn.R")
demo.HuLiu = function(sentences, huliu.dict)
{
  # score and return result
  return(score_sentiment.HuLiu(sentences, huliu.dict, .progress='text'))
}