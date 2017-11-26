huliu.load = function(pos.path, neg.path){
  require("plyr")
  # Download Hu & Liu’s opinion lexicon: http://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html
  # loading lexicon
  pos.path = './data/HuLiu-lexicon/positive-words.txt'
  neg.path = './data/HuLiu-lexicon/negative-words.txt'
  hu.liu.pos = scan(pos.path,
                    what='character', comment.char=';')
  hu.liu.neg = scan(neg.path,
                    what='character', comment.char=';')
  
  # Add a few industry-specific and/or especially emphatic terms
  pos.words = c(hu.liu.pos, 'upgrade', '10')
  neg.words = c(hu.liu.neg, 'wtf', 'wait', 'waiting', 'epicfail', 'mechanical')
  
  # add "not..." to dictionary
  pos.words = c(pos.words, llply(neg.words, function(word) {
    return(paste("not",word,sep=""))}))
  neg.words = c(neg.words, llply(pos.words, function(word) {
    return(paste("not",word,sep=""))}))
  
  huliu.dict = list(pos=pos.words,neg=neg.words)
  return(huliu.dict)
}