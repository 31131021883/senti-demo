# create dictionary from SentiWordnet
# swn.path = "./data/SentiWordNet_3.0.0_20130122.txt"
swn.load = function(swn.path, .progress='text'){
  require(plyr)
  require(stringr)
  require(hash)
  
  # load SentiWordnet
  # Example line:
  # POS ID PosS NegS SynsetTerm#rank Gloss
  # a	01171213	0.75	0	sound#4 good#17	in excellent physical condition; "good teeth"; ... etc
  # rank is based on the popularity of term/word used in that specific case
  # 6 variables separated by tabs
  
  swn.df <- read.table(swn.path, 
                       fill=TRUE, header=TRUE, quote="", sep="\t", encoding="UTF-8", comment.char='@', dec = ".",
                       colClasses=c(POS = "character", 
                                    ID = "character",
                                    PosScore = "double",
                                    NegScore = "double",
                                    SynsetTerms = "character",
                                    Gloss = "character"))
  
  # create dictionary by traveling through all sysnset of SentiWordnet. POS (verb, noun, adj...) is ignored
  # to create swn.tmp - a temporary dictionary with keys are terms/words, values are df(id, rank, score)
  # we use hash - from hash package
  swn.tmp = hash()
  
  # to loop through data frame swn.df,
  # we use d_ply: Split data frame, apply function, and discard results - from plyr package
  d_ply(swn.df, .(ID, PosScore, NegScore, SynsetTerms), function(synset) {
    
    # for each data row:
    id = synset["ID"]
    score = synset["PosScore"] - synset["NegScore"]
    
    # to split SynsetTerms#rank into list of term#rank seperated by spaces,
    # we use str_split - from stringr package
    terms = unlist(str_split(synset["SynsetTerms"], '\\s+'))
    
    # to loop through terms - a list/vector,
    # we use l_ply: Split list/vector, apply function, and discard results - from plyr package
    terms = l_ply(terms, function(term, score, id){
      term.split = unlist(str_split(term, '#'))
      word = term.split[1]
      rank = term.split[2]
      
      if (word!=""){
        if (!has.key(word, swn.tmp)){
          # add new word to swn.tmp
          swn.tmp[[word]] = data.frame(id = id, rank=rank, score=score)
        }else{
          # add more information to existing word in swn.tmp
          swn.tmp[[word]] = rbind(swn.tmp[[word]], data.frame(id, rank, score))
        }
      }
    }, score, id)
    
  }, .progress = .progress)
  
  # create vector of unique words from swn.tmp
  words = names.hash(swn.tmp)
  # create final dictionary with keys are terms/words, values are average sentiment score
  swn.dict = hash()
  l_ply(words, function(word){
    if (word!=""){
      if (has.key(word, swn.tmp)&&!has.key(word,swn.dict)){
        w = swn.tmp[[word]]
        
        # Calculate weighted average. Weigh the synsets according to their rank.
        # sum(score)= 1/2*first + 1/3*second + 1/4*third ..... etc.
        # sum(rank) = 1/1 + 1/2 + 1/3 ...
        
        df.score = data.frame(score = as.numeric(w$PosScore)/as.numeric(w$rank))
        df.rank = data.frame(rank = 1/as.numeric(w$rank))
        word.score = sum(df.score$score)/sum(df.rank$rank)
        swn.dict[[word]] = word.score
      }else{
        print(paste("WARNING: word missing or duplicating:",word,sep = " "))
      }
    }
  }, .progress = .progress)
  
  print("Dictionary from SentiWordnet created.")
  print("To look up sentiment score of word: swn.dict$word, or swn.dict[[word]]")
  return(swn.dict)
}