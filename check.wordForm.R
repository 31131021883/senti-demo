# check verb forms: -es, -s, -ing, -ed
# check adj form (Comparatives & Superlatives): -er, -est
check.wordForm = function(word, dict)
{
  require(hash)
  require(stringr)
  
  score = 0.0
  if (nchar(word)>3){
    if (endsWith(word,"es")){ # -es (goes => go)
      score = dict[[substr(word,1,nchar(word)-2)]]
      if (is.null(score)||score==0){ # -s (creates => create)
        score = dict[[substr(word,1,nchar(word)-1)]]
      }
    }else if (endsWith(word,"s")){ # -s (wants => want)
      score = dict[[substr(word,1,nchar(word)-1)]]
    }else if (endsWith(word,"ied")){ # -ied (studied => study)
      score = dict[[paste0(substr(word,1,nchar(word)-3),"y")]]
    }else if (endsWith(word,"ed")){ # -ed (worked => work)
      score = dict[[substr(word,1,nchar(word)-2)]]
      if (is.null(score)||score==0){ # -ed (created => create)
        score = dict[[substr(word,1,nchar(word)-1)]]
        if (is.null(score)||score==0){ # -ed (stopped => stop)
          if (nchar(word)>5) { 
            if (substr(word,nchar(word)-2,nchar(word)-2)==substr(word,nchar(word)-3,nchar(word)-3)){
              score = dict[[substr(word,1,nchar(word)-3)]]
            }
          }
        }
      }
    }else if (endsWith(word,"er")){ # -er (shorter => short)
      score = dict[[substr(word,1,nchar(word)-2)]]
      if (is.null(score)||score==0){ # -er (safer => safe)
        score = dict[[substr(word,1,nchar(word)-1)]]
        if (is.null(score)||score==0){
          if (endsWith(word,"ier")){ # -er (luckier => lucky)
            score = dict[[paste0(substr(word,1,nchar(word)-3),"y")]]
          }else if (nchar(word)>4) { # -er (bigger => big)
            if (substr(word,nchar(word)-2,nchar(word)-2)==substr(word,nchar(word)-3,nchar(word)-3)){
              score = dict[[substr(word,1,nchar(word)-3)]]
            }
          }
        }
      }
    }else if (endsWith(word,"est")){ # -est (shortest => short)
      score = dict[[substr(word,1,nchar(word)-3)]]
      if (is.null(score)||score==0){ # -est (simplest => simple)
        score = dict[[substr(word,1,nchar(word)-2)]]
        if (is.null(score)||score==0){
          if (endsWith(word,"iest")){ # -est (luckiest => lucky)
            score = dict[[paste0(substr(word,1,nchar(word)-4),"y")]]
          }else if (nchar(word)>5) { # -er (biggest => big)
            if (substr(word,nchar(word)-4,nchar(word)-4)==substr(word,nchar(word)-3,nchar(word)-3)){
              score = dict[[substr(word,1,nchar(word)-4)]]
            }
          }
        }
      }
    }else if (nchar(word)>4){
      if (endsWith(word,"ing")){ # -ing (working => work)
        score = dict[[substr(word,1,nchar(word)-3)]]
        if (is.null(score)||score==0){ # -ing (creating => create)
          score = dict[[paste0(substr(word,1,nchar(word)-3),"e")]]
          if (is.null(score)||score==0){
            if (endsWith(word,"ying")){ # -ing (dying => die)
              score = dict[[paste0(substr(word,1,nchar(word)-4),"ie")]]
            }else if (nchar(word)>5) { # -ing (running => run)
              if (substr(word,nchar(word)-4,nchar(word)-4)==substr(word,nchar(word)-3,nchar(word)-3)){
                score = dict[[substr(word,1,nchar(word)-4)]]
              }
            }
          }
        }
      }
    }
  }
  
  if (is.null(score)){
    score = 0.0
  }
  return(score)
}