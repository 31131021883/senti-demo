# check verb forms: -es, -s, -ing, -ed
# check adj form (Comparatives & Superlatives): -er, -est
check.wordForm.huliu = function(word, huliu.dict)
{
  require(hash)
  require(stringr)
  
  score = 0.0
  if (nchar(word)>3){
    if (endsWith(word,"es")){ # -es (goes => go)
      w = substr(word,1,nchar(word)-2)
      score = !is.na(match(w, huliu.dict$pos)) - !is.na(match(w, huliu.dict$neg))
      if (is.null(score)||score==0){ # -s (creates => create)
        w = substr(word,1,nchar(word)-1)
        score = !is.na(match(w, huliu.dict$pos)) - !is.na(match(w, huliu.dict$neg))
      }
    }else if (endsWith(word,"s")){ # -s (wants => want)
      w = substr(word,1,nchar(word)-1)
      score = !is.na(match(w, huliu.dict$pos)) - !is.na(match(w, huliu.dict$neg))
    }else if (endsWith(word,"ied")){ # -ied (studied => study)
      w = paste0(substr(word,1,nchar(word)-3),"y")
      score = !is.na(match(w, huliu.dict$pos)) - !is.na(match(w, huliu.dict$neg))
    }else if (endsWith(word,"ed")){ # -ed (worked => work)
      w = substr(word,1,nchar(word)-2)
      score = !is.na(match(w, huliu.dict$pos)) - !is.na(match(w, huliu.dict$neg))
      if (is.null(score)||score==0){ # -ed (created => create)
        w = substr(word,1,nchar(word)-1)
        score = !is.na(match(w, huliu.dict$pos)) - !is.na(match(w, huliu.dict$neg))
        if (is.null(score)||score==0){ # -ed (stopped => stop)
          if (nchar(word)>5) { 
            if (substr(word,nchar(word)-2,nchar(word)-2)==substr(word,nchar(word)-3,nchar(word)-3)){
              w = substr(word,1,nchar(word)-3)
              score = !is.na(match(w, huliu.dict$pos)) - !is.na(match(w, huliu.dict$neg))
            }
          }
        }
      }
    }else if (endsWith(word,"er")){ # -er (shorter => short)
      w = substr(word,1,nchar(word)-2)
      score = !is.na(match(w, huliu.dict$pos)) - !is.na(match(w, huliu.dict$neg))
      if (is.null(score)||score==0){ # -er (safer => safe)
        w = substr(word,1,nchar(word)-1)
        score = !is.na(match(w, huliu.dict$pos)) - !is.na(match(w, huliu.dict$neg))
        if (is.null(score)||score==0){
          if (endsWith(word,"ier")){ # -er (luckier => lucky)
            w = paste0(substr(word,1,nchar(word)-3),"y")
            score = !is.na(match(w, huliu.dict$pos)) - !is.na(match(w, huliu.dict$neg))
          }else if (nchar(word)>4) { # -er (bigger => big)
            if (substr(word,nchar(word)-2,nchar(word)-2)==substr(word,nchar(word)-3,nchar(word)-3)){
              w = substr(word,1,nchar(word)-3)
              score = !is.na(match(w, huliu.dict$pos)) - !is.na(match(w, huliu.dict$neg))
            }
          }
        }
      }
    }else if (endsWith(word,"est")){ # -est (shortest => short)
      w = substr(word,1,nchar(word)-3)
      score = !is.na(match(w, huliu.dict$pos)) - !is.na(match(w, huliu.dict$neg))
      if (is.null(score)||score==0){ # -est (simplest => simple)
        w = substr(word,1,nchar(word)-2)
        score = !is.na(match(w, huliu.dict$pos)) - !is.na(match(w, huliu.dict$neg))
        if (is.null(score)||score==0){
          if (endsWith(word,"iest")){ # -est (luckiest => lucky)
            w = paste0(substr(word,1,nchar(word)-4),"y")
            score = !is.na(match(w, huliu.dict$pos)) - !is.na(match(w, huliu.dict$neg))
          }else if (nchar(word)>5) { # -er (biggest => big)
            if (substr(word,nchar(word)-4,nchar(word)-4)==substr(word,nchar(word)-3,nchar(word)-3)){
              w = substr(word,1,nchar(word)-4)
              score = !is.na(match(w, huliu.dict$pos)) - !is.na(match(w, huliu.dict$neg))
            }
          }
        }
      }
    }else if (nchar(word)>4){
      if (endsWith(word,"ing")){ # -ing (working => work)
        w = substr(word,1,nchar(word)-3)
        score = !is.na(match(w, huliu.dict$pos)) - !is.na(match(w, huliu.dict$neg))
        if (is.null(score)||score==0){ # -ing (creating => create)
          w = paste0(substr(word,1,nchar(word)-3),"e")
          score = !is.na(match(w, huliu.dict$pos)) - !is.na(match(w, huliu.dict$neg))
          if (is.null(score)||score==0){
            if (endsWith(word,"ying")){ # -ing (dying => die)
              w = paste0(substr(word,1,nchar(word)-4),"ie")
              score = !is.na(match(w, huliu.dict$pos)) - !is.na(match(w, huliu.dict$neg))
            }else if (nchar(word)>5) { # -ing (running => run)
              if (substr(word,nchar(word)-4,nchar(word)-4)==substr(word,nchar(word)-3,nchar(word)-3)){
                w = substr(word,1,nchar(word)-4)
                score = !is.na(match(w, huliu.dict$pos)) - !is.na(match(w, huliu.dict$neg))
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