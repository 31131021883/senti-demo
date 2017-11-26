source('demo.HuLiu.R')
#source('demo.swn.R')
#source('demo2.swn.R')
source('draft.swn.R')

demo.combine = function(path, huliu.dict, swn.dict, weight){
  require(stringr)
  samples.data <- read.table(path, 
                             fill=TRUE, header=FALSE, quote="", sep="\t", encoding="UTF-8", 
                             col.names = c("sentences","score"))
  samples.sentences = samples.data$sentences
  
  # using HuLiu lexicon
  samples.HuLiu = demo.HuLiu(samples.sentences, huliu.dict)
  
  # using SentiWordnet
  samples.swn = demo.swn(samples.sentences, swn.dict)
  
  # using SentiWordnet as bags of pos and neg words
  samples.swn2 = demo2.swn(samples.sentences, swn.dict)
  
  # calculating average score
  average.score = (samples.HuLiu$score.HuLiu*weight[1]+samples.swn$score.swn*weight[2]+samples.swn2$score.swn*weight[3])/sum(weight)
  final.df = data.frame(samples.HuLiu$score.HuLiu,samples.swn$score.swn,samples.swn2$score.swn, average.score, samples.data$score, samples.sentences)
  
  final.huliu = (samples.HuLiu$score.HuLiu>=0)
  result.huliu = (final.huliu==samples.data$score)
  print(paste(sum(result.huliu)/length(result.huliu), "huliu"))
  
  final.swn = (samples.swn$score.swn>=0)
  result.swn = (final.swn==samples.data$score)
  print(paste(sum(result.swn)/length(result.swn), "swn"))
  
  final.swn2 = (samples.swn2$score.swn>=0)
  result.swn2 = (samples.swn2$score.swn==samples.data$score)
  print(paste(sum(result.swn2)/length(result.swn2), "swn2"))
  
  average.score2 = (samples.HuLiu$score.HuLiu*weight[1] + samples.swn$score.swn*weight[2])/(weight[1]+weight[2])
  final.score2 = (average.score2>=0)
  result.score2 = (average.score2==samples.data$score)
  print(paste(sum(result.score2)/length(result.score2), "huliu + swn"))
  
  final.score = (average.score>=0)
  result = (final.score==samples.data$score)
  print(paste(sum(result)/length(result), "huliu + swn + swn2"))
  
  return(final.df)
}