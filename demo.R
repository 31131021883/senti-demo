source('demo.combine.R')
source('huliu.load.R')
source('swn.load.R')

linh.path = './data/linh.txt'
swn.path = "./data/SentiWordNet_3.0.0_20130122.txt"
amazon.path = './data/sentiment labelled sentences/amazon_cells_labelled.txt'
imdb.path = './data/sentiment labelled sentences/imdb_labelled.txt'
yelp.path = './data/sentiment labelled sentences/yelp_labelled.txt'
pos.path = './data/HuLiu-lexicon/positive-words.txt'
neg.path = './data/HuLiu-lexicon/negative-words.txt'

# load huliu lexicon
# huliu.dict = huliu.load(pos.path, neg.path)
# save(huliu.dict, file='./data/huliu.dict')
load(file='./data/huliu.dict')

# load SentiWordnet
# swn.dict = swn.load(swn.path, .progress='text')
# save(swn.dict, file='./data/swn.dict')
load(file='./data/swn.dict')

weight1 = c(alpha=1/3,beta=1/3,gamma=1/3)
weight = c(alpha=0.4,beta=0.4,gamma=0.2)
weight3 = c(alpha=0.4,beta=0.5,gamma=0.1)
 
linh = demo.combine(linh.path, huliu.dict, swn.dict, weight)   
amazon = demo.combine(amazon.path, huliu.dict, swn.dict, weight)
imdb = demo.combine(imdb.path, huliu.dict, swn.dict, weight) 
yelp = demo.combine(yelp.path, huliu.dict, swn.dict, weight)  
