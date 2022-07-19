cd wikipedia2vec
# python3 -m pip install pandas as pd
import pandas as pd
# this is how you pretrain w2v

# word_embed= pd.read_csv('/Users/rebeccawilder/Desktop/Desktop - Rebecca’s MacBook Air/Research 2021-2022/GitHub/Wilder-Research-ZemlaLab/fluencytask_rebeccascopy/contiguityfor_python.csv')
# check out for how to locate python dir
# similarity between i to i+1

#### TO DO ####


from wikipedia2vec import Wikipedia2Vec
from scipy import spatial
wiki2vec= Wikipedia2Vec.load("enwiki_20180420_500d.pkl")

def similarity(a, b, vec):
    return 1-spatial.distance.cosine(vec.get_word_vector(a), vec.get_word_vector(b))

def similarity_vec(a, b):
    return 1-spatial.distance.cosine(a, b)
  

  
  # cosine having scale invariance is standardized, it's the normative distance for semantic spaces 
