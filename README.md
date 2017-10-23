# wordKernel

## Overview:

In collaboration with Professor Chad Hazlett and Aaron Rudkin.

An approach to word vectorization that represents words by their weighted proximity to other words in corpus.

Generates word embeddings from a corpus of N unique words through outputting an NxN kernel matrix where each ith row and column correspond to a word vector.  The matrix is symmetric and has diagonals of value 1.

The matrix is constructed through iterating through all documents in corpus, where for each document:

1) A one-hot vector is constructed for each unique word, which is then convolved with a smoothing gaussian function.
2) For each unique word-pair (including identical words) present in document, dot product of convolved one-hot vectors is found.  If word-pair is not present, a value of 0 is assigned.

Finally, for each unique word-pair, the dot product values across all in corpus documents are added up.  The entire matrix is then divided by the square-root of the diagonals to achieve diagonals of value 1. 

*(A quick note on L1 vs L2 regularization: L2, with squaring, punishes large values more.  Thus we have L2 optimizing for small coefficients across the board, while L1 tends to zero out some coefficients, helping perform feature selection for a sparser model.  Because L2 often does better than L1 in practice, I'll be testing with L2.)*

## Results:

### wordKernel vs tf-idf vs word2vec for IMDb movie review sentiment prediction task:

| Aggregation | Test |
| ------------- | ------------- |
| Mean  | Content Cell  |
| Max  | Content Cell  |
| Min | |
| Max/Min | | 
| Mean, Top 30% Idf | | 
| Max, Top 30% Idf | | 
| Min, Top 30% Idf | | 
| Min/Max, Top 30% Idf | | 
| Mean, Idf | | 




