# wordKernel

In collaboration with Professor Chad Hazlett and Aaron Rudkin.

An approach to word vectorization that represents words by their weighted proximity to other words in corpus.

Generates word embeddings from a corpus of N unique words through outputting an NxN kernel matrix where each ith row and column correspond to a word vector.  The matrix is symmetric and has diagonals of value 1.

The matrix is constructed through iterating through all documents in corpus, where for each document:

1) A one-hot vector is constructed for each unique word, which is then convolved with a smoothing gaussian function.
2) For each unique word-pair (including identical words) present in document, dot product of convolved one-hot vectors is found.  If word-pair is not present, a value of 0 is assigned.

Finally, for each unique word-pair, the dot product values across all in corpus documents are added up.  The entire matrix is then divided by the square-root of the diagonals to achieve diagonals of value 1. 

| First Header  | Second Header |
| ------------- | ------------- |
| Content Cell  | Content Cell  |
| Content Cell  | Content Cell  |




