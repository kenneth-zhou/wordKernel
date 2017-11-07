# wordKernel

## Abstract

In collaboration with Professor Chad Hazlett and Aaron Rudkin.

An approach to word vectorization that represents words by their weighted proximity to other words in corpus.

Generates word embeddings from a corpus of N unique words through outputting an NxN kernel matrix where each ith row and column correspond to a word vector.  The matrix is symmetric and has diagonals of value 1.

The matrix is constructed through iterating through all documents in corpus, where for each document:

1) A one-hot vector is constructed for each unique word, which is then convolved with a smoothing gaussian function.
2) For each unique word-pair (including identical words) present in document, dot product of convolved one-hot vectors is found.  If word-pair is not present, a value of 0 is assigned.

Finally, for each unique word-pair, the dot product values across all in corpus documents are added up.  The entire matrix is then divided by the square-root of the diagonals to achieve diagonals of value 1. 

## Results

### IMDb movie reviews

#### Word embeddings

#### Sentiment Classification with Regularized Logistic Regression

Generated paragraph embeddings through multiple methods, referencing: https://arxiv.org/pdf/1607.00570.pdf

*(A quick note on L1 vs L2 regularization for logistic regression: L2, with squaring, punishes large values more.  Thus we have L2 optimizing for small coefficients across the board, while L1 tends to zero out some coefficients, helping perform feature selection for a sparser model.  Because L2 often does better than L1 in practice, I'll be testing with L2.)*

<table style="width:100%">
  <tr>
    <th></th>
    <th>wordKernel</th>
    <th>word2vec</th>
    <th>tf-idf</th>
  </tr>
  <tr>
    <td>Mean</td>
    <td align = "center">0.9167</td>
    <td align = "center">0.9041</td>
    <td rowspan = "9" align = "center">0.8597</td>
  </tr>
    <tr>
    <td>Max</td>
    <td align = "center">0.9196</td>
    <td align = "center">0.8498</td>
  </tr>
    <tr>
    <td>Min</td>
    <td align = "center">0.8943</td>
    <td align = "center">0.8541</td>
  </tr>
    <tr>
    <td>Max/Min</td>
    <td align = "center">0.9196</td>
    <td align = "center">0.8621</td>
  </tr>
    <tr>
    <td>Mean, Top 30% Idf</td>
    <td align = "center">0.8198</td>
    <td align = "center">0.7885</td>
  </tr>
    <tr>
    <td>Max, Top 30% Idf</td>
    <td align = "center">0.8208</td>
    <td align = "center">0.7728</td>
  </tr>
    <tr>
    <td>Min, Top 30% Idf</td>
    <td align = "center">0.7597</td>
    <td align = "center">0.7765</td>
  </tr>
    <tr>
    <td>Max/Min, Top 30% Idf</td>
    <td align = "center">0.8211</td>
    <td align = "center">0.7724</td>
  </tr>
    <tr>
    <td>Mean, Idf-weighted</td>
    <td align = "center">0.9139</td>
    <td align = "center">0.8893</td>
</table>
