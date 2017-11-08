# wordKernel

## Abstract

In collaboration with Professor Chad Hazlett and Aaron Rudkin.

An approach to word vectorization that represents words by their weighted proximity to other words in corpus.

Generates word embeddings from a corpus of N unique words through outputting an NxN kernel matrix where each ith row and column correspond to a word vector.  The matrix is symmetric and has diagonals of value 1.

The matrix is constructed through iterating through all documents in corpus, where for each document:

1) A one-hot vector is constructed for each unique word, which is then convolved with a smoothing gaussian function.
2) For each unique word-pair (including identical words) present in document, dot product of convolved one-hot vectors is found.  If word-pair is not present, a value of 0 is assigned.

Finally, for each unique word-pair, the dot product values across all corpus documents are added up.  The entire matrix is then divided by the square-root of the diagonals to achieve diagonals of value 1. 

## Results

### IMDb movie reviews

#### Word embeddings

Pre-trained on 4000 IMDb reviews.  Based on cossine similarity, top 10 most similar words:

<table style="width:100%">
  <tr>
    <th></th>
    <th>wordKernel</th>
    <th>word2vec</th>
  </tr>
  <tr>
    <td>Movie</td>
    <td align = "center">film, bad, watch, time, don, story, seen, movies, people, watching </td>
    <td align = "center">movies, popcorn, segal, lungren, rgv, anytime, fingernails, commented, cringed, rainy</td>
  </tr>
    <tr>
    <td>Girl</td>
    <td align = "center">little, boy, meets, movie, loses, love, film, grabs, priya, guy</td>
    <td align = "center">paulie, madly, meets, marie, boy, withdrawn, pauline, array, longed, salesman</td>
  </tr>
    <tr>
    <td>Positive</td>
    <td align = "center">reviews, note, aspects, comments, message, film, movie, mysteriously, stunned, virtual</td>
    <td align = "center">message, revolting, reviews, comments, inaccuracies, critic, critique, glaring, criticised, hopefully</td>
  </tr>
    <tr>
    <td>Negative</td>
    <td align = "center">comments, scores, stereotype, reviews, imdb, scale, influences, rating, iq, comment</td>
    <td align = "center">coverage, imdb, inaccuracies, criticised, criticism, criticizing, proud, critique, reviews, user</td>
  </tr>
    <tr>
    <td>Bad</td>
    <td align = "center">movie, acting, film, guys, movies, guy, isn, plot, pretty, people</td>
    <td align = "center">segal, criminally, horrible, tiresome, worst, snowman, terrible, awful, laughable, lousy</td>
  </tr>
</table>

#### Sentiment Classification with Regularized Logistic Regression

Generated paragraph embeddings through multiple methods, referencing: https://arxiv.org/pdf/1607.00570.pdf.  wordKernel, word2vec and tf-idf word embeddings are pre-trained on 4000 IMDb reviews.  L2 (ridge) logistic regression models are then trained on paragraph embeddings of the same 4000 reviews, before being tested on 1000 reviews.  AUC values are shown below.

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

### 3 Little Pigs

#### Word embeddings

Pre-trained on 3 Little Pigs, ~1000 total words, ~50 unique words.  Below are top 3 most similar words:

<table style="width:100%">
  <tr>
    <th></th>
    <th>wordKernel</th>
    <th>word2vec</th>
  </tr>
  <tr>
    <td>Wolf</td>
    <td align = "center">little, furiously, hammering</td>
    <td align = "center">up, went, of</td>
  </tr>
    <tr>
    <td>Pig</td>
    <td align = "center">little, wisest, house</td>
    <td align = "center">but, brothers, house</td>
  </tr>
    <tr>
    <td>Little</td>
    <td align = "center">pig, pigs, wisest</td>
    <td align = "center">house, said, from</td>
  </tr>
    <tr>
    <td>House</td>
    <td align = "center">wooden, takes, pig</td>
    <td align = "center">little, like, that</td>
  </tr>
</table>

### A Christmas Carol

#### Word embeddings

Pre-trained on A Christmas Carol, 10515 total words, 3906 unique words.  Below are top 10 most similar words:

<table style="width:100%">
  <tr>
    <th></th>
    <th>wordKernel</th>
    <th>word2vec</th>
  </tr>
  <tr>
    <td>Death</td>
    <td align = "center">caused, condemned, doomed, emotion, vegetation, wander, agonised, profound, bearing, feels</td>
    <td align = "center">spirit's, knocker, darkness, body, shadow, chuckle, within, part, taken, weather</td>
  </tr>
    <tr>
    <td>Scrooge</td>
    <td align = "center">ghost, spirit, nephew, cried, christmas, niece, don, marley, uncle, observed</td>
    <td align = "center">spirit, lead, oh, marley, trembling, spectre, robe, wear, ghost, humbug</td>
  </tr>
    <tr>
    <td>Christmas</td>
    <td align = "center">merry, tune, eve, day, scrooge, companion, fools, bygone hummed, ll</td>
    <td align = "center">year, merry, new, day, god, everybody, happy, good, uncle, laugh</td>
  </tr>
    <tr>
    <td>Cold</td>
    <td align = "center">stirring, piping, biting, roast, nipped, piece, froze, negus, dunstan, oyster</td>
    <td align = "center">within, bright, hung, room, fog, bed, chair, darkness, glorious, warm</td>
  </tr>
</table>
