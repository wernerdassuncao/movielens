
title: 'HarvardX - Data Science Professional Certificate - Capstone - Project MovieLens'
author: "Werner Alencar Advincula Dassuncao"

This project is inspired by the *Netflix Prize* - "an open competition for the best collaborative filtering algorithm to predict user ratings for films, based on previous ratings without any other information about the users or films" ['https://en.wikipedia.org/wiki/Netflix_Prize'].  The video-streaming service Netflix provided the competitors with a training data set of 100,480,507 ratings, provided by 480,189 users regarding 17,770 movies. The competition began on October 2, 2006 and aimed to improve Netflix's *Cinematch* own algorithm by 10%.  In summary, the *training* data set was used to train algorithms and the final model for the team's algorithm was used to make predictions on the *qualifying* data set. The quality of the predictions were scored against the true grades in terms of root mean squared error (RMSE).  

For the scope of this project, we will gather, explore, visualize, analyze and make predictions over the data from the *MovieLens* data set with 10,000,000 ratings provided by GroupLens, a research lab in the Department of Computer Science and Engineering at the University of Minnesota. 

Recommendations can be done using the users own past ratings, but also using a technique *collaborative filtering* can filter out movies that the user might like based on ratings from similar users.

These methods were compared during this project:

  1 - Overall rating average                  
  2 - Movie bias                              
  3 - Movie and User biases                   
  4 - Regularized Movie User biases           
  5 - RecommenderLab IBCF                     
  6 - RecommenderLab UBCF                     
  7 - RecommenderLab POPULAR                  
  8 - Recosystem Parallel Matrix Factorization