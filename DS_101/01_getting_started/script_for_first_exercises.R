probabilities_vector <- c(0.1, 0.1, 0.1, 0.1, 0.1, 0.5)
equalProbVector <- c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6)

rollDice <- function(D = 2, probs = equalProbVector, faces = 1:6){
  sum(sample(faces,D,replace = TRUE, prob = probs))
  
}

rollDice()


results <- replicate(n=1000, expr = rollDice(, probabilities_vector, ), simplify = TRUE)
hist(results)