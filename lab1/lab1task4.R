library(bnlearn)

# Load data
data("asia")

# Calculate cutoff point
cutoff <- round(nrow(asia) * 0.80)

# Split
training_data <- asia[1:cutoff, ]
test_data <- asia[(cutoff + 1):nrow(asia), ]

# Create Bayesian Network
specific_letters <- c("A", "S", "T", "L", "B", "E", "X", "D")
empty_graph <- empty.graph(specific_letters)

# Model as from every node to S
arc.set = matrix(c("A", "S", "T", "S", "L", "S", "B", "S", "E", "S", "X", "S", "D", "S"),
                 ncol = 2, byrow = TRUE,
                 dimnames = list(NULL, c("to", "from")))
arcs(empty_graph) = arc.set

# Rename for clarity
naive_bayes_graph <- empty_graph # Since graph is no longer empty

# Learn parameters
naive_bayes_model <- bn.fit(naive_bayes_graph, training_data)

# Used variables in confusion matrix
true_pos <- 0
false_pos <- 0
true_neg <- 0
false_neg <- 0

# Calculate conditional probabilities
for (row in 1:nrow(test_data)){
  
  # Get the evidence
  A_val <- as.vector(test_data[row, "A"])
  T_val <- as.vector(test_data[row, "T"])
  L_val <- as.vector(test_data[row, "L"])
  B_val <- as.vector(test_data[row, "B"])
  E_val <- as.vector(test_data[row, "E"])
  X_val <- as.vector(test_data[row, "X"])
  D_val <- as.vector(test_data[row, "D"])
  
  # Calculate conditional probability for learned structure
  temp_cond_calc <- cpquery(naive_bayes_model, event = (S == "yes"), 
                            evidence = (A == A_val & T == T_val & L == L_val & 
                                          B == B_val & E == E_val & X == X_val & D == D_val))
  
  # Determine if it is true positive, false positive, 
  #                    true negative or false negative
  if (temp_cond_calc >= 0.5){
    if (test_data[row, "S"] == "yes"){
      # Is a true positive
      true_pos <- true_pos + 1
    }
    else{
      false_pos <- false_pos + 1
    }
  }
  else{
    if (test_data[row, "S"] == "yes"){
      # False negative
      false_neg <- false_neg + 1
    }
    else{
      true_neg <- true_neg + 1
    }
  }
}

cat("TP: ", true_pos, "\n")
cat("FP: ", false_pos, "\n")
cat("FN: ", false_neg, "\n")
cat("TN: ", true_neg, "\n")