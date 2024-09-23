library(bnlearn)

# Load data
data("asia")

# Calculate cutoff point
cutoff <- round(nrow(asia) * 0.80)

# Split
training_data <- asia[1:cutoff, ]
test_data <- asia[(cutoff + 1):nrow(asia), ]

# Learn BN using hill-climbing, will be a DAG
dag <- hc(training_data)

# True DAG
TrueDag = model2network("[A][S][T|A][L|S][B|S][D|B:E][E|T:L][X|E]")

# Plot
plot(dag)

# Learn parameters
fitted_bn <- bn.fit(dag, training_data)
fitted_true <- bn.fit(TrueDag, training_data)

# Used variables in confusion matrix
true_pos <- 0
false_pos <- 0
true_neg <- 0
false_neg <- 0

# Variables for confusion matrix of true graph
true_pos_true <- 0
false_pos_true <- 0
true_neg_true <- 0
false_neg_true <- 0

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
  temp_cond_calc <- cpquery(fitted_bn, event = (S == "yes"), 
            evidence = (A == A_val & T == T_val & L == L_val & 
            B == B_val & E == E_val & X == X_val & D == D_val))
  
  # Calculate conditional probability for true structure
  temp_cond_true <- cpquery(fitted_true, event = (S == "yes"), 
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
  
  # Confusion matrix of true graph
  if (temp_cond_true >= 0.5){
    if (test_data[row, "S"] == "yes"){
      # Is a true positive
      true_pos_true <- true_pos_true + 1
    }
    else{
      false_pos_true <- false_pos_true + 1
    }
  }
  else{
    if (test_data[row, "S"] == "yes"){
      # False negative
      false_neg_true <- false_neg_true + 1
    }
    else{
      true_neg_true <- true_neg_true + 1
    }
  }
}

cat("TP: ", true_pos, "\n")
cat("FP: ", false_pos, "\n")
cat("FN: ", false_neg, "\n")
cat("TN: ", true_neg, "\n")

print("For the true graph: ")
cat("TP: ", true_pos_true, "\n")
cat("FP: ", false_pos_true, "\n")
cat("FN: ", false_neg_true, "\n")
cat("TN: ", true_neg_true, "\n")