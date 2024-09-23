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

# Learn parameters
fitted_bn <- bn.fit(dag, training_data)

parents <- parents(fitted_bn, "S")
children <- children(fitted_bn, "S")

# Used variables in confusion matrix
true_pos <- 0
false_pos <- 0
true_neg <- 0
false_neg <- 0

# Calculate conditional probabilities
for (row in 1:nrow(test_data)){
  
  # Get evidence
  nodes = c()
  node_values = c()
  
  # Add evidence for parents
  for (parent in parents){
    nodes <- append(nodes, parent)
  }
  
  # Add evidence for children
  for (child in children){
    nodes <- append(nodes, child)
    
    # Also, for every children, add its parents value unless said parent is S
    child_parents = parents(fitted_bn, child)
    
    for (child_parent in child_parents){
      if (child_parent == "S"){
        # S itself, do nothing
        } else {
        nodes <- append(nodes, child_parent)
      }
    }
  }
  
  evidence = as.list(test_data[row, nodes])
  
  # Calculate the conditional probability
  temp_cond_calc <- cpquery(fitted_bn, event = (S == "yes"), 
                            evidence = evidence, method = "lw")
  
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