
# Genes discovered that makes koalas and marsupials cute:
koala_gene <- "AGCCTTAAATAACGACCTTC"
marsupial_gene <- "AGCCTTTCAACACGACCTTC"

# Function to determine nucleotide frequency in a sequence:
n_nucleo <- function(nucleo, sequence) {
  result <- gregexpr(nucleo, sequence)
  if (length(result[[1]]) == 1 && result[[1]] == -1) {
    result <- 0
  } else {
    result <- length(result[[1]])
  }
  result
}
n_nucleo("A", koala_gene)

# Q1
# Use n_nucleo()
koala_ACGT_freq <- c(n_nucleo("A", koala_gene),
                     n_nucleo("C", koala_gene),
                     n_nucleo("G", koala_gene),
                     n_nucleo("T", koala_gene))

#Q2
# Function that calculates GC content of a sequence
gc_content <- function(sequence) {
  GC_number <- n_nucleo("C", sequence) + n_nucleo("G", sequence)
  GC_content <- GC_number / nchar(sequence)
  GC_content
}
gc_content("AAG")
gc_content("TGCCCATGGG")

# temp code from example
koala_nucleos <- strsplit(koala_gene, "")[[1]]
marsupial_nucleos <- strsplit(marsupial_gene, "")[[1]]
n_mutations <- sum(koala_nucleos != marsupial_nucleos)
n_mutations

# Function to compare sequence and return number of point mutations
point_mutations <- function(seq1, seq2) {
  seq1_nucleos <- strsplit(seq1, "")[[1]]
  seq2_nucleos <- strsplit(seq2, "")[[1]]
  n_mutations <- sum(seq1_nucleos != seq2_nucleos)
  n_mutations
}
point_mutations(koala_gene, marsupial_gene)


# # Answer assignment questions
# # This code is written in python so the whole thing will be commented out
# # This code has as input a dictionary whose keys have values that are lists that include two numbers (e.g. dict[key] = [4, 8])
# # The code goes through the dictionary, and finds the ratio of the two numbers in each of the key lists
# # The code gives the name of the key that has the highest ratio result, and gives the result back as a percentage
# value = 0
# final_name = ''
# for item in GC_dict:
#   item_val = GC_dict[item][0] / GC_dict[item][1]
#   if item_val > value:
#   final_name = item
#   value = item_val
# print(final_name)
# print(value*100)
# # Re-written in function form:
# # The GC_dict is replaced with a general dictionary parameter (called dict)
# def max_ratio_val_from_dict(dict):
#   value = 0
#   final_name = ''
#   for item in GC_dict:
#     item_val = GC_dict[item][0] / GC_dict[item][1]
#     if item_val > value:
#       final_name = item
#       value = item_val
#     return [final_name, value]
