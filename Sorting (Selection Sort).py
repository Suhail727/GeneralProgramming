A = [64, 25, 12, 22, 11] 
  
# Traverse through all array elements, find min value and swap with first value
for i in range(len(A)): 
    # Consider first index element as minimum
    min_idx = i 
    # Find the minimum element in remaining  
    # unsorted array 
    for j in range(i+1, len(A)): 
        # If any element is smaller than value at minimum index, update minimum index
        if A[min_idx] > A[j]: 
            min_idx = j      
    # Swap the found minimum element with current index         
    A[i], A[min_idx] = A[min_idx], A[i] 
  
# Driver code to test above 
print ("Sorted array") 
for i in range(len(A)): 
    print("%d" %A[i]),  
