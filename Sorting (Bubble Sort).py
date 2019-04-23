import sys 
A = [64, 25, 12, 22, 11] 
  
# Traverse through all array elements 
for i in range(len(A)): 
    # Set flag to check that if in any pass, if none of the elements are swapped, then 
    # the array is already sorted, hence we can break the outer loop.
    flag=0
    # len(A)-i-1 is to stop checking the sorted elements 
    # which are at the end of the list after each pass
    for j in range(len(A)-i-1):
      if(A[j+1]<A[j]):
        A[j+1],A[j] = A[j],A[j+1]
        flag=1
    if flag==0:
      break
# Driver code to test above 
print ("Sorted array") 
for i in range(len(A)): 
    print("%d" %A[i])
