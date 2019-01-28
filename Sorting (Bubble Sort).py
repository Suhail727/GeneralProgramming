import sys 
A = [64, 25, 12, 22, 11] 
  
# Traverse through all array elements 
for i in range(len(A)): 
    flag=0
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