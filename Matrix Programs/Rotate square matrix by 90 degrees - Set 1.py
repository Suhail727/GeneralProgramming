# Python3 program to rotate a matrix by 90 degrees 
# as explained in https://www.youtube.com/watch?v=Jtu6dJ0Cb94
N = 4
  
# An Inplace function to rotate  
# N x N matrix by 90 degrees in 
# anti-clockwise direction 
def rotateMatrix(mat): 
    
    # Length of matrix
    last = N-1
    # Consider all squares one by one 
    # Number of levels will be N/2 (i.e Iterate through 2 squares for 4x4 matrix)
    totallevels=N/2
    # Initial Level
    level = 0
    
    while (level<totallevels):     
        # Consider elements in group    
        # of 4 in current square 
        for i in range(level,last): 
          # Swapping All 4 Corner Values 
          mat[level][i],mat[i][last]=mat[i][last],mat[level][i]
          # Move to second element in first row and swap all corresponding values again
          mat[level][i],mat[last][last-i+level] = mat[last][last-i+level],mat[level][i]
          # Move to third element in first row and swap all corresponding values again
          mat[level][i],mat[last-i+level][level] = mat[last-i+level][level],mat[level][i]
        # Go to inner square now  
        level += 1
        # Decrement length 
        last -= 1

  
# Function to pr the matrix 
def displayMatrix( mat ): 
      
    for i in range(0, N): 
          
        for j in range(0, N): 
              
            print (mat[i][j], end = ' ') 
        print ("") 
      
      
  
  
# Driver Code 
mat = [[0 for x in range(N)] for y in range(N)] 
  
# Test case 1 
mat = [ [1, 2, 3, 4 ], 
        [5, 6, 7, 8 ], 
        [9, 10, 11, 12 ], 
        [13, 14, 15, 16 ] ] 
          
''' 
# Test case 2 
mat = [ [1, 2, 3 ], 
        [4, 5, 6 ], 
        [7, 8, 9 ] ] 
  
# Test case 3 
mat = [ [1, 2 ], 
        [4, 5 ] ] 
          
'''
  
rotateMatrix(mat) 
  
# Print rotated matrix 
displayMatrix(mat) 
  