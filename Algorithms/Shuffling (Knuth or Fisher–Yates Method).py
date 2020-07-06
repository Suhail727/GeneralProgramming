import random 
  
# A function to generate a random permutation of arr[] 
def randomize (arr, n): 
    # Start from first element, increment i
    # Pick a random index (j) between 0 and i, then swap the elements i and j
    for i in range(len(arr)): 
        # Pick a random index from 0 to i 
        j = random.randint(0,i+1) 
  
        # Swap arr[i] with the element at random index 
        arr[i],arr[j] = arr[j],arr[i] 
    return arr 
  
# Driver program to test above function. 
arr = [1, 2, 3, 4, 5, 6, 7, 8] 
n = len(arr) 
print(randomize(arr, n)) 
  