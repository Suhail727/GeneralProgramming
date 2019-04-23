def quickSort(arr,low,high):
  if low < high:
    partitionindex = partition (arr,low,high)
    quickSort(arr,low,partitionindex-1)
    quickSort(arr,partitionindex+1,high)

def partition(arr,low,high):
  # Consider last element as pivot
  pivot = arr[high]
  # Consider another index which denotes where the pivot element should finally be inserted
  partitionindex = low
  # Check all elements in list and 
  # if any element is smaller than pivot, swap that element with value at partition index and increment partition index
  for i in range(low, high):
    if arr[i] <= pivot:
      arr[partitionindex], arr[i] = arr[i] , arr[partitionindex]
      partitionindex += 1
  # Finally place pivot element at the partition index 
  arr[partitionindex], arr[high] = arr[high], arr[partitionindex]
  return partitionindex



# Code to print the list 
def printList(arr): 
  for i in range(len(arr)):         
    print(arr[i],end=" ") 
  print() 
  
# driver code to test the above code 
if __name__ == '__main__': 
  arr = [12, 11, 13, 5, 6, 7]  
  n = len(arr)
  print ("Given array is", end="\n")  
  printList(arr) 
  quickSort(arr,0,n-1) 
  print("Sorted array is: ", end="\n") 
  printList(arr) 
