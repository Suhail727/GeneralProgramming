def mergeSort(a):
  # Sort only if there is more thanone element in array
  if len(a)>1:
    # Find the middle index and create 2 lists, left and right.
    mid = len(a)//2
    l = a[:mid]
    r = a[mid:]
    # Call the function recursively until left and right lists have only one element each
    mergeSort(l)
    mergeSort(r)
    # Merge Function
    # i for left list, j for right list, k for main list
    i = j = k = 0
    # i and j should be within size of left and right list. If they aren't, it means either list has become empty.
    # In that case move to next while loop
    while i < len(l) and j < len(r):
      # Check which is smaller value from left and right list, whichever is smaller append to main list
      if l[i] < r[j]:
        a[k] = l[i]
        i += 1
      else:
        a[k] = r[j]
        j += 1
      k += 1
    # If either left or right list becomes empty, just append the remaining list elements to the main list.
    while i < len(l):
      a[k] = l[i]
      i += 1
      k += 1
    while j < len(r):
      a[k] = r[j]
      j += 1
      k += 1


# Code to print the list 
def printList(arr): 
  for i in range(len(arr)):         
    print(arr[i],end=" ") 
  print() 
  
# driver code to test the above code 
if __name__ == '__main__': 
  arr = [12, 11, 13, 5, 6, 7]  
  print ("Given array is", end="\n")  
  printList(arr) 
  mergeSort(arr) 
  print("Sorted array is: ", end="\n") 
  printList(arr) 
