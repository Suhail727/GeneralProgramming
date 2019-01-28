def mergeSort(a):
  if len(a)>1:
    mid = len(a)//2
    l = a[:mid]
    r = a[mid:]
    mergeSort(l)
    mergeSort(r)

    i = j = k = 0
    while i < len(l) and j < len(r):
      if l[i] < r[j]:
        a[k] = l[i]
        i += 1
      else:
        a[k] = r[j]
        j += 1
      k += 1
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