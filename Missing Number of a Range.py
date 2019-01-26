def missingNum( arr,  n): 
  
    minvalue = min(arr) 
   
    # here we xor of all the number 
    xornum = 0
    for  i in range (0,n): 
        xornum ^= (minvalue) ^ arr[i] 
        minvalue = minvalue+1
      
   
    # xor last number 
    return xornum ^ minvalue 
      
# Driver method 
arr = [ 13, 12, 11, 15 ] 
n = len(arr) 
print (missingNum(arr, n))