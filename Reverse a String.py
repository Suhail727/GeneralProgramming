# Function to 
# reverse a string 
def reverseStr(str): 
    n = len(str) 
      
    # initialising a empty 
    # string 'str1' 
    str1 = ''
    i = n-1
    while i >=0:
      str1 += str[i]
      i -= 1
    print(str1)    
  
# Driver Code  
def main(): 
    str = "geeksforgeeks"; 
    reverseStr(str); 
      
if __name__=="__main__": 
    main()      
      
