def printReverse(s): 
    #Taking all the words in a list 
    l = [s for s in s.split(' ')]  
    #printing the first word as it is 
    print(l[0], end = ' ') 
    for i in range(1, len(l)-1): 
        #printing middle words reversed 
        print(l[i][::-1], end = ' ') 
    #printing the last word as it is 
    print(l[len(l)-1]) 
  
s = "Hi how are you geeks"
printReverse(s) 
