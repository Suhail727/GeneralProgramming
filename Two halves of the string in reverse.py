def solve(s) : 
    l = len(s) 
    x = l // 2
    y = l 
    # Calculating the two halves 
    # of string s as first and  
    # second. The final string p 
    p = "" 
    while (x > 0 and y > l / 2) : 
        # It joins the characters to 
        # final string in reverse order 
        p =  p + s[x - 1] 
        x = x - 1
        # It joins the characters to 
        # final string in reverse order 
        p = p + s[y - 1] 
        y = y - 1
    if (y > l // 2) : 
        p = p + s[y - 1] 
        y = y - 1 
    print (p) 
  
# Driver code 
s = "sunshin"
  
# Calling function 
solve(s) 
