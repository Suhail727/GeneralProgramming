operators = set(['+','-','*','/','(',')','^'])

def evaluate_postfix(expression):
  stack = []
  for character in expression:
    if character not in operators:
      stack.append(character)
    elif stack:
      var1 = stack.pop()
      var2 = stack.pop()
      stack.append(str(eval(var1 + character + var2)))
  return (stack.pop())
  
expression = input('Enter postfix expression: ')
print('Postfix expression: ',expression)
print('Evaluation of expression: ',evaluate_postfix(expression))
# input: 231*+9-
# output: 