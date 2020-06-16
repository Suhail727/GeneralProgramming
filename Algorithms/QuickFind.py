class QuickFind(object):
  graph=[]
  n=0
  #Function use to create structure with input total number of nodes in graph
  def __init__(self,m):   
    x=0
    self.n=m
    while x<self.n:
      self.graph.insert(x,x)
      x=x+1
  #Function use to check result and return boolean answer
  def CheckConnection(self,p,q):
    return self.graph[p]==self.graph[q]
  #Function use to create connection between graph and store in structure and return nothing
  def Unite(self,p,q):
    p_graph=self.graph[p]
    x=0
    while x<self.n:
      if self.graph[x]==p_graph:
        self.graph[x]=self.graph[q]
      x=x+1

obj=QuickFind(5) #Total number of nodes in graph
obj.Unite(0,1) #Create connection between 0 and 1
obj.Unite(2,3) #Create connection between 2 and 3
obj.Unite(3,4) #Create connection between 3 and 4
print(obj.CheckConnection(0,1)) #Check connection between 0 and 1
print(obj.CheckConnection(1,2)) #Check Connection between 1 and 2