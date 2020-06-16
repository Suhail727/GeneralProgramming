class UnionFind:
    def __init__(self, n):
        self._id = list(range(n))
        self._sz = [1] * n
        self.cc = n  # connected components

    def _root(self, i):
        j = i
        # Make Every node point to it's grandparent, thereby halving time
        # This is Path Compression

        # Check if the index and value are same, if they are not, it means it's not the root
        while (j != self._id[j]):
          # Change value to point to the grandparent instead of the parent
          self._id[j] = self._id[self._id[j]]
          j = self._id[j]
        return j

    def find(self, p, q):
        return self._root(p) == self._root(q)

    def union(self, p, q):
        i = self._root(p)
        j = self._root(q)
        # If both have same root, skip
        if i == j:
            return
        # Check size array to determine which tree is smaller
        # If i is the smaller tree, insert value of root j (point it to j)
        # Also increment size of j by the size of what i was.
        if (self._sz[i] < self._sz[j]):
            self._id[i] = j
            self._sz[j] += self._sz[i]
        else:
            self._id[j] = i
            self._sz[i] += self._sz[j]
        self.cc -= 1

obj=UnionFind(5) #Total number of nodes in graph
obj.union(0,1) #Create connection between 0 and 1
obj.union(2,3) #Create connection between 2 and 3
obj.union(3,4) #Create connection between 3 and 4
print(obj.find(0,1)) #Check connection between 0 and 1
print(obj.find(1,2)) #Check Connection between 1 and 2