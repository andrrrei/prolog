## Predicate Descriptions

### **9. sort(L1, L2)**
Sorts a list `L1` into a sorted version `L2` using recursive insertion sort.  
- **Base Case**. An empty list is already sorted.  
- **Recursive Case**. The head of the list is inserted into the sorted tail using the `insert/3` helper predicate.

### **10. subset(M1, M2)**
Checks if `M1` is a subset of `M2`.  
- **Base Case**. An empty list is a subset of any list.  
- **Recursive Case**. The first element of `M1` must belong to `M2`, and the rest of the elements are checked recursively.

### **11. intersection(M1, M2, M3)**
Finds the intersection of two lists `M1` and `M2`, storing the result in `M3`.  
- **Base Case**. If one of the lists is empty, the intersection is empty.  
- **Recursive Case**. Elements from `M1` that are members of `M2` are added to the result list `M3`.

### **16. path(X, Y, L)**
Finds a path `L` between two nodes `X` and `Y` in a graph.  
- **Base Case**. If the start and end nodes are the same, the path is complete.  
- **Recursive Case**. Explores adjacent nodes (`edge/3` predicate) while avoiding cycles by checking visited nodes.

### **17. min_path(X, Y, L)**
Finds the shortest path `L` from node `X` to node `Y`.  
- Uses `findall/3` to generate all possible paths and their costs.  
- Selects the path with the minimum cost using a helper predicate.

### **18. short_path(X, Y, L)**
Finds the shortest path `L` between nodes `X` and `Y` using Breadth-First Search (BFS).  
- Traverses the graph level-by-level, expanding paths until the target node is reached.  
- The final path is reversed to present it from start to end.

### **19. edge_cost(X, Y, Cost)**
Calculates the cost of traveling between nodes `X` and `Y`.  
- Relies on the `edge/3` predicate to retrieve the cost of the edge.  
- Used as a helper predicate for pathfinding and cost computation.

### **20. permute(L1, L2)**
Generates all permutations of a list `L1` and stores them in `L2`.  
- Recursively selects an element and inserts it into all possible positions in the remaining list.
