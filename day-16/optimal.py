
from functools import cache

import re

# Nodes in the cave graph:
Nodes = []

# Edges is the cave graph:
Edges = {}

# Per-node flow values in the cave:
Flow = {}

# A list of all nodes of non-zero flow:
MajorNodes = [ "AA" ]

# A suitably large number for computing the shortest path:
infinity = 100000


## The first step is to reduce the graph to a new, smaller graph of only the "major" nodes.

## Edges in the major graph are labeled with the number of steps it takes to reach that node.

## To compute these distances, we need to use Floyd-Warshall.



# A table to hold shortest paths between all nodes:
ShortestPath = {}



# Helpers to smooth over get and setting shortest paths:
def get_shortest_path(a,b):
  paths = ShortestPath.get(a)
  if paths is None:
    paths = {}
    ShortestPath[a] = paths

  distance = paths.get(b)
  if distance is None:
    distance = infinity
    paths[b] = distance 

  return distance


def set_shortest_path(a,b,n):
  paths = ShortestPath.get(a)
  if paths is None:
    paths = {}
    ShortestPath[a] = paths

  paths[b] = n


# For debugging, print all the shortest paths:
def print_shortest_paths():
  for a in ShortestPath:
    print("Shortest from " + a + ": ") ;
    paths = ShortestPath[a]
    for b in paths:
      print("  to " + b + ": " + str(paths[b]))


# For debugging, dump out dot files for the original graph and the smaller graph:
def dump_dot():
  dotfile = open("graph.dot", "w")

  dotfile.write("digraph Cave {\n\n")
 
  for a in Nodes:
    neighbors = Edges[a]
    for b in neighbors:
      dotfile.write("   " + a + " -> " + b + "\n") ;

  dotfile.write("}\n")



  dotfile = open("major-graph.dot", "w")

  dotfile.write("digraph Cave {\n\n")

  for a in MajorNodes:
    for b in MajorNodes:
       dotfile.write("   " + a + " -> " + b + " [ label=\"" + str(get_shortest_path(a,b)) + "\" ]\n") ;

  dotfile.write("}\n")



# Use Floyd-Warshall to compute all shortest paths,
# populating ShortestPath[node1][node1] in the process:

def find_shortest_paths():
  
  # Initialize every node to be distance zero from itself:
  for n in Nodes:
    set_shortest_path(n,n,0)

  # Initialize every edge:
  for a in Edges:
    neighbors = Edges[a]
    for b in neighbors:
      set_shortest_path(a,b,1)

  def dist(a,b):
   return get_shortest_path(a,b)
 
  # Look at every start, intermediate, end triple:
  for k in Nodes:
    for i in Nodes:
      for j in Nodes:

         # Check for a shorter route from i to j through k:
         current_path_length = dist(i,j)
         thru_k_path_length  = dist(i,k) + dist(k,j)

         # If it's shorter, update the ShortestPath table:
         if current_path_length > thru_k_path_length:
            if i == j: 
              print("updating myself: " + i)
              print(" old: " + str(current_path_length))
              print(" new: " + str(thru_k_path_length))
            set_shortest_path(i,j, thru_k_path_length)

  return




# A depth-first search of the graph to compute the best possible flow 
#   - from a starting node
#   - given a set of nodes already seen
#   - given a specific amount of time remaining
#   - as constrained by a set of available nodes

def best_flow( from_node, visited_nodes , time_left , available_nodes ):

   # Check if there's any time left:
   if time_left <= 0:
     # No time left to release any pressure.
     return 0, visited_nodes

   # Check if we've already visited this node:
   if from_node in visited_nodes:
     # We've already done the best we can from here.
     return 0, visited_nodes

   # It costs a minute to open this valve:
   if Flow[from_node] > 0:
     # Spend a minute to open it if it has flow:
     time_left = time_left - 1 

   # Find total pressure released from opening this node:
   local_flow = Flow[from_node] * time_left

   # Add this node to the visited set:
   new_visited_nodes = visited_nodes | { from_node }

   # Look at the score for all neighbors, take the best:
   max_sub_score = 0
   max_sub_visited = set()

   # Look at every major node as a possible next step:
   for n in available_nodes:

     # Skip this node:
     if from_node == n:
       continue 
  
     dist_n = get_shortest_path( from_node, n )
  
     # Find the best we could do if we went to n next:
     score,path = best_flow( n , new_visited_nodes , time_left - dist_n, available_nodes )
     
     # If this beats the previous best, use this instead:
     if score > max_sub_score:
       max_sub_score = score
       max_sub_visited = path
  
   return (local_flow + max_sub_score), (max_sub_visited | { from_node })


# A single-argument, cached function that finds the best answer
# for 26 minutes for the provided set of available nodes:
@cache
def best_flow_in_26( available_nodes ):
  return best_flow( 'AA', frozenset(), 26, available_nodes )

 


# Process the into a graph structure:
input_file = open('input.txt', 'r') 
lines = input_file.readlines() 
input_file.close()

for line in lines:
  line = line.strip()
  matches = re.search("Valve ([A-Z][A-Z]).*rate=([0-9]+).*valves? ([A-Z, ]+)", line)

  if not matches:
    error("bad input")
    exit()

  node_name = matches.group(1)
  rate = matches.group(2)
  edges = matches.group(3).split(", ")

  print(node_name + ": ")
  print("  rate:  " + rate)
  print("  edges: " + ", ".join(edges))

  rate = int(rate)
  Nodes = [node_name] + Nodes
  Flow[node_name] = rate
  Edges[node_name] = edges

  if rate > 0:
    MajorNodes = [node_name] + MajorNodes 
  

  

# Populate the shortest paths table:
find_shortest_paths()

print_shortest_paths()

dump_dot()

# Part 1:

score,_ = best_flow( 'AA', frozenset(), 30, frozenset(MajorNodes) )

print("Part 1: " + str(score))


# Find all bipartitions of a set:
def bipartitions(s):

  if not s: 
    return [ (frozenset(),frozenset()) ]

  if len(s) == 1: 
    return [ (frozenset(),s) , (s,frozenset()) ]

  elem = min(s)
  subset = s - { elem }
  sub_partitions = bipartitions(subset)
  return sum([ [(left | {elem},right), (left,right | {elem})] for (left,right) in sub_partitions ], [])


high_score = 0

# Look at all bipartitions of the major nodes:
partitions = bipartitions(frozenset(MajorNodes))

# Find the highest combined score:
for left,right in partitions:
  left_score,_ = best_flow_in_26( left | { 'AA' } )
  right_score,_ = best_flow_in_26( right | { 'AA' } )
  score = left_score + right_score
  if score > high_score:
     high_score = score

print("Part 2: " + str(high_score))



