library(purrr)
library(DescTools)
library(sets)


# Read in the heightmap from input.txt:
file_name <- "input.txt"
con <- file(file_name, open="r")
lines <- readLines(con)
input_lines <- strsplit(lines, "\n")


# Keep track of part 1, distance to S:
distance_to_start <- Inf

# Keep track of part 2, distance to any a:
distance_to_any_a <- Inf

# Get the height of a character in the heightmap:
char2height <- function(char) {

  if (char == "S")
    result <- char2height('a')
  else if (char == "E")
    result <- char2height('z')
  else {
    zval <- strtoi(CharToAsc("z"))
    charval <- strtoi(CharToAsc(char))
    result <- zval - charval
  }
}

# Split a string into a vector of characters:
str2chars <- function (str) {
  result <- unlist(strsplit(str, "")) 
}

# Turn the input into a matrix:
height_map <- data.frame(do.call(rbind, map( input_lines, str2chars )))

# Get the dimensions of the map:
ncols <- ncol(height_map) 
nrows <- nrow(height_map) 

# Create a new matrix to store the shortest distance
# between any two points in the map:
shortest <- matrix(Inf, nrow=nrows, ncol=ncols)


# A helper function that returns a list of neighboring points:
neighbors <- function(i,j) {
  # Every location has up to 4 neighbors
  # (i-1,j) 
  # (i+1,j) 
  # (i,j-1)
  # (i,j+1)

  up    <- `if`(i-1 >= 1,      list( pair(i-1,j) ), list())
  down  <- `if`(i+1 <= nrows,  list( pair(i+1,j) ), list())
  left  <- `if`(j-1 >= 1,      list( pair(i,j-1) ), list())
  right <- `if`(j+1 <= ncols,  list( pair(i,j+1) ), list())

  result <- append(append(up,down), append(left,right))
}

# To find the shortest path, we'll repeatedly iterate over 
# every location in the map to "tighten" the shortest path matrix.

# For each location, it will check to see if any of its reachable
# neighbors permit a new shortest path.


# changed will track whether a new shortest path emerged
# anywhere during the last tightening:
changed <- 0

# The core routine for "tightening" the shortest matrix:
tighten <- function () {

  # Loop over all locations:
  for (i in 1:nrows) {
    for (j in 1:ncols) {

      # Get the character height:
      c_height <- height_map[i,j]       # as a char
      i_height <- char2height(c_height) # as an int

      # If this is the endpoint, mark it:
      if (c_height == "E") {
        # print("found E") ;
        shortest[i,j] <<- 0
        next 
      }

      # Check every neighbor for a shorter path:
      for (p in neighbors(i,j)) {
        ii <- p[[1]]
        jj <- p[[2]]
       
        nc_height <- height_map[ii,jj]
        ni_height <- char2height(nc_height) 

        # Look at the diffence in height between this location and 
        # its neighbor:
        delta <- i_height - ni_height ;

        if (delta <= 1) {
          # We could move from here to this neighbor.
          shortest_thru_neighbor <- shortest[ii,jj] + 1 

          # If this creates a shorter path through this node, record it:
          if (shortest_thru_neighbor < shortest[i,j]) {
            changed <<- 1
            shortest[i,j] <<- shortest_thru_neighbor
          }
        }
      }

      # Check to see if we've found the start:
      if (c_height == "S")
        distance_to_start <<- shortest[i,j]

      # Check to see if we've found any a at all:
      if ((c_height == "a") || (c_height == "S")) {
        # This is a valid start point, too.
        distance_to_any_a <<- min(distance_to_any_a,shortest[i,j])
      }

    }
  }
}


# Runs tighten() until there have been no changes:
find_shortest <- function() {

  tighten()
  changed <<- 1
  progress <- 0
  while (changed) {
    progress <- progress + 1
    print(paste("making progress: ", progress))
    changed <<- 0
    tighten() 
  }

}


find_shortest()

print("shortest: ")
print(shortest)


print(paste("Part 1, shortest distance to start: ", distance_to_start))

print(paste("Part 2, shortest distance to any a: ", distance_to_any_a))

