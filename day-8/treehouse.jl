
# Open the input file:
input_file = open("input.txt", "r")


# A place to store the height map:
tree_heights = [] # An array of rows of tree hights


# Read in the height map, line by line:
line = 0  
while (!eof(input_file))

  global line 
  global tree_heights

  next = readline(input_file)

  append!(tree_heights,[ [ parse(Int64,c) for c in split(next,"") ] ])
  
  line += 1

end

close(input_file) 

# Dimensions of the height map:
width = line
height = line # assuming width and height of the matrix are the same


# A matrix to track whether a tree is visible from any direction:
visible = zeros(Int64,width,height)


# Compute visibility for every tree in every direction

# Scan each column from top to bottom
for x in 1:width
  max = -1
  for y in 1:height
    if tree_heights[y][x] > max
      max = tree_heights[y][x]
      visible[y,x] = 1
    end
  end
end

# Scan each column from bottom to top
for x in 1:width
  max = -1
  for y in height:-1:1
    if tree_heights[y][x] > max
      max = tree_heights[y][x]
      visible[y,x] = 1
    end
  end
end


# Scan each row from left to right:
for y in 1:height
  max = -1
  for x in 1:width
    if tree_heights[y][x] > max
      max = tree_heights[y][x]
      visible[y,x] = 1
    end
  end
end


# Scan each row from right to left:
for y in 1:height
  max = -1
  for x in width:-1:1
    if tree_heights[y][x] > max
      max = tree_heights[y][x]
      visible[y,x] = 1
    end
  end
end



# For part 2, compute the scenic score of a given location:
function scenic_score(x,y)
  global tree_heights

  loc_height = tree_heights[y][x]

  # Look right
  right_view = findfirst(h -> h >= loc_height, 
                        [ tree_heights[y][i] for i in (x+1):width ])

  if (isnothing(right_view))
    right_view = width-x
  end

  # Look left
  left_view = findfirst(h -> h >= loc_height, 
                        [ tree_heights[y][i] for i in (x-1):-1:1 ])

  if (isnothing(left_view))
    left_view = x-1
  end


  # Look up
  up_view = findfirst(h -> h >= loc_height, 
                      [ tree_heights[j][x] for j in (y-1):-1:1 ])

  if (isnothing(up_view))
    up_view = y-1
  end


  # Look down
  down_view = findfirst(h -> h >= loc_height, 
                      [ tree_heights[j][x] for j in (y+1):height ])

  if (isnothing(down_view))
    down_view = height-y
  end

  return up_view * down_view * left_view * right_view
end


# Add up the number of visible trees:
part1 = sum([visible[y,x] for y in 1:height for x in 1:width])

println("Part 1: $part1")

# Scan the entire forest for the top scenic score:
max_score = 0
for x in 1:width
  for y in 1:height
    global max_score
    local score
    score = scenic_score(x,y)
    if (score > max_score)
      max_score = score
    end
  end
end  

println("Part 2: $max_score")
