
WrapMode = 1 -- Determines if edge-wrapping behavior is part 1 or part 2

PrintTrace = false -- Print every state after each move?

-- Read in the input:
function file_exists(file)
  local f = io.open(file, "rb")
  if f then f:close() end
  return f ~= nil
end


function lines_from(file)
  if not file_exists(file) then return {} end
  local lines = {}
  for line in io.lines(file) do 
    lines[#lines + 1] = line
  end
  return lines
end

InputFile = 'input.txt'
Lines = lines_from(InputFile)


-- A grid/matrix to hold the world:
World = {} 

-- Width and height of the entire map:
Width = 0
Height = 0 



-- Pull off the password line at the end first:
Password = Lines[#Lines] ;
print ("password: ", password)

-- Cut out the bottom to Lines:
table.remove(Lines, #Lines) ;
table.remove(Lines, #Lines) ;


-- Look over all lines to find Width and Height:
Height = 0
for line_no,line in pairs(Lines) do
  Height = Height + 1
  Width = math.max(Width, #line)
end

for i = 1, Width do 
  World[i] = {}  
end


-- Load the input into the World matrix:
j = 1 
for line_no,line in pairs(Lines) do
  
   for i = 1, Width do 
     local c 
     if (i <= #line) then
       c = line:sub(i,i)
     else
       c = " "
     end
      World[i][j] = c
   end

   j = line_no + 1
end


-- Checks if a coordinate is within bounds of the map:
function in_bounds(x,y)
  
  local result = true

  column = World[x]
  if column ~= nil then
    c = column[y]

    if (c == nil) then
      result = false
    elseif (#c == 0) then
      result = false 
    elseif (c == " ") then
      result = false
    end    

  else
    result = false
  end

  return result
end

-- Retrieve the character at global map location x,y:
function block_at(x,y)  
  local result
  if (in_bounds(x,y)) then
    result = World[x][y]
  else
    result = " "
  end
  return result 
end


-- Compute the starting location:
X = 1
Y = 1

function reset() 
   X = 1
   Y = 1

  while (not in_bounds(X,1)) do
    X = X + 1
  end

  Facing = ">" ; 

  print("start: ", X, Y)

end



-- Tables to encode simple functions:

-- Rotate facing to the right:
RightTable = {}
RightTable[">"] = "v";
RightTable["v"] = "<";
RightTable["<"] = "^";
RightTable["^"] = ">";

-- Rotate facing to the left:
LeftTable = {}
LeftTable[">"] = "^";
LeftTable["v"] = ">";
LeftTable["<"] = "v";
LeftTable["^"] = "<";

-- Maps a facing to a cardinal direction
CardinalTable = {} 
CardinalTable[">"] = "E";
CardinalTable["<"] = "W";
CardinalTable["^"] = "N";
CardinalTable["v"] = "S";

-- Maps a cardinal direction to a facing:
FacingTable = {} 
FacingTable["E"] = ">";
FacingTable["W"] = "<";
FacingTable["N"] = "^";
FacingTable["S"] = "v";


-- Rotate the character right:
function turn_right() 
  Facing = RightTable[Facing]
end

-- Rotate the character left:
function turn_left()
  Facing = LeftTable[Facing] 
end

-- Find the block directly in front of the character, wrapping around the edge of the map if so:
function location_in_front()

  local fx = X
  local fy = Y

  if Facing == "^" then
    fy = fy - 1
  elseif Facing == ">" then
    fx = fx + 1
  elseif Facing == "v" then
    fy = fy + 1 
  elseif Facing == "<" then
    fx = fx - 1
  end

  if not in_bounds(fx,fy) then
    fx = X
    fy = Y

    if Facing == "^" then

      while (in_bounds(fx,fy)) do
        fy = fy + 1
      end
      fy = fy - 1 -- back up one block

    elseif Facing == ">" then

      while (in_bounds(fx,fy)) do
        fx = fx - 1
      end
      fx = fx + 1 -- back up one block

    elseif Facing == "v" then

      while (in_bounds(fx,fy)) do
        fy = fy - 1
      end
      fy = fy + 1 -- back up one block

    elseif Facing == "<" then

      while (in_bounds(fx,fy)) do
        fx = fx + 1
      end
      fx = fx - 1 -- back up one block

    end
  end

  return fx,fy
end

-- Move the marker forward one space:
function advance() 
  local nx,ny ;
  local new_facing ;
 
  -- Check for part 1 or part 2:
  if WrapMode == 1 then
    nx,ny = location_in_front()
    new_facing = Facing
  else
    nx,ny,new_facing = warp_location_in_front()
  end
  
  local block_in_front = block_at(nx,ny) 

  if block_in_front == "#" then
    -- do nothing, can't move
  else
    X = nx
    Y = ny
    Facing = new_facing
  end

end



-- Print out the World:
function print_world()

  for j = 1,Height do

    local s = "" ;

    for i = 1,Width do
      c = World[i][j]
      if i == X and j == Y then
        c = Facing
      end
      s = s .. c
    end

    print(s)
  end

end

-- Parse the next command from a password:
function next_command(cmds) 
  local tok
  local remainder

  local m 

  if #cmds == 0 then
    tok = nil
    remainder = nil
  elseif cmds:match("^L") then
    remainder = cmds:sub(2)
    tok = "L"

  elseif cmds:match("^R") then
    remainder = cmds:sub(2)
    tok = "R"

  else
    tok = cmds:match("^%d+")
    remainder = cmds:sub(1 + #tok)
  end


  return tok,remainder
end

-- Run through all the commands in a list:
function run_commands(cmds) 

  local tok

  tok,cmds = next_command(cmds)
  while (tok) do

    -- Act on tok
    if tok == "R" then
      turn_right()
    elseif tok == "L" then
      turn_left() 
    else 
      steps = tonumber(tok)
      for i = 1,steps do
        advance()
      end
    end

    if PrintTrace then
      print_world()
    end


    tok,cmds = next_command(cmds)
  end

end

-- Compute the final password score:
FacingScore = {}
FacingScore[">"] = 0
FacingScore["v"] = 1
FacingScore["<"] = 2
FacingScore["^"] = 3

function compute_final_password() 
  return 1000 * Y + 4 * X + FacingScore[Facing] 
end


-- Set the world to the initial state:
reset() 

-- Run through the commands in password:
run_commands(Password) 

-- print_world()

print("Part 1, final password: ", compute_final_password())





-- Part 2

-- For part 2, we have to divide the original map into 50x50 regions.

-- Then, we assign true north, true south, true east, true west based on the original map.

-- When we fold the world, it's possible to get different combinations of N, S, E and W where edges meet.

-- When we cross from one region to another, we first assume that they're aligned as if adjacent on the global map.

-- There's a rotation function that determines how coordinates shift for every possible combination of {N,S,E,W} and {N,S,E,W} depending on how the cubes line up.

-- So, then, we rotate the point new region until its north aligns to true north.


RegionSize = 50

-- Find the region code given a global point on the map:
function region(glob_x,glob_y) 
  return string.format("%d,%d",(glob_x-1) // RegionSize, (glob_y-1) // RegionSize)
end

-- Find the local regional coordinates given a global point on the map:
function global_to_regional(glob_x,glob_y)
  return math.fmod(glob_x-1, RegionSize)+1, math.fmod(glob_y-1,RegionSize)+1
end

-- Find the global position given a region and its regional coordinate:
function regional_to_global(region_code,reg_x,reg_y)
  local block_X = tonumber(region_code:sub(1,1))
  local block_Y = tonumber(region_code:sub(3,3))
  return block_X*50 + reg_x, block_Y*50+reg_y
end



-- The region adjacency table is specific to the input / folding of the cube:

-- It's hard-coded for the challenge input here, but it is possible to compute the general case.

-- There are only 11 possible unfoldings of a cube, so it's possible to match on all of them.

-- Or, you can decompose the unfoldings locally to infer stitching between edges.

-- For example, for the general case, for a region pattern like this:

-- AC
-- B

-- allows us to infer that B East "stitches" to C South.

-- Working through this particular input gives this relationship between all the edges and regions:

-- AdjacentRegion => {N,S,E,W} => Region * {N,S,E,W}
AdjacentRegion = {} 
AdjacentRegion["1,0"] = {}
AdjacentRegion["2,0"] = {}
AdjacentRegion["1,1"] = {}
AdjacentRegion["1,2"] = {}
AdjacentRegion["0,2"] = {}
AdjacentRegion["0,3"] = {}

AdjacentRegion["1,0"]["N"] = { "0,3", "W" }
AdjacentRegion["1,0"]["E"] = { "2,0", "W" }
AdjacentRegion["1,0"]["S"] = { "1,1", "N" }
AdjacentRegion["1,0"]["W"] = { "0,2", "W" }

AdjacentRegion["2,0"]["N"] = { "0,3", "S" }
AdjacentRegion["2,0"]["E"] = { "1,2", "E" }
AdjacentRegion["2,0"]["S"] = { "1,1", "E" }
AdjacentRegion["2,0"]["W"] = { "1,0", "E" }

AdjacentRegion["1,1"]["N"] = { "1,0", "S" }
AdjacentRegion["1,1"]["E"] = { "2,0", "S" }
AdjacentRegion["1,1"]["S"] = { "1,2", "N" }
AdjacentRegion["1,1"]["W"] = { "0,2", "N" }

AdjacentRegion["1,2"]["N"] = { "1,1", "S" }
AdjacentRegion["1,2"]["E"] = { "2,0", "E" }
AdjacentRegion["1,2"]["S"] = { "0,3", "E" }
AdjacentRegion["1,2"]["W"] = { "0,2", "E" }

AdjacentRegion["0,2"]["N"] = { "1,1", "W" }
AdjacentRegion["0,2"]["E"] = { "1,2", "W" }
AdjacentRegion["0,2"]["S"] = { "0,3", "N" }
AdjacentRegion["0,2"]["W"] = { "1,0", "W" }

AdjacentRegion["0,3"]["N"] = { "0,2", "S" }
AdjacentRegion["0,3"]["E"] = { "1,2", "S" }
AdjacentRegion["0,3"]["S"] = { "2,0", "N" }
AdjacentRegion["0,3"]["W"] = { "1,0", "N" }

-- TODO: Could write an internal consistency checker to make sure
-- that every adjacency maps back to itself in the other direction.



function TODO (msg)
  print("TODO: ", msg)
  os.exit()
end


-- Returns reg_x, reg_y, facing after rotating it within a region:
function rotate_point_right(reg_x, reg_y, facing)
  -- To rotate a point in a region once to the right:

  local new_reg_x, new_reg_y 
  local new_facing

  -- x' = RegionSize - y
  -- y' = x
  -- Facing = RightTable[Facing]
  
  -- To rotate a point in a region once to the left:
  -- 
  -- x' = y
  -- y' = RegionSize - x
  -- Facing = LeftTable[Facing]
  
  -- Or, one left rotation = three right rotations:

  -- First change to the 0-based coordinate system:
  reg_x = reg_x - 1
  reg_y = reg_y - 1

  -- Transform:

  -- print("RegionSize: ", RegionSize)
  -- print("reg_y", reg_y)

  new_reg_x = (RegionSize-1) - reg_y
  new_reg_y = reg_x
  new_facing = RightTable[facing]

  -- Change back to 1-based coordinates:
  new_reg_x = new_reg_x + 1
  new_reg_y = new_reg_y + 1

  return new_reg_x, new_reg_y, new_facing
end
 

-- Rotate a regional point to the left within the region:
function rotate_point_left(reg_x, reg_y, facing)
  reg_x, reg_y, facing = rotate_point_right(reg_x,reg_y,facing) 
  reg_x, reg_y, facing = rotate_point_right(reg_x,reg_y,facing) 
  reg_x, reg_y, facing = rotate_point_right(reg_x,reg_y,facing) 
  return reg_x, reg_y, facing
end




-- Returns new global x, new global y, new facing
-- for the point just across the board in given direction:
function warp_cross_border_location(from_reg,direction,reg_x,reg_y)

  -- Grab the destination region and the opposing border:
  local to_reg_border = AdjacentRegion[from_reg][direction]

  local to_reg = to_reg_border[1] -- new region
  local opposing_border = to_reg_border[2] -- N, S, E, W, border crossed to enter

  -- One of {N,S,E,W}{N,S,E,W}
  local xform = direction .. opposing_border 

  local glob_x, glob_y, new_facing

  -- When crossing between regions at these edge types,
  -- here are the rotations needed to restore
  -- the new point to the original "True North" system:
  
  -- E<->W, W<->E, N<->S, S<->N -- no rotation
 
  -- rotate left twice, or rotate right twice, i.e., flip <,> or ^,v
  -- E<->E, W<->W, S<->S, N<->N 
  
  -- E->S -- rotate left
  -- S->W -- rotate left
  -- N->E -- rotate left
  -- W->N -- rotate left

  -- E->N -- rotate right
  -- S->E -- rotate right
  -- N->W -- rotate right
  -- W->S -- rotate right

  local new_reg_x
  local new_reg_y 

  -- Find the location in the new region, first assuming
  -- that region has the same True North orientation as
  -- the current region.  Then, correct with rotations as needed.

  if direction == "E" then
    new_reg_x = 1
    new_reg_y = reg_y
  elseif direction == "W" then
    new_reg_x = RegionSize
    new_reg_y = reg_y
  elseif direction == "N" then
    new_reg_x = reg_x 
    new_reg_y = RegionSize
  elseif direction == "S" then
    new_reg_x = reg_x
    new_reg_y = 1
  else
    TODO("Not possible!")
  end

  local old_facing = FacingTable[direction]

  -- Now figure out how to rotate to re-align with the True North system:
  if xform == "EW" or
     xform == "WE" or
     xform == "NS" or
     xform == "SN" then

     -- Same coordinate system; same facing
     new_facing = old_facing

  elseif xform == "EE" or
         xform == "WW" or
         xform == "SS" or
         xform == "NN" then

     new_reg_x, new_reg_y, new_facing = rotate_point_right (new_reg_x, new_reg_y, old_facing)
     new_reg_x, new_reg_y, new_facing = rotate_point_right (new_reg_x, new_reg_y, new_facing)

  elseif xform == "EN" or
         xform == "SE" or
         xform == "NW" or
         xform == "WS" then

     new_reg_x, new_reg_y, new_facing = rotate_point_right (new_reg_x, new_reg_y, old_facing)

  elseif xform == "ES" or
         xform == "SW" or
         xform == "NE" or
         xform == "WN" then

     new_reg_x, new_reg_y, new_facing = rotate_point_left (new_reg_x, new_reg_y, old_facing)

  else

    TODO("not possible!")

  end

  glob_x, glob_y = regional_to_global(to_reg, new_reg_x, new_reg_y)

  return glob_x, glob_y, new_facing 

end


-- Returns the global location in front of this one, as well as the new
-- facing, should the movement be made (regardless of whether it is possible)
function warp_location_in_front()

  local reg_code
  local reg_x
  local reg_y

  -- Transform to region coordinotes first:
  reg_code = region(X,Y)
  reg_x, reg_y = global_to_regional(X,Y) ;

  -- Check if the location in front would cross a region boundary:
  
  local cardinal = CardinalTable[Facing]

  if cardinal == "N" and reg_y == 1 then 
    -- Cross the north border:
    return warp_cross_border_location(reg_code, cardinal, reg_x, reg_y)
  elseif cardinal == "E" and reg_x == RegionSize then
    -- Cross the east border:
    return warp_cross_border_location(reg_code, cardinal, reg_x, reg_y)
  elseif cardinal == "S" and reg_y == RegionSize then
    -- Cross the south border:
    return warp_cross_border_location(reg_code, cardinal, reg_x, reg_y)
  elseif cardinal == "W" and reg_x == 1 then
    -- Cross the west border:
    return warp_cross_border_location(reg_code, cardinal, reg_x, reg_y)
  else
    -- Look at a point locally within the region:

    -- No change for facing direction.
    -- We can use the old location_in_front() routine to find this.
    x_new,y_new = location_in_front()
    return x_new,y_new,Facing
  end
    
  
end

-- Now, run Part 2:

PrintTrace = false
WrapMode = 2 -- part 2 wrapping behavior

reset()

run_commands(Password) 

-- print_world()

print ("X,Y,facing: ", X, Y, Facing)



print("Part 2, final password: ", compute_final_password())

