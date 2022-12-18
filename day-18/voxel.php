<?php


$lines = file('input.txt') ;


# Two 3D arrays to store properties of each coordinate:
$LavaVoxels = array() ;  # Holds 1 at that coordinate if it's a lava voxel.
$WaterVoxels = array() ;  # Holds 1 at that coordinaet if it's a water voxel.

# Note: These could have been combined, but it allowed the $WaterVoxels to double
# as a visited set for the breadth-first-search that simulated water flowing around
# the droplet.



# To store a conservative bounding cube that surrounds the lava droplet:
$MinX = false ;
$MaxX = false ;

$MinY = false ;
$MaxY = false ;

$MinZ = false ;
$MaxZ = false ;


# Returns true iff this coordinate is part of the lava droplet.
function is_lava($x,$y,$z) {
  global $LavaVoxels ;

  $index = $x . "," . $y . "," . $z ;

  if (array_key_exists($index, $LavaVoxels)) {
    return $LavaVoxels[$index] ;
  }
  
  return false ;
}

# Returns true iff this coordinate is water.
function is_water($x,$y,$z) {
  global $WaterVoxels ;

  $index = $x . "," . $y . "," . $z ;

  if (array_key_exists($index, $WaterVoxels)) {
    return $WaterVoxels[$index] ;
  }
  
  return false ;
}



# Return all the voxels that share an index with this coordinate.
function facing_voxels($index) {

  list($x,$y,$z) = coords($index) ;
  $neighbors = [] ;

  foreach ([-1,1] as $i) 
    array_push($neighbors, vindex($x+$i, $y, $z)) ;

  foreach ([-1,1] as $j) 
    array_push($neighbors, vindex($x, $y+$j, $z)) ;

  foreach ([-1,1] as $k) 
    array_push($neighbors, vindex($x, $y, $z+$k)) ;


  return $neighbors ;
}

# Transform coordinates to a string-like index:
function vindex($x,$y,$z) {
  return $x . "," . $y . "," . $z ;
}


# Transform a string index into coordinates:
function coords($index) {
  list($x,$y,$z) = preg_split("/,/",$index) ;

  return [intval($x),intval($y),intval($z)];
}


# Store the input into a simulated 3D array,
# using concatenated strings of coordinates 
# as indices:
foreach ($lines as $line) {
  $index = trim($line) ; 

  $LavaVoxels[$index] = 1 ;
  
  # echo $line ;
}


# Compute a conservative bounding cube for the lava droplet:
function find_bounding_cube() {
  global $LavaVoxels ;
  global $MinX, $MaxX, $MinY, $MaxY, $MinZ, $MaxZ ;

  $first = true ;
  
  foreach ($LavaVoxels as $k => $v) {
    list($x,$y,$z) = coords($k) ;

    if ($first) {
      $MinX = $x ;
      $MaxX = $x ;

      $MinY = $y ;
      $MaxY = $y ;

      $MinZ = $z ;
      $MaxZ = $z ;

      $first = false ;
    }

    $MinX = min($MinX,$x) ;
    $MaxX = max($MaxX,$x) ;

    $MinY = min($MinY,$y) ;
    $MaxY = max($MaxY,$y) ;

    $MinZ = min($MinZ,$z) ;
    $MaxZ = max($MaxZ,$z) ;
  }

  # Bump out the boundaries by 1 in all directions 
  # to allow water to fully flow around:

  $MinX-- ;
  $MaxX++ ;
 
  $MinY-- ;
  $MaxY++ ;

  $MinZ-- ;
  $MaxZ++ ;

}


# Compute the total surface area, including interior space:
function surface_area () {
  global $LavaVoxels ;

  $area = 0 ;

  foreach ($LavaVoxels as $k => $v) {
    list($x,$y,$z) = coords($k) ;
  
    $area += 6 ;
   
    $neighbors = facing_voxels($k) ;
  
    foreach ($neighbors as $n) {
      if (array_key_exists($n,$LavaVoxels)) {
        $area -= 1 ;
      }
    }
  }

  return $area ;
}


# True iff a coordiante is outside the bounding cube:
function is_out_of_bounds ($index) {
  global $MinX, $MaxX, $MinY, $MaxY, $MinZ, $MaxZ ;

  list($x,$y,$z) = coords($index) ;

  if ($x < $MinX) return true;
  if ($y < $MinY) return true;
  if ($z < $MinZ) return true;

  if ($x > $MaxX) return true;
  if ($y > $MaxY) return true;
  if ($z > $MaxZ) return true;

  return false ;
}


# Find the Part 1 surface area:
$SurfaceArea = surface_area() ;

echo ("Total surface area, including pockets, is $SurfaceArea\n") ;

find_bounding_cube() ;


echo ("Bounding cube: $MinX,$MaxX; $MinY,$MaxY; $MinZ,$MaxZ\n") ;


# Conduct a breadth-first search to determine where the water flows:
function breadth_first_search() {
  global $LavaVoxels ;
  global $WaterVoxels ;

  global $MinX, $MaxX, $MinY, $MaxY, $MinZ, $MaxZ ;

  $start = vindex($MaxX,$MaxY,$MaxZ) ;

  $queue = [$start] ;

  while (count($queue) > 0) {
    $next = array_shift($queue) ;

    # echo "exploring water: $next\n" ;

    if (is_out_of_bounds($next)) {
      # Skip over this voxel.
      continue ;
    }
      
    # Check if we've already visited this while scanning for
    # all possible water voxels; $WaterVoxels acts like a visited set.
    if (array_key_exists($next,$WaterVoxels)) {
      continue ;
    }
      
    # Check if this is already marked as lava:
    if (array_key_exists($next, $LavaVoxels)) {
       # It's a lava voxel, so mark it as not water:
       $WaterVoxels[$next] = 0;
    } else {
       # Water can flow here, so mark it as water:
       $WaterVoxels[$next] = 1; 
       # echo "Marking water: $next\n" ;

       # Now add the neighbors to the queue, so that we'll explore them too.
       foreach (facing_voxels($next) as $neighbor) {
         array_push($queue, $neighbor) ;
       }
    }


  }
}


# Follow the water as it flows around the droplet:
breadth_first_search() ;




# Computes surface area, but it only counts a surface if it's facing a water voxel:
function outside_surface_area () {
  global $LavaVoxels ;

  $area = 0 ;

  foreach ($LavaVoxels as $k => $v) {
    list($x,$y,$z) = coords($k) ;

    # echo "Considering: $k\n" ;
  
    $neighbors = facing_voxels($k) ;
  
    foreach ($neighbors as $n) {
      # echo " neighbor: $n" ;
      list($nx, $ny, $nz) = coords($n) ;

      if (is_water($nx, $ny, $nz)) {
        # echo " is water\n";
        $area++ ;
      } else if (is_lava($nx, $ny, $nz)) {
        # echo " is lava\n";
      } else {
        # echo " is neither\n";
      }
    }
  }

  return $area ;
}

$OutsideSurfaceArea = outside_surface_area() ;

echo "OutsideSurfaceArea: $OutsideSurfaceArea\n"



?>
