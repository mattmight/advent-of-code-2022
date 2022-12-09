#!/bin/bash

# This script will consume movement commands on stdin,
# and output successive locations of the tail, one per line


# A constant for the length of the rope as number of knots:
declare -i knots
knots=$1


# Keep track of the location of all the knots:
declare -a kx # an array of integers: kx[i] is x coord of i'th knot
declare -a ky # an array of integers: ky[i] is y coord of i'th knot

# Initialize the location of all knots to (0,0):
for i in $(seq 1 1 $knots)
do
  kx[$i]=0
  ky[$i]=0
done

# (kx[1],      ky[1])      is the location of the head
# (kx[$knots], ky[$knots]) is the location of the tail


# Create a tail trail log to record locations of the tail:
echo 0,0


# handleYank moves a knot (if needed) toward the previous knot:
function handleYank {

  # The index in the rope which we're "yanking":
  declare -i i
  i=$1

  # displacement between this knot and the previous:
  declare -i dx
  declare -i dy

  # "normalized" deltas -- will always be -1, 0 or 1:
  declare -i ndx
  declare -i ndy


  # Make the prior knot the "head"
  declare -i hx
  declare -i hy
  hx=${kx[$i-1]}
  hy=${ky[$i-1]}

  # Make the current knot the "tail"
  declare -i tx
  declare -i ty
  tx=${kx[$i]}
  ty=${ky[$i]}

  # Displacement between head and tail:
  dx=$hx-$tx
  dy=$hy-$ty

  case $dx in 
    -2) ndx=-1 ;;
    -1) ndx=-1 ;;
    0)  ndx=0  ;;
    1)  ndx=1  ;;
    2)  ndx=1  ;;
  esac

  case $dy in 
    -2) ndy=-1 ;;
    -1) ndy=-1 ;;
    0)  ndy=0  ;;
    1)  ndy=1  ;;
    2)  ndy=1  ;;
  esac

  # Move the tail based on the relative location of the head:
  case $dx,$dy in

    # overlapping -- do nothing:
    0,0) :          ;;
 
    # directly adjacent -- do nothing:
    1,1) :          ;;
    1,0) :          ;;
    1,-1) :         ;;

    0,1) :          ;;
    0,0) :          ;;
    0,-1) :         ;;

    -1,1) :         ;;
    -1,0) :         ;;
    -1,-1) :        ;;

    # two spaces away; move one closer:
    0,*) ty=ty+$ndy ;;
    *,0) tx=tx+$ndx ;;

    # more than two away on a diagonal, move diagonally:
    *) ty=ty+$ndy; 
       tx=tx+$ndx   ;;

  esac

  # Update this index in the rope with its new location:
  kx[$i]=$tx
  ky[$i]=$ty


  # Check if this was the last knot (the tail):
  if [[ $knots == $i ]];
  then
    # Mark this location for the tail in the log:
    echo $tx,$ty 
  fi

}

# Process an individual movement command:
function processMove {

  # Direction of the move:
  dir=$1

  # Amount of spaces to move:
  declare -i amount 
  amount=$2

  # The direction of the move:
  declare -i dx
  declare -i dy

  dx=0
  dy=0

  # Repeat the movement $amount times:
  for i in $(seq 1 1 $amount)
  do

    # Figure out the direction:
    case $dir in

      U) dy=1  ;;
      D) dy=-1 ;;
      L) dx=-1 ;;
      R) dx=1  ;;

    esac

    # Move the head by (dx,dy)
    let hx=${kx[1]}+$dx
    let hy=${ky[1]}+$dy
    kx[1]=$hx
    ky[1]=$hy

    # Move each knot in the rope, one by one, from front to back:
    for i in $(seq 2 1 $knots)
    do
      handleYank $i
    done

  done

  # echo "moved head to (${kx[1]},${ky[1]})"
  # echo "moved tail to (${kx[$knots]},${ky[$knots]})"
 

}

# Read each movement command, line by line:
while read line; 
do
  processMove $line
done

