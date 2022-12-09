#!/bin/bash

# Declare location of head and tail as integer variables:
declare -i hx
declare -i hy

declare -i tx
declare -i ty

# Both head and tail start at the origin:
hx=0
hy=0

tx=0
ty=0

# Create a trail log:
echo 0,0 > trail.log

function handleYank {
  declare -i dx
  declare -i dy

  # "normalized" deltas -- will always be -1, 0 or 1:
  declare -i ndx
  declare -i ndy

  dx=$1
  dy=$2

  echo "yank: $dx,$dy"

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

  echo "normalized: $ndx,$ndy; regular: $dx,$dy"


  case $dx,$dy in

    # overlapping -- do nothing:
    0,0) echo "overlap"         ;;     

    # directly adjacent -- do nothing:
    1,1) :         ;;
    1,0) echo "right"         ;;
    1,-1) :        ;;

    0,1) :         ;;
    0,0) :         ;;
    0,-1) :        ;;

    -1,1) :        ;;
    -1,0) :        ;;
    -1,-1) :       ;;

    # two spaces away; move one closer:
    0,*) echo "moving ty $tx by $ndx"; ty=ty+$ndy ;;
    *,0) echo "moving tx $ty by $ndy"; tx=tx+$ndx ;;

    # more than two away on a diagonal, move diagonally:
    *) echo "crashed through"; ty=ty+$ndy; 
       tx=tx+$ndx   ;;

  esac

  # Mark this location for the tail in the log:
  echo $tx,$ty >> trail.log


  echo "done yanking"
}

function processMove {

  declare -i amount 

  declare -i dx
  declare -i dy

  dir=$1
  amount=$2

  echo "dir: $dir; amount: $amount"

  for i in $(seq 1 1 $amount)
  do

    echo " moving $dir"
    case $dir in

      U) hy=hy+1 ;;

      D) hy=hy-1 ;;

      L) hx=hx-1 ;;

      R) hx=hx+1 ;;

    esac

    dx=hx-tx
    dy=hy-ty

    handleYank $dx $dy

  done

  echo "moved head to ($hx,$hy)"
  echo "moved tail to ($tx,$ty)"
 

}

while read line; 
do
  processMove $line
done

