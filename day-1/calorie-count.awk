#!/usr/bin/awk -f


BEGIN {
  current_count = 0 ;
}

NF == 0 {
  print current_count ;
  current_count = 0 ;
  next ;
}

{ current_count += $0 }


