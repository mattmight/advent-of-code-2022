#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <limits.h>


// Compute the sack priority according to the rules for the value of each character:
int sack_priority (char c) {
  if (c >= 'a') 
    return (c - 'a') + 1;

  return (c - 'A') + 27 ;
}


int main (int arg, char* argv[]) {

  // Prepare to store each line of input:
  size_t buf_length = 30 ;
  ssize_t line_len ;
  char* line_buf = (char*)malloc(buf_length * sizeof(char)) ;

  if (!line_buf) {
    printf("Out of memory!") ;
    exit(-1) ;
  }


  // Keep track of part 1 score:
  unsigned int total_priority = 0 ;

  // Keep track of part 2 score:
  unsigned int total_badge_priority = 0 ;


  // Tracks elf number for each group of three -- 0, 1 or 2:
  unsigned int elf_number = 0 ;

  // A place to hold the badge once discovered:
  char found_badge ;


  // Grab the next line of input:
  while ((line_len = getline(&line_buf, &buf_length, stdin)) > 0) {

    // Cut off the newline at the end of each line:
    size_t sack_size = line_len-1 ;
    line_buf[sack_size] = 0 ;

    // Get the size of each compartment:
    size_t compartment_size = sack_size / 2;

    // Create a table to track characters seen in the first compartment:
    bool has_been_seen[CHAR_MAX] ; 
    memset(has_been_seen, 0, sizeof(has_been_seen)) ;

     
    // For part 2, for each character, we'll use a bitmap to 
    // track which elf has seen it in their sack within each
    // group of three.

    // For example:
    // 000 = no elf has yet seen it
    // 001 = elf 0 has seen it
    // 010 = elf 1 has seen it
    // 101 = elf 2 and elf 0 have seen it
    const unsigned int ELF0 = 1 << 0 ;             // 001
    const unsigned int ELF1 = 1 << 1 ;             // 010
    const unsigned int ELF2 = 1 << 2 ;             // 100
    const unsigned int ALL3 = ELF0 | ELF1 | ELF2 ; // 111

    // A table indexed by characters tracks which elf has seen each character:
    unsigned int elfs_with[CHAR_MAX] ;


    if (elf_number == 0) {
      // This is the first elf in this group of three,
      // so reset the bitmaps of which elf has 
      // seen which chars:
      memset(elfs_with, 0, sizeof(elfs_with)) ;

      // Set the found badge back to bogus:
      found_badge = '@' ; // Bogus value, so we should never see this.
    }

    // Find the bitmask for the current elf:
    unsigned int elf_bit = 1 << elf_number ;

    // Keep track of the current item location in the sack:
    unsigned int index = 0 ;

    // A place to hold the sack_type once discovered:
    char sack_type = '!' ; // Bogus value, so we should never see this.


    // Loop over all items in the first compartment:
    while (index < compartment_size) {

      // Grab the next item from the first compartment:
      char item = line_buf[index] ;

      // Mark this item as seen:
      has_been_seen[item] = true ;

      // Set the seen bit for current elf, and
      // check to see if all 3 have now seen it:
      if ((elfs_with[item] |= elf_bit) == ALL3) {
        // All three elves have this, so it's the badge:
        found_badge = item ;
      }

      ++index ;
    }


    while (index < sack_size) {

      // Grab the next item from the second compartment:
      char item = line_buf[index] ;

      // Check to see if this was witnessed in the first compartment:
      if (has_been_seen[item]) {
        // This was in the first comparment; we found the sack type:
        sack_type = item ;
      }

      // Set the seen bit for current elf, and
      // check to see if all 3 have now seen it:
      if ((elfs_with[item] |= elf_bit) == ALL3) {
        // All three elves have this, so it's the badge:
        found_badge = item ;
      }

      ++index; 
    }


    int priority = sack_priority(sack_type) ;

    total_priority += priority ;

    // Useful during debugging:
    // printf("%c(%i): '%s', %zu, %zu\n",sack_type,priority,line_buf,compartment_size,line_len);


    // Check if this was the last elf in this group of three:
    if (elf_number == 2) {

      // Useful during debugging:
      // printf("badge found: %c\n", found_badge);

      // This was the last elf for this group; check for the badge.
      total_badge_priority += sack_priority(found_badge) ;
    }

    // Move to the next elf in the group for the next iteration:
    elf_number = (elf_number + 1) % 3 ;
  }

  printf("Part 1 total priority: %u\n", total_priority) ;

  printf("Part 2 total badge priority: %u\n", total_badge_priority) ;

  
  // Clean up:
  free(line_buf) ; 

  return EXIT_SUCCESS ;
}
