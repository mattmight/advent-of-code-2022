
// Node.js library for file IO:
const fs = require('fs') ;

// Read in the input:
var input_string = fs.readFileSync('input.txt','utf8') ;

// Massage it into a legal JavaScript literal:
input_string = "[" + input_string.replaceAll(']\n','],\n') + "]" ;

// Convert it to a JavaScript value:
var input = eval(input_string) ;

// Make a copy of the input for part 2:
var input_copy = input.slice(0) ;




// Compare trees of variable degree, represented as nested lists:
function tree_compare(left, right) {

  // If both are numbers, compare the numbers:
  if (Number.isSafeInteger(left) && Number.isSafeInteger(right))
    return left - right ;

  // If only the left is a number, promote it to a list and try again:
  if (Number.isSafeInteger(left)) {
    return tree_compare([left], right) ;
  }

  // If only the right is a number, promote it to a list and try again:
  if (Number.isSafeInteger(right)) {
    return tree_compare(left,[right]) ;
  }

  // Both are lists.

  // If both are [], then they are equal.
  if ((left.length == 0) && (right.length == 0))
    return 0 ;

  // If the left is empty, the left is lesser:
  if (left.length == 0)
    return -1 ;

  // If the right is empty, the right is lesser:
  if (right.length == 0)
    return 1 ;

  // Both must be lists, and neither can be empty. 

  // Compare the first element in each:
  var first_comp = tree_compare(left[0],right[0]) ;

  if (first_comp < 0) return first_comp ;
  if (first_comp > 0) return first_comp ;

  // The first elements are equal.

  // Compare the remainder of the lists:
  return tree_compare(left.slice(1), right.slice(1)) ;
}


// Track the current index while moving through all the pairs:
var currentIndex = 1 ;

// A running sum of the indices in which the pairs are in the right order:
var rightOrderSum = 0 ;

// Conduct the next pair-wise comparison:
function compareNext() {
  // console.log("--") ;

  var lhs = input.shift() ;
  var rhs = input.shift() ;

  // console.log(lhs);
  // console.log(rhs);

  var cmp = tree_compare(lhs, rhs) ;

  // console.log(cmp) ;

  if (cmp < 0)
    rightOrderSum += currentIndex ;

  currentIndex++ ;
}

// Run through the entire input, comparing each pair:
while (input.length > 0) {
  compareNext() ;
}

// Output part 1:
console.log("Part 1 answer: " + rightOrderSum) ;

var two_val = [[2]] ;
var six_val = [[6]] ;

// Add these two into the input:
input_copy.push( two_val, six_val ) ;

// Sort the input under the tree ordering:
input_copy.sort(tree_compare) ;

// console.log(input_copy) ;

// Find where the decoder values landed:
var idx1 = input_copy.indexOf(two_val) ;
var idx2 = input_copy.indexOf(six_val) ;

console.log("Part 2, decoder key: " + (idx1+1) * (idx2+1)) ;
