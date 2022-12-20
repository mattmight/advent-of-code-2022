import { readFileSync } from 'fs' ;


// A helper to turn strings into numbers:
function string2number(str : string) : number {
  return parseInt(str);
}

// Grab the input from the file:
let rawInput : string = readFileSync('input.txt', 'utf-8') ;

let stringInput : string[] = rawInput.split('\n') ;

if (stringInput[stringInput.length-1] == "") {
  stringInput.pop() ;
}


// A doubly linked linked to hold all of the elements:
class MixItem {

  value: number ;
  length : number ;

  isHead : boolean ;

  prev: MixItem ;
  next: MixItem ;


  constructor (value : number, length : number) {
    this.value = value ;
    this.length = length ;
    this.isHead = false ;
  }

  // To render this as a string:
  asStringUntil(start : MixItem) : string {
    if (this == start)
      return "" ;

    let rest : string ;

    if (start == null) {
      rest = this.next.asStringUntil(this) ;
    } else {
      rest = this.next.asStringUntil(start) ;
    }

    return this.value + ",\n" + rest ;
  }

  // To find the zero element:
  findZero() : MixItem {
    if (this.value == 0) {
      return this;
    } else {
      return this.next.findZero() ;
    }
  }

  // To return the nth element after this one:
  nth(index : number) : MixItem {
    if (index == 0)
      return this;
    else 
      return this.next.nth(index-1) ;
  }

  // Moves this node forward by one:
  moveForward() {
    let A : MixItem = this ;      // Move A forward
    let B : MixItem = this.next ;
    let preA : MixItem = A.prev ;
    let postB : MixItem = B.next ;
 
    // Swap A and B:
    preA.next = B ;
    A.next = postB
    A.prev = B ;

    B.next = A ;
    B.prev = preA ;

    postB.prev = A ;

    // Check to see if the head of the list changed:
    if (A.isHead) {
      A.isHead = false ;
      B.isHead = true ;
    }
  }

  // Moves this node backward by one:
  moveBackward() {
    let A : MixItem = this.prev ;
    let B : MixItem = this ;        // Move B backward
    let preA : MixItem = A.prev ;
    let postB : MixItem = B.next ;
 
    // Swap A and B:
    preA.next = B ;
    A.next = postB
    A.prev = B ;

    B.next = A ;
    B.prev = preA ;

    postB.prev = A ;

    // Check to see if the head of the list changed:
    if (B.isHead) {
      postB.isHead = true ;
      B.isHead = false ;
    }

  }

  toString(): string {
    if (this.isHead) 
      return this.asStringUntil(null) ;
    else
      return this.next.toString(); 
  }
}


// Processes the input as an array of numbers to prepare for mixing:
function processInputArray(input : number[]) {

  MixItems = new Array(input.length) ;

  for (let i = 0; i < input.length; ++i) {
    let mitem : MixItem = new MixItem(input[i],input.length) ;

    MixItems[i] = mitem;
    
    if (i > 0) {
      mitem.prev = MixItems[i-1] ;
      mitem.prev.next = mitem ;
    }
  }

  MixItems[0].isHead = true ;

  MixItems[MixItems.length-1].next = MixItems[0] ;
  MixItems[0].prev = MixItems[MixItems.length-1] ;

} 


// Convert the original input into an array of numbers:
let input : number[] = stringInput.map(string2number) ;

// An array to hold all of the original items in their original order:
let MixItems : MixItem[] ;

// Load up MixItems:
processInputArray(input) ;


// Perform the mixing operation:
function mixItUp () {
  for (let i = 0; i < MixItems.length; ++i ) {

    let mitem = MixItems[i] ;

    let count = mitem.value ;

    // For large values, save time by looking at moves mod 
    // the number of items in the list not counting this one:
    count = count % (MixItems.length-1) ;

    // Move the element, one hop at a time:
    if (count < 0) {
      while (count != 0) {
        mitem.moveBackward() ;
        count++ ;
      }
    } else if (count > 0) {
      while (count != 0) {
        mitem.moveForward() ;
        count-- ;
      }
    }
  }

}

// Perform the decryption:
mixItUp() ;

// Print out the answer for Part 1:
let zero : MixItem = MixItems[0].findZero() ;

let n1000 = zero.nth(1000).value ;
let n2000 = zero.nth(2000).value ;
let n3000 = zero.nth(3000).value ;

console.log("1000: " + n1000) ;
console.log("2000: " + n2000) ; 
console.log("3000: " + n3000) ;

console.log("Part 1: " + (n1000 + n2000 + n3000)) ;




// Reset everything for part 2:

input = stringInput.map(string2number) ;
processInputArray(input) ;


// Now multiply every value by the key:

const Key = 811589153 ;

for (let i = 0; i < MixItems.length; ++i) {
  MixItems[i].value = MixItems[i].value * Key ;
}


// Mix 10 times:
for (let i = 0; i < 10; ++i) {
  mixItUp() ;
}

// Print out part 2:
zero = MixItems[0].findZero() ;

n1000 = zero.nth(1000).value ;
n2000 = zero.nth(2000).value ;
n3000 = zero.nth(3000).value ;

console.log("1000: " + n1000) ;
console.log("2000: " + n2000) ; 
console.log("3000: " + n3000) ;

console.log("Part 2: " + (n1000 + n2000 + n3000)) ;


