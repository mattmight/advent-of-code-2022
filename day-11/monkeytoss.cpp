#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include <gmpxx.h>

#include <iostream>
#include <vector>

using namespace std;


// WARNING: unsigned long was barely big enough for part 1
typedef unsigned long item_t ;


// Common modulus for all item arithmetic in part 2:
item_t N ; 

// Part 2 rests on realizing that all of the divisors are prime

// Hence, one can compute multiplication and addition modulo 
// the product of all of the individual moduli to avoid integer overflow.


// Determine if the relief step is allowed (Part 1: yes, Part 2: no)
bool allow_relief = true ;


// A class to represent an individual Monkey:
class Monkey ;


// A vector that will store all of the monkeys; indexed by number:
vector<Monkey*> monkeys;




// An abstract base class for expressions:
class Expr {
  public:

  // Evaluate an expression given the old value:
  virtual item_t eval(item_t old) = 0 ; 
} ;


// A class for numeric constant expressions:
class NumExpr : public Expr {
  int value ;

  public:

  NumExpr(int value) {
    this->value = value ;
  } 

  item_t eval(item_t old) {
    (void)old;
    return value ;
  }
} ;


// A class for the variable old:
class OldExpr : public Expr {
  public:

  item_t eval(item_t old) {
    return old ;
  }
} ;


// A class to represent the sum of expressions:
class SumExpr : public Expr {
  Expr* left ;
  Expr* right ;

  public:

  SumExpr(Expr* left, Expr* right) {
    this->left = left ;
    this->right = right ;
  }

  item_t eval(item_t old) {
    return left->eval(old) + right->eval(old) ;
  }
} ;


// A class to represent the product of expressions:
class ProductExpr : public Expr {
  Expr* left ;
  Expr* right ;

  public:

  ProductExpr(Expr* left, Expr* right) {
    this->left = left ;
    this->right = right ;
  }


  item_t eval(item_t old) {
    return left->eval(old) * right->eval(old) ;
  }
} ;



// A class to represent a monkey:
class Monkey {

  public:

  // Monkey name 
  int name = 0 ;

  // Divisor for this monkey:
  int divisor = 0 ;

  // List of items in its possession:
  vector<item_t>* items ;

  // The operation to perform on each item during a round:
  Expr* operation ;

  // Where to throw if an item is divisible or not:
  int on_true ;
  int on_false ;

  // A counter for the total number of inspecions made:
  int inspections = 0 ;

  Monkey(int name, int divisor, vector<item_t>* items, Expr* operation, int on_true, int on_false) {

    this->inspections = 0 ;

    this->name = name ;
    this->divisor = divisor ;
    this->items = items ;
    this->operation = operation ;
    this->on_true = on_true ;
    this->on_false = on_false ;
  }
 
  // Takes an item from another monkey into its possession:
  void receive_item(item_t item) {
    items->push_back(item) ;
  }

  // Process all of the items, using part 1 logic:
  void process_items() {
    for (item_t old_worry : *items) {

      // Inspect this object:
      inspections++ ;

      // First do the new worry operation:
      item_t new_worry = operation->eval(old_worry) ;
      
      // Monkey gets bored; divide worry by 3:
      new_worry = new_worry / 3 ;

      // Check if new_worry is divisible by divisor:
      if ((new_worry % divisor) == 0) {
        monkeys[on_true]->receive_item(new_worry) ;
      } else {
        monkeys[on_false]->receive_item(new_worry) ;
      }

      // WARNING: This assumes a monkey never tosses to itself:
      items->clear() ;
    }
  }


  // Process all of the items, using part 2 logic:
  void process_items_part2() {
    for (item_t old_worry : *items) {

      // Inspect this object:
      inspections++ ;

      // First do the new worry operation mod N:
      item_t new_worry = operation->eval(old_worry) % N ;
      
      // Check if new_worry is divisible by divisor:
      if ((new_worry % divisor) == 0) {
        monkeys[on_true]->receive_item(new_worry) ;
      } else {
        monkeys[on_false]->receive_item(new_worry) ;
      }

      // WARNING: This assumes a monkey never tosses to itself:
      items->clear() ;
    }
  }



  // Prints the contents of this monkey:
  void print_monkey() {
    printf("Monkey %i: ", this->name) ;
    for (item_t i : *items) {
      cout << i << " ";
    }

    printf("; inspections = %i\n", inspections) ;
  }
} ;




// Mutably trim the whitespace off the right side of string:
void right_trim(char* str) {
  char* end = &str[strlen(str)-1];
  while (isspace(*end)) {
    end-- ;
  }
  end[1] = '\0';
}


// Parse <num> or `old` from a string:
Expr* parse_atom(char* atom) {
  if (strcmp(atom,"old") == 0)
    return new OldExpr() ;

  int num = atoi(atom) ;

  return new NumExpr(num) ;
}


// Parse a Monkey* from stdin:
Monkey* parse_monkey () {

  // Read the monkey code line by line:

  size_t buf_length = 30 ;
  ssize_t line_len ;
  char* line_buf = (char*)malloc(buf_length * sizeof(char)) ;

  if (!line_buf) {
    printf("out of memory") ;
    exit(-1) ;
  }

  printf("\n\n")  ;

  // LINE 1: Monkey <num>:
  line_len = getline(&line_buf, &buf_length, stdin) ;

  // Check for end of file or blank line.

  if (line_len <= 0) {
    // End of file; no monkeys left:
    return NULL ;
  }

  // Cut off whitespace on the end:
  right_trim(line_buf); 

  // Check for a blank line and skip it:
  if (strcmp(line_buf,"") == 0) {
    // Skip it:
    line_len = getline(&line_buf, &buf_length, stdin); 
  }

  // Make a copy of the string in the input buffer:
  char* line = strdup(line_buf) ;

  // Use space, comma and colon as delimiters:
  char delims[] = " ,\n:";

  // Start tokenizing:
  char* tok ;

  tok = strtok(line,delims) ; // eat Monkey
  line = tok + strlen(tok)+1 ;

  char* monkey_name = strtok(line,delims) ;
  int monkey_num = atoi(monkey_name) ;

  printf("monkey: %i\n", monkey_num) ;

 
  // LINE 2: Starting items: <num> { , <num> }  
  line_len = getline(&line_buf, &buf_length, stdin) ;
  line = strdup(line_buf) ;
  
  vector<item_t>* items = new vector<item_t>();

  tok = strtok(line,delims) ;  // eat "Starting"
  line = tok + strlen(tok)+1 ;

  tok = strtok(line,delims) ;  // eat "items:"
  line = tok + strlen(tok)+1 ;

  char* sfirst = strtok(line,delims) ;
  item_t first = atoi(sfirst);

  line = sfirst + strlen(sfirst)+1;

  items->push_back(first) ;
 
  char* next = NULL ;
  while ((next = strtok(line,delims)) != NULL) {
    items->push_back(atoi(next)) ;
    line = next + strlen(next) + 1 ;
  }

  for (item_t i : *items) {
    cout << " item: " << i << endl ;
  }


  // LINE 3: Operation: new = <expr> <op> <expr>
  line_len = getline(&line_buf, &buf_length, stdin) ;
  line = strdup(line_buf) ;
  

  tok = strtok(line,delims) ;  // eat "Operation"
  line = tok + strlen(tok)+1 ;

  tok = strtok(line,delims) ;  // eat "new"
  line = tok + strlen(tok)+1 ;

  tok = strtok(line,delims) ;  // eat "="
  line = tok + strlen(tok)+1 ;

  char* lhs ;
  char* rhs ;
  char* op ;

  Expr* left;
  Expr* right; 
  Expr* operation ;

  tok = strtok(line,delims) ;  // eat "<expr>"
  line = tok + strlen(tok)+1 ;
  lhs = tok ;

  tok = strtok(line,delims) ;  // eat "<op>"
  line = tok + strlen(tok)+1 ;
  op = tok ;

  tok = strtok(line,delims) ;  // eat "<expr>"
  line = tok + strlen(tok)+1 ;
  rhs = tok ;

  left = parse_atom(lhs) ; 
  right = parse_atom(rhs) ;

  if (strcmp(op,"+") == 0)
    operation = new SumExpr(left,right) ;

  else if (strcmp(op,"*") == 0)
    operation = new ProductExpr(left,right) ;

  else {
    printf("unknown operation: %s\n", op);
    exit(-1); 
  }



  // LINE 4: Test: divisible by <num>
  line_len = getline(&line_buf, &buf_length, stdin) ;
  line = strdup(line_buf) ;
  

  tok = strtok(line,delims) ;  // eat "Test"
  line = tok + strlen(tok)+1 ;

  tok = strtok(line,delims) ;  // eat "divisible"
  line = tok + strlen(tok)+1 ;

  tok = strtok(line,delims) ;  // eat "by"
  line = tok + strlen(tok)+1 ;

  
  tok = strtok(line,delims) ; // eat <num>
  int divisor = atoi(tok) ;

  printf("divisor is %i\n", divisor) ;


  // LINE 4: If true: throw to monkey <num>
  line_len = getline(&line_buf, &buf_length, stdin) ;
  line = strdup(line_buf) ;


  tok = strtok(line,delims) ;  // eat "If"
  line = tok + strlen(tok)+1 ;

  tok = strtok(line,delims) ;  // eat "true"
  line = tok + strlen(tok)+1 ;

  tok = strtok(line,delims) ;  // eat "throw"
  line = tok + strlen(tok)+1 ;

  tok = strtok(line,delims) ;  // eat "to"
  line = tok + strlen(tok)+1 ;

  tok = strtok(line,delims) ;  // eat "monkey"
  line = tok + strlen(tok)+1 ;

  tok = strtok(line,delims) ; // <num>
  int if_true = atoi(tok);

  printf("on true: %i\n", if_true) ;




  // LINE 4: If false: throw to monkey <num>
  line_len = getline(&line_buf, &buf_length, stdin) ;
  line = strdup(line_buf) ;


  tok = strtok(line,delims) ;  // eat "If"
  line = tok + strlen(tok)+1 ;

  tok = strtok(line,delims) ;  // eat "false"
  line = tok + strlen(tok)+1 ;

  tok = strtok(line,delims) ;  // eat "throw"
  line = tok + strlen(tok)+1 ;

  tok = strtok(line,delims) ;  // eat "to"
  line = tok + strlen(tok)+1 ;

  tok = strtok(line,delims) ;  // eat "monkey"
  line = tok + strlen(tok)+1 ;

  tok = strtok(line,delims) ; // <num>
  int if_false = atoi(tok);

  printf("on false: %i\n", if_false) ;


  Monkey* monkey = new Monkey(monkey_num, divisor, items, operation, if_true, if_false) ;


  return monkey ;
}


// Run through all of the monkeys, procesing all items 
// according to the logic of part 1 or part 2 as appropriate:
void run_round() {
  for (unsigned int i = 0; i < monkeys.size(); ++i) {
    Monkey* monkey = monkeys[i];
    if (allow_relief)
      monkey->process_items() ;
    else
      monkey->process_items_part2() ;
  }
}


// Print the state of all the monkeys:
void print_monkeys() {
  for (unsigned int i = 0; i < monkeys.size(); ++i) {
    Monkey* monkey = monkeys[i];
    monkey->print_monkey() ;
  }
}


// Compare two monkeys based on number of inspections:
bool compare_monkey(Monkey* left, Monkey* right) {
  return left->inspections > right->inspections ;
}



int main (int argc, char* argv[]) {
  (void) argc;
  (void) argv;

  // Number of rounds:
  int rounds = 20 ;
  allow_relief = true ;

  // If there as an argument, assume part 2:
  if (argc > 1)  {
    allow_relief = 0 ;
    rounds = 10000 ;
  }

 
  // Parse all monkeys:
  Monkey* monkey = NULL ;
  
  while ((monkey = parse_monkey()) != NULL) {
    monkeys.push_back(monkey) ;
  }

  // Find the modular basis, assuming all the divisors are prime:
  N = 1 ;
  for (Monkey* m : monkeys) {
    N = N * m->divisor ;
  }


  cout << "Common modulus, N = " << N << endl;
  cout << endl;

  // Print initial state:
  printf("initial state:\n") ;
  print_monkeys() ;

  // Run each round and print the result:
  for (int i = 0; i < rounds; ++i ) {
   run_round() ;
   printf("\nnext round:\n") ;
   print_monkeys() ;
  }

  // Sort the list by number of inspections:
  sort ( monkeys.begin(), monkeys.end(), compare_monkey ) ; 

  // Print the sorted list:
  cout << endl;
  cout << "sorted: " << endl ;
  print_monkeys() ;

  // Compute and print monkey business:
  printf("Monkey business: \n") ;

  // Need to use multiprecision arithmetic to avoid overflow:
  mpz_class a = monkeys[0]->inspections ;
  mpz_class b = monkeys[1]->inspections ;

  mpz_class monkey_business = a * b ;

  cout << monkey_business << endl;
 
  return EXIT_SUCCESS;
}
