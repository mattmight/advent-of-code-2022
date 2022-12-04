/*
 To abstract out and simplify interval logic, 
 we'll create a class Interval for handling closed intervals.
 */
class Interval {

  private int low ;
  private int high ;

  public Interval(int low, int high) {
    this.low = low ;
    this.high = high ;
  }


  // Checks if this interval *fully* contains another:
  public boolean contains(Interval B) {
    return (B.low >= this.low) &&
           (B.high <= this.high) ;
  }


  // Computes the intesection of two intervals as another Interval:
  public Interval intersectionWith(Interval B) {

    Interval A = this ;

    /* Scenarios:

     Case 1: A contains B:

     AAAAAAAA
       BBBBB


     Case 2: B contains A:

        AAAA
     BBBBBBBB

     
     Case 3: A and B are disjoint:

     AAAA
             BBBBBB
     
           or

     BBBBB
              AAAAAA

     Case 4: A overlaps B, with A higher than B
     
     
        AAAAAA
     BBBBBB


     Case 5: A overlaps B, with A lower than B

     AAAAAA
       BBBBBBBBB

     */

     // Case 1:
     if (A.contains(B)) {
       return B ;
     }

     // Case 2:
     if (B.contains(A)) {
       return A ;
     }

     // Case 3:
     if ( (B.low > A.high) || (A.low > B.high) )
       return new Interval(1,-1) ;

     // Case 4:
     if (B.low < A.low) {
       return new Interval(A.low, B.high) ;
     }

     // Case 5:
     if (A.low < B.low) {
       return new Interval(B.low, A.high) ;
     }


     throw new RuntimeException("Interval logic error") ;
  }

  // Checks if there is no number in the interval:
  public boolean isEmpty() {
    return this.low > this.high ;
  }

  public String toString() {
    return "[" + this.low +","+ this.high + "]" ;
  }

}

public class CleanUp {

  public static void main (String[] args) {

     // Track wholly contained intervals for part 1:
     int containmentCount = 0 ;

     // Track overlapping intervals for part 2:
     int overlapCount = 0 ;


     // WARNING: We're not going to verify that the input is in the correct format,
     // but this will work as long as the input is in the correct format.

     // For example, the delimiters could be inverted on a line -- "1,3-4,5" -- 
     // instead of "1-3,4-5" and it would still accept this.

     // Create a very simple scanner with '-', ',' and whitespace as delimiters:
     java.util.Scanner scanner = new java.util.Scanner(System.in).useDelimiter("[-,\\s]") ;


     while (scanner.hasNextInt()) {

       int low1 = -1, high1 = -1 ;
       int low2 = -1, high2 = -1 ;


       try {

           // Grab the next two integers for the first pair:
           low1  = scanner.nextInt() ;
           high1 = scanner.nextInt() ;

           // Grab the next two integers for the second pair:
           low2 = scanner.nextInt() ;
           high2 = scanner.nextInt() ;


       } catch (java.util.NoSuchElementException nse) {

         // Bogus input?
         System.err.println("Invalid input") ;
         break ;

       }

       System.out.println("low1: " + low1 + "; high1: " + high1) ;
       System.out.println("low2: " + low2 + "; high2: " + high2) ;

       Interval interval1 = new Interval(low1,high1) ;
       Interval interval2 = new Interval(low2,high2) ;
      
       // Check if one interval fully contains the other for part 1:
       if (interval1.contains(interval2) || interval2.contains(interval1)) {
         containmentCount++ ; 
         System.out.println(interval1 + " contains " + interval2) ;
       }


       // Check if the intervals overlap in any way for part 2:
       if (!interval1.intersectionWith(interval2).isEmpty()) {
         overlapCount++ ;
         System.out.println(interval1 + " overlaps " + interval2) ;
       }

       // Print the results:
       System.out.println("Part 1 count: " + containmentCount) ;
       System.out.println("Part 2 count: " + overlapCount) ;

     }

     
  } // end main

} // end CleanUp
