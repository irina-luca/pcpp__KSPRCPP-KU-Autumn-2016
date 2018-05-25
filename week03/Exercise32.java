// Week 3
import java.util.Arrays;
import java.util.function.Function;
import java.lang.Math;

public class Exercise32 {
  public static void main(String[] args) {
    // Exercise 3.2.1
    final int N = 10_000_001;
    int[] binary = new int[N];
    Arrays.parallelSetAll(binary, x -> apply(x));    
    // Exercise 3.2.1: TEST
    for(int i = 0; i < 10; i++) System.out.println(binary[i]);
    
    System.out.println("=======================================");    
    // Exercise 3.2.2
    Arrays.parallelPrefix(binary, (left, current) -> computePrefixSums(left, current));
    // Exercise 3.2.2: TEST
    for(int i = 0; i < 10; i++) System.out.println(binary[i]);
    System.out.println(binary[N - 1]);
    
    System.out.println("=======================================");    
    // Exercise 3.2.3
    for(int i = N/10; i < N; i += N/10) {
      System.out.println(binary[i] * Math.log(i) / i);
    }
    
  }

  
  // Exercise 3.2.1
  public static int apply( int x )
  {
    if(isPrime(x)) return 1;
      else return 0;
  }
  // Exercise 3.2.2
  public static int computePrefixSums( int left, int current )
  {
    return left + current;
  }

  private static boolean isPrime(int n) {
    int k = 2;
    while (k * k <= n && n % k != 0)
      k++;
    return n >= 2 && k * k > n;
  }
}