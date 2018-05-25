// Week 3
import java.util.Arrays;
import java.util.function.Function;
import java.lang.Math;
import java.util.stream.Stream;
import java.util.stream.IntStream;
import java.util.stream.DoubleStream;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.DoubleAdder;

public class Exercise34 {
  public static void main(String[] args) {
    final int N = 999_999_999;

    // Exercise 3.4.1
    System.out.println("Exercise 3.4.1");
    DoubleStream sDoubleNP = IntStream.range(1, N).mapToDouble(i -> 1.0 / i);  
    long startNP = System.nanoTime();
    System.out.printf("Sum: %20.16f%n", sDoubleNP.sum()); // Time => 9.491756342s
    long endNP = System.nanoTime();
    long elapsedTimeNP = endNP - startNP;
    System.out.println("Time: " + (double)elapsedTimeNP / 1000000000.0);

    // Exercise 3.4.2
    System.out.println("Exercise 3.4.2");
    DoubleStream sDoubleP = IntStream.range(1, N).mapToDouble(i -> 1.0 / i);  
    long startP = System.nanoTime();
    System.out.printf("Sum: %20.16f%n", sDoubleP.parallel().sum());
    long endP = System.nanoTime();
    long elapsedTimeP = endP - startP;
    System.out.println("Time: " + (double)elapsedTimeP / 1000000000.0); // Time => 1.774106279s
    
    // Exercise 3.4.3
    System.out.println("Exercise 3.4.3");
    double sumSeq = 0.0;
    for(int i = 1; i < N; i++) {
      sumSeq += 1.0 / i;
    }
    System.out.printf("Sum: %20.16f%n", sumSeq);
    // *** Computer floating-point addition is not associative => the order of summation matters
    
    // Exercise 3.4.4
    System.out.println("Exercise 3.4.4");
    Supplier s = new Supplier();
    // Method 1
    DoubleStream sDoubleGenerate_1 = makeFiniteStream(N, s);
    long sTime_344 = System.nanoTime();
    System.out.printf("Sum: %20.16f%n", sDoubleGenerate_1.sum());
    long eTime_344 = System.nanoTime();
    long elapsedTime_344 = eTime_344 - sTime_344;
    System.out.println("Time: " + (double)elapsedTime_344 / 1000000000.0);

    // Method 2
    // DoubleStream sDoubleGenerate_2 = doubles(N);
    // System.out.printf("Sum: %20.16f%n", sequentialSum(sDoubleGenerate_2));
    // sDoubleGenerate_1.forEach(System.out::println);
    // sDoubleGenerate_2.forEach(System.out::println);

    // Exercise 3.4.5
    System.out.println("Exercise 3.4.5");
    double sumDGenerateP = DoubleStream.generate(() -> 1.0/s.get()).limit(N).parallel().sum();
    long sTime_345 = System.nanoTime();
    System.out.printf("Sum: %20.16f%n", sumDGenerateP);
    long eTime_345 = System.nanoTime();
    long elapsedTime_345 = eTime_345 - sTime_345;
    System.out.println("Time: " + (double)elapsedTime_345 / 1000000000.0);

    // Results were as follows:
    // - 1st run => Sum:   0.9357837523290414, Time: 0.00114269
    // - 2nd run => Sum:   0.9365990582314885, Time: 0.001258156
    // - 3rd run => Sum:   0.9360718500418981, Time: 0.001010544
    // - 4th run => Sum:   0.9401374138184023, Time: 0.001299211
    // As an explanation for getting both sum and time different when parallelizing, accessing n from the Supplier class as its mutable state without synchronization leads to parallel streams mutating it and givig unreasonable results.
  }

  public static DoubleStream doubles(int n) {
    DoubleStream.Builder isb = DoubleStream.builder();
    int count = 1;
    while (count <= n) {
      isb.accept(1.0/count);
      count++;
    }
  
    return isb.build();
  }

  public static DoubleStream makeFiniteStream(int k, Supplier s) {
    return DoubleStream.generate(() -> 1.0/s.get()).limit(k);
  }

  public static double sequentialSum(DoubleStream s) {
    return s.sequential().sum();
  }


  
}

class Supplier {
    private int n = 1;

    public int get() {
        return n++;
    }

}