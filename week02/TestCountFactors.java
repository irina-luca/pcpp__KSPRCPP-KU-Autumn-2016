// For week 2
// sestoft@itu.dk * 2014-08-29

import java.util.concurrent.atomic.*;

class TestCountFactors {
  public static void main(String[] args) {
    // Initial
    // final int range = 5_000_000;
    // int count = 0;
    // long startTime = System.currentTimeMillis();
    // for (int p=0; p<range; p++)
    //   count += countFactors(p);
    // long endTime = System.currentTimeMillis();
    // long elapsedTime = endTime - startTime;
    // System.out.printf("Total number of factors is %9d%n", count);
    // System.out.printf("Elapsed time is %d", elapsedTime);

    // Exercise 2.1, 3
    // final int range = 5_000_000;
    // MyAtomicInteger count = new MyAtomicInteger(0);
    // int threadCount = 10;
    // int perThread = 500_000;
    // Thread[] threads = new Thread[threadCount];
    // long startTime = System.currentTimeMillis();
    // for(int t = 0; t < threadCount; t++) {
    //   final int from = perThread * t,
    //   to = (t + 1 == threadCount) ? range : perThread * (t + 1);
    //   threads[t] = new Thread(() -> {
    //     for(int i = from; i < to; i++) {
    //       count.addAndGet(countFactors(i));
    //     }
    //   });
    // }

    // for(int t = 0; t < threadCount; t++)
    //   threads[t].start();
    
    // for(int t = 0; t < threadCount; t++)
    //   try { threads[t].join(); } catch (InterruptedException exn) { }

    // long endTime = System.currentTimeMillis();
    // long elapsedTime = endTime - startTime;

    // System.out.printf("Total number of factors is %9d%n", count.get());
    // System.out.printf("Elapsed time is %d", elapsedTime);


    // Exercise 2.1, 5
    final int range = 5_000_000;
    AtomicInteger count = new AtomicInteger(0);
    int threadCount = 10;
    int perThread = 500_000;
    Thread[] threads = new Thread[threadCount];
    long startTime = System.currentTimeMillis();
    for(int t = 0; t < threadCount; t++) {
      final int from = perThread * t,
      to = (t + 1 == threadCount) ? range : perThread * (t + 1);
      threads[t] = new Thread(() -> {
        for(int i = from; i < to; i++) {
          count.addAndGet(countFactors(i));
        }
      });
    }

    for(int t = 0; t < threadCount; t++)
      threads[t].start();
    
    for(int t = 0; t < threadCount; t++)
      try { threads[t].join(); } catch (InterruptedException exn) { }

    long endTime = System.currentTimeMillis();
    long elapsedTime = endTime - startTime;

    System.out.printf("AtomicInteger use case: Total number of factors is %9d%n", count.get());
    System.out.printf("AtomicInteger use case: Elapsed time is %d", elapsedTime);
  }

  public static int countFactors(int p) {
    if (p < 2) 
      return 0;
    int factorCount = 1, k = 2;
    while (p >= k * k) {
      if (p % k == 0) {
      	factorCount++;
      	p /= k;
      } else 
      	k++;
    }
    return factorCount;
  }
}

class MyAtomicInteger { // must be a thread-safe integer 
  private int value;

  public MyAtomicInteger(int value) {
    this.value = value;
  }
  public synchronized int addAndGet(int amount) {
    value += amount;
    return value;
  }
  public synchronized int get() {
    return value;
  }
}