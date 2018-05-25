// For week 2
// sestoft@itu.dk * 2014-09-04

import java.util.concurrent.atomic.*;

class SimpleHistogram {
  public static void main(String[] args) {
    // final Histogram histogram = new Histogram1(30);
    // histogram.increment(7);
    // histogram.increment(13);
    // histogram.increment(7);
    // dump(histogram);
    
    // Exercise 2.3, 2
    final int range = 5_000_000;
    final Histogram h2 = new Histogram2(30);
    int perThread = 500_000;
    int threadCount = 10;
    Thread[] threads = new Thread[threadCount];
    long startTime = System.currentTimeMillis();
    for(int t = 0; t < threadCount; t++) {
      final int from = perThread * t,
                to = (t + 1 == threadCount) ? range : perThread * (t + 1);
      threads[t] = new Thread(() -> {
        for(int i = from; i < to; i++) {
          h2.increment(countFactors(i));
        }
      });
    }

    for(int t = 0; t < threadCount; t++) 
      threads[t].start();

    for(int t = 0; t < threadCount; t++) 
      try { threads[t].join(); } catch (InterruptedException exn) { }
    long endTime = System.currentTimeMillis();
    long elapsedTime = endTime - startTime;
    System.out.printf("Histogram2: Elapsed time is %d", elapsedTime);
    dump(h2);

    // Exercise 2.3, 3
    // final int range = 5_000_000;
    // final Histogram h3 = new Histogram3(30);
    // int perThread = 500_000;
    // int threadCount = 10;
    // Thread[] threads = new Thread[threadCount];
    // long startTime = System.currentTimeMillis();
    // for(int t = 0; t < threadCount; t++) {
    //   final int from = perThread * t,
    //             to = (t + 1 == threadCount) ? range : perThread * (t + 1);
    //   threads[t] = new Thread(() -> {
    //     for(int i = from; i < to; i++) {
    //       h3.increment(countFactors(i));
    //     }
    //   });
    // }

    // for(int t = 0; t < threadCount; t++) 
    //   threads[t].start();

    // for(int t = 0; t < threadCount; t++) 
    //   try { threads[t].join(); } catch (InterruptedException exn) { }
    // long endTime = System.currentTimeMillis();
    // long elapsedTime = endTime - startTime;
    // System.out.printf("Histogram3: Elapsed time is %d", elapsedTime);
    // dump(h3);

    // Exercise 2.3, 4
    // final int range = 5_000_000;
    // final Histogram h4 = new Histogram4(30);
    // int perThread = 500_000;
    // int threadCount = 10;
    // Thread[] threads = new Thread[threadCount];
    // long startTime = System.currentTimeMillis();
    // for(int t = 0; t < threadCount; t++) {
    //   final int from = perThread * t,
    //             to = (t + 1 == threadCount) ? range : perThread * (t + 1);
    //   threads[t] = new Thread(() -> {
    //     for(int i = from; i < to; i++) {
    //       h4.increment(countFactors(i));
    //     }
    //   });
    // }

    // for(int t = 0; t < threadCount; t++) 
    //   threads[t].start();

    // for(int t = 0; t < threadCount; t++) 
    //   try { threads[t].join(); } catch (InterruptedException exn) { }
    // long endTime = System.currentTimeMillis();
    // long elapsedTime = endTime - startTime;
    // System.out.printf("Histogram4: Elapsed time is %d", elapsedTime);
    // dump(h4);
    
    // Exercise 2.3, 6
    // final int range = 5_000_000;
    // final Histogram h5 = new Histogram5(30);
    // int perThread = 500_000;
    // int threadCount = 10;
    // Thread[] threads = new Thread[threadCount];
    // long startTime = System.currentTimeMillis();
    // for(int t = 0; t < threadCount; t++) {
    //   final int from = perThread * t,
    //             to = (t + 1 == threadCount) ? range : perThread * (t + 1);
    //   threads[t] = new Thread(() -> {
    //     for(int i = from; i < to; i++) {
    //       h5.increment(countFactors(i));
    //     }
    //   });
    // }

    // for(int t = 0; t < threadCount; t++) 
    //   threads[t].start();

    // for(int t = 0; t < threadCount; t++) 
    //   try { threads[t].join(); } catch (InterruptedException exn) { }
    // long endTime = System.currentTimeMillis();
    // long elapsedTime = endTime - startTime;
    // System.out.printf("Histogram5: Elapsed time is %d", elapsedTime);
    // dump(h5);
  }

  public static void dump(Histogram histogram) {
    int totalCount = 0;
    for (int bin=0; bin<histogram.getSpan(); bin++) {
      System.out.printf("%4d: %9d%n", bin, histogram.getCount(bin));
      totalCount += histogram.getCount(bin);
    }
    System.out.printf("      %9d%n", totalCount);
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

interface Histogram {
  public void increment(int bin);
  public int getCount(int bin);
  public int getSpan();
  public int[] getBins();
}

class Histogram1 implements Histogram {
  private int[] counts;
  public Histogram1(int span) {
    this.counts = new int[span];
  }
  public void increment(int bin) {
    counts[bin] = counts[bin] + 1;
  }
  public int getCount(int bin) {
    return counts[bin];
  }
  public int getSpan() {
    return counts.length;
  }
  public int[] getBins() {
    int[] bins = new int[getSpan()];
    for(int i = 0; i < getSpan(); i++) {
      bins[i] = getCount(i);
    }
    return bins;
  }
}

class Histogram2 implements Histogram {
  private final int[] counts;
  public Histogram2(int span) {
    this.counts = new int[span];
  }
  public synchronized void increment(int bin) {
    counts[bin] = counts[bin] + 1;
  }
  public synchronized int getCount(int bin) {
    return counts[bin];
  }
  public int getSpan() {
    return counts.length;
  }
  public synchronized int[] getBins() {
    int[] bins = new int[getSpan()];
    for(int i = 0; i < getSpan(); i++) {
      bins[i] = getCount(i);
    }
    return bins;
  }
}

class Histogram3 implements Histogram {
  private final AtomicInteger[] counts;
  public Histogram3(int span) {
    this.counts = new AtomicInteger[span];
  }
  public void increment(int bin) {
    if(counts[bin] == null) counts[bin] = new AtomicInteger();
    counts[bin].incrementAndGet();
  }
  public int getCount(int bin) {
    if(counts[bin] == null) return 0;
    return counts[bin].get();
  }
  public int getSpan() {
    return counts.length;
  }
  public int[] getBins() {
    int[] bins = new int[getSpan()];
    synchronized (this) {
      for(int i = 0; i < getSpan(); i++) {
        bins[i] = getCount(i);
      }      
    }
    return bins;
  }
}

class Histogram4 implements Histogram {
  private final AtomicIntegerArray counts;
  public Histogram4(int span) {
    this.counts = new AtomicIntegerArray(span);
  }
  public void increment(int bin) {
    counts.incrementAndGet(bin);
  }
  public int getCount(int bin) {
    return counts.get(bin);
  }
  public int getSpan() {
    return counts.length();
  }
  public int[] getBins() {
    int[] bins = new int[getSpan()];
    synchronized (this) {
      for(int i = 0; i < getSpan(); i++) {
        bins[i] = getCount(i);
      }      
    }
    return bins;
  }
}

class Histogram5 implements Histogram {
  private final LongAdder[] counts;
  public Histogram5(int span) {
    this.counts = new LongAdder[span];
    for(int i = 0; i < span; i++) 
      counts[i] = new LongAdder();
  }
  public void increment(int bin) {
    counts[bin].increment();
  }
  public int getCount(int bin) {
    return counts[bin].intValue();
  }
  public int getSpan() {
    return counts.length;
  }
  public int[] getBins() {
    int[] bins = new int[getSpan()];
    synchronized (this) {
      for(int i = 0; i < getSpan(); i++) {
        bins[i] = getCount(i);
      }      
    }
    return bins;
  }
}
