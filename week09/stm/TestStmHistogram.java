// For week 10
// sestoft@itu.dk * 2014-11-05, 2015-10-14

// Compile and run like this under Linux and MacOS:
//   javac -cp ~/lib/multiverse-core-0.7.0.jar TestStmHistogram.java
//   java -cp ~/lib/multiverse-core-0.7.0.jar:. TestStmHistogram

// Compile and run like this under Windows -- note the SEMICOLON:
//   javac -cp multiverse-core-0.7.0.jar TestStmHistogram.java
//   java -cp multiverse-core-0.7.0.jar;. TestStmHistogram

// For the Multiverse library:
import org.multiverse.api.references.*;
import static org.multiverse.api.StmUtils.*;

// Multiverse locking:
import org.multiverse.api.LockMode;
import org.multiverse.api.Txn;
import org.multiverse.api.callables.TxnVoidCallable;

import java.util.concurrent.Callable;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.CyclicBarrier;

class TestStmHistogram {
  public static void main(String[] args) {
    countPrimeFactorsWithStmHistogram();
  }

  private static void countPrimeFactorsWithStmHistogram() {
    final Histogram histogram = new StmHistogram(30);
    final Histogram total = new StmHistogram(30);
    final int transferCount = 200,
              transferRate = 30;
    final int range = 4_000_000;
    final int threadCount = 10, perThread = range / threadCount;
    final CyclicBarrier 
      startBarrier = new CyclicBarrier(threadCount + 1), 
      stopBarrier = startBarrier;
    final Thread[] threads = new Thread[threadCount];
    for (int t=0; t<threadCount; t++) {
      final int from = perThread * t, 
                to = (t+1 == threadCount) ? range : perThread * (t+1); 
      threads[t] = new Thread(() -> { 
	      try { startBarrier.await(); } catch (Exception exn) { }
	      for (int p=from; p<to; p++) 
		      histogram.increment(countFactors(p));
	      System.out.print("*");
	      try { stopBarrier.await(); } catch (Exception exn) { }
	    });
      threads[t].start();
    }
    try { startBarrier.await(); } catch (Exception exn) { }

    // Exercise 9.2.6 + 9.2.7: Start when threads have started
    for(int i = 0; i < transferCount; i++) {
      // Exercise 9.2.6:
      // total.transferBins(histogram);
      // Exercise 9.2.7:
      total.transferBins(total);

      try { Thread.sleep(transferRate); } catch (InterruptedException exn) { }
    }

    try { stopBarrier.await(); } catch (Exception exn) { }
    System.out.println("Dump histogram: Start");
    dump(histogram);
    System.out.println("Dump histogram: End");

    // Exercise 9.2.6: Dump total
    System.out.println("Dump total: Start");
    dump(total);
    System.out.println("Dump total: End");
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
  void increment(int bin);
  int getCount(int bin);
  int getSpan();
  int[] getBins();
  int getAndClear(int bin);
  void transferBins(Histogram hist);
}

class StmHistogram implements Histogram {
  private final TxnInteger[] counts;

  public StmHistogram(int span) {
    this.counts = new TxnInteger[span];
    for(int i = 0; i < span; i++) {
      this.counts[i] = newTxnInteger(0);
    }
  }

  // Exercise 9.2.1
  public void increment(int bin) {
    atomic(() -> this.counts[bin].getAndIncrement(1));
  }

  // Exercise 9.2.1
  public int getCount(int bin) {
    return atomic(() -> counts[bin].get());
  }

  // Exercise 9.2.1
  public int getSpan() {
    return counts.length;
  }

  // Exercise 9.2.3
  public int[] getBins() {
    int[] binCounts = new int[getSpan()];
    atomic(() -> {
      for(int bin = 0; bin < getSpan(); bin++) {
        binCounts[bin] = getCount(bin);
      }
    });
    return binCounts;
  }

  // Exercise 9.2.4
  public int getAndClear(int bin) {
    // int count = getCount(bin);
    // this.counts[bin].atomicSet(0);
    // return atomic(() -> count);
    return this.counts[bin].atomicGetAndSet(0);
  }

  // Exercise 9.2.5
  public void transferBins(Histogram hist) {
    // We chose (b): use a transaction for each bin, that atomically transfers that bin
    for(int bin = 0; bin < hist.getSpan(); bin++) {
      final int binToMoveTo = bin,
                amountToIncrementWith = hist.getAndClear(bin);
      atomic(() -> this.counts[binToMoveTo].getAndIncrement(amountToIncrementWith));
    }
  }
}

