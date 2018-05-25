package com.company;
/**
 * Created by irilu on 11/12/2016.
 */
import java.util.concurrent.CyclicBarrier;

class TestHistogram {
    public static void main(String[] args) {
//        countPrimeFactorsWithHistogram(new StmHistogram(30));
        countPrimeFactorsWithHistogram(new Histogram2(30));
//        countPrimeFactorsWithHistogram(new CasHistogram(30));
//        countPrimeFactorsWithHistogram(new CasHistogram(30));
//        countPrimeFactorsWithHistogram(new StmHistogram(30));
    }

    private static void countPrimeFactorsWithHistogram(final Histogram histogram)
    {
        Timer timer = new Timer();
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
        timer.play();
        try { stopBarrier.await(); } catch (Exception exn) { }
        System.out.println("Elapsed time is: " + timer.check());
//        System.out.println("Dump histogram: Start");
        dump(histogram);
//        System.out.println("Dump histogram: End");
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
