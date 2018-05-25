package com.company;

import java.util.Random;
import java.util.concurrent.CyclicBarrier;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Created by irilu on 11/23/2016.
 */
class PutTakeTest extends Tests {
    // We could use one CyclicBarrier for both starting and stopping,
    // precisely because it is cyclic, but the code becomes clearer by
    // separating them:
    protected CyclicBarrier startBarrier, stopBarrier;
    protected final UnboundedQueue<Integer> bq;
    protected final int nTrials, nPairs;
    protected final AtomicInteger putSum = new AtomicInteger(0);
    protected final AtomicInteger takeSum = new AtomicInteger(0);

    public PutTakeTest(MSQueue<Integer> bq, int npairs, int ntrials) {
        this.bq = bq;
        this.nTrials = ntrials;
        this.nPairs = npairs;
        this.startBarrier = new CyclicBarrier(npairs * 2 + 1);
        this.stopBarrier = new CyclicBarrier(npairs * 2 + 1);
    }

    void test(ExecutorService pool) {
        try {
            for (int i = 0; i < nPairs; i++) {
                pool.execute(new Producer());
                pool.execute(new Consumer());
            }
            startBarrier.await(); // wait for all threads to be ready
            stopBarrier.await();  // wait for all threads to finish
//            assertTrue(bq.isEmpty());
            assertEquals(putSum.get(), takeSum.get());
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    class Producer implements Runnable {
        public void run() {
            try {
                Random random = new Random();
                int sum = 0;
                startBarrier.await();
                for (int i = nTrials; i > 0; --i) {
                    int item = random.nextInt();
                    bq.enqueue(item);
                    sum += item;
                }
                putSum.getAndAdd(sum);
                stopBarrier.await();
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        }
    }

    class Consumer implements Runnable {
        public void run() {
            try {
                startBarrier.await();
                int sum = 0;
                for (int i = nTrials; i > 0; --i) {
                    Integer item = null;
                    while(item == null) {
                        item = bq.dequeue();
                    }
                    sum += item;
                }
                takeSum.getAndAdd(sum);
                stopBarrier.await();
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        }
    }
}
