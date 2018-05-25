import java.util.Random;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.Executors;
import java.util.concurrent.ExecutorService;

class TestMap {
  private static final ExecutorService pool = Executors.newCachedThreadPool();
  public final AtomicInteger threadSum = new AtomicInteger();
  public final AtomicInteger mapSum = new AtomicInteger();
  private final StripedWriteMap<Integer, String> map;
  // private final WrapConcurrentHashMap<Integer, String> map;
  private final int threadCount = 16,
            lockCount = 7,
            bucketCount = 77, 
            nTrials = 1000;
  private final int[][] counts = new int[threadCount][threadCount];
  private final CyclicBarrier barrier = new CyclicBarrier(threadCount + 1);


  public static void main(String[] args) {
    TestMap testConcurrent = new TestMap();
    testConcurrent.test();
  }

  public TestMap() {
    // this.map = new WrapConcurrentHashMap<Integer,String>();
    this.map = new StripedWriteMap(bucketCount, lockCount);
    
  }
  
  void test() {
    try {
      for (int i = 0; i < threadCount; i++) {
        pool.execute(new WorkerThread(i));
      }      
      barrier.await(); // wait for all threads to be ready
      barrier.await();  // wait for all threads to finish

      // After threads ran, we need to sum the map's keys and put the sum into mapSum
      map.forEach((key, value) -> {
        mapSum.addAndGet(key);
      });

      // Exercise 8.1.2: We do the assert
      assert mapSum.get() == threadSum.get();
      System.out.println("threadSum.get(): " + threadSum.get());
      System.out.println("mapSum.get(): " + mapSum.get());

      // Exercise 8.1.4: We check if all entries in the map are of form "t:k" (or at least plausible)
      for(int i = 0; i < threadCount; i++) { // because our randomRange was 100
        final int t = i;
        AtomicInteger threadEntries = new AtomicInteger();
        map.forEach((key, value) -> {
          int threadIndex = Integer.parseInt(value.split(":")[0]);
          int randomKey = Integer.parseInt(value.split(":")[1]);

          assert randomKey == key;
          assert value.split(":").length == 2;
          assert threadIndex >= 0 && t < threadCount;
        });
      }

      // Exercise 8.1.5: We check that a thread's entries == counts[that thread's index]
      // First accumulate the thread counts
      int[] tCounts = new int[threadCount];
      for(int x = 0; x < threadCount; x++) {
        for(int y = 0; y < threadCount; y++) {
          tCounts[y] += counts[x][y];
        }
      }
      // Then accumulate the mapThreadCounts
      for(int x = 0; x < threadCount; x++) {
        final int myThreadX = x;
        final AtomicInteger mapThreadXCount = new AtomicInteger();
        map.forEach((k, v) -> {
          if(myThreadX == Integer.parseInt(v.split(":")[0]))
            mapThreadXCount.addAndGet(1);
        });

        assert tCounts[x] == mapThreadXCount.get();
      }

      // Shutdown the pool
      pool.shutdown();
    } catch (Exception e) {
      throw new RuntimeException(e);
    }
  }

  class WorkerThread implements Runnable {
    private final int randomRange = 100;
    public final int index;

    public WorkerThread(int index) {
      this.index = index;
    }

    public void run() {
      try {
        barrier.await();

        int sum = 0;
        for (int i = nTrials; i > 0; --i) {
          Random random = new Random();
          int randomKey_containsKey = random.nextInt(randomRange),
              randomKey_put = random.nextInt(randomRange),
              randomKey_putIfAbsent = random.nextInt(randomRange),
              randomKey_remove = random.nextInt(randomRange);


          // ## containsKey() : Test ##
          boolean exists = map.containsKey(randomKey_containsKey);

          // ## put() : Test ##
          String resVal_put = index + ":" + randomKey_put;
          String resRK_put = map.put(randomKey_put, resVal_put);
          counts[index][index]++;
          if(resRK_put == null) {
            sum += randomKey_put;
          } else { // so in this case, the thread put a value for the same key previously put by another thread, right?!
            int previousThread = Integer.parseInt(resRK_put.split(":")[0]);
            counts[index][previousThread]--;
          }

          // ## putIfAbsent() : Test ##
          String resVal_putIfAbsent = index + ":" + randomKey_putIfAbsent;
          String resRK_putIfAbsent = map.putIfAbsent(randomKey_putIfAbsent, resVal_putIfAbsent);
          if(resRK_putIfAbsent == null) {
              sum += randomKey_putIfAbsent;
              counts[index][index]++;
          }

          // ## remove() : Test ##
          String resRK_remove = map.remove(randomKey_remove);
          if(resRK_remove != null) {
              sum -= randomKey_remove;  
              int previousThread = Integer.parseInt(resRK_remove.split(":")[0]);
              counts[index][previousThread]--;
          }
        }
        threadSum.addAndGet(sum);  

        barrier.await();
      } catch (Exception e) {
        throw new RuntimeException(e);
      }
    }
  }
}