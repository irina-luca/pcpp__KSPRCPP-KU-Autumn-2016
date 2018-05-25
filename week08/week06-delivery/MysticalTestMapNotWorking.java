import java.util.Random;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.Executors;
import java.util.concurrent.ExecutorService;

class Tests {
  public static void assertEquals(int x, int y) throws Exception {
    if (x != y) 
      throw new Exception(String.format("ERROR: %d not equal to %d%n", x, y));
  }

  public static void assertTrue(boolean b) throws Exception {
    if (!b) 
      throw new Exception(String.format("ERROR: assertTrue"));
  }
}

class TestMap extends Tests {
  protected CyclicBarrier barrier;
  protected final WrapConcurrentHashMap<Integer,String> map;
  // protected final StripedWriteMap<Integer,String> map;
  protected static final ExecutorService pool = Executors.newCachedThreadPool();
  protected final int   lockCount = 7, 
  						bucketCount = 77, 
  						threads = 16, 
  						randomRange = 100,
  						nTrials = 1000;
  protected final AtomicInteger mapSum = new AtomicInteger(0);
  protected final AtomicInteger threadSum = new AtomicInteger(0);



  public static void main(String[] args) {
	TestMap mt = new TestMap();
	mt.test(pool);
   }

  public TestMap() {
    this.map = new WrapConcurrentHashMap<Integer,String>();
    // this.map = new StripedWriteMap(bucketCount, lockCount);
    this.barrier = new CyclicBarrier(threads + 1);
  }
  
  void test(ExecutorService pool) {
    try {
      for (int i = 0; i < threads; i++) {
        pool.execute(new WorkerThread());
      }      
      barrier.await(); // wait for all threads to be ready
      barrier.await();  // wait for all threads to finish

      // After threads ran, we need to sum the map's keys and put the sum into mapSum
      map.forEach((key, value) -> mapSum.getAndAdd(key));
      // We do the assert
      // assertEquals(mapSum.get(), threadSum.get());
      System.out.println("threadSum.get(): " + threadSum.get());
      System.out.println("mapSum.get(): " + mapSum.get());
      // Shutdown the pool
      pool.shutdown();
    } catch (Exception e) {
      throw new RuntimeException(e);
    }
  }

  class WorkerThread implements Runnable {
    public void run() {
      try {
        barrier.await();
        for (int i = nTrials; i > 0; --i) {
        	int sum = 0;
        	// Create random keys for testing each method
			Random random = new Random();
			int   randomKey_containsKey = random.nextInt(randomRange),
				  randomKey_put = random.nextInt(randomRange),
				  randomKey_putIfAbsent = random.nextInt(randomRange),
				  randomKey_remove = random.nextInt(randomRange);

			// Testing the methods
			boolean randomKey_containsKeyPresent = map.containsKey(randomKey_containsKey);
			sum += map.put(randomKey_put, "") == null ? randomKey_put : 0;
			sum += map.putIfAbsent(randomKey_putIfAbsent, "") == null ? randomKey_putIfAbsent : 0;
			sum -= map.remove(randomKey_remove) == null ? randomKey_remove : 0;
        	// Add to the threadSum
        	threadSum.getAndAdd(sum);
        }
        barrier.await();
      } catch (Exception e) {
        throw new RuntimeException(e);
      }
    }
  }
}