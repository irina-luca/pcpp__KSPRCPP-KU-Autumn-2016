import java.util.Random;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.Executors;
import java.util.concurrent.ExecutorService;

class TestConcurrentMap {
    private static final ExecutorService pool = Executors.newCachedThreadPool();
	public final AtomicInteger threadSum = new AtomicInteger();
	public final AtomicInteger mapSum = new AtomicInteger();
    // private final StripedWriteMap<Integer, String> map;
    private final WrapConcurrentHashMap<Integer, String> map;
    private final int threadCount = 16,
    				  lockCount = 7,
    				  bucketCount = 77;
    private final CyclicBarrier barrier = new CyclicBarrier(threadCount + 1);


    public static void main(String[] args) {
		TestConcurrentMap testConcurrent = new TestConcurrentMap();
		testConcurrent.test();
    }

    public TestConcurrentMap() {
	//	this.map = new StripedMap<>(bucketCount, lockCount);
	// this.map = new StripedWriteMap<>(bucketCount, lockCount);
		this.map = new WrapConcurrentHashMap();
    }

    public void test() {
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
	  	private final int randomRange = 100;

	  	public final int index;

	  	public WorkerThread(int index) {
	  	    this.index = index;
	  	}
	  	
	  	public void run() {
	  	    try {
	  		// int seed = (this.hashCode() ^ (int) System.nanoTime());
	  		int sum = 0; // sum == all added keys, minus all removed keys
	  		Random random = new Random();
	  		int   randomKey_containsKey = random.nextInt(randomRange),
	  		    randomKey_put = random.nextInt(randomRange),
	  		    randomKey_putIfAbsent = random.nextInt(randomRange),
	  		    randomKey_remove = random.nextInt(randomRange);

	  		barrier.await();

	  		// Put test
	  		// String val = index + ":" + randomKey_put;
	  		String v = map.put(randomKey_put, "");
	  		if(v == null) {
	  		    sum += randomKey_put;
	  		}

	  		// Contains key test
	  		boolean exists = map.containsKey(randomKey_containsKey);

	  		// putIfAbsent Test
	  		// val = index + ":" + randomKey_putIfAbsent;
	  		v = map.putIfAbsent(randomKey_putIfAbsent, "");
	  		if(v == null) {
	  		    sum += randomKey_putIfAbsent;
	  		}

	  		// remove test
	  		v = map.remove(randomKey_remove);
	  		if(v != null) {
	  		    sum -= randomKey_remove;  
	  		}

	  		threadSum.addAndGet(sum);		
	  		barrier.await();
	  	    } catch (Exception e) {
	  		throw new RuntimeException(e);
	  	    }
	  	}
    }    
}