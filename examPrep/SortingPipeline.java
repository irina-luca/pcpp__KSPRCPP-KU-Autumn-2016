package com.company;// Pipelined sorting using P>=1 stages, each maintaining an internal
// collection of size S>=1.  Stage 1 contains the largest items, stage
// 2 the second largest, ..., stage P the smallest ones.  In each
// stage, the internal collection of items is organized as a minheap.
// When a stage receives an item x and its collection is not full, it
// inserts it in the heap.  If the collection is full and x is less
// than or equal to the collections's least item, it forwards the item
// to the next stage; otherwise forwards the collection's least item
// and inserts x into the collection instead.

// When there are itemCount items and stageCount stages, each stage
// must be able to hold at least ceil(itemCount/stageCount) items,
// which equals (itemCount-1)/stageCount+1.

// sestoft@itu.dk * 2016-01-10

import org.multiverse.api.references.TxnDouble;
import org.multiverse.api.references.TxnInteger;

import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.IntToDoubleFunction;

import static org.multiverse.api.StmUtils.*;

public class SortingPipeline {
    public static void main(String[] args) {
        SystemInfo();
        final int count = 40, P = 4; // Before, count = 100_000, but I changed it according to exercise 5.2
        final double[] arr = DoubleArray.randomPermutation(count);

        // TO DO: Create and run pipeline to sort numbers from arr
//    // Exercise 5.2
//    WrappedArrayDoubleQueue[] queues = new WrappedArrayDoubleQueue[5];
//    for(int i = 0; i <= 4; i++) { // P+1 queues
//      queues[i] = new WrappedArrayDoubleQueue();
//    }
//    sortPipeline(arr, P, queues);
        // Exercise 5.3
//    final int countForExercise_5_3 = 100_000;
//    final double[] arrForExercise_5_3 = DoubleArray.randomPermutation(countForExercise_5_3);
//    WrappedArrayDoubleQueue[] queuesForExercise_5_3 = new WrappedArrayDoubleQueue[5];
//    for(int i = 0; i <= 4; i++) { // P+1 queues
//      queuesForExercise_5_3[i] = new WrappedArrayDoubleQueue();
//    }
//    System.out.println(Mark7(String.format("measureSortArray_100000_numbers %6d", 1),
//    i -> {
//      sortPipeline(arrForExercise_5_3, P, queuesForExercise_5_3);
//      return 1.1;
//    }));
//        // Exercise 6.3
//        final int countForExercise_6_3 = 100_000;
//        final double[] arrForExercise_6_3 = DoubleArray.randomPermutation(countForExercise_6_3);
//        BlockingDoubleQueue[] queuesForExercise_6_3 = new BlockingDoubleQueue[5];
//        for (int i = 0; i <= 4; i++) { // P+1 queues
//            queuesForExercise_6_3[i] = new BlockingNDoubleQueue(50);
//        }
//        System.out.println(Mark7(String.format("measureSortArray_100000_numbers %6d", 1),
//                i -> {
//                    sortPipeline(arrForExercise_6_3, P, queuesForExercise_6_3);
//                    return 1.1;
//                }));


//        // Exercise 7.3
//        final int countForExercise_7_3 = 100_000;
//        final double[] arrForExercise_7_3 = DoubleArray.randomPermutation(countForExercise_7_3);
//        UnboundedDoubleQueue[] queuesForExercise_7_3 = new UnboundedDoubleQueue[5];
//        for (int i = 0; i <= 4; i++) { // P+1 queues
//            queuesForExercise_7_3[i] = new UnboundedDoubleQueue();
//        }
//        System.out.println(Mark7(String.format("measureSortArray_100000_numbers %6d", 1),
//                i -> {
//                    sortPipeline(arrForExercise_7_3, P, queuesForExercise_7_3);
//                    return 1.1;
//                }));
//        // Exercise 8.6
//        final int p = 8;
//        final int countForExercise_8_6 = 100_000;
//        final double[] arrForExercise_8_6 = DoubleArray.randomPermutation(countForExercise_8_6);
//        NolockNDoubleQueue[] queuesForExercise_8_6 = new NolockNDoubleQueue[9];
//        for (int i = 0; i <= 8; i++) { // P+1 queues
//            queuesForExercise_8_6[i] = new NolockNDoubleQueue();
//        }
//        System.out.println(Mark7(String.format("measureSortArray_100000_numbers %6d", 1),
//                i -> {
//                    sortPipeline(arrForExercise_8_6, p, queuesForExercise_8_6);
//                    return 1.1;
//                }));

//        // Exercise 9.3
//        final int countForExercise_9_3 = 100_000;
//        final double[] arrForExercise_9_3 = DoubleArray.randomPermutation(countForExercise_9_3);
//        MSUnboundedDoubleQueue[] queuesForExercise_9_3 = new MSUnboundedDoubleQueue[5];
//        for (int i = 0; i <= 4; i++) { // P+1 queues
//            queuesForExercise_9_3[i] = new MSUnboundedDoubleQueue();
//        }
//        System.out.println(Mark7(String.format("measureSortArray_100000_numbers %6d", 1),
//                i -> {
//                    sortPipeline(arrForExercise_9_3, P, queuesForExercise_9_3);
//                    return 1.1;
//                }));

        // Exercise 10.3
        final int countForExercise_10_3 = 100_000;
        final double[] arrForExercise_10_3 = DoubleArray.randomPermutation(countForExercise_10_3);
        StmBlockingNDoubleQueue[] queuesForExercise_10_3 = new StmBlockingNDoubleQueue[5];
        for (int i = 0; i <= 4; i++) { // P+1 queues
            queuesForExercise_10_3[i] = new StmBlockingNDoubleQueue();
        }
        System.out.println(Mark7(String.format("measureSortArray_100000_numbers %6d", 1),
                i -> {
                    sortPipeline(arrForExercise_10_3, P, queuesForExercise_10_3);
                    return 1.1;
                }));
    }

    private static void sortPipeline(double[] arr, int P, BlockingDoubleQueue[] queues) {
        int N = arr.length; // The infinites, itemCount
        int S = N / P; // Not sure about this yet
        // TO DO
        Thread[] threads = new Thread[P + 2];
        // Create the DoubleGenerator thread
        threads[0] = new Thread(new DoubleGenerator(arr, N, queues[0]));
        // Create the SortingStage(s) threads
        for (int t = 1; t < P + 1; t++) {
            threads[t] = new Thread(new SortingStage(queues[t - 1], queues[t], S, N + (P - t) * S));
        }
        // Create the SortedChecker thread
        threads[P + 1] = new Thread(new SortedChecker(N, queues[P]));
        for (int t = 0; t < P + 2; t++)
            threads[t].start();
        try {
            for (int t = 0; t < P + 2; t++)
                threads[t].join();
        } catch (InterruptedException exn) {
            System.out.println("sortPipeline: InterruptedException");
        }
    }

    static class SortingStage implements Runnable {
        // TO DO: field declarations, constructor, and so on
        private final BlockingDoubleQueue input;
        private final BlockingDoubleQueue output;
        private final int S; // size of its internal collection
        private int itemCount; // = N + (P - i) * S () how many items it needs to produce as output before it terminates)
        private double[] heap; // internal collection
        private int heapSize; // heap size = k, how many items are in the collection at any point

        SortingStage(BlockingDoubleQueue input, BlockingDoubleQueue output, int s, int itemCount) {
            this.input = input;
            this.output = output;
            this.S = s;
            this.itemCount = itemCount;
            this.heap = new double[S];
            this.heapSize = 0;
        }

        public void run() {
            // TO DO
            while (itemCount > 0) {
                double x = 0;
                try {
                    x = input.take();
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
                if (heapSize < S) { // heap not full, put x into it
                    heap[heapSize++] = x;
                    DoubleArray.minheapSiftup(heap, heapSize - 1, heapSize - 1);
                } else if (x <= heap[0]) { // x is small, forward
                    try {
                        output.put(x);
                    } catch (InterruptedException e) {
                        e.printStackTrace();
                    }
                    itemCount--;
                } else { // forward least, replace with x
                    double least = heap[0];
                    heap[0] = x;
                    DoubleArray.minheapSiftdown(heap, 0, heapSize - 1);
                    try {
                        output.put(least);
                    } catch (InterruptedException e) {
                        e.printStackTrace();
                    }
                    itemCount--;
                }
            }
        }
    }

    static class DoubleGenerator implements Runnable {
        private final BlockingDoubleQueue output;
        private final double[] arr;  // The numbers to feed to output
        private final int infinites;

        public DoubleGenerator(double[] arr, int infinites, BlockingDoubleQueue output) {
            this.arr = arr;
            this.output = output;
            this.infinites = infinites;
        }

        public void run() {
            for (int i = 0; i < arr.length; i++)  // The numbers to sort
                try {
                    output.put(arr[i]);
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
            for (int i = 0; i < infinites; i++)   // Infinite numbers for wash-out
                try {
                    output.put(Double.POSITIVE_INFINITY);
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
        }
    }

    static class SortedChecker implements Runnable {
        // If DEBUG is true, print the first 100 numbers received
        private final static boolean DEBUG = false; // Changed from false to true in 5.2
        private final BlockingDoubleQueue input;
        private final int itemCount; // the number of items to check

        public SortedChecker(int itemCount, BlockingDoubleQueue input) {
            this.itemCount = itemCount;
            this.input = input;
        }

        public void run() {
            int consumed = 0;
            double last = Double.NEGATIVE_INFINITY;
            while (consumed++ < itemCount) {
                double p = 0;
                try {
                    p = input.take();
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
                if (DEBUG && consumed <= 100)
                    System.out.print(p + " ");
                if (p <= last)
                    System.out.printf("Elements out of order: %g before %g%n", last, p);
                last = p;
            }
            if (DEBUG)
                System.out.println();
        }
    }

    // --- Benchmarking infrastructure ---

    // NB: Modified to show milliseconds instead of nanoseconds

    public static double Mark7(String msg, IntToDoubleFunction f) {
        int n = 10, count = 1, totalCount = 0;
        double dummy = 0.0, runningTime = 0.0, st = 0.0, sst = 0.0;
        do {
            count *= 2;
            st = sst = 0.0;
            for (int j = 0; j < n; j++) {
                Timer t = new Timer();
                for (int i = 0; i < count; i++)
                    dummy += f.applyAsDouble(i);
                runningTime = t.check();
                double time = runningTime * 1e3 / count;
                st += time;
                sst += time * time;
                totalCount += count;
            }
        } while (runningTime < 0.25 && count < Integer.MAX_VALUE / 2);
        double mean = st / n, sdev = Math.sqrt((sst - mean * mean * n) / (n - 1));
        System.out.printf("%-25s %15.1f ms %10.2f %10d%n", msg, mean, sdev, count);
        return dummy / totalCount;
    }

    public static void SystemInfo() {
        System.out.printf("# OS:   %s; %s; %s%n",
                System.getProperty("os.name"),
                System.getProperty("os.version"),
                System.getProperty("os.arch"));
        System.out.printf("# JVM:  %s; %s%n",
                System.getProperty("java.vendor"),
                System.getProperty("java.version"));
        // The processor identifier works only on MS Windows:
        System.out.printf("# CPU:  %s; %d \"cores\"%n",
                System.getenv("PROCESSOR_IDENTIFIER"),
                Runtime.getRuntime().availableProcessors());
        java.util.Date now = new java.util.Date();
        System.out.printf("# Date: %s%n",
                new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssZ").format(now));
    }

    // Crude wall clock timing utility, measuring time in seconds

    static class Timer {
        private long start, spent = 0;

        public Timer() {
            play();
        }

        public double check() {
            return (System.nanoTime() - start + spent) / 1e9;
        }

        public void pause() {
            spent += System.nanoTime() - start;
        }

        public void play() {
            start = System.nanoTime();
        }
    }
}

// ----------------------------------------------------------------------

// Queue interface

interface BlockingDoubleQueue {
    double take() throws InterruptedException;

    void put(double item) throws InterruptedException;
}

// The queue implementations

// TO DO
// TO DO
class WrappedArrayDoubleQueue implements BlockingDoubleQueue {
    private final ArrayBlockingQueue<Double> underlyingAbq;

    WrappedArrayDoubleQueue() {
        this.underlyingAbq = new ArrayBlockingQueue<Double>(50);
    }

    @Override
    public double take() throws InterruptedException {
        return underlyingAbq.take();
    }

    @Override
    public void put(double item) throws InterruptedException {
        underlyingAbq.put(item);
    }
}

class BlockingNDoubleQueue implements BlockingDoubleQueue {
    private final int capacity; // n
    private final double[] items;
    private int head, tail, size;

    BlockingNDoubleQueue(int n) {
        this.capacity = n;
        this.items = new double[this.capacity];
        this.head = 0;
        this.tail = 0;
        this.size = 0;
    }

    @Override
    public double take() throws InterruptedException {
        synchronized (this) {
            while (this.size == 0) {
                try {
                    this.wait();
                } catch (InterruptedException exn) {
                }
            }
            double item = this.items[this.head];
            this.head = (this.head + 1) % this.capacity;
            this.size--;
            this.notifyAll();
            return item;
        }
    }

    @Override
    public void put(double item) throws InterruptedException {
        synchronized (this) {
            while (this.size == this.capacity) { // the array is full => then wait until there is space to put
                try {
                    this.wait();
                } catch (InterruptedException exn) {
                }
            }
            this.items[this.tail] = item;
            this.tail = (this.tail + 1) % this.capacity;
            this.size++;
            this.notifyAll();
        }
    }
}

class UnboundedDoubleQueue implements BlockingDoubleQueue {
    private static class Node {
        final double item;
        Node next;

        public Node(double item, Node next) {
            this.item = item;
            this.next = next;
        }
    }

    private Node head, tail;

    UnboundedDoubleQueue() {
        head = tail = new Node(0, null);
    }

    public void put(double item) { // at tail
        synchronized (this) { // intrinsic lock
            Node node = new Node(item, null);
            this.tail.next = node;
            this.tail = node;
            this.notifyAll();
        }
    }

    public double take() {     // from head
        synchronized (this) { // intrinsic lock
            while (this.head.next == null) { // while nothing to take
                try {
                    this.wait(); // thread blocks
                } catch (InterruptedException exn) {
                }
            }
            // when there is smth to take
            Node first = this.head; // save the head
            this.head = first.next; // ...
//            this.notifyAll();
            return this.head.item;
        }
    }
}

class NolockNDoubleQueue implements BlockingDoubleQueue {
    private final double[] items;
    private final int capacity;
    private volatile int tail = 0, head = 0;

    NolockNDoubleQueue() {
        this.capacity = 50;
        this.items = new double[this.capacity];
    }

    @Override
    public double take() throws InterruptedException {
        while (tail == head) {
        }
        double item = items[head % items.length];
        head++;
        return item;
    }

    @Override
    public void put(double item) throws InterruptedException {
        while (tail - head == items.length) {
        }
        items[tail % items.length] = item;
        tail++;
    }
}

class MSUnboundedDoubleQueue implements BlockingDoubleQueue {
    private final AtomicReference<Node> head, tail;

    public MSUnboundedDoubleQueue() {
        Node dummy = new Node(0.0, null);
        head = new AtomicReference<Node>(dummy);
        tail = new AtomicReference<Node>(dummy);
    }

    private static class Node {
        final double item;
        final AtomicReference<Node> next;

        public Node(double item, Node next) {
            this.item = item;
            this.next = new AtomicReference<Node>(next);
        }
    }

    @Override
    public double take() throws InterruptedException {
        while (true) {
            Node first = head.get(), last = tail.get(), next = first.next.get(); // D3
            if (first == head.get()) {        // D5
                if (first == last) {
                    if (next == null)
                        continue;
                    else
                        tail.compareAndSet(last, next);
                } else {
                    double result = next.item;
                    if (head.compareAndSet(first, next)) // D13
                        return result;
                }
            }
        }
    }

    @Override
    public void put(double item) throws InterruptedException {
        Node node = new Node(item, null);
        while (true) {
            Node last = tail.get(), next = last.next.get();
            if (last == tail.get()) {         // E7
                if (next == null) {
                    // In quiescent state, try inserting new node
                    if (last.next.compareAndSet(next, node)) { // E9
                        // Insertion succeeded, try advancing tail
                        tail.compareAndSet(last, node);
                        return;
                    }
                } else
                    // Queue in intermediate state, advance tail
                    tail.compareAndSet(last, next);
            }
        }
    }
}

class StmBlockingNDoubleQueue implements BlockingDoubleQueue {
    private final int capacity; // n
    private final TxnDouble[] items;
    private TxnInteger head, tail, size;

    StmBlockingNDoubleQueue() {
        this.capacity = 50;
        this.items = new TxnDouble[this.capacity];
        for (int i = 0; i < this.capacity; i++) {
            this.items[i] = newTxnDouble(0);
        }
        this.head = newTxnInteger(0);
        this.tail = newTxnInteger(0);
        this.size = newTxnInteger(0);
    }

    @Override
    public double take() throws InterruptedException {
        return atomic(() -> {
            if (this.size.get() == 0) {
//                System.out.println("test take");
                retry();
            }
            double item = this.items[this.head.get()].get();
            this.head.set((this.head.get() + 1) % this.capacity);
            this.size.decrement();
            return item;

        });
    }

    @Override
    public void put(double item) throws InterruptedException {
        atomic(() -> {
            if (this.size.get() == this.capacity) { // the array is full => then wait until there is space to put
//                System.out.println("test put");
                retry();
            }
            this.items[this.tail.get()].set(item);
            this.tail.set((this.tail.get() + 1) % this.capacity);
            this.size.increment();
        });
    }
}
// ----------------------------------------------------------------------

class DoubleArray {
    public static double[] randomPermutation(int n) {
        double[] arr = fillDoubleArray(n);
        shuffle(arr);
        return arr;
    }

    private static double[] fillDoubleArray(int n) {
        double[] arr = new double[n];
        for (int i = 0; i < n; i++)
            arr[i] = i + 0.1;
        return arr;
    }

    private static final java.util.Random rnd = new java.util.Random();

    private static void shuffle(double[] arr) {
        for (int i = arr.length - 1; i > 0; i--)
            swap(arr, i, rnd.nextInt(i + 1));
    }

    // Swap arr[s] and arr[t]
    private static void swap(double[] arr, int s, int t) {
        double tmp = arr[s];
        arr[s] = arr[t];
        arr[t] = tmp;
    }

    // Minheap operations for parallel sort pipelines.
    // Minheap invariant:
    // If heap[0..k-1] is a minheap, then heap[(i-1)/2] <= heap[i] for
    // all indexes i=1..k-1.  Thus heap[0] is the smallest element.

    // Although stored in an array, the heap can be considered a tree
    // where each element heap[i] is a node and heap[(i-1)/2] is its
    // parent. Then heap[0] is the tree's root and a node heap[i] has
    // children heap[2*i+1] and heap[2*i+2] if these are in the heap.

    // In heap[0..k], move node heap[i] downwards by swapping it with
    // its smallest child until the heap invariant is reestablished.

    public static void minheapSiftdown(double[] heap, int i, int k) {
        int child = 2 * i + 1;
        if (child <= k) {
            if (child + 1 <= k && heap[child] > heap[child + 1])
                child++;
            if (heap[i] > heap[child]) {
                swap(heap, i, child);
                minheapSiftdown(heap, child, k);
            }
        }
    }

    // In heap[0..k], move node heap[i] upwards by swapping with its
    // parent until the heap invariant is reestablished.
    public static void minheapSiftup(double[] heap, int i, int k) {
        if (0 < i) {
            int parent = (i - 1) / 2;
            if (heap[i] < heap[parent]) {
                swap(heap, i, parent);
                minheapSiftup(heap, parent, k);
            }
        }
    }
}
