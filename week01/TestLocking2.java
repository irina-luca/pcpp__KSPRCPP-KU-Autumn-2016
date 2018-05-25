
// For week 1
// sestoft@itu.dk * 2015-10-29

import java.util.HashSet;

public class TestLocking2 {
  public static void main(String[] args) {
    DoubleArrayList dal1 = new DoubleArrayList();
    DoubleArrayList dal2 = new DoubleArrayList();
    DoubleArrayList dal3 = new DoubleArrayList();


    // Thread t1 = new Thread(() -> {
    //   dal1 = new DoubleArrayList();
    //   dal1.add(42.1); dal1.add(7.2); dal1.add(9.3); dal1.add(13.4);
    //   dal1.set(2, 11.3);
    //   for (int i=0; i<dal1.size(); i++)
    //     System.out.println(dal1.get(i));
    // });

    Thread t2 = new Thread(() -> {
      for(int i = 1;i< 1000;i++) {
        new DoubleArrayList();
       dal2.add(1.0*i);

      }
    });
    Thread t3 = new Thread(() -> {
      for(int i = 0;i < 2000;i++) {
        new DoubleArrayList();
        dal3.add(1.0 * i);

      }
    });
    t2.start();t3.start();
      try {
        t2.join(); t3.join(); }
      catch (InterruptedException exn) {
      }
    System.out.printf("Total size = %d%n", DoubleArrayList.totalSize());
    System.out.printf("All lists  = %s%n", DoubleArrayList.allLists());
  }
}

class DoubleArrayList {
  private static int totalSize = 0;
  private static HashSet<DoubleArrayList> allLists = new HashSet<>();
  private static Object totalSizeLock = new Object();
  private static Object allListsLock = new Object();
  private static Object constructorLock = new Object();

  // Invariant: 0 <= size <= items.length
  private double[] items = new double[2];
  private int size = 0;

  public DoubleArrayList() {
    synchronized(constructorLock) {
      allLists.add(this);
      
    }
  }

  // Number of items in the double list
  public int size() {
    return size;
  }

  // Return item number i, if any
  public double get(int i) {
    if (0 <= i && i < size)
      return items[i];
    else
      throw new IndexOutOfBoundsException(String.valueOf(i));
  }

  // Add item x to end of list
  public boolean add(double x) {
    if (size == items.length) {
      double[] newItems = new double[items.length * 2];
      for (int i=0; i<items.length; i++)
  newItems[i] = items[i];
      items = newItems;
    }
    items[size] = x;
    size++;
    synchronized(totalSizeLock){totalSize++;}
    return true;
  }

  // Replace item number i, if any, with x
  public double set(int i, double x) {
    if (0 <= i && i < size) {
      double old = items[i];
      items[i] = x;
      return old;
    } else
      throw new IndexOutOfBoundsException(String.valueOf(i));
  }

  // The double list formatted as eg "[3.2, 4.7]"
  public String toString() {
    StringBuilder sb = new StringBuilder("[");
    for (int i=0; i<size; i++)
      sb.append(i > 0 ? ", " : "").append(items[i]);
    return sb.append("]").toString();
  }

  public static int totalSize() {
    synchronized(totalSizeLock){return totalSize;}
  }

  public static HashSet<DoubleArrayList> allLists() {
    synchronized(allListsLock){ return allLists; }
  }
}