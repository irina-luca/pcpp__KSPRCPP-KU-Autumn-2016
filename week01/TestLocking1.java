// For week 1
// sestoft@itu.dk * 2015-10-29

public class TestLocking1 {
  public static void main(String[] args) {
    DoubleArrayList dal1 = new DoubleArrayList();
    // dal1.add(42.1); dal1.add(7.2); dal1.add(9.3); dal1.add(13.4); 
    // dal1.set(2, 11.3);
    // DoubleArrayList dal2 = new DoubleArrayList();
    // dal2.add(90.1); dal2.add(80.2); dal2.add(70.3); dal2.add(60.4); dal2.add(50.5); 
    // DoubleArrayList dal3 = new DoubleArrayList();


    Thread t1 = new Thread(() -> {
      while (true) {
        for (int i=0; i<10; i++)      
          dal1.add(i * 1.0);
      }
    });
    Thread t2 = new Thread(() -> {
      while (true) {
        for (int i=10; i>0; i--)      
          dal1.add(i * 1.0);
      }
    });
    t2.start();
    try {
      Thread.sleep(100);

    } catch(InterruptedException e) {}
    t1.start();
    // try { t1.join(); t2.join(); } catch (InterruptedException exn) { }

    for (int i=0; i<dal1.size(); i++)
      System.out.println(dal1.get(i));
  }
}

class DoubleArrayList {
  // Invariant: 0 <= size <= items.length
  private double[] items = new double[2];
  private int size = 0;

  // Number of items in the double list
  public synchronized int size() {
    return size;
  }

  // Return item number i, if any
  public synchronized double get(int i) {
    if (0 <= i && i < size) 
      return items[i];
    else 
      throw new IndexOutOfBoundsException(String.valueOf(i));
  }

  // Add item x to end of list
  public synchronized boolean add(double x) {
    if (size == items.length) {
      double[] newItems = new double[items.length * 2];
      for (int i=0; i<items.length; i++)
	newItems[i] = items[i];
      items = newItems;
    }
    items[size] = x;
    size++;
    return true;
  }

  // Replace item number i, if any, with x
  public synchronized double set(int i, double x) {
    if (0 <= i && i < size) {
      double old = items[i];
      items[i] = x;
      return old;
    } else 
      throw new IndexOutOfBoundsException(String.valueOf(i));
  }

  // The double list formatted as eg "[3.2, 4.7]"
  public synchronized String toString() {
    StringBuilder sb = new StringBuilder("[");
    for (int i=0; i<size; i++)
      sb.append(i > 0 ? ", " : "").append(items[i]);
    return sb.append("]").toString();
  }
}
