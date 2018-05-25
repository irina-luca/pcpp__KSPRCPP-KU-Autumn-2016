class Printer {
	public static void main(String[] args) {
		System.out.println("Hello, Printer.java");

		// Printer p = new Printer();
		// p.print();
		Thread t1 = new Thread(() -> {
			while(true) {
				// p.print();
				Printer.print();
			}
		});
		Thread t2 = new Thread(() -> {
			while(true) {
				// p.print();
				Printer.print();
			}
		});
		t1.start();
		t2.start();
	}

	public static void print() {
		synchronized(Printer.class) { // the reflection API in Java, lock on the class RT object; the reflection class object
			System.out.print("-");
			try { 
				Thread.sleep(50); 
			} catch (InterruptedException exn) { 

			}
			System.out.print("|");
			
		}
	}
}
