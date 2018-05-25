package com.company;
import java.util.concurrent.atomic.AtomicReference;

public class SimpleRWTryLock {
    private AtomicReference<Holders> holders; // if holders == null => lock is unheld

    public SimpleRWTryLock() {
        this.holders = new AtomicReference<Holders>(null);
    }
    public boolean readerTryLock() { // repeatedly ?! so is it a while?
//        final Thread actual = Thread.currentThread();
//        Holders oldVal = this.holders.get();
//        if(oldVal == null)
//            return this.holders.compareAndSet(oldVal, new ReaderList(actual));
//
//        if(oldVal instanceof ReaderList)
//            return this.holders.compareAndSet(oldVal, new ReaderList(actual, (ReaderList) oldVal));
//        return false;
        final Thread actual = Thread.currentThread();
        Holders oldVal;
        do {
            oldVal = this.holders.get();
            if(oldVal == null)
                return this.holders.compareAndSet(oldVal, new ReaderList(actual));

            if(oldVal instanceof ReaderList)
                return this.holders.compareAndSet(oldVal, new ReaderList(actual, (ReaderList) oldVal));

        } while(oldVal == null || (oldVal instanceof ReaderList));

        return false;
    }

    public void readerUnlock() { // does this also need a loop?!
//        final Thread actual = Thread.currentThread();
//        Holders oldVal = this.holders.get();
//
//        if(oldVal != null && oldVal instanceof ReaderList && ((ReaderList) oldVal).contains(actual)) {
//            this.holders.compareAndSet(oldVal, ((ReaderList) oldVal).remove(actual));
//        } else {
//            throw new RuntimeException("Cannot unlock reader");
//        }
        final Thread actual = Thread.currentThread();
        Holders oldVal;
        do {
            oldVal = this.holders.get();
            if(this.holders.compareAndSet(oldVal, ((ReaderList) oldVal).remove(actual)))
                return;

            if(oldVal == null || !(oldVal instanceof ReaderList) || !((ReaderList) oldVal).contains(actual))
                throw new RuntimeException("Cannot unlock reader");

        } while(oldVal != null && (oldVal instanceof ReaderList) && ((ReaderList) oldVal).contains(actual));


    }
    public boolean writerTryLock() {
        final Thread actual = Thread.currentThread();
        Holders oldVal = this.holders.get();
        if(oldVal == null) { // lock is currently unheld in this case
            Writer newVal = new Writer(actual);
            return this.holders.compareAndSet(oldVal, newVal);
        }
        return false;
    }
    public void writerUnlock() {
        final Thread actual = Thread.currentThread();
        Holders oldVal = this.holders.get();

        if(oldVal != null && oldVal.isActualThread(actual)) { // lock is currently held by the current thread
            this.holders.compareAndSet(oldVal, null);
        } else {
            throw new RuntimeException("Not lock holder || Thread is not the actual one");
        }

    }

    private static abstract class Holders {
        public final Thread thread;

        public Holders(Thread thread) {
            this.thread = thread;
        }

        public boolean isActualThread(Thread thread) {
            return this.thread == thread;
        }
    }
    private static class ReaderList extends Holders {
        private final ReaderList next;

        public ReaderList(Thread thread) {
            super(thread);
            this.next = null;
        }
        public ReaderList(Thread thread, ReaderList actual) {
            super(thread);
            this.next = (actual == null) ? null : actual;
        }

        public boolean contains(Thread t) {
            if(this.thread == t)
                return true;
            if(this.next == null)
                return false;
            return next.contains(t);
        }
        public ReaderList remove(Thread t) {
            if(this.thread == t)
                return this.next;
            if(this.next == null)
                return null;
            return new ReaderList(this.thread, next.remove(t));
        }


    }
    private static class Writer extends Holders {
        public Writer(Thread thread) {
            super(thread);
        }
    }


    public static void sequentialTest() {
        System.out.println("sequentialTest => Start");
        SimpleRWTryLock rwLock = new SimpleRWTryLock();

        assert rwLock.holders.get() == null;
        assert rwLock.writerTryLock() == true;
        assert rwLock.holders.get() != null;
        // Writer cannot lock, because lock is held by another writer
        assert rwLock.writerTryLock() == false;
        // Reader cannot lock either (and same goes for multiple readers)
        assert rwLock.readerTryLock() == false;
        assert rwLock.readerTryLock() == false;
        rwLock.writerUnlock();
        assert rwLock.holders.get() == null;
        // Multiple readers can tryLock
        assert rwLock.readerTryLock() == true;
        assert rwLock.readerTryLock() == true;
        assert rwLock.readerTryLock() == true;
        // Writer cannot long while reader is holding the lock
        assert rwLock.writerTryLock() == false;
        rwLock.readerUnlock();
        rwLock.readerUnlock();
        rwLock.readerUnlock();
        assert rwLock.holders.get() == null;
        // Cannot unlock when lock is not held: for reader
        boolean exnReadThrown = false;
        try {
            rwLock.readerUnlock();
        } catch (RuntimeException exn) {
            exnReadThrown = true;
        }
        assert exnReadThrown == true;
        assert rwLock.writerTryLock() == true;
        rwLock.writerUnlock();
        // Cannot unlock when lock is not held: for writer
        boolean exnWriteThrown = false;
        try {
            rwLock.writerUnlock();
        } catch (RuntimeException exn) {
            exnWriteThrown = true;
        }
        assert exnWriteThrown == true;
        System.out.println("sequentialTest => End");
    }

    public static void advancedTest() {
        System.out.println("advancedTest => Start");
        System.out.println("advancedTest => End");

    }

    public static void main(String[] args) {
        sequentialTest();
        advancedTest();

    }
}
