package com.company;
import static org.multiverse.api.StmUtils.atomic;

/**
 * Created by irilu on 11/12/2016.
 */
class Histogram2 implements Histogram {
    private final int[] counts;
    public Histogram2(int span) {
        this.counts = new int[span];
    }
    public synchronized void increment(int bin) {
        counts[bin] = counts[bin] + 1;
    }
    public synchronized int getCount(int bin) {
        return counts[bin];
    }
    public int getSpan() {
        return counts.length;
    }
    public synchronized int[] getBins() {
        int[] bins = new int[getSpan()];
        for(int i = 0; i < getSpan(); i++) {
            bins[i] = getCount(i);
        }
        return bins;
    }

    public synchronized int getAndClear(int bin) {
        int old = this.counts[bin];
        this.counts[bin] = 0;
        return old;
    }
    public synchronized void transferBins(Histogram hist) {
        for(int bin = 0; bin < hist.getSpan(); bin++) {
            final int binToMoveTo = bin,
                    amountToIncrementWith = hist.getAndClear(bin);
            this.counts[binToMoveTo] = this.counts[binToMoveTo] + amountToIncrementWith;
        }
    }
}
