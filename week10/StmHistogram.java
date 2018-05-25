package com.company;
/**
 * Created by irilu on 11/12/2016.
 */
// For the Multiverse library:
import org.multiverse.api.references.*;
import static org.multiverse.api.StmUtils.*;

// Multiverse locking:
import org.multiverse.api.LockMode;
import org.multiverse.api.Txn;
import org.multiverse.api.callables.TxnVoidCallable;

class StmHistogram implements Histogram {
    private final TxnInteger[] counts;

    public StmHistogram(int span) {
        this.counts = new TxnInteger[span];
        for(int i = 0; i < span; i++) {
            this.counts[i] = newTxnInteger(0);
        }
    }

    // Exercise 9.2.1
    public void increment(int bin) {
        atomic(() -> this.counts[bin].getAndIncrement(1));
    }

    // Exercise 9.2.1
    public int getCount(int bin) {
        return atomic(() -> counts[bin].get());
    }

    // Exercise 9.2.1
    public int getSpan() {
        return counts.length;
    }

    // Exercise 9.2.3
    public int[] getBins() {
        int[] binCounts = new int[getSpan()];
        atomic(() -> {
            for(int bin = 0; bin < getSpan(); bin++) {
                binCounts[bin] = getCount(bin);
            }
        });
        return binCounts;
    }

    // Exercise 9.2.4
    public int getAndClear(int bin) {
        // int count = getCount(bin);
        // this.counts[bin].atomicSet(0);
        // return atomic(() -> count);
        return this.counts[bin].atomicGetAndSet(0);
    }

    // Exercise 9.2.5
    public void transferBins(Histogram hist) {
        // We chose (b): use a transaction for each bin, that atomically transfers that bin
        for(int bin = 0; bin < hist.getSpan(); bin++) {
            final int binToMoveTo = bin,
                    amountToIncrementWith = hist.getAndClear(bin);
            atomic(() -> this.counts[binToMoveTo].getAndIncrement(amountToIncrementWith));
        }
    }
}
