import java.util.concurrent.atomic.AtomicInteger;

/**
 * Created by irilu on 11/12/2016.
 */
public class CasHistogram implements Histogram {
    private final AtomicInteger[] counts;

    public CasHistogram (int span) {
        this.counts = new AtomicInteger[span];
        for (int i = 0; i < span; i++) {
            this.counts[i] = new AtomicInteger(0);
        }
    }
    @Override
    public void increment(int bin) {
        int oldValue, newValue;
        do {
            oldValue = this.counts[bin].get();
            newValue = oldValue + 1;
        } while(!counts[bin].compareAndSet(oldValue, newValue));
    }

    @Override
    public int getCount(int bin) {
        return this.counts[bin].get();
    }

    @Override
    public int getSpan() {
        return this.counts.length;
    }

    @Override
    public int[] getBins() {
        int[] binCounts = new int[this.getSpan()];
        for (int bin = 0; bin < this.getSpan(); bin++) {
            binCounts[bin] = this.getCount(bin);
        }
        return binCounts;
    }

    @Override
    public int getAndClear(int bin) {
        int oldValue, newValue = 0;
        do {
            oldValue = this.counts[bin].get();
        } while(!this.counts[bin].compareAndSet(oldValue, newValue));
        return oldValue;
    }

    @Override
    public void transferBins(Histogram hist) {
        for(int bin = 0; bin < this.getSpan(); bin++) {
            int oldValue, newValue;
            do {
                oldValue = this.counts[bin].get();
                newValue = hist.getAndClear(bin);
            } while(!this.counts[bin].compareAndSet(oldValue, newValue));
        }
    }
}
