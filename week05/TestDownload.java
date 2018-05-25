// For week 5
// sestoft@itu.dk * 2014-09-19

import java.net.URL;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;
import java.util.Map;
import java.util.Arrays;
import java.util.HashMap;
import java.util.ArrayList;
import java.util.List;

import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Executors;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;

public class TestDownload {
  private static final ExecutorService executor 
    = Executors.newWorkStealingPool();

  private static final String[] urls = 
  { "http://www.itu.dk", "http://www.di.ku.dk", "http://www.miele.de",
    "http://www.microsoft.com", "http://www.dr.dk",
    "http://www.vg.no", "http://www.tv2.dk", "http://www.google.com",
    "http://www.ing.dk", "http://www.dtu.dk", "http://www.eb.dk", 
    "http://www.nytimes.com", "http://www.guardian.co.uk", "http://www.lemonde.fr",   
    "http://www.welt.de", "http://www.dn.se", "http://www.heise.de", "http://www.wsj.com", 
    "http://www.bbc.co.uk", "http://www.dsb.dk", "http://www.bmw.com", "https://www.cia.gov" 
  }; // , "http://www.amazon.com"

  public static void main(String[] args) throws IOException {
    // Exercise 5.3.1
    String url = "https://www.wikipedia.org/";
    String page = getPage(url, 10);
    System.out.printf("%-30s%n%s%n", url, page);

    // Exercise 5.3.2
    Map<String,String> myMap = getPages(urls, 3);
    myMap.entrySet().forEach(entry -> System.out.println("url: " + entry.getKey() + ", chars count: " + entry.getValue().length()));
    
    // for(Map.Entry<String, String> entry : myMap.entrySet()) {
    //     String theUrl = entry.getKey();
    //     String theFetchedText = entry.getValue();
    //     System.out.println("url: " + theUrl + ", chars count: " + theFetchedText.length());
    // }
    
    // Exercise 5.3.3
    Timer timer = new Timer();
    double[] times = new double[5];
    for(int i = 0; i < 5; i++) {
      Map<String,String> myMapGetPages = getPages(urls, 3);
      times[i] = timer.check();
    }
    System.out.println(Arrays.toString(times));

    // Exercise 5.3.4
    // test as in Ex 5.3.3
    double[] timesParallel = new double[5];
    Map<String,String> myMapParallel = new HashMap<String,String>();
    for(int i = 0; i < 5; i++) {
    Timer timerParallel = new Timer();
      myMapParallel = getPagesParallel(urls, 3);
      timesParallel[i] = timerParallel.check();
    }
    System.out.println(Arrays.toString(timesParallel));

    // compare time for fetching 23 webpages sequentially vs. in parallel
    Timer timerSeq = new Timer();
    Map<String,String> myMapSeq = getPages(urls, 23);
    System.out.println("Time for getting 23 webpages sequentially: " + timerSeq.check());
    
    Timer timerPar = new Timer();
    Map<String,String> myMapPar = getPagesParallel(urls, 23);
    System.out.println("Time for getting 23 webpages in parallel: " + timerPar.check());
    
    
  }

  public static Map<String, String> getPagesParallel(String[] urls, int maxLines) throws IOException {
    Map<String,String> myMap = new HashMap<String, String>();

    List<Future<?>> futures = new ArrayList<Future<?>>(); 
    for(String url:urls) {
      futures.add(executor.submit(() -> {
        try {
          myMap.put(url, getPage(url, maxLines));
        } catch(IOException exn) { System.out.println(exn); }
      }));
    }

    try {
     for (Future<?> fut : futures)
     fut.get();
    } catch (InterruptedException exn) { System.out.println(exn); } 
      catch (ExecutionException exn) { throw new RuntimeException(exn); } 
    
    return myMap;
  }

  public static Map<String,String> getPages(String[] urls, int maxLines) throws IOException {
    Map<String,String> myMap = new HashMap<String, String>();
    for(String url:urls) {
      myMap.put(url, getPage(url, maxLines));
    }
    return myMap;
  }

  public static String getPage(String url, int maxLines) throws IOException {
    // This will close the streams after use (JLS 8 para 14.20.3):
    try (BufferedReader in 
         = new BufferedReader(new InputStreamReader(new URL(url).openStream()))) {
      StringBuilder sb = new StringBuilder();
      for (int i=0; i<maxLines; i++) {
        String inputLine = in.readLine();
        if (inputLine == null)
          break;
        else
          sb.append(inputLine).append("\n");
      }
      return sb.toString();
    }
  }
}

