// Week 3
// sestoft@itu.dk * 2015-09-09

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.IntSummaryStatistics;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeMap;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.stream.IntStream;
import java.util.function.Function;


public class TestWordStream {
  public static void main(String[] args) {
    // Exercise 3.3.1: TEST
    System.out.println("// Exercise 3.3.1: TEST");
    String filename = "words";
    System.out.println(readWords(filename).count());

    // Exercise 3.3.2: TEST
    System.out.println("// Exercise 3.3.2: TEST");
    Stream<String> myStream332 = readWords(filename);
    readFirstKWords(myStream332, 3);

    // Exercise 3.3.3: TEST
    System.out.println("// Exercise 3.3.3: TEST");
    Stream<String> myStream333 = readWords(filename);
    findAndPrintAllWordsWithAtLeastKLetters(myStream333, 22);

    // Exercise 3.3.4: TEST
    System.out.println("// Exercise 3.3.4: TEST");
    Stream<String> myStream334_ValueFound = readWords(filename);
    Stream<String> myStream334_ValueNotFound = readWords(filename);
    findAndPrintSomeWordWithAtLeastKLetters(myStream334_ValueFound, 22);
    findAndPrintSomeWordWithAtLeastKLetters(myStream334_ValueNotFound, 90);

    // Exercise 3.3.5: TEST
    System.out.println("// Exercise 3.3.5: TEST");
    Stream<String> myStream335 = readWords(filename);
    long startFindAllPalindromes = System.nanoTime();
    findAllPalindromes(myStream335);
    long endFindAllPalindromes = System.nanoTime();
    System.out.println("Elapsed time: " + ((double)(endFindAllPalindromes - startFindAllPalindromes) / 1000000000.0));

    // Exercise 3.3.6: TEST
    System.out.println("// Exercise 3.3.6: TEST");
    Stream<String> myStream336 = readWords(filename);
    long startFindAllPalindromesParallel = System.nanoTime();
    findAllPalindromesParallel(myStream336);
    long endFindAllPalindromesParallel = System.nanoTime();
    System.out.println("Elapsed time: " + ((double)(endFindAllPalindromesParallel - startFindAllPalindromesParallel) / 1000000000.0));
    // The parallel version seems to be 0.1s faster than the sequential version.
    
     // Exercise 3.3.7: TEST // Not sure if it gives the right results
    System.out.println("// Exercise 3.3.7: TEST");
    Stream<String> myStream337_Min = readWords(filename);
    Stream<String> myStream337_Max = readWords(filename);
    Stream<String> myStream337_Avg = readWords(filename);
    // Stream<String> myStream337_All = readWords(filename);
    streamOfLengthsMin(myStream337_Min);
    streamOfLengthsMax(myStream337_Max);
    streamOfLengthsAvg(myStream337_Avg);
    // streamOfLengthsAll(myStream337_All);
    
    // Exercise 3.3.8: TEST
    System.out.println("// Exercise 3.3.8: TEST");
    // Stream<String> myStream338 = readWords(filename);
    // streamCollectGroupingBy(myStream338);
    // TO DO optional
    // Stream<String> myStream338_Optional = readWords(filename);
    // streamCollectGroupingBy_Optional(myStream338_Optional);

    // Exercise 3.3.9: TEST 
    System.out.println("// Exercise 3.3.9: TEST");
    // Map<Character,Integer> irinaTest = letters("irina");
    // irinaTest.forEach((c,i)->System.out.println("Character : " + c + " Count : " + i));
    Stream<String> myStream339 = readWords(filename);
    streamMapLettersOccurences(myStream339, 10);
    System.out.println("#####################################################");
    Stream<String> myStream339_Trial2 = readWords(filename);
    streamMapAllLettersOccurencesToSingleMap(myStream339_Trial2);
    System.out.println("#####################################################");
    Stream<String> myStream339_Trial3 = readWords(filename);
    streamMapLettersOccurencesFromUnifiedMap(myStream339_Trial3, 10);

    // Exercise 3.3.10
    System.out.println("// Exercise 3.3.10: TEST");
    Stream<String> myStream3310 = readWords(filename);
    streamCountLetterEOccurencesThroughMaps(myStream3310);

    // Exercise 3.3.10 // version with reduce
    System.out.println(readWords(filename).map(TestWordStream::letters).filter(x -> x.containsKey('e'))
           .reduce((x, y) -> {
               x.put('e', x.get('e') + y.get('e'));
               return x;
           }).get().get('e')); 

    // Exercise 3.3.11
    System.out.println("// Exercise 3.3.11: TEST");
    Stream<String> myStream3311 = readWords(filename);
    long startNP = System.nanoTime();
    findAndPrintAllSetsOfAnagrams(myStream3311);
    long endNP = System.nanoTime();
    long elapsedTimeNP = endNP - startNP;
    System.out.println((double)elapsedTimeNP / 1000000000.0); // => Time was 53.552465998s

    Stream<String> myStream3311_count = readWords(filename);
    findAndPrintAllSetsOfAnagramsCOUNT(myStream3311_count);

    // Exercise 3.3.12
    System.out.println("// Exercise 3.3.12: TEST");
    Stream<String> myStream3312 = readWords(filename);
    long startP = System.nanoTime();
    findAndPrintAllSetsOfAnagramsParallel(myStream3312);
    long endP = System.nanoTime();
    long elapsedTimeP = endP - startP;
    System.out.println((double)elapsedTimeP / 1000000000.0); // => Time was 117.619049472s
  }

  // Exercise 3.3.1
  public static Stream<String> readWords(String filename) {
    try {
      BufferedReader reader = new BufferedReader(new FileReader(filename));
      return reader.lines(); // returns a Stream<String>, as per Java 8 documentation
    } catch (IOException exn) { 
      return Stream.<String>empty();
    }
  }
  // Exercise 3.3.2
  public static void readFirstKWords(Stream<String> stream, int k) {
    stream.limit(k).forEach(System.out::println);
  }

  // Exercise 3.3.3
  public static void findAndPrintAllWordsWithAtLeastKLetters(Stream<String> stream, int k) {
    stream.filter(w -> w.length() >= k).forEach(System.out::println);
  }

  // Exercise 3.3.4
  public static void findAndPrintSomeWordWithAtLeastKLetters(Stream<String> stream, int k) {
    System.out.println(stream.filter(w -> w.length() >= k).findAny().orElse("Value Not Found"));
  }

  // Exercise 3.3.5
  public static void findAllPalindromes(Stream<String> stream) {
    // System.out.println("findAllPalindromes count is:" + stream.filter(w -> isPalindrome(w)).count());
    stream.filter(w -> isPalindrome(w)).forEach(System.out::println);
  }

  // Exercise 3.3.6
  public static void findAllPalindromesParallel(Stream<String> stream) {
    // System.out.println("findAllPalindromesParallel count is:" + stream.parallel().filter(w -> isPalindrome(w)).count());
    stream.parallel().filter(w -> isPalindrome(w)).forEach(System.out::println);
  }

  // Exercise 3.3.7
  public static void streamOfLengthsMin(Stream<String> stream) {
     stream.mapToInt(String::length).min().ifPresent(System.out::println);
  }
  public static void streamOfLengthsMax(Stream<String> stream) {
     stream.mapToInt(String::length).max().ifPresent(System.out::println);
  }
  public static void streamOfLengthsAvg(Stream<String> stream) {
     stream.mapToInt(String::length).average().ifPresent(System.out::println);
  }

  // Exercise 3.3.8
  public static void streamCollectGroupingBy(Stream<String> stream) {
    Map<Integer, List<String>> myMap = stream.collect(Collectors.groupingBy(String::length));
    myMap.forEach((i, list) -> {
      System.out.println("Group with length == " + i);
      list.forEach(y -> System.out.println(y));
    });
  }
  // TO DO!!! Optional
  // public static void streamCollectGroupingBy_Optional(Stream<String> stream) {
  //   Map<Integer, List<String>> myMap = stream.collect(Collectors.groupingBy(String::length), String::length);
  //   // myMap.forEach((i, list) -> {
  //   //   System.out.println("Optional: Group with length == " + i);
  //   //   list.forEach(y -> System.out.println(y));
  //   // });
  // }
  
  // Exercise 3.3.9
  public static void streamMapLettersOccurences(Stream<String> stream, int k) {
    stream.limit(k).map(w -> letters(w)).forEach(m -> {
      // for each word turned into a map
      m.forEach((c,i)->System.out.println("Character : " + c + " Count : " + i));
    });
  }

  public static void streamMapAllLettersOccurencesToSingleMap(Stream<String> stream) {
    stream.map(w -> letters(w)).flatMap(m -> m.entrySet().stream()).collect(Collectors.toMap(Entry::getKey, Entry::getValue, Math::max));
  }

  public static void streamMapLettersOccurencesFromUnifiedMap(Stream<String> stream, int k) {
    stream.limit(k).map(w -> letters(w)).flatMap(m -> m.entrySet().stream()).collect(Collectors.toMap(Entry::getKey, Entry::getValue, (a, b) -> a+b)).forEach((c,i)->System.out.println("Character : " + c + " Count : " + i));
  }

  // Exercise 3.3.10
  public static void streamCountLetterEOccurencesThroughMaps(Stream<String> stream) {
    stream.map(w -> letters(w)).flatMap(m -> m.entrySet().stream()).collect(Collectors.toMap(c -> c.getKey() == 'e' || c.getKey() == 'E', Entry::getValue, (a, b) -> a+b)).forEach((b,i) -> {
          if(b == true) System.out.println(i);
    });
  }

  // Exercise 3.3.11 
  public static void findAndPrintAllSetsOfAnagrams(Stream<String> stream) {
    Map<Map<Character,Integer>, List<String>> anagrams =
    stream.collect(Collectors.groupingBy(s -> letters(s)));

    anagrams.forEach((mapCI,listS)->System.out.println(listS));
  }
  public static void findAndPrintAllSetsOfAnagramsCOUNT(Stream<String> stream) {
    Map<Map<Character,Integer>, Long> anagrams =
    stream.collect(Collectors.groupingBy(s -> lettersConsideringLowercase(s), Collectors.counting()));
    System.out.println(anagrams.entrySet().stream().count());
  }

  // Exercise 3.3.12
  public static void findAndPrintAllSetsOfAnagramsParallel(Stream<String> stream) {
    stream.parallel().collect(Collectors.groupingBy(s -> letters(s))).forEach((mapCI,listS)->System.out.println(listS));
  }

  public static boolean isPalindrome(String s) {
    return s.equals(new StringBuilder(s).reverse().toString()); 
  }

  public static Map<Character,Integer> letters(String s) {
    // System.out.println("Analyzing word: " + s);
    Map<Character,Integer> res = new TreeMap<>();
    for(int i = 0; i < s.length(); i++) {
      char c = s.charAt(i);
      Integer value = res.get(c);
      if(value != null) {
        res.put(c, new Integer(value + 1));
      } else {
        res.put(c, 1);
      }
    }
    return res;
  }
  public static Map<Character,Integer> lettersConsideringLowercase(String s) {
    // System.out.println("Analyzing word: " + s);
    s = s.toLowerCase();
    Map<Character,Integer> res = new TreeMap<>();
    for(int i = 0; i < s.length(); i++) {
      char c = s.charAt(i);
      Integer value = res.get(c);
      if(value != null) {
        res.put(c, new Integer(value + 1));
      } else {
        res.put(c, 1);
      }
    }
    return res;
  }
}
