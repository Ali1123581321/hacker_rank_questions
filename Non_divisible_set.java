import java.io.*;
import java.util.*;
import java.util.stream.*;
import static java.util.stream.Collectors.toList;

class Result {
    //To remove the numbers that have the reminder equal to half (k), because these numbers will always add upp to devide evenly on k
    public static List<Integer> removeInvalids(List<Integer> s, int k){
        return (s.stream().filter(elem ->  elem % k != (k/2))).collect(Collectors.toList());
    }
    //Finds the sets that have most numbers. We decide to take either (a) or k - (a), where (a) is the reminder when dividing over k
    //if we have numbers that divides evenly over k, then we can only take one of them
    //if we have a number that if added to any other number won't give a number that evenly divides over k, then we add 1 to the size
    public static int checkSets(HashMap<Integer, List<Integer>> s, int size, int k){
        int checked[] = new int[k];
        int tmpSize = 0;
        if(s.containsKey(0))
            size++;
        for(int i = 1; i < k; i++){
            if(checked[i] == 1){
                continue;
            }
            else{
                if(s.containsKey(i)){
                    tmpSize = s.get(i).size();
                    if(tmpSize == 0){
                        size++;
                    }
                    else if(s.containsKey(k - i)){
                        size += Math.max(tmpSize, s.get(k - i).size());
                    }else{
                        size += tmpSize;
                    }
                    checked[i] = 1;
                    checked[k - i] = 1;
                    tmpSize = 0;
                }
            }
        }return size;
    }
    
    /*
     * Complete the 'nonDivisibleSubset' function below.
     *
     * The function is expected to return an INTEGER.
     * The function accepts following parameters:
     *  1. INTEGER k
     *  2. INTEGER_ARRAY s
     */

    public static int nonDivisibleSubset(int k, List<Integer> s) {
        int size = 0;
        //if there are numbers that divides evenly over k
        if(k % 2 == 0){
            int length = s.size();
            s = removeInvalids(s, k);
            if(length != s.size())
                size++;         
        }
        //A hashmap that will contain all the sets. The key will be the reminder, and the value will be the numbers that will divide
        //evenly when added to the key
        HashMap<Integer, List<Integer>> sets = new HashMap<Integer, List<Integer>>();
        int curr = 0;
        for(int i = 0; i < s.size(); i++){
            curr = s.get(i) % k;
            if(sets.containsKey(curr)){
                //if the set is empty, that is there are no numbers that divides evenly given this reminder, and the reminder is not zero
                //then the size is increased
                if(curr != 0 && sets.get(curr).size() == 0){
                    size++;
                }continue;
            }
            //initialize an empty set for the given key, and then finds the numbers that will divide evenly, when add to the given reminder
            sets.put(curr, new ArrayList<Integer>());
            for(int n = 0; n < s.size(); n++){
                if(curr + (s.get(n) % k) == k){
                    sets.get(curr).add(s.get(n));
                }
            }
        }int result = checkSets(sets, size, k);
        System.out.println(sets);
        return result;
    }

}

public class Non_divisible_set {
    public static void main(String[] args) throws IOException {
        BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(System.in));
        BufferedWriter bufferedWriter = new BufferedWriter(new FileWriter(System.getenv("OUTPUT_PATH")));

        String[] firstMultipleInput = bufferedReader.readLine().replaceAll("\\s+$", "").split(" ");

        int n = Integer.parseInt(firstMultipleInput[0]);

        int k = Integer.parseInt(firstMultipleInput[1]);

        List<Integer> s = Stream.of(bufferedReader.readLine().replaceAll("\\s+$", "").split(" "))
            .map(Integer::parseInt)
            .collect(toList());

        int result = Result.nonDivisibleSubset(k, s);

        bufferedWriter.write(String.valueOf(result));
        bufferedWriter.newLine();

        bufferedReader.close();
        bufferedWriter.close();
    }
}
