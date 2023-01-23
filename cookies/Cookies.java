import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.*;
import java.util.stream.Stream;
class Cookies{

    static int cookies(List<Integer> A, int k){
        Queue<Integer> q = new PriorityQueue<>();
        int i = 0;
        int moves = 0;
        while(i < A.size()){
            q.add(A.get(i));
            i++;
        }int least_sweet;
        int second_least_sweet;
        int mixed;
        while(q.size() > 1 && q.peek() < k){
            least_sweet = q.poll();
            second_least_sweet = q.poll();
            mixed = least_sweet + 2*second_least_sweet;
            moves++;
            q.add(mixed);
        }if(q.peek() >= k){
            return moves;
        }else{
            return -1;
        }
    }

    public static void main(String args[]) throws IOException{
        BufferedReader bufferedReader = new BufferedReader(new FileReader("C:/Users/slims/OneDrive/cookies/tests.txt"));
        //BufferedWriter bufferedWriter = new BufferedWriter(new FileWriter(System.getenv("OUTPUT_PATH")));

        String[] firstMultipleInput = bufferedReader.readLine().replaceAll("\\s+$", "").split(" ");

        int n = Integer.parseInt(firstMultipleInput[0]);

        int k = Integer.parseInt(firstMultipleInput[1]);

        List<Integer> A = Stream.of(bufferedReader.readLine().replaceAll("\\s+$", "").split(" ")).map(Integer::parseInt).toList();

        int result = cookies(A, k);
        System.out.println(result);
        //bufferedWriter.write(String.valueOf(result));
        //bufferedWriter.newLine();

        bufferedReader.close();
        //bufferedWriter.close();
    }
}
