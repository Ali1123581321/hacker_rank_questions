import java.util.*;
class Lego_blocks{
    List<Integer> possible_blocks;
    List<List<Integer>> results;
    List<List<Integer>> powers;


    public Lego_blocks(){
        results = new ArrayList<>();
        powers = new ArrayList<>();
        results.add(new ArrayList<>()); results.add(new ArrayList<>()); results.add(new ArrayList<>()); results.add(new ArrayList<>());
        results.add(new ArrayList<>()); results.add(new ArrayList<>()); results.add(new ArrayList<>()); results.add(new ArrayList<>());
        powers.add(new ArrayList<>()); powers.add(new ArrayList<>()); powers.add(new ArrayList<>()); powers.add(new ArrayList<>());
        powers.add(new ArrayList<>()); powers.add(new ArrayList<>()); powers.add(new ArrayList<>()); powers.add(new ArrayList<>());
        possible_blocks = new ArrayList<>();
        possible_blocks.add(1); possible_blocks.add(2); possible_blocks.add(4); possible_blocks.add(8); possible_blocks.add(15);
        possible_blocks.add(29); possible_blocks.add(56);
        int fst = 8; int sec = 15; int thrd = 29; int frth = 56;
        int result;
        for(int i = 7; i < 1001; i++){
            powers.add(new ArrayList<>());
            results.add(new ArrayList<>());
            result = fst + sec + thrd + frth;
            possible_blocks.add(result);
            fst = sec; sec = thrd; thrd = frth; frth = result;
        }
    }


    private int valid_blocks(int n, int m){
        double result = 0;
        if(n != 0 && m == 1)
            return 1;
        else if(n == 1 && m <= 4)
            return 1;
        else{
            List<Integer> local_results = results.get(n - 1);
            List<Integer> local_powers = powers.get(n - 1);
            if(local_results.size() >= m)
                return local_results.get(m - 1);
            if(local_powers.size() < m){
                int size = local_powers.size();
                for(int i = size; i < m; i++){
                    local_powers.add((int) Math.pow(possible_blocks.get(i), n));
                }
            }if((local_results).isEmpty())
                local_results.add(1);
            int size = local_results.size();
            while(size < m){
                result = (int) Math.pow(possible_blocks.get(size), n);
                for(int i = 0; i < size; i++){
                    result = result - (local_results.get(i)*local_powers.get(size - i - 1));
                }local_results.add((int) (result % 1000000007));
                size++;
            }return (int) (result % 1000000007);
        }
    }

    public static void main(String[] args) {
        Lego_blocks lego_blocks = new Lego_blocks();
        int x = lego_blocks.valid_blocks(4, 6);
        System.out.println(x);
    }
}