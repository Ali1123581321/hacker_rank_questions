    private int check_order(List<Integer> arr){
        List<Integer> arr_prim = new ArrayList<>();
        arr_prim.addAll(arr);
        arr_prim.sort(Comparator.naturalOrder());
        HashMap<Integer, Integer> hashed_integers = new HashMap<>();
        HashMap<Integer, Integer> hashed_integers_reversed = new HashMap<>();
        HashMap<Integer, Integer> hashed_indeces = new HashMap<>();
        HashMap<Integer, Integer> hashed_indeces_reversed = new HashMap<>();
        int reverse_count = 0;
        for(int i = 0; i < arr.size(); i++){
            int curr = arr.get(i);
            hashed_integers.put(curr, i);
            hashed_integers_reversed.put(curr, i);
            hashed_indeces.put(i, curr);
            hashed_indeces_reversed.put(i, curr);
        }
        int count = 0;
        int tmp_i;
        int curr;
        int tmp_v;
        for(int i = 0; i < arr.size(); i++){
            curr = arr_prim.get(i);
            tmp_i = hashed_integers.get(curr);
            if(tmp_i != i){
                count++;
                tmp_v = hashed_indeces.get(i);
                hashed_indeces.replace(tmp_i, tmp_v);
                hashed_integers.replace(tmp_v, tmp_i); 
            }curr = arr_prim.get(arr.size() - i - 1);
            tmp_i = hashed_integers_reversed.get(curr);
            if(tmp_i != i){
                reverse_count++;
                tmp_v = hashed_indeces_reversed.get(i);
                hashed_indeces_reversed.replace(tmp_i, tmp_v);
                hashed_integers_reversed.replace(tmp_v, tmp_i);
            }
        }
        return Math.min(reverse_count, count);
    }