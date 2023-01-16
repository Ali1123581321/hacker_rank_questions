class Result {
    
    private int check_for_swap(List<Integer> arr, int start, int min){
        if(start == arr.size() - 1)
            return start;
        else if(arr.get(start + 1) >= arr.get(start - 1)){
            if(start > 1 && arr.get(start) >= arr.get(start - 2))
                return start;
            return -1;
        }for(int i = start + 1; i < arr.size(); i++){
            if(arr.get(i) <= min){
                return i;
            }
        }return -1;
    }
    
    
    private int is_consecutive(List<Integer> arr, int start, int max){
        int i = start;
        while(i + 1 < arr.size() && arr.get(i) > arr.get(i + 1)){
            i++;
        }
        //if(i + 1 < arr.size() && arr.get(i + 1) < max)
          //  return -1;
        //else
            return i;
    }
    
    private void max_count(List<Integer> arr){
        int reverse_count = 0;
        int swap_index1 = 0;
        int swap_index2 = 0;
        int swap_count = 0;
        int max = arr.get(0);
        for(int i = 1; i < arr.size(); i++){
            if(arr.get(i) > max){
                max = arr.get(i);
            }else{
                if(reverse_count > 0 || swap_count > 0){
                   System.out.println("no");
                   return; 
                }
                swap_index1 = i - 1;
                swap_index2 = is_consecutive(arr, i, max);
                /*if(swap_index2 == -1){
                   System.out.println("no");
                   return; 
                }else{*/
                    if(swap_index2 == i){
                        int tmp_index = check_for_swap(arr, i, arr.get(i));
                        if(tmp_index == -1){
                            System.out.println("no");
                            return;
                        }else{
                            swap_count++;
                            swap_index2 = tmp_index;
                            int tmp = arr.get(tmp_index);
                            arr.set(tmp_index, arr.get(i - 1));
                            arr.set(i - 1, tmp);
                            max = arr.get(i);
                        }
                    }else{
                        reverse_count++;
                        i = swap_index2;
                    }
                //}
            }
        }System.out.println("yes");
        if(swap_count > 0)
            System.out.println("swap " + (swap_index1 + 1) + " " + (swap_index2 + 1));
        else if(reverse_count > 0)
            System.out.println("reverse " + (swap_index1 + 1) + " " + (swap_index2 + 1));
    }
    
    /*
     * Complete the 'almostSorted' function below.
     *
     * The function accepts INTEGER_ARRAY arr as parameter.
     */

    public static void almostSorted(List<Integer> arr){
        Result r = new Result();
        r.max_count(arr);
    }
}