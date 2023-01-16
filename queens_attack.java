    public static int queensAttack(int n, int k, int r_q, int c_q, List<List<Integer>> obstacles){
    // Write your code here
        int above_attack_rows = n - r_q;
        //System.out.println("above_attack_rows = " + above_attack_rows);
        int below_attack_rows = r_q - 1;
        //System.out.println("below_attack_rows = " + below_attack_rows);
        int left_attack_cols = c_q - 1;
        //System.out.println("left_attack_cols = " + left_attack_cols);
        int right_attack_cols = n - c_q;
        //System.out.println("right_attack_cols = " + right_attack_cols);
        int upper_left_diag = Math.min((n - r_q), c_q - 1);
        System.out.println("upper_left_diag = " + upper_left_diag);
        int upper_right_diag = Math.min((n - r_q), (n - c_q));
        System.out.println("upper_right_diag = " + upper_right_diag);
        int lower_left_diag = Math.min(r_q - 1, c_q - 1);
        System.out.println("lower_left_diag = " + lower_left_diag);
        int lower_right_diag = Math.min(r_q - 1, (n - c_q));
        System.out.println("lower_right_diag = " + lower_right_diag);
        List<Integer> current_obstacle = new ArrayList<>();
        int row;
        int col;
        for(int i = 0; i < obstacles.size(); i++){
            current_obstacle = obstacles.get(i);
            row = current_obstacle.get(0);
            col = current_obstacle.get(1);
            if(row == r_q){
                if(col < c_q){
                    if(left_attack_cols > c_q - col - 1){
                        left_attack_cols = c_q - col - 1;
                    }
                }else{
                    if(col - c_q - 1 < right_attack_cols){
                        right_attack_cols = col - c_q - 1;
                    }         
                }
            }else if(col == c_q){
                if(row < r_q){
                    if(below_attack_rows > (r_q - row - 1)){
                        below_attack_rows = r_q - row - 1;
                    }
                }else{
                    if(above_attack_rows > (row - r_q) - 1){
                        above_attack_rows = row - r_q - 1;
                    }
                }
            }else if(Math.abs(row - r_q) == Math.abs(col - c_q)){
                //System.out.println("col is " + col + " row is " + row);
                if(row < r_q && col < c_q){
                    //System.out.println("col is " + col + " row is " + row);
                    //System.out.println("value is " + (row - r_q - 1));
                    lower_left_diag = Math.min(lower_left_diag, (Math.abs(row - r_q) - 1));
                }else if(row < r_q && col > c_q){
                    lower_right_diag = Math.min(lower_right_diag, (Math.abs(row - r_q) - 1));
                }else if(row > r_q && col < c_q){
                    upper_left_diag = Math.min(upper_left_diag, (Math.abs(row - r_q) - 1));
                }else if(row > r_q && col > c_q){
                    upper_right_diag = Math.min(upper_right_diag, (Math.abs(row - r_q) - 1));
                }
            }
        }
        /*System.out.println("above_attack_rows = " + above_attack_rows);
        System.out.println("below_attack_rows = " + below_attack_rows);
        System.out.println("left_attack_cols = " + left_attack_cols);
        System.out.println("right_attack_cols = " + right_attack_cols);*/
        /*System.out.println("upper_left_diag = " + upper_left_diag);
        System.out.println("upper_right_diag = " + upper_right_diag);
        System.out.println("lower_left_diag = " + lower_left_diag);
        System.out.println("lower_right_diag = " + lower_right_diag);*/
        return (left_attack_cols + right_attack_cols + below_attack_rows + above_attack_rows + upper_right_diag + upper_left_diag + lower_right_diag + lower_left_diag);
    }
}