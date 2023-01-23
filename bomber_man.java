import java.util.*;

class Result {
    
    static int[][] translate_grid(List<String> grid, int r, int c){
        int[][] int_grid = new int[r][c];
        char curr_char;
        for(int i = 0; i < r; i++){
            for(int n = 0; n < c; n ++){
                curr_char = grid.get(i).charAt(n);
                if(curr_char == 'O'){
                    int_grid[i][n] = 0;
                }else if(curr_char == '.'){
                    int_grid[i][n] = -1;
                }
            }
        }return int_grid;
    }
    static List<String> translate_back_to_list(int[][] int_grid, int r, int c){
        List<String> str_grid = new ArrayList<>();
        for(int i = 0; i < r; i++){
            StringBuilder sb = new StringBuilder();
            for(int n = 0; n < c; n++){
                if(int_grid[i][n] == 0){
                    sb.append('O');
                }else
                    sb.append('.');
            }str_grid.add(sb.toString());
        }return str_grid;
    }
    static void detonate_bomb(int grid[][], int i, int n, int r, int c){
        grid[i][n] = -1;
        if(i > 0 && i < r - 1){
            grid[i - 1][n] = -1;
            if(grid[i + 1][n] == -1)
                grid[i + 1][n] = 1;
        }else if(i + 1 <= r - 1){
            if(grid[i + 1][n] == -1)
                grid[i + 1][n] = 1;
        }else if(i - 1 >= 0){
            grid[i - 1][n] = -1;
        }
        if(n != 0 && n < c - 1){
            grid[i][n - 1] = -1;
            if(grid[i][n + 1] == -1){
                    grid[i][n + 1] = 1;
            }
        }else if(n + 1 <= c - 1){
            if(grid[i][n + 1] == -1){
                    grid[i][n + 1] = 1;
            }
        }else if(n - 1 >= 0){
            grid[i][n - 1] = -1;
        }
    }
    
    /*
     * Complete the 'bomberMan' function below.
     *
     * The function is expected to return a STRING_ARRAY.
     * The function accepts following parameters:
     *  1. INTEGER n
     *  2. STRING_ARRAY grid
     */
    /*Fill it with zeros, simultianosly let the bomb from last implanting detonate
    */
    public static List<String> bomberMan(int n, List<String> grid, int r, int c) {
    // Write your code here
    if(n % 2 == 0){
        for(int i = 0; i < grid.size(); i++){
            grid.replaceAll(x -> x.replaceAll(".","o"));
        }print_grid(grid);
        return grid;
    }
        int curr_int;
        int[][] int_grid = translate_grid(grid, r, c);
        while(n > 1){
            for(int i = 0; i < r; i++){
                for(int m = 0; m < c; m++){
                    curr_int = int_grid[i][m];
                    if(curr_int == 0){
                        detonate_bomb(int_grid, i, m, r, c);
                    }else if(curr_int == 1){
                        int_grid[i][m] = -1;
                    }else{
                        int_grid[i][m] = 0;
                    }
                }
            }n = n - 2;
        }grid = translate_back_to_list(int_grid, r, c);
        print_grid(grid);
        return grid;
    }
    static void print_grid(List<String> grid){
        for(int i = 0; i < grid.size(); i++){
            System.out.println(grid.get(i));
        }
    }
}

public class bomber_man {
    public static void main(String[] args){

        List<String> grid = new ArrayList<>();
        grid.add(".......");
        grid.add("...O.O.");
        grid.add("....O..");
        grid.add("..O....");
        grid.add("OO...OO");
        grid.add("OO.O...");

        List<String> result = Result.bomberMan(2, grid, 6, 7);

    }
}
/*
 * 6 7 5
.......
...O.O.
....O..
..O....
OO...OO
OO.O...
 */