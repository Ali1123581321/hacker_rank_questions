import Prelude

min_max_locked ::[(Int, Int)] -> (Int, Int) -> (Int, Int) -> [(Int , Int)]
min_max_locked [] min_y max_y = [min_y, max_y]
min_max_locked (point:points) min_p max_p = min_max_locked points new_min new_max
    where
        find_new_min = (\(x1,y1) (x2,y2) -> if y1 < y2 then (x1,y1) else (x2,y2))
        find_new_max = (\(x1,y1) (x2,y2) -> if y1 > y2 then (x1,y1) else (x2,y2))
        new_max = find_new_max max_p point
        new_min = find_new_min min_p point

min_max::[(Int, Int)] -> [(Int, Int)]
min_max arr = min_max_locked arr first_point first_point
    where
        first_point = head arr


compute_slope::(Int, Int) -> (Int, Int) -> Double
compute_slope (x1, y1) (x2, y2) = slope
    where
        dx = x2 - x1
        dy = y2 - y1
        slope = (fromIntegral dy)/(fromIntegral dx)

find_point_to_the_right_moving_down_locked::[(Int, Int)] -> (Int, Int) -> Double -> (Int, Int) -> (Int, Int)
find_point_to_the_right_moving_down_locked [] _ _ current_minimum_point = current_minimum_point
find_point_to_the_right_moving_down_locked ((x',y'):xys) (x, y) current_minimum_slope current_minimum_point
    |x' == x && y' < y && current_minimum_slope == (-1/0) = find_point_to_the_right_moving_down_locked xys (x,y) (-1/0) point_with_smallest_y
    |x' == x && y' < y && current_minimum_slope /= (-1/0) = find_point_to_the_right_moving_down_locked xys (x,y) current_minimum_slope current_minimum_point
    |y' == y && x' > x = find_point_to_the_right_moving_down_locked xys (x,y) 0 point_with_biggest_x
    |x' < x || y' > y = find_point_to_the_right_moving_down_locked xys (x,y) current_minimum_slope current_minimum_point
    |x == x' && y' == y = find_point_to_the_right_moving_down_locked xys (x,y) current_minimum_slope current_minimum_point
    |otherwise = find_point_to_the_right_moving_down_locked xys (x,y) new_minimum_slope new_minimum_point
    where
        slope = compute_slope (x,y) (x',y')
        new_minimum_slope = max current_minimum_slope slope
        find_new_minimum_point = (\current_minimum_slope slope -> if current_minimum_slope < slope then (x',y') else current_minimum_point)
        new_minimum_point = find_new_minimum_point current_minimum_slope slope
        find_point_with_biggest_x = (\(x1, y1) (x2, y2) -> if x1 > x2 then (x1,y1) else (x2,y2))
        point_with_biggest_x = find_point_with_biggest_x (x',y') current_minimum_point
        find_point_with_smallest_y = (\(x1, y1) (x2, y2) -> if y1 < y2 then (x1,y1) else (x2,y2))
        point_with_smallest_y = find_point_with_smallest_y (x',y') current_minimum_point


find_point_to_the_left_moving_down_locked::[(Int, Int)] -> (Int, Int) -> Double -> (Int, Int) -> (Int, Int)
find_point_to_the_left_moving_down_locked [] _ _ current_minimum_point = current_minimum_point
find_point_to_the_left_moving_down_locked ((x',y'):xys) (x, y) current_minimum_slope current_minimum_point
    |x' == x && y' < y && current_minimum_slope == (1/0) = find_point_to_the_left_moving_down_locked xys (x,y) (1/0) point_with_smallest_y
    |x' == x && y' < y && current_minimum_slope /= (1/0) = find_point_to_the_left_moving_down_locked xys (x,y) current_minimum_slope current_minimum_point
    |y' == y  && x' < x = find_point_to_the_left_moving_down_locked xys (x,y) 0 point_with_smallest_x
    |x' > x || y' > y = find_point_to_the_left_moving_down_locked xys (x,y) current_minimum_slope current_minimum_point
    |x == x' && y' == y = find_point_to_the_left_moving_down_locked xys (x,y) current_minimum_slope current_minimum_point
    |otherwise = find_point_to_the_left_moving_down_locked xys (x,y) new_minimum_slope new_minimum_point
    where
        slope = compute_slope (x',y') (x,y)
        new_minimum_slope = min current_minimum_slope slope
        find_new_minimum_point = (\current_minimum_slope slope -> if current_minimum_slope > slope then (x',y') else current_minimum_point)
        new_minimum_point = find_new_minimum_point current_minimum_slope slope
        find_point_with_smallest_x = (\(x1, y1) (x2, y2) -> if x1 < x2 then (x1,y1) else (x2,y2))
        point_with_smallest_x = find_point_with_smallest_x (x',y') current_minimum_point
        find_point_with_smallest_y = (\(x1, y1) (x2, y2) -> if y1 < y2 then (x1,y1) else (x2,y2))
        point_with_smallest_y = find_point_with_smallest_y (x',y') current_minimum_point


find_point_to_the_right_moving_up_locked::[(Int, Int)] -> (Int, Int) -> Double -> (Int, Int) -> (Int, Int)
find_point_to_the_right_moving_up_locked [] _ _ current_minimum_point = current_minimum_point
find_point_to_the_right_moving_up_locked ((x',y'):xys) (x, y) current_minimum_slope current_minimum_point
    |x' == x && y' > y = find_point_to_the_right_moving_up_locked xys (x,y) current_minimum_slope current_minimum_point
    |y' == y && x' > x = find_point_to_the_right_moving_up_locked xys (x,y) 0 point_with_biggest_x
    |x' < x || y' < y = find_point_to_the_right_moving_up_locked xys (x,y) current_minimum_slope current_minimum_point
    |x == x' && y' == y = find_point_to_the_right_moving_up_locked xys (x,y) current_minimum_slope current_minimum_point
    |otherwise = find_point_to_the_right_moving_up_locked xys (x,y) new_minimum_slope new_minimum_point
    where
        slope = compute_slope (x,y) (x',y')
        new_minimum_slope = min current_minimum_slope slope
        find_new_minimum_point = (\current_minimum_slope slope -> if current_minimum_slope > slope then (x',y') else current_minimum_point)
        new_minimum_point = find_new_minimum_point current_minimum_slope slope
        find_point_with_biggest_x = (\(x1, y1) (x2, y2) -> if x1 > x2 then (x1,y1) else (x2,y2))
        point_with_biggest_x = find_point_with_biggest_x (x',y') current_minimum_point
        find_point_with_biggest_y = (\(x1, y1) (x2, y2) -> if y1 > y2 then (x1,y1) else (x2,y2))
        point_with_biggest_y = find_point_with_biggest_y (x',y') current_minimum_point


find_point_to_the_left_moving_up_locked::[(Int, Int)] -> (Int, Int) -> Double -> (Int, Int) -> (Int, Int)
find_point_to_the_left_moving_up_locked [] _ _ current_minimum_point = current_minimum_point
find_point_to_the_left_moving_up_locked ((x',y'):xys) (x, y) current_minimum_slope current_minimum_point
    |x' == x && y' > y = find_point_to_the_left_moving_up_locked xys (x,y) current_minimum_slope current_minimum_point
    |y' == y && x' < x = find_point_to_the_left_moving_up_locked xys (x,y) 0 point_with_smallest_x
    |x' > x || y' < y = find_point_to_the_left_moving_up_locked xys (x,y) current_minimum_slope current_minimum_point
    |x == x' && y' == y = find_point_to_the_left_moving_up_locked xys (x,y) current_minimum_slope current_minimum_point
    |otherwise = find_point_to_the_left_moving_up_locked xys (x,y) new_minimum_slope new_minimum_point
    where
        slope = compute_slope (x,y) (x',y')
        new_minimum_slope = max current_minimum_slope slope
        find_new_minimum_point = (\current_minimum_slope slope -> if current_minimum_slope < slope then (x',y') else current_minimum_point)
        new_minimum_point = find_new_minimum_point current_minimum_slope slope
        find_point_with_smallest_x = (\(x1, y1) (x2, y2) -> if x1 < x2 then (x1,y1) else (x2,y2))
        point_with_smallest_x = find_point_with_smallest_x (x',y') current_minimum_point
        find_point_with_biggest_y = (\(x1, y1) (x2, y2) -> if y1 > y2 then (x1,y1) else (x2,y2))
        point_with_biggest_y = find_point_with_biggest_y (x',y') current_minimum_point



find_point_to_the_left_moving_down::[(Int, Int)] -> (Int, Int) -> (Int, Int)
find_point_to_the_left_moving_down points current_point = find_point_to_the_left_moving_down_locked points current_point (1/0) current_point

find_point_to_the_right_moving_down::[(Int, Int)] -> (Int, Int) -> (Int, Int)
find_point_to_the_right_moving_down points current_point = find_point_to_the_right_moving_down_locked points current_point (-1/0) current_point

find_point_to_the_right_moving_up::[(Int, Int)] -> (Int, Int) -> (Int, Int)
find_point_to_the_right_moving_up points current_point = find_point_to_the_right_moving_up_locked points current_point (1/0) current_point

find_point_to_the_left_moving_up::[(Int, Int)] -> (Int, Int) -> (Int, Int)
find_point_to_the_left_moving_up points current_point = find_point_to_the_left_moving_up_locked points current_point (-1/0) current_point



find_all_points_to_the_left_moving_up_locked::[(Int, Int)] -> (Int,Int) -> [(Int, Int)]
find_all_points_to_the_left_moving_up_locked arr (x,y)
    |(x,y) == result_point = []
    |otherwise = result_point:(find_all_points_to_the_left_moving_up_locked arr result_point)
        where
            result_point = find_point_to_the_left_moving_up arr (x,y)

find_all_points_to_the_left_moving_up::[(Int, Int)] -> [(Int, Int)]
find_all_points_to_the_left_moving_up arr = min_point:(find_all_points_to_the_left_moving_up_locked new_arr min_point)
    where
        min_point = head $ min_max arr
        new_arr = remove arr min_point


find_all_points_to_the_right_moving_up_locked::[(Int, Int)] -> (Int,Int) -> [(Int, Int)]
find_all_points_to_the_right_moving_up_locked arr (x,y)
    |(x,y) == result_point = []
    |otherwise = result_point:(find_all_points_to_the_right_moving_up_locked arr result_point)
        where
            result_point = find_point_to_the_right_moving_up arr (x,y)

find_all_points_to_the_right_moving_up::[(Int, Int)] -> [(Int, Int)]
find_all_points_to_the_right_moving_up arr = min_point:(find_all_points_to_the_right_moving_up_locked new_arr min_point)
    where
        min_point = head $ min_max arr
        new_arr = remove arr min_point


find_all_points_to_the_right_moving_down_locked::[(Int, Int)] -> (Int,Int) -> [(Int, Int)]
find_all_points_to_the_right_moving_down_locked arr (x,y)
    |(x,y) == result_point = []
    |otherwise = result_point:(find_all_points_to_the_right_moving_down_locked arr result_point)
        where
            result_point = find_point_to_the_right_moving_down arr (x,y)

find_all_points_to_the_right_moving_down::[(Int, Int)] -> [(Int, Int)]
find_all_points_to_the_right_moving_down arr = max_point:(find_all_points_to_the_right_moving_down_locked new_arr max_point)
    where
        [_,max_point] = min_max arr
        new_arr = remove arr max_point



find_all_points_to_the_left_moving_down_locked::[(Int, Int)] -> (Int,Int) -> [(Int, Int)]
find_all_points_to_the_left_moving_down_locked arr (x,y)
    |(x,y) == result_point = []
    |otherwise = result_point:(find_all_points_to_the_left_moving_down_locked arr result_point)
        where
            result_point = find_point_to_the_left_moving_down arr (x,y)

find_all_points_to_the_left_moving_down::[(Int, Int)] -> [(Int, Int)]
find_all_points_to_the_left_moving_down arr = max_point:(find_all_points_to_the_left_moving_down_locked new_arr max_point)
    where
        [_,max_point] = min_max arr
        new_arr = remove arr max_point

remove::[(Int,Int)] -> (Int,Int) -> [(Int,Int)]
remove [] _ = []
remove (x:xs) x' |x == x' = xs
                 |otherwise = x:(remove xs x')


calculate_distance::[(Int, Int)] -> Double
calculate_distance [] = 0
calculate_distance [x] = 0
calculate_distance ((x1,y1):(x2,y2):points) = distance + calculate_distance((x2,y2):points)
    where
        distance = sqrt $ fromIntegral (dx^2 + dy^2)
        dx = x1 - x2
        dy = y1 - y2

calculate_total_distance::[(Int,Int)] -> Double
calculate_total_distance arr = calculate_distance down_left + calculate_distance down_right + calculate_distance up_left + calculate_distance up_right
    where
        down_left = find_all_points_to_the_left_moving_down arr
        down_right = find_all_points_to_the_right_moving_down arr
        up_left = find_all_points_to_the_left_moving_up arr
        up_right = find_all_points_to_the_right_moving_up arr
