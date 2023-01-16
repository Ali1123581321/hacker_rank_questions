import Data.List
import Data.IO


remove_white_spaces::String -> (Int, Int)
remove_white_spaces str = (first, second)
    where
        first = read (takeWhile (\x -> x /= ' ') str) ::Int
        second = read (drop 1 (dropWhile (\x -> x /= ' ') str)) ::Int

find_rectangle_area::[(Int, Int)] -> IO Int
find_rectangle_area coords = do
    let (x_coords, y_coords) = unzip coords
    let max_x = foldr (\acc x -> if x > acc then x else acc) (head x_coords) (tail x_coords)
    putStrLn(show max_x)
    let min_x = foldr (\acc x -> if x < acc then x else acc) (head x_coords) (tail x_coords)
    putStrLn(show min_x)
    let max_y = foldr (\acc y -> if y > acc then y else acc) (head y_coords) (tail y_coords)
    putStrLn(show max_y)
    let min_y = foldr (\acc y -> if y < acc then y else acc) (head y_coords) (tail y_coords)
    putStrLn(show min_y)
    return ((max_x - min_x) * (max_y - min_y))
    
    
find_triangles_area::[(Int, Int)] -> Double -> Double
find_triangles_area [] acc = acc
find_triangles_area [(x, y), (x', y')] acc = acc + ((fromIntegral (abs(x - x') * abs(y - y')))/2)
find_triangles_area (coord1:coord2:coords) acc = find_triangles_area (coord2:coords) new_acc
    where
        new_acc = acc + ((fromIntegral (x_coord * y_coord))/2)
        x_coord = abs((fst coord1) - (fst coord2))
        y_coord = abs((snd coord1) - (snd coord2))


solve::Int -> IO Double
solve m = do
    strs <- sequence [getLine | _ <- [1..m]]
    let coords = [remove_white_spaces str | str <- strs]
    let last_coord = last coords
    let first_coord = head coords
    let acc = ((fromIntegral (abs(fst first_coord - fst last_coord) * abs(snd first_coord - snd last_coord)))/2)
    rectangle_area <- find_rectangle_area coords
    let triangles_area = find_triangles_area coords acc
    putStrLn(show rectangle_area)
    return ((fromIntegral rectangle_area) - triangles_area)


main::IO()
main = do
    m <- getLine
    result <- solve (read m ::Int)
    putStrLn(show result)
    
    
    
    
