main :: IO ()
main = print (solutions' [1,3,7,10,25,50] 765)


data Op = Add | Sub | Mul | Div

instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"


valid :: Op -> Int -> Int -> Bool
valid _ 0 _ = False
valid _ _ 0 = False
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y = y /= 1 && x `mod` y == 0



apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Mul x y = x * y
apply Div x y = x `div` y
apply Sub x y = x - y


data Expr = Val Int| App Op Expr Expr

instance Show Expr where
    show (Val x) = show x
    show (App op e1 e2) = visa e1 ++ show op ++ visa e2
                where
                    visa (Val x) = show x
                    visa (App op e1 e2) = "(" ++ show e1 ++ show op ++ show e2 ++ ")"


values :: Expr -> [Int]
values (Val x) = [x]
values (App op e1 e2) = values e1 ++ values e2



eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App op a b) = [apply op x y| x <- (eval a), y <- (eval b), valid op x y]

remove :: Int -> [a] -> [a]
remove 0 arr = arr
remove 1 (x:xs) = xs
remove n (x:xs) = x: (remove (n - 1) xs)

subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = subs xs ++ (map (x:) (subs xs))
{-- I slutet kommer den att ha [[]] ++ (map (4:) [[]] => [[]] ++ [[4]]) = [[], [4]]
    och resten förljer. [[], [4]] ++ (map (3:) [[], [4]]) => [[], [4], [3], [3, 4]] osv--}




interLeave :: a -> [a] -> [[a]]
interLeave x arr = [put n x arr | n <- [0..(length arr)]]
-- en påhittad for loop

put :: Int -> a -> [a] -> [a]
put 0 x [] = [x]
put 0 x (y:ys) = x:(y:ys)
put n x (y:ys) = y:put (n - 1) x ys

interLeave' :: a -> [a] -> [[a]]
interLeave' a [] = [[a]]
interLeave' a (x:xs) = (a:x:xs) : ([(x:arr)| arr <- interLeave' a xs])
                     --(a:(x:xs)): (map (x:) (interLeave' a xs))  båda betyder samma sak
-- det kommer till [[a]] och det blir bas fallet. interLeave 1 [2,3,4] ger:
    {-- sista steget blir inteLeave 1 [] som ger [[1]] och det blir
     [1, 4] : [(4: [1])|<- [[1]] = [[1,4], [4, 1]] som är en sub list och anropas igen som resultat av interLeave
     (1:3:[4]) : [(3:arr)| arr <- [[1,4] , [4, 1]]]--}                     



perms :: [a] -> [[a]]
perms [] = [[]]
--perms [x, y] = [[x,y], [y, x]] my solution
perms (x:xs) = concat $ map (interLeave x) (perms xs)
{--perms [1,2,3] = [[1,2,3], [2,1,3], [2,3,1], [1,3,2], [3,1,2], [3,2,1]]
I min lösning skrev jag båda sätten som man kan permutera ett par och där ifrån kan man bygga på resten för att det 
som blir kvar är att sätta första värdet i alla platserna
interLeave 3 [[]] = [[3]] => interLeave 2 [[3]] => [[2,3] , [3,2]] => interLeave 1 [[2,3], [3,2]]
 --}


choices :: [a] -> [[a]]
choices = concat . map perms . subs
{--choices skapar alla möjliga kombinationer av alla möjliga sub lists
 --}

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]

split :: [a] -> [([a], [a])]
split [] = []
split arr = [splitAt n arr| n <- [1..(length arr - 1)]]


split' :: [a] -> [([a], [a])]
split' [x, y] = [([x], [y])]
split' (x:xs) = ([x], xs) : [(x:a, b)| (a, b) <- split' xs]
{-- när man har kommit till bas fallet, får man [([x], [y])]
split' [2,3,4] = ([2], [3,4]) : [(2:[3], [4])| ([3], [4]) <- split' [3,4] som är lika med [([3], [4])]]
--}


combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

combine' :: Result -> Result -> [Result]
combine' (l, lx) (r, rx) = [(App o l r, apply o lx rx) | o <- ops, valid o lx rx]

ops :: [Op]
ops = [Add,Sub,Mul,Div]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs arr = [e | (ls, rs) <- split arr, l <- exprs ls, r <- exprs rs, e <- (combine l r)]


solutions :: [Int] -> Int -> [Expr]
solutions arr n = [e | arr' <- choices arr, e <- exprs arr', eval e == [n]]


type Result = (Expr,Int)


results :: [Int] -> [Result]
results [] = []
results [x] = [(Val x, x)]
results arr = [res| (l, r) <- split arr, (lr, li) <- results l, (rr, ri) <- results r, res <- combine' (lr, li) (rr, ri)]


solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [e | ns' <- choices ns, (e,m) <- results ns', m == n]