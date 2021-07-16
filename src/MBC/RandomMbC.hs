module RandomMbC where

import System.Random
import System.Random.Shuffle

import Data.Time.Clock.POSIX

import Language
--import LanguageMbC
import AuxiliaryFormulasMbC

{-instance Random For where
  random g = (head fs, g')
    where (fs, g') = randomSamples allNodes 1 g
          allNodes = [Verum, V 0] ++ [N Verum] ++ [I Verum Verum, A Verum Verum, D Verum Verum]
  randomR range g =
    case range of
      (V len, V k) -> generateCpc len k g
      _            -> random g
-}

niladicNodes  = [Verum, V 0]
unaryNodes    = [N Verum, U Verum, C Verum]
binaryNodes   = [I Verum Verum, A Verum Verum, D Verum Verum]
nNiladicNodes = length niladicNodes
nUnaryNodes   = length unaryNodes
nBinaryNodes  = length binaryNodes

arity :: For -> Int
arity x
  | any (x ==) niladicNodes = 0
  | any (x ==) unaryNodes   = 1
  | any (x ==) binaryNodes  = 2
  | otherwise               = 0

generateCpc :: RandomGen g => Int -> Int -> g -> [For]
generateCpc = mkForGenerator [V 0, N Verum, A Verum Verum, D Verum Verum, I Verum Verum, E Verum Verum]

generateSimpleCpc :: RandomGen g => Int -> Int -> g -> [For]
generateSimpleCpc = mkForGenerator [V 0, N Verum, A Verum Verum, D Verum Verum, I Verum Verum]

generateMbc :: RandomGen g => Int -> Int -> g -> [For]
generateMbc = mkForGenerator [V 0, N Verum, C Verum, U Verum, I Verum Verum, A Verum Verum, D Verum Verum]

generateImp :: RandomGen g => Int -> Int -> g -> [For]
generateImp = mkForGenerator [V 0, N Verum, I Verum Verum]

generateMbcImp :: RandomGen g => Int -> Int -> g -> [For]
generateMbcImp = mkForGenerator [V 0, N Verum, C Verum, U Verum, I Verum Verum]


-- |Oblicza n!. Zapamiętuje wszystkie obliczone wcześniej wartości.
factorial :: Int -> Integer
factorial 0 = 1
factorial n = (toInteger n) * factorialCache!!(n-1)
factorialCache = [factorial n | n <- [0..]]


-- |Oblicza liczbę podziałów zbioru n-elementowego na k rozdzielnych
-- podzbiorów. Zapamiętuje wszystkie obliczone wcześniej wartości.
stirling :: Int -> Int -> Integer
stirling n k
  | n < k            = 0
  | n == k || k == 1 = 1
  | otherwise        = stirlingCache!!(n-1)!!(k-1) + (toInteger k)*stirlingCache!!(n-1)!!k
stirlingCache = [[stirling n k | k <- [0..]] | n <- [0..]]


-- |Tworzy ciąg @uw@ taki, że @xs == wu@ i @length w == n@
rotate :: Int -> [a] -> [a]
rotate n xs = post ++ pref
  where (pref, post) = splitAt n xs


-- |Tworzy ciąg długości @n@ powstały z losowo wybranych elementów @list@.
-- Wybory losowe mogą się powtarzać. Zwrócona zostaje para tak powstałego ciągu
-- z nowym stanem generatora liczb pseudolosowych.
randomSamples :: RandomGen g => [a] -> Int -> g -> ([a], g)
randomSamples list n g = (map (list !!) indexes, g1)
  where (g0, g1) = System.Random.split g
        indexes  = take n (randomRs (0, (length list) - 1) g0)


-- |Przyjmuje ciąg par wag z pewnymi wartościami. Zwrócona zostaje jedna losowo
-- wybrana z tych wartości, przy czym szanse jej wylosowania są równe stosunkowi
-- przypisanej wagi do sumy wszystkich wag w wejściowym ciągu. Funkcja zwraca
-- parę wybranego elementu z nowym stanem generatora liczb pseudolosowych.
weightedChoice :: RandomGen g => [(Integer, a)] -> g -> (a, g)
weightedChoice weights g = ((snd$head$filter (\(x, _) -> r <= x) intervals), g')
  where intervals = scanl1 (\(x, _) (y, a) -> (x+y, a)) weights
        (r, g') = randomR (1, fst $ last intervals) g


-- |Funkcja przyjmuje ciąg liczbowy oraz /ciąg/ formuł, następnie przypisuje
-- występującym w nim węzłom wartości z ciągu liczbowego zachowując przy tym ich
-- kolejność.
nameVars :: [Int] -> [For] -> [For]
nameVars [] fs             = fs
nameVars (v:vs) (Verum:fs) = (V v : nameVars vs fs)
nameVars (v:vs) (V _:fs)   = (V v : nameVars vs fs)
nameVars vs (f:fs)         = (f   : nameVars vs fs)


-- |Tworzy i zwraca formułę o zmiennej @n@ przemianowanej na @Verum@, wyższe
-- numery zmiennych zostają zdekrementowane.
varToVerum :: Int -> For -> For
varToVerum n (V v)
  | v == n    = Verum
  | v > n     = V (v-1)
  | otherwise = V v
varToVerum n (N f) = N (varToVerum n f)
varToVerum n (U f) = U (varToVerum n f)
varToVerum n (C f) = C (varToVerum n f)
varToVerum n (E f g) = E (varToVerum n f) (varToVerum n g)
varToVerum n (I f g) = I (varToVerum n f) (varToVerum n g)
varToVerum n (A f g) = A (varToVerum n f) (varToVerum n g)
varToVerum n (D f g) = D (varToVerum n f) (varToVerum n g)


-- |Przyjmuje ciąg formuł odpowiadający poprawnej formule zapisanej
-- w notacji Łukasiewicza i zwraca odpowiadającą mu formułę oraz "resztę",
-- która w przypadku głównego (nierekurencyjnego) wywołania funkcji powinna być
-- zwrócona jako pusta lista.
buildFor :: [For] -> (For, [For])
buildFor [] = (Verum, [])
buildFor (h:t) =
  case h of
    Verum -> (Verum, t)
    V v   -> (V v, t)
    N _   -> (N f, u)
    U _   -> (U f, u)
    C _   -> (C f, u)
    E _ _ -> (E f g, v)
    I _ _ -> (I f g, v)
    A _ _ -> (A f g, v)
    D _ _ -> (D f g, v)
  where
    (f, u) = (buildFor t)
    (g, v) = (buildFor u)


-- |Dla ciągu formuł @fs@ oblicza długość jego prefiksu, po którego
-- przerotowaniu powstanie ciąg formuł odpowiadający poprawnej formule
-- zapisanej w notacji Łukasiewicza.
prefixLength :: [For] -> Int
prefixLength fs    = last $ filter checkIndex [1..length fs]
  where checkIndex = (!!) (map (1 ==) $ scanl sumUp 0 $ map arity fs)
        sumUp      = \x y -> if x == 1 then 1 - y else x + 1 - y


-- |Funkcja zwraca losowo wybraną funkcję RGF o długości @n@
-- i maksymalnej wartości @k-1@.
generateRGF :: RandomGen g => Int -> Int -> g -> ([Int], g)
generateRGF n k g
  | k == 1    = (replicate n 0, g)
  | n == k    = ([0..n-1], g)
  | otherwise = (rgf, g')
  where a = stirling (n-1) (k-1)
        b = (toInteger k)*(stirling (n-1) k)
        (r, g1) = randomR (1, a+b) g
        (shorter, g2) = if r <= a then
          generateRGF (n-1) (k-1) g1
        else
          generateRGF (n-1) k g1
        (v, g') = if r <= a then
          (k-1, g2)
        else
          randomR (0 :: Int, k-1) g2
        rgf = shorter ++ [v]


genSamples :: RandomGen g => (Int -> Int -> Int -> g -> (For, g)) -> [Int] -> [Int] -> [Int] -> Int -> g -> ([For], g)
genSamples genDet lens ns ks each g = genAll genDet [(len, n, k) | len <- lens, n <- ns, k <- ks, _ <- [1..each]] g
  where
    genAll :: RandomGen g => (Int -> Int -> Int -> g -> (For, g)) -> [(Int, Int, Int)] -> g -> ([For], g)
    genAll _ [] g = ([], g)
    genAll genDet ((len, n, k):t) g = ((f:fs), g'')
      where
        (f, g')   = genDet len n k g
        (fs, g'') = genAll genDet t g'


mkForGenerator :: RandomGen g => [For] -> Int -> Int -> g -> [For]
mkForGenerator language len k = mkListGenerator $ (snd $ mkGeneratorPair language) len k


mkListGenerator :: RandomGen g => (g -> (For, g)) -> (g -> [For])
mkListGenerator gen g = (f:fs)
  where (f, g') = gen g
        fs      = mkListGenerator gen g'


mkDetGenerator :: RandomGen g => [For] -> Int -> Int -> Int -> g -> (For, g)
mkDetGenerator = fst . mkGeneratorPair

mkArbGenerator :: RandomGen g => [For] -> Int -> Int -> g -> (For, g)
mkArbGenerator = snd . mkGeneratorPair


mkGeneratorPair :: RandomGen g => [For] -> (Int -> Int -> Int -> g -> (For, g), Int -> Int -> g -> (For, g))
mkGeneratorPair language = (genDet, genArb)
  where
    niladicNodes  = filter ((0 ==) . arity) language
    unaryNodes    = filter ((1 ==) . arity) language
    binaryNodes   = filter ((2 ==) . arity) language

    nNiladicNodes = length niladicNodes
    nUnaryNodes   = length unaryNodes
    nBinaryNodes  = length binaryNodes


    -- |Zwraca liczbę formuł o sprecyzowanej długości @len@,
    -- liczbie instancji zmiennych @n@ oraz liczbie różnych zmiennych @k@.
    countDet :: Int -> Int -> Int -> Integer
    countDet len n k
      | len == 0  = 0
      | otherwise = (factorial (len-1))
                    `div` (factorial n)
                    `div` (factorial u)
                    `div` (factorial b)
                    * ((toInteger nBinaryNodes)^b)
                    * ((toInteger nUnaryNodes)^u)
                    * (stirling n k)
      where b = n - 1
            u = len - b - n


    -- |Oblicza liczbę możliwych formuł o sprecyzowanej długości @len@
    -- oraz liczbie różnych zmiennych @k@. Zwrócony wynik jest w postaci ciągu
    -- wyników cząstkowych takich, że na @n@-tej pozycji jest liczba formuł
    -- o @k+n@ instancjach zmiennych.
    countArb :: Int -> Int -> [Integer]
    countArb len 0 = []
    countArb len k = map (\n -> countDet len n k) [k .. (len+1) `div` 2]


    -- |Zwraca losowo wybraną formułę o sprecyzowanej długości @len@,
    -- liczbie instancji zmiennych @n@ oraz liczbie różnych zmiennych @k@.
    genDet :: RandomGen g => Int -> Int -> Int -> g -> (For, g)
    genDet len n k g = (for, g')
      where (niladic, g0) = randomSamples [V 0] n g
            (unary, g1)   = randomSamples unaryNodes (len - (2*n-1)) g0
            (binary, g2)  = randomSamples binaryNodes (n-1) g1
            nodes         = niladic ++ unary ++ binary
            (g3, g4)      = System.Random.split g2
            unfixed       = shuffle' nodes (length nodes) g3
            unnamed       = rotate (prefixLength unfixed) unfixed
            (rgf, g')     = generateRGF n k g4
            unbuilt       = nameVars rgf unnamed
            (for, _)      = buildFor unbuilt


    -- |Zwraca losowo wybraną formułę o sprecyzowanej długości @len@,
    -- oraz liczbie różnych zmiennych @k@.
    genArb :: RandomGen g => Int -> Int -> g -> (For, g)
    genArb len k g = genDet len n k g'
      where (n, g') = weightedChoice (zip (countArb len k) [k ..]) g
