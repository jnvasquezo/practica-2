import Data.Char (isDigit)

-- Funcion que descompone el codigo y genera las características
analyzeCode :: String -> Either String String
analyzeCode input
  | length input /= 8 = Left "Error: El codigo debe tener exactamente 8 dígitos."
  | not (all isDigit input) = Left "Error: La entrada debe ser un número de 8 dígitos."
  | not (isValidPeriod periodDigits) = Left "Error: Período de admisión inválido."
  | otherwise = Right $ unwords [period, category, consecutiveNum, parity]
  where
    code = read input :: Int
    periodDigits = take 3 input
    period = getPeriod periodDigits
    category = getCategory code
    consecutiveNum = getConsecutiveNum code
    parity = getParity code

-- Verificamos si los digitos son validos
isValidPeriod :: String -> Bool
isValidPeriod pd = pd `elem` ["241", "242", "251", "252", "261", "262"]

-- Extraemos el perido de admisiion
getPeriod :: String -> String
getPeriod "241" = "2024-1"
getPeriod "242" = "2024-2"
getPeriod "251" = "2025-1"
getPeriod "252" = "2025-2"
getPeriod "261" = "2026-1"
getPeriod "262" = "2026-2"
getPeriod _     = "Periodo de admisión inválido" 

-- Extraemos y clasificamos la categoria del programa
getCategory :: Int -> String
getCategory code = case classifyNumber categoryNum of
  "abundant" -> "Administrative"
  "perfect"  -> "Engineering"
  "deficient" -> "Humanities"
  where
    categoryNum = read (take 2 (drop 3 (show code))) :: Int

-- Clasificamos sengun el esquema de Nicomanchus
classifyNumber :: Int -> String
classifyNumber n
  | aliquotSum n > n = "abundant"
  | aliquotSum n == n = "perfect"
  | otherwise = "deficient"

-- Calculamos la suma alícuota
aliquotSum :: Int -> Int
aliquotSum n = sum [x | x <- [1..n-1], n `mod` x == 0]

-- Extraemos los ultimos 3 digitos
getConsecutiveNum :: Int -> String
getConsecutiveNum code = "num" ++ show consecutiveNum
  where
    consecutiveNum = read (drop 5 (show code)) :: Int

-- Determina si es par o impar
getParity :: Int -> String
getParity code
  | code `mod` 2 == 0 = "even"
  | otherwise = "odd"

printResult :: Either String String -> IO ()
printResult (Left errorMsg) = putStrLn errorMsg
printResult (Right output) = putStrLn output

main :: IO ()
main = do
    identificacion <- getLine
    printResult (analyzeCode identificacion)
