import Text.Printf (printf)
calculateOldTax :: Double -> Double
calculateOldTax income
  | income <= 250000 = 0
  | income <= 500000 = 0.05 * (income - 250000)
  | income <= 1000000 = 12500 + 0.2 * (income - 500000)
  | otherwise = 12500 + 100000 + 0.3 * (income - 1000000)

calculateNewTax :: Double -> Double
calculateNewTax income
  | income <= 250000 = 0
  | income <= 500000 = 0.05 * (income - 250000)
  | income <= 750000 = 12500 + 0.1 * (income - 500000)
  | income <= 1000000 = 12500 + 25000 + 0.15 * (income - 750000)
  | income <= 1250000 = 12500 + 25000 + 37500 + 0.2 * (income - 1000000)
  | income <= 1500000 = 12500 + 25000 + 37500 + 50000 + 0.25 * (income - 1250000)
  | otherwise = 12500 + 25000 + 37500 + 50000 + 62500 + 0.3 * (income - 1500000)

main :: IO ()
main = do
  putStrLn "Enter your annual income:"
  incomeInput <- getLine
  let income = read incomeInput :: Double
  putStrLn "Choose tax regime:"
  putStrLn "A. Old Tax Regime"
  putStrLn "B. New Tax Regime"
  regime <- getLine
  let tax = case regime of
              "A" -> calculateOldTax income
              "B" -> calculateNewTax income
              _   -> error "Invalid choice! Please select A or B."
  printf "Your calculated tax is: %.2f\n" tax
