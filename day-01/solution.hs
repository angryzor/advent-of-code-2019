fuelFor mass = max 0 (floor (fromIntegral mass / 3.0) - 2)

totalFuel 0 = 0
totalFuel currentFuel = currentFuel + totalFuel (fuelFor currentFuel)

main = do
  masses <- lines <$> getContents

  let fuelPerModule = fuelFor . read <$> masses

  putStrLn "Answer to part 1:"
  print $ sum fuelPerModule

  putStrLn "Answer to part 2:"
  print $ sum (totalFuel <$> fuelPerModule)
