module Command
    where
import Data.Char ( toLower )

data Command = Add String
             | Display String
             | Change String Int
             | Rooms
             | Simulation String
    deriving (Eq, Show)


command :: String -> Either String Command
command = interpret . words

same :: Eq (a) => (b -> a) -> b -> b -> Bool
same f a b = f a == f b

equals :: String -> String -> Bool
s `equals` t = same (map toLower . take (length s)) s t

interpret :: [String] -> Either String Command
interpret (cmd:args) | cmd `equals` "add" =
    pure Add <*> head <$> (required "name" args)

interpret (cmd:args) | cmd `equals` "display" =
    pure Display <*> head <$> (required "name" args)

interpret (cmd:args) | cmd `equals` "change" =
    pure Change <*> head <$> (required "name" args) <*> ((head <$> (required "number" (tail args))) >>= isANumber >>= withinLimits (0,200))

interpret (cmd:args) | cmd `equals` "rooms" = Right $ Rooms

interpret (cmd:args) | cmd `equals` "simulation" =
    pure Simulation <*> head <$>  (required "name" args)

interpret ss = Left ("unknown command: " <> (unwords ss))

required :: String -> [String] -> Either String [String]
required label [] = Left ("missing " <> label)
required _ args = Right args

isANumber :: String -> Either String Int
isANumber s = case (reads s) of
                [] -> Left ("not a number: " <> s)
                ((n,_):_) -> Right n

withinLimits :: (Int,Int) -> Int -> Either String Int
withinLimits (min,max) n | n < min = Left "number too small"
                         | n > max = Left "number too large"
                         | otherwise = Right n
