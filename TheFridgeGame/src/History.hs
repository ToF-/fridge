module History where
import Room

type History = [Room]

emptyHistory :: History
emptyHistory = []

record :: Room -> History -> History
record = (:)

at :: History -> Int -> Room
h `at` n = let i = negate n - 1
            in if i < length h then h!!i else initialRoom

report :: History -> [(Int, Temperature, Position)]
report h = map (\(n,(t,p)) -> (n,t,p)) $ zip [0..] (reverse h)
