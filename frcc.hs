type Year = Int

data Month =
      January
    | February
    | March
    | April
    | May
    | June
    | July
    | August
    | September
    | October
    | November
    | December
    deriving (Enum, Eq, Ord, Show, Read)

data RepublicanMonth =
      Vendemiaire
    | Brumaire
    | Frimaire
    | Nivose
    | Pluviose
    | Ventose
    | Germinal
    | Floreal
    | Prairial
    | Messidor
    | Thermidor
    | Fructidor
    | SansCulottides
    deriving (Enum, Eq, Ord)

type Day = Int

type Date = (Year, Month, Day)
type RepublicanDate = (Year, RepublicanMonth, Day)

show_date :: Date -> String
show_date (year, month, day) =
    let s = case day `mod` 10 of
            1 -> show day ++ "st"
            2 -> show day ++ "nd"
            3 -> show day ++ "rd"
            _ -> show day ++ "th" in
    show month ++ " " ++ s ++ ", " ++ show year

-- roman :: Year -> String

show_rep_date :: RepublicanDate -> String
show_rep_date (year, month, day) =
    let s = if day == 1 then "1er" else show day in
    case month of
        Vendemiaire -> s ++ " Vendémiaire an " ++ show year
        Brumaire -> s ++ " Brumaire an " ++ show year
        Frimaire -> s ++ " Frimaire an " ++ show year
        Nivose -> s ++ " Nivôse an " ++ show year
        Pluviose -> s ++ " Pluviôse an " ++ show year
        Ventose -> s ++ " Ventôse an " ++ show year
        Germinal -> s ++ " Germinal an " ++ show year
        Floreal -> s ++ " Floréal an " ++ show year
        Prairial -> s ++ " Prairial an " ++ show year
        Messidor -> s ++ " Messidor an " ++ show year
        Thermidor -> s ++ " Thermidor an " ++ show year
        Fructidor -> s ++ " Fructidor an " ++ show year
        SansCulottides ->
            let s' = case day of
                    1 -> "Jour de la vertu, an "
                    2 -> "Jour du génie, an "
                    3 -> "Jour du travail, an "
                    4 -> "Jour de l'opinion, an "
                    5 -> "Jour des récompenses, an "
                    _ -> "Jour de la Révolution, an " in
            s' ++ show year

republican_equivalent :: (Month, Day) -> (RepublicanMonth, Day)
republican_equivalent (month, day)
    | (month, day) >= (December, 21)    = (Nivose, day - 20)
    | (month, day) >= (December, 1)     = (Frimaire, day + 10)
    | (month, day) >= (November, 21)    = (Frimaire, day - 20)
    | (month, day) >= (November, 1)     = (Brumaire, day + 10)
    | (month, day) >= (October, 22)     = (Brumaire, day - 21)
    | (month, day) >= (October, 1)      = (Vendemiaire, day + 9)
    | (month, day) >= (September, 22)   = (Vendemiaire, day - 21)
    | (month, day) >= (September, 17)   = (SansCulottides, day - 16)
    | (month, day) >= (September, 1)    = (Fructidor, day + 14)
    | (month, day) >= (August, 18)      = (Fructidor, day - 17)
    | (month, day) >= (August, 1)       = (Thermidor, day + 13)
    | (month, day) >= (July, 19)        = (Thermidor, day - 18)
    | (month, day) >= (July, 1)         = (Messidor, day + 12)
    | (month, day) >= (June, 19)        = (Messidor, day - 18)
    | (month, day) >= (June, 1)         = (Prairial, day + 12)
    | (month, day) >= (May, 20)         = (Prairial, day - 19)
    | (month, day) >= (May, 1)          = (Floreal, day + 11)
    | (month, day) >= (April, 20)       = (Floreal, day - 19)
    | (month, day) >= (April, 1)        = (Germinal, day + 11)
    | (month, day) >= (March, 21)       = (Germinal, day - 20)
    | (month, day) >= (March, 1)        = (Ventose, day + 10)
    | (month, day) >= (February, 19)    = (Ventose, day - 18)
    | (month, day) >= (February, 1)     = (Pluviose, day + 12)
    | (month, day) >= (January, 20)     = (Pluviose, day - 19)
    | otherwise                         = (Nivose, day + 11)

decrement :: RepublicanDate -> RepublicanDate
decrement (year, month, 1) =
    case month of
        Vendemiaire -> (year - 1, SansCulottides, 6)
        _ -> (year, pred month, 30)
decrement (year, month, day) = (year, month, day - 1)

convert :: Date -> RepublicanDate
convert (original_date@(year, month, day)) =
        -- the base republican year is (year - 1792)
    let base_ryear = year - 1792
        -- if date >= (September 22nd, or September 23rd if the year is a multiple of 4), increment the republican year
        -- if the base republican year is 0 we need to say this is year 1 (there is no year 0 in the republican calendar)
        is_mult_of_4 = if base_ryear `mod` 4 == 0 then 1 else 0
        ryear =
            case base_ryear of
                0 -> 1
                _ -> base_ryear + (if (month, day) >= (September, 22 + is_mult_of_4) then 1 else 0)
        -- get the generic republican equivalent according to the calendar
        (rmonth, rday) = republican_equivalent (month, day)
        -- if the republican year is a multiple of 4 and the date is not between March 1st and September 21, decrement the day
        decrement_condition = ryear `mod` 4 == 0 && ((month, day) > (September, 21) || ((month, day) < (March, 1)))
        rdate' = if decrement_condition then decrement (ryear, rmonth, rday) else (ryear, rmonth, rday)
        -- if the original date is after March 1st 1800, decrement the day
        rdate = if original_date > (1800, March, 1) then decrement rdate' else rdate' in
    -- decrement the day for each century year that is not a multiple of 400
    dec 1900 original_date rdate
    where
        dec y original_date republican_date =
            if y `mod` 400 == 0 then
                dec (y + 100) original_date republican_date
            else if original_date > (y, March, 1) then
                dec (y + 100) original_date (decrement republican_date)
            else republican_date

main :: IO ()
main = do
    putStrLn "day month year (numeric)"
    date <- getLine
    let [d', m', y'] = map read $ words date
    let (y, m, d) = (y', toEnum (m' - 1), d')
    putStrLn $ show_date (y, m, d) ++ " = " ++ (show_rep_date $ convert (y, m, d))