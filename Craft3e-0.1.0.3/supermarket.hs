module Supermarket where
import Prelude hiding (lookup)
import Chapter6 hiding (fst)
import Data.List hiding (lookup)

sampleBill :: BillType
sampleBill = [("Dry Sherry, 1lt",540), ("Fish Fingers", 121),
                    ("Orange Jelly", 56), ("Hula Hoops (Giant)", 133),
                    ("Unkown Item", 0), ("Dry Sherry, 1lt",540)]

sampleTill :: TillType
sampleTill = [1234, 4719, 3814, 1112, 1113, 1234]
                    
-- 6.39
formatPence :: Price -> String
formatPence p = pounds ++ "." ++ (padLeft pence 2)
    where
        pounds = show $ div p 100
        pence = show $ mod p 100
        padLeft s w = (replicate (w - (length s)) '0') ++ s

-- 6.40
formatLine :: (Name, Price) -> String
formatLine (n, p) = n ++ dots ++ formatted_price ++ "\n"
    where
        formatted_price = formatPence p
        dots = replicate (lineLength - (length n) - (length formatted_price)) '.'
       
-- 6.41
formatLines :: [(Name, Price)] -> String
formatLines lines = concat [ formatLine line | line <- lines ]

-- 6.42
makeTotal :: BillType -> Price
makeTotal bill = sum [ price | (name,price) <- bill ]

-- 6.43
formatTotal :: Price -> String
formatTotal price = "\nTotal" ++ dots ++ formatted_price
    where
        formatted_price = formatPence price
        dots = replicate (lineLength - (length formatted_price) - 5) '.'
 
-- 6.44
formatBill :: BillType -> String
formatBill bill = formatted_bill ++ formatted_discount ++ formatted_total ++ "\n"
    where
        formatted_bill = formatLines bill
        discount = makeDiscount bill
        formatted_discount = if discount > 0 then formatDiscount discount else ""
        formatted_total = formatTotal $ (makeTotal bill) - discount

-- 6.45
look :: Database -> BarCode -> (Name, Price)
look db code = if null look_result then ("Unknown Item",0) else head look_result
    where
        look_result = [ (n,p) | (c,n,p) <- db, c == code ]
        
-- 6.46
lookup :: BarCode -> (Name, Price)
lookup code = look codeIndex code
        
-- 6.47
makeBill :: TillType -> BillType
makeBill codes = [lookup code | code <- codes, (lookup code /= ("Unknown Item",0)) ]

produceBill :: TillType -> String
produceBill = formatBill . makeBill

-- 6.48
unique :: (Eq a) => [a] -> [a]
unique [] = []
unique (x:xs) 
    | elem x xs = unique xs
    | otherwise = x : unique xs

makeDiscount :: BillType -> Price
makeDiscount bill = sum $ [(div num_occurs 2) * 100 | (item,num_occurs) <- items_occurrences, num_occurs > 1]
    where
        items = fst $ unzip bill
        occurrences item = length [ i | i <- items, i == item ]
        unique_items = unique items
        items_occurrences = zip unique_items [occurrences item | item <- unique_items]
        
formatDiscount :: Price -> String
formatDiscount discount = "\nDiscount" ++ dots ++ formatted_discount ++ "\n"
    where
        formatted_discount = formatPence discount
        dots = replicate (lineLength - 8 - (length formatted_discount)) '.'
 
-- 6.49
add :: BarCode -> (Name, Price) -> Database -> Database
add code (n,p) db = nub $ (code, n, p) : db
