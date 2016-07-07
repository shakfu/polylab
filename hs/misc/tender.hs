module Tender where

type Name = String
type Country = String
type Portion = String
type Description = String
type Proportion = Double
type Competitor = (String, Country)

data Currency = USD Double | SAR Double | GBP Double deriving Show

data DateTime = Date (Integer, Int, Int) | Days Integer | Months Integer deriving Show

data BidProcedure = OpenBid String | ClosedBid String deriving Show

data PriceBasis = Firm String | Variable String deriving Show

data Bid = Bid {
    client :: Name,
    country :: Country,
    tender_id :: String,
    project_description :: Description,
    bid_procedure :: BidProcedure,
    bid_closing_date :: DateTime,
    bid_opening_date :: DateTime,
    competitors :: [Competitor],
    scope_of_work :: String,
    completion_period :: DateTime,
    price_basis :: [PriceBasis],
    validity :: DateTime,
    bid_currencies :: [(Portion, Currency)],
    terms :: Terms,
    mode_of_payment :: [(Portion, Currency)],
    defects_liability_period :: [(Portion, DateTime)],
    penalty :: [(Portion, Proportion)],
    bank_guarantees :: [(Currency, DateTime)],
    budgetary_estimate :: Currency,
    notes :: String
    } deriving Show

data Terms = Terms {
    supply :: [(Proportion, Description)],
    local_transportation :: [(Proportion, Description)],
    installation :: [(Proportion, Description)]
    } deriving Show

bid = Bid {
    client = "ABC Corp",
    country = "USA",
    tender_id = "ABC No K2BSP - P1 - 3",
    project_description = "Power System Project",
    bid_procedure = OpenBid "International Competitive Bidding",
    bid_closing_date = Date (30, 3, 2009),
    bid_opening_date = Date (30, 3, 2009),
    competitors = [("XXX", "South Africa"), ("ZZZ", "India")],
    scope_of_work = "Small Factory",
    completion_period = Months 20,
    price_basis = [Firm "Foreign Currency Portion",
                   Variable "Local Currency Portion"],
    validity = Days 182,
    bid_currencies = [("Offshore", USD 0.9), ("Local", SAR 0.1)],
    terms = Terms {
        supply = [(0.1, "Advance payment against Bank Guarantee")],
        local_transportation = [(0.1, "Advance payment against Bank Guarantee")],
        installation = [(0.1, "Advance payment against Bank Guarantee")]
    },
    mode_of_payment = [("Foreign Currency", USD 0.1)],
    defects_liability_period = [("From date of completion", Months 18),
                                ("From date of operational acceptance", Months 12)],
    penalty = [("Contract Price per week", 0.05), ("Max Contract Value", 0.1)],
    bank_guarantees = [(USD 200000.0, Days 210)],
    budgetary_estimate = USD 7500000.0,
    notes = "etc.."
}

process :: Bid -> Double
process Bid {budgetary_estimate = USD v} = v * 3.751


main = do
    print bid
