--ApplicativeEg.hs

type Name = String

data Employee = Employee { name :: Name , phone :: String} deriving (Show)

mname1 , mname2 :: Maybe Name
mname1 = Just "Test1"
mname2 = Just "Test2"
mname3 = Nothing

mphone1,mphone2 :: Maybe String
mphone1 = Just "123"
mphone2 = Just "234"
mphone3 = Nothing

emp1 = Employee <$> mname1 <*> mphone1
emp2 = Employee <$> mname3 <*> mphone2
