type Digit = Char

type Presses = Int


data Option = [Digit] | Cap deriving (Eq, Show)

data Button = Button Key Option deriving (Eq, Show)
 
data DaPhone = DaPhone [Button] deriving (Eq, Show)


convo :: [String]
convo =
    ["Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol ok. Have u ever tasted alcohol lol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "Ok. Do u think I am pretty Lol",
    "Lol ya",
    "Haha thanks just making sure rofl ur turn"]

-- validButtons = "1234567890*#"
