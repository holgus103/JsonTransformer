module Operations where 

-- filter : .[] | .message
-- assignment: .[].message = 11
-- relative assignment: .[].message = .[].topic
-- appending: .message += "test"
-- remove : del .[].message

data Op = Filtering [String]
        |  Assignment [String] String
        |  Appending [String] String
        |  Removal [String]
        | None deriving (Show)

