module Enums where 

-- filter : .[] | .message
-- assignment: .[].message = 11
-- relative assignment: .[].message = .[].topic
-- appending: .message += "test"
-- remove : del .[].message

data Op = Filtering [String]
        |  AssignmentD [String] String
        |  AssignmentR [String] String
        |  AddD [String] String
        |  AddR [String] String
        |  Removal [String]
        | None deriving (Show)

data Action = FieldRemove | FieldAssignD String | FieldSrc | FieldDst String| FieldAddSrc 

data ConduitResult = Empty | NonEmpty
