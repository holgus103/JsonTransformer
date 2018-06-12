module Enums where 

data Op = Filtering [String]
        |  AssignmentD [String] String
        |  AssignmentR [String] String
        |  AddD [String] String
        |  AddR [String] String
        |  Removal [String]
        | None deriving (Show)

data Action = FieldRemove | FieldAssignD String | FieldSrc | FieldDst String| FieldAddSrc 

data ConduitResult = Empty | NonEmpty
