module Enums where 

data Op =
        -- | Represents a filtering operation with an array of strings representing fields
        Filtering [String]
        -- | Represents a direct assignment with an array representing the target field and a string a value
        |  AssignmentD [String] String
        -- | Represents a relative assignment with an array representing the target field and a source field
        |  AssignmentR [String] String
        -- | Represents a direct field addition with an array representing the target field and a string a value
        |  AddD [String] String
        -- | Represents a relative fiedl addition with an array representing the target field and a source field
        |  AddR [String] String
        -- | Represents a removal operation  with an array representing the target field
        |  Removal [String]
        -- | No operation
        | None deriving (Show)

 
data Action = 
        -- | Remove field action
        FieldRemove 
        -- | Direct assingnment action containing a value
        | FieldAssignD String 
        -- | Marks a relative assignment source field
        | FieldSrc 
        -- | Marks a relative assignment destination field
        | FieldDst 
        -- | Marks a relative addition source field
        | FieldAddSrc 
        -- | Signifies an array addition at a certain index 
        | ArrayAdd String

data ConduitResult =
        -- | Signifies no output for a subconduit
        Empty 
        -- | Signifies that the subconduit has written some data to the output stream
        | NonEmpty
