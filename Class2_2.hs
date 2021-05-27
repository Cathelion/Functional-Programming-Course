module Class2_2 where

data File
  = File String
  | Dir String [File]
 deriving ( Eq, Show )
 
type FileSystem = [File]
                                           
search :: FileSystem -> String -> [String]
search files name =
  [ name
  | File name' <- files
  , name == name'
  ] ++
  [ dir ++ "/" ++ path
  | Dir dir files' <- files
  , path <- search files' name
  ]

exampleFileSystem :: FileSystem
exampleFileSystem =
  [ File "apa"
  , Dir "bepa" [ File "apa", Dir "bepa" [], Dir "cepa" [ File "bepa" ] ]
  , Dir "cepa" [ Dir "bepa" [], Dir "cepa" [ File "apa" ] ]
  ]

