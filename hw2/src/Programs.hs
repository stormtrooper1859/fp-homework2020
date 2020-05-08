{-# LANGUAGE OverloadedStrings #-}

module Programs (getProgram) where

import Programs.Cat (cat)
import Programs.ChangeDirectory (changeDirectory)
import Programs.List (list)
import Programs.MakeDirectory (makeDirectory)
import Programs.Remove (remove)
import Programs.WriteDataInFile (writeDataInFile)
import Typings (Subprogram)


defaultProgram :: String -> Subprogram
defaultProgram name _ _ = return $ Just $ name ++ " not found"


getProgram :: String -> Subprogram
getProgram x = case x of
            "cd"    -> changeDirectory
            "ls"    -> list
            "mkdir" -> makeDirectory
            "cat"   -> cat
            "wf"    -> writeDataInFile
            "rm"    -> remove
            _       -> defaultProgram x
