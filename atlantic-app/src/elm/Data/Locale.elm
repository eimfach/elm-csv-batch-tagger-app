module Data.Locale exposing (Locale, encodeLocale, getDefaultLocale, getEnglishLocale, getGermanLocale, isEnglishLocale, isGermanLocale, localeDecoder, translateApplyTags, translateErrorHeading, translateErrorParsingYourFile, translateLocale, translateManageYourTags, translateSelectAcsvFile, translateTag)

import Json.Decode as Decode
import Json.Encode as Encode


type Locale
    = DE
    | EN


type alias Translation =
    Locale -> String



-- "Tag already exists"
-- "There was an error reading your file : "
-- "Tag" (used as header)
-- -table (used in filename)
-- " Records that will be tagged"
-- "There were no matching records found"
-- "Sorting Tables"
-- "Index for TableData.Header lookup failed."
-- "TableData lookup failed."
-- "Please select a file to work with first. Your file may be empty."
-- "Table Data lookup failed. No Data given."
-- "Can you tell me the dataformats of each column of your table ?"
-- "Choose Dataformats"
-- Select a tag to tag your records:
-- "expecting float number"
-- "There are no records yet to choose from, please select a file."
-- "  Records are processed in order provided by your file."
-- "There are no records yet to choose from, please select a file."
-- "    How Batch Tagging works: Choose a column and insert a keyword to match datasets which have these keyword in a cell. Every matching dataset is then tagged by the tag you choose next."
-- "Select a keyword (Plain or Regex)"
-- "enter a tag"
-- Locale


translateLocale : Translation
translateLocale locale =
    case locale of
        EN ->
            "Locale"

        DE ->
            "Sprache"


translateTagAlreadyExists : Translation
translateTagAlreadyExists locale =
    case locale of
        EN ->
            "Tag already exists"

        DE ->
            "Die Kategorie exisitert bereits"


translateErrorParsingYourFile : Translation
translateErrorParsingYourFile locale =
    case locale of
        EN ->
            "There was an error parsing your file. The contents of your file are not supported."

        DE ->
            "Fehler beim Einlesen deiner Datei. Die Inhalte deiner Datei werden nicht unterstützt."


translateErrorHeading : Translation
translateErrorHeading locale =
    case locale of
        EN ->
            "Error"

        DE ->
            "Fehler"


translateSelectAcsvFile : Translation
translateSelectAcsvFile locale =
    case locale of
        EN ->
            "Select a .csv file"

        DE ->
            "Wähle eine .csv Datei aus"


translateManageYourTags : Translation
translateManageYourTags locale =
    case locale of
        EN ->
            "Manage your tags"

        DE ->
            "Verwalte deine Kategorien"


translateTag : Translation
translateTag locale =
    case locale of
        EN ->
            "Tag"

        DE ->
            "Kategorie"


translateApplyTags : Locale -> Int -> String
translateApplyTags locale count =
    case locale of
        EN ->
            "Apply tags (" ++ String.fromInt count ++ " left)"

        DE ->
            "Ordne Einträge einer Kategorie zu (" ++ String.fromInt count ++ " Einträge übrig)"



{--
 : Translation
 : Translation
 : Translation
 : Translation
 : Translation
 : Translation
 : Translation
 : Translation
 : Translation
 : Translation
 : Translation
 : Translation
 : Translation
 : Translation
 : Translation
 : Translation        
 --}


isEnglishLocale locale =
    locale == EN


isGermanLocale locale =
    locale == DE


getEnglishLocale =
    EN


getGermanLocale =
    DE


getDefaultLocale =
    EN


encodeLocale : Locale -> Encode.Value
encodeLocale locale =
    case locale of
        DE ->
            Encode.string "DE"

        EN ->
            Encode.string "EN"


createLocaleDecoder : String -> Decode.Decoder Locale
createLocaleDecoder encodedFormat =
    case parseLocale encodedFormat of
        Ok locale ->
            Decode.succeed locale

        Err err ->
            Decode.fail err


parseLocale : String -> Result String Locale
parseLocale locale =
    case locale of
        "DE" ->
            Ok DE

        "EN" ->
            Ok EN

        _ ->
            Err "Invalid locale encoding"


localeDecoder : Decode.Decoder Locale
localeDecoder =
    Decode.string |> Decode.andThen createLocaleDecoder
