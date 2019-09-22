module Data.Locale exposing (Locale, encodeLocale, getDefaultLocale, getEnglishLocale, getGermanLocale, isEnglishLocale, isGermanLocale, localeDecoder, translateApplyTags, translateBatchTagging, translateCancel, translateDefaultTags, translateEnterATag, translateErrorHeading, translateErrorParsingYourFile, translateHowBatchTaggingWorks, translateLocale, translateManageYourTags, translateNoMatchingRecordsFound, translateRecordsThatWillBeTagged, translateSave, translateSelectAKeywordOrRegex, translateSelectATagToTag, translateSelectAcsvFile, translateSingleTagging, translateTableFileName, translateTag, translateTaggedRecords)

import Json.Decode as Decode
import Json.Encode as Encode


type Locale
    = DE
    | EN


type alias Translation =
    Locale -> String


isEnglishLocale : Locale -> Bool
isEnglishLocale locale =
    locale == EN


isGermanLocale : Locale -> Bool
isGermanLocale locale =
    locale == DE


getEnglishLocale : Locale
getEnglishLocale =
    EN


getGermanLocale : Locale
getGermanLocale =
    DE


getDefaultLocale : Locale
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



-- "Tag already exists"
-- "There was an error reading your file : "
-- "Sorting Tables"
-- "Index for TableData.Header lookup failed."
-- "TableData lookup failed."
-- "Please select a file to work with first. Your file may be empty."
-- "Table Data lookup failed. No Data given."
-- "Choose Dataformats"
-- Select a tag to tag your records:
-- "expecting float number"
-- "There are no records yet to choose from, please select a file."
-- "  Records are processed in order provided by your file."
-- "    How Batch Tagging works: Choose a column and insert a keyword to match datasets which have these keyword in a cell. Every matching dataset is then tagged by the tag you choose next."


translateDefaultTags : Locale -> List String
translateDefaultTags locale =
    case locale of
        EN ->
            [ "finance", "household", "expenses" ]

        DE ->
            [ "finanzen", "haushalt", "ausgaben" ]


translateLocale : Translation
translateLocale locale =
    case locale of
        EN ->
            "Locale"

        DE ->
            "Sprache"


translateSave : Translation
translateSave locale =
    case locale of
        EN ->
            "Save"

        DE ->
            "Speichern"


translateCancel : Translation
translateCancel locale =
    case locale of
        EN ->
            "Speichern"

        DE ->
            "Abbrechen"


translateHowBatchTaggingWorks locale =
    case locale of
        EN ->
            "How Batch Tagging works: Choose one or more columns and insert a keyword to match datasets which have these keyword in a cell. Every matching dataset is then tagged by the tag you choose next."

        DE ->
            "Wie Stapel Zuordnung funktioniert: Gebe zuerst einen Suchbegriff für eine oder mehrere Spalten ein. Diese Suchbegriffe werden den übereinstimmenden Einträgen zugeordnet. Wähle als nächtes eine Kategorie um die Einträge in deine kategorisierten Einträge zu übernehmen."


translateSelectATagToTag : Translation
translateSelectATagToTag locale =
    case locale of
        EN ->
            "Select a tag to proceed tagging your matching records"

        DE ->
            "Wähle eine Kategorie um die übereinstimmenden Einträge zuzuordnen"


translateTaggedRecords : Translation
translateTaggedRecords locale =
    case locale of
        EN ->
            "Tagged records"

        DE ->
            "Kategorisierte Einträge"


translateSelectAKeywordOrRegex : Translation
translateSelectAKeywordOrRegex locale =
    case locale of
        EN ->
            "Insert a keyword (Plain or Regex) ..."

        DE ->
            "Suchbegriff oder Regulären Ausdruck eingeben ..."


translateBatchTagging : Translation
translateBatchTagging locale =
    case locale of
        EN ->
            "Batch Tagging"

        DE ->
            "Stapel Zuordnung"


translateSingleTagging : Translation
translateSingleTagging locale =
    case locale of
        EN ->
            "Single Tagging"

        DE ->
            "Einzel Zuordnung"


translateTableFileName : Translation
translateTableFileName locale =
    case locale of
        EN ->
            "table"

        DE ->
            "tabelle"


translateEnterATag : Translation
translateEnterATag locale =
    case locale of
        EN ->
            "enter a tag ..."

        DE ->
            "Kategorie eingeben ..."


translateNoMatchingRecordsFound : Translation
translateNoMatchingRecordsFound locale =
    case locale of
        EN ->
            "There were no matching records found"

        DE ->
            "Es wurden keine Einträge gefunden, die mit deinen Suchkriterien übereinstimmen"


translateRecordsThatWillBeTagged : Locale -> Int -> String
translateRecordsThatWillBeTagged locale count =
    case locale of
        EN ->
            String.fromInt count ++ " Records that will be tagged"

        DE ->
            String.fromInt count ++ " Einträge die kategorisiert werden sollen"


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
