module Locale exposing (Locale, encodeLocale, getDefaultLocale, getEnglishLocale, getGermanLocale, isEnglishLocale, isGermanLocale, localeDecoder, translateApplyTags, translateAskForDrop, translateBatchTagging, translateCancel, translateCheckboxStackData, translateDefaultTags, translateDeleteYourLocalData, translateDrop, translateEmptyFileText, translateEmptyFileTitle, translateEnterATag, translateErrorHeading, translateErrorParsingYourFile, translateHowBatchTaggingWorks, translateHowManualTaggingWorks, translateImport, translateImportData, translateImportDataNoRecordsFound, translateImportDataNoRecordsFoundTitle, translateIncompatibleDataComparison, translateIncompatibleDataIntro, translateIncompatibleDataTitle, translateInfoOnHowDataIsStored, translateIrregularRowsText, translateIrregularRowsTitle, translateLocale, translateManageYourTags, translateNoMatchingRecordsFound, translateNoRecordsToChooseFromSelectAfile, translateNoWorkingData, translateProceed, translateRecordsThatWillBeTagged, translateSave, translateSelectAKeywordOrRegex, translateSelectATagToTag, translateSelectAcsvFile, translateSingleTagging, translateTableFileName, translateTag, translateTagAlreadyExists, translateTaggedRecords, translateTitleDeleteLocalData, translateTitleRemainingWorkingData, translateUncheckStackDataWarning, translateViewSourceCode, translateViewUserDocumentation, translateWarningDeleteLocalData, translateWarningLabel)

import Json.Decode as Decode
import Json.Encode as Encode
import Parsers exposing (parseFloat)


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
-- "expecting float number"


translateViewUserDocumentation : Translation
translateViewUserDocumentation locale =
    case locale of
        EN ->
            "View User Documentation"

        DE ->
            "Benutzer Dokumentation anzeigen"


translateNoRecordsToChooseFromSelectAfile : Translation
translateNoRecordsToChooseFromSelectAfile locale =
    case locale of
        EN ->
            "There are no records to choose from, please select a file."

        DE ->
            "Es sind keine Einträge vorhanden, bitte wähle eine Datei aus."


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


translateProceed : Translation
translateProceed locale =
    case locale of
        EN ->
            "proceed"

        DE ->
            "fortfahren"


translateWarningDeleteLocalData : Translation
translateWarningDeleteLocalData locale =
    case locale of
        EN ->
            "Warning: Your data will be permanently deleted!"

        DE ->
            "Warnung: Deine Daten werden permanent gelöscht!"


translateTitleDeleteLocalData : Translation
translateTitleDeleteLocalData locale =
    case locale of
        EN ->
            "Delete your local data"

        DE ->
            "Deine lokalen Daten löschen"


translateDeleteYourLocalData : Translation
translateDeleteYourLocalData locale =
    case locale of
        EN ->
            "Delete data"

        DE ->
            "Daten löschen"


translateInfoOnHowDataIsStored : Translation
translateInfoOnHowDataIsStored locale =
    case locale of
        EN ->
            "Your data is stored locally in your Browser solely."

        DE ->
            "Deine Daten werden ausschließlich lokal im Browser gespeichert."


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
            "Cancel"

        DE ->
            "Abbrechen"


translateHowManualTaggingWorks : Translation
translateHowManualTaggingWorks locale =
    case locale of
        EN ->
            "Records are processed in order provided by your file."

        DE ->
            "Einträge werden in der selben Reihenfolge wie in Datei verarbeitet."


translateHowBatchTaggingWorks : Translation
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
            "Add a .csv file"

        DE ->
            ".csv Datei hinzufügen"


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


translateImportData : Locale -> String
translateImportData locale =
    case locale of
        EN ->
            "Import Data from CSV File"

        DE ->
            "Importiere Daten von einer CSV Datei"


translateImportDataNoRecordsFound : Locale -> String
translateImportDataNoRecordsFound locale =
    case locale of
        EN ->
            "Your data does not include any records."

        DE ->
            "Deine Daten beinhalten keine Einträge."


translateImportDataNoRecordsFoundTitle : Locale -> String
translateImportDataNoRecordsFoundTitle locale =
    case locale of
        EN ->
            "No records"

        DE ->
            "Keine Einträge"


translateImport : Locale -> String
translateImport locale =
    case locale of
        EN ->
            "Import"

        DE ->
            "Importieren"


translateViewSourceCode : Locale -> String
translateViewSourceCode locale =
    case locale of
        DE ->
            "Quellcode anzeigen"

        EN ->
            "View Source Code"


translateCheckboxStackData : Locale -> String
translateCheckboxStackData locale =
    case locale of
        EN ->
            "Stack data ?"

        DE ->
            "Aktuelle Datensätze behalten ?"


translateWarningLabel : Locale -> String
translateWarningLabel locale =
    case locale of
        EN ->
            "Warning"

        DE ->
            "Warnung"


translateUncheckStackDataWarning : Locale -> String
translateUncheckStackDataWarning locale =
    case locale of
        EN ->
            "Disabling this will discard all untagged data."

        DE ->
            "Deaktivierung wird alle unkategorisierten Daten verwerfen."


translateEmptyFileTitle : Locale -> String
translateEmptyFileTitle locale =
    case locale of
        EN ->
            "Empty File"

        DE ->
            "Leere Datei"


translateEmptyFileText : Locale -> String
translateEmptyFileText locale =
    case locale of
        EN ->
            "Your file seems to be empty. Please choose another one."

        DE ->
            "Deine Datei scheint keinen Inhalt zu haben. Bitte wähle eine andere Datei."


translateIrregularRowsTitle : Locale -> String
translateIrregularRowsTitle locale =
    case locale of
        EN ->
            "Irregular rows"

        DE ->
            "Unreguläre Einträge"


translateIrregularRowsText : Locale -> Int -> String
translateIrregularRowsText locale count =
    case locale of
        EN ->
            "Your data contains " ++ String.fromInt count ++ " irregular rows in length"

        DE ->
            "Deine Daten haben beinhalten " ++ String.fromInt count ++ " irregulär lange Einträge"


translateDrop : Locale -> String
translateDrop locale =
    case locale of
        EN ->
            "Drop & Proceed"

        DE ->
            "Verwerfen & Fortfahren"


translateAskForDrop : Locale -> String
translateAskForDrop locale =
    case locale of
        EN ->
            "Do you want to drop theses records and proceed ?"

        DE ->
            "Möchtest du diese Einträge verwerfen und fortfahren ?"


translateIncompatibleDataTitle : Locale -> String
translateIncompatibleDataTitle locale =
    case locale of
        EN ->
            "Incompatible Data"

        DE ->
            "Inkompatible Daten"


translateIncompatibleDataIntro : Locale -> String
translateIncompatibleDataIntro locale =
    case locale of
        EN ->
            "The data you are trying to import is different then the data in your current workspace. Your workspace data is structured like this: "

        DE ->
            "Die Daten die du importieren willst, unterscheiden sich von denen in deinem Workspace. Deine Workspace Daten sind wie folgt strukturiert: "


translateIncompatibleDataComparison : Locale -> String
translateIncompatibleDataComparison locale =
    case locale of
        EN ->
            "Where as the data you are trying to import is structured like this: "

        DE ->
            "Zum Vergleich, die Daten die du importieren willst, sind wie folgt strukturiert: "


translateNoWorkingData : Locale -> String
translateNoWorkingData locale =
    case locale of
        EN ->
            "No working data ..."

        DE ->
            "Keine Arbeitsdaten vorhanden ..."


translateTitleRemainingWorkingData : Locale -> String
translateTitleRemainingWorkingData locale =
    case locale of
        EN ->
            "Remaining working data"

        DE ->
            "Restliche Arbeitsdaten"
