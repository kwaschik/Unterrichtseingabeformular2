port module Main exposing (..)

import Browser
import Html exposing (Html, Attribute, section, h1, div, label, input, textarea
                     , select, option, button, text)
import Html.Attributes exposing (id, class, value, multiple, selected, rows, type_)
import Html.Events exposing (on, onInput, onClick)
import Json.Decode as Decode
import Json.Encode as Encode
import List 
import Maybe
import Set
import String
import Tuple 


main =
  Browser.element { init = init
                  , view = view
                  , update = update
                  , subscriptions = \_ -> Sub.none 
                  }

-- PORTS

port save : String -> Cmd msg


-- MODEL

-- Hierarchie
type alias Fach = 
    { id                : String
    , name              : String
    , counter           : Int
    }
type alias Fachbereich =
    { fachId            : String
    , id                : String
    , name              : String
    , counter           : Int
    }
type alias Thema =
    { fachId            : String
    , fachbereichId     : String
    , id                : String
    , name              : String
    , counter           : Int
    }
type alias Themenbereich =
    { fachId            : String
    , fachbereichId     : String
    , themaId           : String
    , id                : String
    , name              : String
    , counter           : Int
    }
type alias Dysfunktionen =
    { fachId            : String
    , fachbereichId     : String
    , themaId           : String
    , themenbereichId   : String
    , id                : String
    , name              : String
    , counter           : Int
    }
type alias Pathologie =
    { fachId            : String
    , fachbereichId     : String
    , themaId           : String
    , themenbereichId   : String
    , id                : String
    , name              : String
    , counter           : Int
    }

-- ausserhalb der Hierarchie
type alias Jahrgang = 
    { id                : String
    , name              : String
    , counter           : Int
    }
type alias Dozent = 
    { id                : String
    , name              : String
    , counter           : Int
    }

type alias Model =
    { faecher           : List Fach             -- Liste aller Fächer
    , selFaecher        : List String           -- Liste akt. gewählter Fächer
    , fachbereiche      : List Fachbereich      -- Liste aller Fachbereiche
    , selFachbereiche   : List String           -- Liste akt. gewählter Fachbereiche
    , themen            : List Thema            -- Liste aller Themen
    , selThemen         : List String           -- Liste akt. gewählter Themen
    , themenbereiche    : List Themenbereich    -- Liste aller Themenbereiche
    , selThemenbereiche : List String           -- Liste akt. gewählter Themenbereiche
    , dysfunktionen     : List Dysfunktionen    -- Liste aller Dysfunktionen
    , selDysfunktionen  : List String           -- Liste akt. gewählter Dysfunktionen
    , pathologien       : List Pathologie       -- Liste aller Pathologien
    , selPathologien    : List String           -- Liste akt. gewählter Pathologien

    -- Daten ausserhalb der Hierarchie
    , jahrgaenge        : List Jahrgang         -- Liste aller Jahrgänge
    , selJahrgang       : String                -- akt. gewählter Jahrgang
    , dozenten          : List Dozent           -- Liste aller Dozenten
    , selDozent         : String                -- akt. gewählter Dozent
    , stunden           : List String           -- Liste mit vorgegebenen Stundenwerten
    , selStunden        : String                -- akt. gewählte Anzahl Stunden
    , selDatum          : String                -- akt. eingegebenes Datum
    , selAssistent      : String                -- akt. eingegebener Assistent
    , selBemerkungen    : String                -- akt. eingegebene Bemerkung
    }

decoderHelper field t = 
    Decode.map3 t
        (Decode.field "id" Decode.string)
        (Decode.at ["fields", field] Decode.string)
        (Decode.at ["fields", "Counter"] Decode.int)
        
decoder field constructor =
    Decode.at [field, "records"]
        (Decode.list
            (decoderHelper field constructor))

decodeAirtableFachbereiche =
    Decode.at ["Fachbereich", "records"] 
        (Decode.list 
            (Decode.at ["fields", "Fach"] (Decode.index 0 Decode.string)
            |> Decode.andThen (\s -> 
                decoderHelper "Fachbereich" (Fachbereich s))))
decodeAirtableThemen =
    Decode.at ["Thema", "records"] 
        (Decode.list 
            (Decode.at ["fields", "Fach"] (Decode.index 0 Decode.string)
            |> Decode.andThen (\s1 ->
                (Decode.at ["fields", "Fachbereich"] (Decode.index 0 Decode.string)
                |> Decode.andThen (\s2 ->
                    decoderHelper "Thema" (Thema s1 s2))))))
decodeAirtableThemenbereiche =
    Decode.at ["Themenbereich", "records"] 
        (Decode.list 
            (Decode.at ["fields", "Fach"] (Decode.index 0 Decode.string)
            |> Decode.andThen (\s1 ->
                (Decode.at ["fields", "Fachbereich"] (Decode.index 0 Decode.string)
                |> Decode.andThen (\s2 ->
                    (Decode.at ["fields", "Thema"] (Decode.index 0 Decode.string)
                    |> Decode.andThen (\s3 ->
                        decoderHelper "Themenbereich" (Themenbereich s1 s2 s3))))))))
decodeAirtableDysfunktionen =
    Decode.at ["Dysfunktionen", "records"] 
        (Decode.list 
            (Decode.at ["fields", "Fach"] (Decode.index 0 Decode.string)
            |> Decode.andThen (\s1 ->
                (Decode.at ["fields", "Fachbereich"] (Decode.index 0 Decode.string)
                |> Decode.andThen (\s2 ->
                    (Decode.at ["fields", "Thema"] (Decode.index 0 Decode.string)
                    |> Decode.andThen (\s3 ->
                        Decode.maybe (Decode.at ["fields", "Themenbereich"] (Decode.index 0 Decode.string))
                        |> Decode.andThen (\s4 ->
                            decoderHelper "Dysfunktionen" (Dysfunktionen s1 s2 s3 (Maybe.withDefault "" s4))))))))))
decodeAirtablePathologien =
    Decode.at ["Pathologie", "records"] 
        (Decode.list 
            (Decode.at ["fields", "Fach"] (Decode.index 0 Decode.string)
            |> Decode.andThen (\s1 ->
                (Decode.at ["fields", "Fachbereich"] (Decode.index 0 Decode.string)
                |> Decode.andThen (\s2 ->
                    (Decode.at ["fields", "Thema"] (Decode.index 0 Decode.string)
                    |> Decode.andThen (\s3 ->
                        Decode.maybe (Decode.at ["fields", "Themenbereich"] (Decode.index 0 Decode.string))
                        |> Decode.andThen (\s4 ->
                            decoderHelper "Pathologie" (Pathologie s1 s2 s3 (Maybe.withDefault "" s4))))))))))

init : Decode.Value -> (Model, Cmd Message)
init v =
    let sort = List.sortBy .counter
        faecher         = case (Decode.decodeValue (decoder "Fach" Fach) v) of
                            Err e -> [] --Debug.log (Decode.errorToString e) []
                            Ok ds -> sort ds
        fachbereiche    = case (Decode.decodeValue decodeAirtableFachbereiche v) of
                            Err e -> []
                            Ok ds -> sort ds
        themen          = case (Decode.decodeValue decodeAirtableThemen v) of
                            Err e -> []
                            Ok ds -> sort ds
        themenbereiche  = case (Decode.decodeValue decodeAirtableThemenbereiche v) of
                            Err e -> []
                            Ok ds -> sort ds
        dysfunktionen   = case (Decode.decodeValue decodeAirtableDysfunktionen v) of
                            Err e -> []
                            Ok ds -> sort ds
        pathologien     = case (Decode.decodeValue decodeAirtablePathologien v) of
                            Err e -> []
                            Ok ds -> sort ds
        jahrgaenge      = case (Decode.decodeValue (decoder "Jahrgang" Jahrgang) v) of
                            Err e -> []
                            Ok ds -> sort ds
        dozenten        = case (Decode.decodeValue (decoder "Dozent" Dozent) v) of
                            Err e -> []
                            Ok ds -> sort ds
    in
    ({ jahrgaenge = jahrgaenge
     , selJahrgang = ""
     , faecher = faecher
     , selFaecher = []
     , fachbereiche = fachbereiche
     , selFachbereiche = []
     , stunden = [ "1", "2", "3", "4", "5", "6", "7", "8" ]
     , selStunden = ""
     , themen = themen
     , selThemen = []
     , themenbereiche = themenbereiche
     , selThemenbereiche = []
     , dysfunktionen = dysfunktionen
     , selDysfunktionen = []
     , pathologien = pathologien
     , selPathologien = []
     , dozenten = dozenten 
     , selDozent = ""
     , selDatum = ""
     , selAssistent = ""
     , selBemerkungen = ""
     }, Cmd.none)


-- UPDATE

type Message = SelectFaecher (List String)
             | SelectFachbereiche (List String)
             | SelectThemen (List String)
             | SelectThemenbereiche (List String)
             | SelectDysfunktionen (List String)
             | SelectPathologien (List String)
             | SelectJahrgang String
             | SelectDozent String
             | SelectStunden String
             | InputAssistent String
             | InputBemerkungen String
             | InputDatum String
             | Save
             | Reset


--dbgUpdate message model =
--    Debug.log (Debug.toString message) (update message model)

-- helper functions
in_ : List String -> (a -> String) -> a -> Bool
in_ values flt = \x -> List.member (flt x) values

in_or_empty : List String -> (a -> String) -> a -> Bool
in_or_empty values flt = 
    case values of
        [] -> always True
        _  -> in_ values flt

maybeIn : List String -> (a -> String) -> a -> Maybe String
maybeIn values flt x = 
    case (in_ values flt x) of
        True  -> Just (flt x)
        False -> Nothing

intersect : List String -> List String -> List String
intersect ys = List.filter (\x -> List.member x ys)

withDefault : List String -> List String -> List String
withDefault default list = if (List.isEmpty list) then default else list

-- zum Speichern in airtable-Format bringen 
airtableName m = 
    let dozent   = List.map .name << List.filter (\x -> x.id == m.selDozent) <| m.dozenten
        jahrgang = List.map .name << List.filter (\x -> x.id == m.selJahrgang) <| m.jahrgaenge
        faecher  = List.map .name << List.filter (.id |> in_ m.selFaecher) <| m.faecher
    in  String.join "_" <| dozent ++ jahrgang ++ faecher ++ [m.selDatum]

encodeModel : Model -> Encode.Value
encodeModel m = Encode.object 
                    [ ("Name", Encode.string <| airtableName m)
                    , ("Jahrgang", Encode.list Encode.string (if (m.selJahrgang == "") then [] else [m.selJahrgang]))
                    , ("Fach", Encode.set Encode.string <| Set.fromList m.selFaecher)
                    , ("Fachbereich", Encode.set Encode.string <| Set.fromList m.selFachbereiche)
                    , ("Thema", Encode.set Encode.string <| Set.fromList m.selThemen)
                    , ("Themenbereich", Encode.set Encode.string <| Set.fromList m.selThemenbereiche)
                    , ("Dysfunktionen", Encode.set Encode.string <| Set.fromList m.selDysfunktionen)
                    , ("Pathologie", Encode.set Encode.string <| Set.fromList m.selPathologien)
                    , ("Stunden", Encode.int <| Maybe.withDefault 0 <| String.toInt m.selStunden)
                    , ("Dozent", Encode.list Encode.string (if (m.selDozent == "") then [] else [m.selDozent]))
                    , ("Datum", Encode.string m.selDatum)
                    , ("Assistent", Encode.string m.selAssistent)
                    , ("Notes", Encode.string m.selBemerkungen)
                    ]


-- bei einer Selektion die Hierarchie aufsteigend durchlaufen und auf jeder
-- Ebene die entsprechenden Anpassungen/Selektionen vornehmen
-- nämlich: aus der Menge der selektierten Datensätze der aktuellen Ebene
-- die Ids der dadurch mitselektierten Datensätze der übergeordneten Ebene
-- ermitteln und zur Selektion an die übergeordnete Ebene übergeben
upward message model =
    case message of
        SelectFaecher ids        -> { model | selFaecher = ids }

        SelectFachbereiche ids   -> let selected = List.map .fachId << List.filter (.id |> in_ ids) <| model.fachbereiche
                                        fIds     = withDefault model.selFaecher selected
                                    in upward (SelectFaecher fIds) { model | selFachbereiche = ids }

        SelectThemen ids         -> let selected = List.map .fachbereichId << List.filter (.id |> in_ ids) <| model.themen
                                        fbIds    = withDefault model.selFachbereiche selected
                                    in upward (SelectFachbereiche fbIds) { model | selThemen = ids }

        SelectThemenbereiche ids -> let selected = List.map .themaId << List.filter (.id |> in_ ids) <| model.themenbereiche
                                        tIds     = withDefault model.selThemen selected
                                    in upward (SelectThemen tIds) { model | selThemenbereiche = ids }

        SelectDysfunktionen ids  -> let selected = List.filter (.id |> in_ ids) model.dysfunktionen
                                        tbIds    = withDefault model.selThemenbereiche <| List.map .themenbereichId selected
                                        -- es gibt Dysfunktionen, die keinem Themenbereich untergeordnet sind, dann Themen prüfen
                                        tIds     = withDefault model.selThemen <| List.map .themaId selected
                                    in 
                                    if (List.isEmpty (List.filter ((/=)"") tbIds))
                                    then upward (SelectThemen tIds) { model | selThemenbereiche = [], selDysfunktionen = ids }
                                    else upward (SelectThemenbereiche tbIds) { model | selDysfunktionen = ids }

        SelectPathologien ids    -> let selected = List.filter (.id |> in_ ids) model.pathologien
                                        tbIds    = withDefault model.selThemenbereiche <| List.map .themenbereichId selected
                                        -- es gibt Pathologien, die keinem Themenbereich untergeordnet sind, dann Themen prüfen
                                        tIds     = withDefault model.selThemen <| List.map .themaId selected
                                    in
                                    if (List.isEmpty (List.filter ((/=)"") tbIds))
                                    then upward (SelectThemen tIds) { model | selThemenbereiche = [], selPathologien = ids }
                                    else upward (SelectThemenbereiche tbIds) { model | selPathologien = ids }

        _                        -> model

-- bei einer Selektion die Hierarchie absteigend durchlaufen und auf jeder
-- Ebene die entsprechenden Anpassungen/Selektionen vornehmen
-- nämlich: die auf der untergeordneten Ebene akt. selektierten Datensätze
-- durch die Selektion der akt. Ebene einschränken und zur Selektion
-- an die untergeordnete Ebene übergeben
downward message model =
    case message of
        SelectFaecher ids        -> let fbIds = intersect model.selFachbereiche 
                                             <| List.filterMap (.fachId |> maybeIn ids) model.fachbereiche
                                    in downward (SelectFachbereiche fbIds) { model | selFaecher = ids }

        SelectFachbereiche ids   -> let thIds = intersect model.selThemen 
                                             <| List.filterMap (.fachbereichId |> maybeIn ids) model.themen
                                    in downward (SelectThemen thIds) { model | selFachbereiche = ids }

        SelectThemen ids         -> let tbIds = intersect model.selThemenbereiche 
                                             <| List.filterMap (.themaId |> maybeIn ids) model.themenbereiche
                                    in downward (SelectThemenbereiche tbIds) { model | selThemen = ids }

        SelectThemenbereiche ids -> let dIds  = intersect model.selDysfunktionen 
                                             <| List.filterMap (.themenbereichId |> maybeIn ids) model.dysfunktionen
                                        pIds  = intersect model.selPathologien 
                                             <| List.filterMap (.themenbereichId |> maybeIn ids) model.pathologien
                                    in (downward (SelectDysfunktionen dIds) >> downward (SelectPathologien pIds))
                                         { model | selThemenbereiche = ids }

        SelectDysfunktionen ids  -> { model | selDysfunktionen = ids }
        SelectPathologien ids    -> { model | selPathologien = ids }
        _                        -> model

update message model =
    let flt = List.filter ((/=)"")
        msg = message --Debug.log (Debug.toString model.selDysfunktionen) message
    in
    case msg of
        SelectFaecher ids        -> (downward (SelectFaecher (flt ids)) model, Cmd.none)
        SelectFachbereiche ids   -> (upward (SelectFachbereiche (flt ids)) model |> downward (SelectFachbereiche (flt ids)), Cmd.none)
        SelectThemen ids         -> (upward (SelectThemen (flt ids)) model |> downward (SelectThemen (flt ids)), Cmd.none)
        SelectThemenbereiche ids -> (upward (SelectThemenbereiche (flt ids)) model |> downward (SelectThemenbereiche (flt ids)), Cmd.none)
        SelectDysfunktionen ids  -> (upward (SelectDysfunktionen (flt ids)) model, Cmd.none)
        SelectPathologien ids    -> (upward (SelectPathologien (flt ids)) model, Cmd.none)
        SelectJahrgang id        -> ({ model | selJahrgang = id }, Cmd.none)
        SelectDozent id          -> ({ model | selDozent = id }, Cmd.none)
        SelectStunden id         -> ({ model | selStunden = id }, Cmd.none)
        InputAssistent data      -> ({ model | selAssistent = data }, Cmd.none)
        InputBemerkungen data    -> ({ model | selBemerkungen = data }, Cmd.none)
        InputDatum       data    -> ({ model | selDatum = data }, Cmd.none)
        Reset                    -> ({ model | selJahrgang = ""
                                             , selFaecher = []
                                             , selFachbereiche = []
                                             , selStunden = ""
                                             , selThemen = []
                                             , selThemenbereiche = []
                                             , selDysfunktionen = []
                                             , selPathologien = []
                                             , selDozent = ""
                                             , selDatum = ""
                                             , selAssistent = ""
                                             , selBemerkungen = ""
                                     }, Cmd.none)
        Save                     -> (model, save <| Encode.encode 0 <| encodeModel model)

onMultiChange : (List String -> Message) -> Attribute Message
onMultiChange createMessage =
    on "change" (Decode.map (createMessage << (List.map Tuple.second))
        (Decode.at ["target", "selectedOptions"] 
            (Decode.keyValuePairs (Decode.field "value" Decode.string))))


-- VIEW

select_ lbl attributes options = 
    div [class "field"] 
        [ label [class "label"] 
                [text lbl]
        , div [class "control"] 
            [
            if (List.member (multiple True) attributes)
            then div [class "select is-multiple"]
                    [select attributes <| (option [value ""] [text "-"])::options]
            else div [class "select"]
                    [select attributes <| (option [value ""] [text "-"])::options]
            ]
        ]

availableThemenbereiche model = 
    List.filter (.fachId |> in_or_empty model.selFaecher)
        (List.filter (.fachbereichId |> in_or_empty model.selFachbereiche) 
            (List.filter (.themaId |> in_or_empty model.selThemen) 
                model.themenbereiche))
availableDysfunktionen model =
    List.filter (.fachId |> in_or_empty model.selFaecher)
        (List.filter (.fachbereichId |> in_or_empty model.selFachbereiche) 
            (List.filter (.themaId |> in_or_empty model.selThemen) 
                (List.filter (.themenbereichId |> in_or_empty model.selThemenbereiche) 
                    model.dysfunktionen)))
availablePathologien model = 
    List.filter (.fachId |> in_or_empty model.selFaecher)
        (List.filter (.fachbereichId |> in_or_empty model.selFachbereiche) 
            (List.filter (.themaId |> in_or_empty model.selThemen) 
                (List.filter (.themenbereichId |> in_or_empty model.selThemenbereiche) 
                    model.pathologien)))

isMultiSelect model = (not <| List.isEmpty model.selThemenbereiche)
                   || (List.isEmpty <| availableThemenbereiche model)

view : Model -> Html Message
view model =
  section [class "section"]
    [div [class "container"]
        [section [class "hero"]
            [div [class "hero-body"]
                [div [class "container"]
                    [h1 [class "title"] [text "Unterrichtseingabeformular"]]
                ]
            ]
        , div []
            [ select_ "Jahrgang" [onInput SelectJahrgang] 
                (List.map (\e -> option [selected (e.id == model.selJahrgang), value e.id] [text e.name]) 
                    model.jahrgaenge)
            , select_ "Fach" [onMultiChange SelectFaecher] 
                (List.map (\e -> option [selected (List.member e.id model.selFaecher), value e.id] [text e.name]) 
                    model.faecher)
            , select_ "Fachbereich" [onMultiChange SelectFachbereiche] 
                (List.map (\e -> option [selected (List.member e.id model.selFachbereiche), value e.id] [text e.name]) 
                    (List.filter (.fachId |> in_or_empty model.selFaecher)
                        model.fachbereiche))
            , select_ "Anzahl Stunden" [onInput SelectStunden] 
                (List.map (\e -> option [selected (e == model.selStunden), value e] [text e]) 
                    model.stunden)
            , select_ "Thema" [onMultiChange SelectThemen] 
                (List.map (\e -> option [selected (List.member e.id model.selThemen), value e.id] [text e.name]) 
                    (List.filter (.fachId |> in_or_empty model.selFaecher)
                        (List.filter (.fachbereichId |> in_or_empty model.selFachbereiche) 
                            model.themen)))
            , select_ "Themenbereich" [onMultiChange SelectThemenbereiche] 
                (List.map (\e -> option [selected (List.member e.id model.selThemenbereiche), value e.id] [text e.name]) 
                    (availableThemenbereiche model))

            -- Probleme beim Multiselect auf dem Tablet (Android)
            -- nach der Auswahl des 1. Elements, sind plötzlich 2 ausgewählt??
            , select_ "Dysfunktionen" [multiple <| isMultiSelect model, onMultiChange SelectDysfunktionen] 
                (List.map (\e -> option [selected (List.member e.id model.selDysfunktionen), value e.id] [text e.name]) 
                    (availableDysfunktionen model))
            , select_ "Pathologie" [multiple <| isMultiSelect model, onMultiChange SelectPathologien] 
                (List.map (\e -> option [selected (List.member e.id model.selPathologien), value e.id] [text e.name]) 
                    (availablePathologien model))

            , div [class "field"] [
                label [class "label"] [text "Datum"], 
                div [class "control"] 
                    [input [class "input", type_ "date", onInput InputDatum, value model.selDatum] []]
              ]
            , select_ "Dozent" [onInput SelectDozent] 
                (List.map (\e -> option [selected (e.id == model.selDozent), value e.id] [text e.name]) 
                    model.dozenten)
            , div [class "field"] [
                label [class "label"] [text "Assistent"], 
                div [class "control"] 
                    [input [class "input", type_ "text", onInput InputAssistent, value model.selAssistent] []]
              ]
            , div [class "field"] [
                label [class "label"] [text "Bemerkungen"], 
                div [class "control"] 
                    [textarea [class "textarea", rows 3, onInput InputBemerkungen, value model.selBemerkungen] []]
              ]
            , div [class "field is-grouped"] [
                div [class "control"] [button [class "button is-primary", onClick Save] [text "Speichern"]],
                div [class "control"] [button [class "button is-text", onClick Reset] [text "Zurücksetzen"]]
              ]
            ]
        ]
    ]
