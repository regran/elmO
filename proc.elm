port module Proc exposing (..)

import Html
import Html.Attributes
import Html.Events
import Http
import Dict exposing (..)
import Json.Decode as JD


main = 
    Html.program 
        {init=init
        , view=view
        , update=update
        , subscriptions=subscriptions
        }


---MODEL---
type alias Model = 
    {img: Maybe Image,
    view: View,
    content: String,
    desc: String,
    pix: Dict String String
    }
                
                
type alias Image =
    {contents: String,
    name: String
    }

type View =
    Label
    | Text
    | Face
    | Wait

---UPDATE
type Msg =
    ViewChange View
    | Get
    | Drop
    | Replace
    | Upload Image
    | Read String
    | Chill

port getImage : String->Cmd msg 
port dropImage : String->Cmd msg
port drop : String->Cmd msg

update: Msg -> Model -> (Model, Cmd msg)
update msg model =
    case msg of
        ViewChange v ->
            case v of
                Label ->
                    ({model|content = "A label"}, Cmd.none)
                Text -> 
                    ({model|content = "Some text"}, Cmd.none)
                Face ->
                    ({model|content = "A face"}, Cmd.none)
                Wait ->
                    ({model|content = "Waiting"}, Cmd.none)
        Get ->
            (model, getImage "file")
        Drop ->
            (model, dropImage "dropbox")

        Replace ->
            (model, drop "dropbox")

        Upload data ->
            ({model|img = Just (Image data.contents data.name)}, Cmd.none)
        Read s ->
            ({model|content=s}, Cmd.none)
        Chill ->
            (model, Cmd.none)

---VIEW---

view model =
    let viewImg =
            case model.img of
                Nothing ->
                    Html.label [Html.Attributes.for "file", Html.Attributes.class "dropbox", Html.Attributes.id "dropbox"] []
                Just i->
                    Html.img [Html.Attributes.class "image", Html.Attributes.src i.contents, Html.Attributes.title i.name, Html.Attributes.id "dropbox", Html.Events.onWithOptions "drop" options (JD.succeed Replace), Html.Events.onWithOptions "dragenter" options (JD.succeed Chill), Html.Events.onWithOptions "dragover" options (JD.succeed Chill)] []
    in
        Html.div [Html.Attributes.class "meow"]
        [Html.div [Html.Attributes.class "container"]  
        [viewImg,
        Html.div [] [],
        Html.input [Html.Attributes.type_ "file", Html.Attributes.id "file", Html.Attributes.hidden True, Html.Events.on "change" (JD.succeed Get)] [],
        Html.label [Html.Attributes.for "file", Html.Events.on "change" (JD.succeed Get), Html.Attributes.class "choosebutt"] [Html.text "Choose File"],
        Html.div [] [],
        Html.div [Html.Attributes.class "head"] [
        headerButton "Label" (ViewChange Label) model, 
        headerButton "Text" (ViewChange Text) model,
        headerButton "Face" (ViewChange Face) model],
        Html.div [Html.Attributes.class "body"]  [Html.text model.content],
        Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "style.css" ] []]]


---HTML

headerButton: String->Msg->Model->Html.Html Msg
headerButton s m model=
    Html.button [Html.Attributes.class "headbutt", Html.Events.onClick (m)] [Html.text s]

options: Html.Events.Options
options = Html.Events.Options False True

--SUBSCRIPTIONS

port fileContentRead : (Image->msg) -> Sub msg
port jsonresponse : (String -> msg) -> Sub msg

subscriptions: Model -> Sub Msg
subscriptions model =
    Sub.batch [fileContentRead Upload, jsonresponse Read]

--INIT--
init: (Model, Cmd msg)
init = (Model (Nothing) (Wait) "There will be attribute descriptions or a prompt for an image here someday" "" Dict.empty, dropImage "dropbox")
