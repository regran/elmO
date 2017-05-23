port module Proc exposing (..)

import Html
import Html.Attributes
import Html.Events
import Http
import Dict exposing (..)



main = 
    Html.program 
        {init=init
        , view=view
        , update=update
        , subscriptions=subscriptions
        }


---MODEL---
type alias Model = 
    {img: Image,
    view: View,
    content: String,
    desc: String,
    pix: Dict String String 
    }
                
                
type Image =
    Waiting
    | Picture String String

type View =
    Label
    | Text
    | Face
    | Wait

---UPDATE
type Msg =
    ViewChange View
    | Upload String

--port getImage : String -> Cmd msg 

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
        Upload s ->
            ({model|img = Picture s "stuff"}, Cmd.none)


---VIEW---

view model =
    let viewImg =
            case model.img of
                Waiting ->
                    Html.text ""
                Picture f n ->
                    Html.img [Html.Attributes.class "image", Html.Attributes.src f, Html.Attributes.title n] []
    in
        Html.div [Html.Attributes.class "meow"]
        [Html.div [Html.Attributes.class "container"]  
        [viewImg,
        Html.div [] [],
        Html.input [Html.Attributes.type_ "file", Html.Attributes.class "choosebutt"] [],
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

--SUBSCRIPTIONS

--port jsonresponse : (String -> msg) -> Sub msg

subscriptions: Model -> Sub Msg
subscriptions model =
    Sub.none


--INIT--
init: (Model, Cmd msg)
init = (Model (Waiting) (Wait) "There will be attribute descriptions or a prompt for an image here someday" "" Dict.empty, Cmd.none)
