import Html
import Html.Attributes
import Html.Events
import Http
import Dict exposing (..)
import FileReader

main = Html.beginnerProgram {model=model, view=view, update=update}


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

update: Msg -> Model -> Model
update msg model =
    case msg of
        ViewChange v ->
            case v of
                Label ->
                    {model|content = "A label"}
                Text -> 
                    {model|content = "Some text"}
                Face ->
                    {model|content = "A face"}
                Wait ->
                    {model|content = "Waiting"}
        Upload s ->
            {model|img = Picture s "stuff"}


---VIEW---

view model =
    Html.div [Html.Attributes.class "meow"]
    [Html.div [Html.Attributes.class "container"]  
    [Html.img [Html.Attributes.height 390, Html.Attributes.src "whooo", Html.Attributes.alt "Valid image required"] [],
    Html.div [] [],
    Html.button [Html.Attributes.class "choosebutt", Html.Events.onClick (Upload "file")] [Html.text "Choose file"],
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

--INIT--

model = Model (Waiting) (Wait) "There will be attribute descriptions or a prompt for an image here someday" "" Dict.empty
