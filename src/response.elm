module Response exposing (..)
import Html
import Html.Attributes
import Json.Decode as JD
import Svg
import Svg.Attributes
import Regex exposing (..)




deString j = replace All (regex "\\\\") (\_ -> "\\") (replace All (regex "}\"") (\_ -> "}") (replace All (regex "\"{") (\_ -> "{") j))


decodeJ j = featresult (JD.decodeString (biggestdec) (j))





---DISPLAY
drawFeat f =
  let content =
    case f of
      Label desc conf ->
        [Html.text desc,
        Html.span [Html.Attributes.style [("float", "right")]] [Html.text (String.append (toString (round (100*conf))) "%")],
        Html.div [] [],
        rectBar (toString (100*conf))]
      Text s ->
        [Html.text (s)]
      Face j so a  ->
        List.append (safefaceHelper "Joy: " j) (List.append (safefaceHelper "Sorrow: " so) (safefaceHelper "Anger: " a) )
        
      Safe a m v ->
        List.append (safefaceHelper "Adult: " a) (List.append (safefaceHelper "Medical: " m) (safefaceHelper "Violence: " v))
      Failed ->
        [Html.text "FAILED"]
  in
    [Html.div [Html.Attributes.style [("width", "100%")]] content]
   
      
rectBar s = 
      Svg.svg [Svg.Attributes.height "20", Svg.Attributes.width "100%", Svg.Attributes.preserveAspectRatio "none", Svg.Attributes.viewBox "0 0 110 20"]
      [Svg.rect [ Svg.Attributes.fill "#E0E0E0", Svg.Attributes.x "0", Svg.Attributes.y "0", Svg.Attributes.width "100", Svg.Attributes.height "10"] [],
      Svg.rect [ Svg.Attributes.fill "#4CAF50", Svg.Attributes.x "0", Svg.Attributes.y "0", Svg.Attributes.width s, Svg.Attributes.height "10"] []]
      
safefaceHelper s n =
  [Html.text s, rectBar (toString (n*20)), Html.div [] []]
   
toFeatlist = List.map toFeat
 
labels = List.filter isLabel
faces = List.filter isFace
texts = List.filter isText
safes = List.filter isSafe



isLabel f =
  case f of 
    Label s c ->
      True
    Text s ->
      False
    Face q w e  ->
      False
    Safe q w e  ->
      False
    Failed ->
      False

isText f =
  case f of 
    Label s c ->
      False
    Text s ->
      True
    Face q w e  ->
      False
    Safe q w e  ->
      False
    Failed ->
      False
      
isFace f =
  case f of 
    Label s c ->
      False
    Text s ->
      False
    Face q w e  ->
      True
    Safe q w e  ->
      False
    Failed ->
      False
      
isSafe f =
  case f of 
    Label s c ->
      False
    Text s ->
      False
    Face q w e  ->
      False
    Safe q w e  ->
      True
    Failed ->
      False


--DECODERS
featresult v =
  case v of
    Err e ->
      [Feature "FAILED" (Stuff e 0 0 0 0 0 0 0)]
    Ok r ->
      r

stuffresult v =
    case v of
        Err e ->
            Stuff e 0 0 0 0 0 0 0
        Ok r ->
            r
      
toFeat : Feature -> Feat
toFeat f =
  if f.feat=="LABEL_DETECTION" then Label f.traits.desc f.traits.conf
  else if f.feat=="TEXT_DETECTION" then Text f.traits.desc
  else if f.feat=="FACE_DETECTION" then Face f.traits.joy f.traits.sad f.traits.mad 
  else if f.feat=="SAFE_SEARCH_DETECTION" then Safe f.traits.adult f.traits.med f.traits.vio 
  else Failed

biggestdec = (JD.field "responses" (JD.index 0 (JD.at ["dataList"] lildec)))


lildec = (JD.field "optTypedata" (JD.list bigdec))

    
    
bigdec = 
  JD.map2 Feature
    (JD.field "attrKey" JD.string)
    (JD.field "attrValue" (JD.map (\a-> stuffresult (JD.decodeString featdec a)) JD.string))
    
featdec =
  JD.map8 Stuff
    (JD.oneOf [JD.field "description_" JD.string, JD.succeed ""])
    (JD.oneOf [JD.field "score_" JD.float, JD.succeed 0])
    (JD.oneOf [JD.field "joyLikelihood_" JD.int, JD.succeed 0])
    (JD.oneOf [JD.field "angerLikelihood_" JD.int, JD.succeed 0])
    (JD.oneOf [JD.field "sorrowLikelihood_" JD.int, JD.succeed 0])
    (JD.oneOf [JD.field "medical_" JD.int, JD.succeed 0])
    (JD.oneOf [JD.field "violence_" JD.int, JD.succeed 0])
    (JD.oneOf [JD.field "adult_" JD.int, JD.succeed 0])
 

    

type Feat = 
  Label String Float --description confidence
  | Text String ---description
  | Face Int Int Int ---joy sorrow anger surprise
  | Safe Int Int Int ---adult spoof medical violence
  | Failed
  
type alias Feature =
  {feat : String,
   traits : Stuff
  }
  
type alias Stuff =
  {desc: String, ---Label or text description
   conf: Float, ---Label confidence
   joy: Int, ---Face happiness likelihood
   mad: Int, ---Face anger likelihood
   sad: Int, ---Face sorrow likelihood
   med: Int, ---Safe search medical content likelihood
   vio: Int, --Safe search violent content likelihood
   adult: Int --Safe search adult content likelihood
   
  }