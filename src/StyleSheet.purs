module StyleSheet where

import Prelude
import CSS
import CSS.Selector
import CSS.Color
import CSS.Common 
import Data.Maybe(fromMaybe)
import CSS.Border
import CSS.Font
import Data.Array.NonEmpty as AN
import Data.NonEmpty as NE
import CSS.TextAlign as TA
import CSS.Flexbox as F
import CSS.Property(Prefixed(..),Value(..))


expo :: CSS
expo = do
    mainContent
    headFoot
    foot
    mainMain
    contentArea
    displayArea
    numDisplay
    howcalDisplay
    clickArea
    operator
    divClassRow
    clickOperaBtn
    clickOperaRowBtn
    clickOperaBtnAct

mainContent :: CSS
mainContent = star & byClass "main-content" ? do
        width $ px 560.0
        height $ px 570.0
        margin_ auto 
        padding_ $ px 50.0
        color black
        backgroundColor $ fromMaybe black  (fromHexString "#e9e9e9")
        border solid (px 1.0) (fromMaybe red $ fromHexString "#8f8f8f")
        borderRadius  (px 10.0) (px 10.0) (px 10.0) (px 10.0)

hexColor :: String -> Color
hexColor xs = fromMaybe white $ fromHexString xs

padding_ x = padding x x x x

headFoot :: CSS
headFoot = element "header,footer" ? do
    padding_ $ px 50.0
    fontFamily ["Sawarabi Mincho"] $ NE.NonEmpty sansSerif []
    color white
    backgroundColor $ hexColor "#28263f"
    minWidth $ px 612.0
    marginBottom $ px 50.0

f = putStyleSheet $ element "header,footer" ? mainContent

foot :: CSS
foot = 
    let m x = 
            let zero = px 0.0 
            in  margin x zero zero zero
    in element "footer" ? (m (px 50.0))

mainMain :: CSS
mainMain = element "main" ? do
    let serif = GenericFontFamily $ Value $ Plain "serif"
    fontFamily ["Ultra"] $ NE.NonEmpty serif []
    TA.textAlign TA.center
    minWidth $ px 560.0

contentArea :: CSS
contentArea = star & byClass "content-area" ? (width $ px 560.0)

margin_ x = margin x x x x

displayArea :: CSS
displayArea = star & byClass "display-area" ? do
    backgroundColor $ hexColor "#ffffff"
    display flex
    F.flexDirection row
    margin_ $ px 10.0
    width $ px 520.0
    height $ px 100.0
    border solid (px 5.0) (hexColor "#a0a0dd")
    let br x = let xsize = px x in borderRadius xsize xsize xsize xsize
    br 10.0

numDisplay :: CSS
numDisplay = star & byId "num_display" ? do
    width $ px 490.0
    fontSize $ px 50.0

howcalDisplay :: CSS
howcalDisplay = star & byId "howcal_display" ? do
    width $ px 30.0
    fontSize $ px 30.0

clickArea :: CSS
clickArea = star & byClass "click-area" ? do
    display flex
    F.flexDirection rowReverse

classname :: String -> Selector
classname xs = star & byClass xs

idName :: String -> Selector
idName xs = star & byId xs

operator :: CSS
operator = classname "operator" ? do
    display flex
    F.flexDirection column

attrName :: String -> String -> Selector
attrName xs ys = element xs & attr ys

divClassRow :: CSS
divClassRow = attrName "div" "class*=\"row\"" ? do
    display flex
    F.flexDirection row

clickOperaBtn :: CSS
clickOperaBtn = classname "click-area .operator .btn" ? do
    fontSize $ px 30.0
    fontWeight bold
    width $ px 110.0
    height $ px 60.0
    let m x = margin x x x x
    m (px 10.0)
    TA.textAlign TA.center
    color $ hexColor "#ffffff"
    backgroundColor $ hexColor "#a0a0dd"
    border solid (px 5.0) (hexColor "#a0a0dd")
    let b' x = borderRadius (px x) (px x) (px x) (px x)
    b' (10.0)

clickOperaRowBtn :: CSS
clickOperaRowBtn = classname "click-area .operator .row5 .btn" ? width (px 390.0)

clickOperaBtnAct :: CSS
clickOperaBtnAct = classname "click-area .operator .btn:acive" ? do
    backgroundColor $ hexColor "#ffffff"
    color $ hexColor "#a0a0dd"
