module Config where

import Prelude
import CSS
import CSS.Selector
import CSS.Color
import CSS.Border
import CSS.Font
import CSS.TextAlign
import CSS.Text
import CSS.VerticalAlign(verticalAlign,textBottom)


expo :: CSS
expo = do
    body
    img
    paragraphs
    ul
    a
    aHover

body :: CSS
body = element "body" ? do
    let m x = margin x x x x
    m $ px 0.0
    let p x = padding x x x x
    p $ px 0.0
    backgroundColor white
    color black
    fontSize $ px 15.0
    lineHeight $ px 2.0

img :: CSS
img = element "img" ? verticalAlign textBottom

paragraphs :: CSS
paragraphs = 
    let m x = margin x x x x 
    in element "p,h1,h2,h3,h4,h5,h6" ? m (px 0.0)

ul :: CSS
ul = element "ul" ? do
    let m x = margin x x x x
        p x = padding x x x x
    m $ px 0.0
    p $ px 0.0

a :: CSS
a = element "a" ? do
    color black
    textDecoration noneTextDecoration

aHover :: CSS
aHover = element "a:hover" ? textDecoration underline