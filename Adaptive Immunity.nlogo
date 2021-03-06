breed [cells cell]
breed [viruses virus]
breed [antibodies antibody]

viruses-own [
  attached?
  target
  age
]
cells-own [
  dying?
  death-ticks
]
globals [
  count-red
  count-orange
  count-yellow
  count-green
  count-violet
]


to set-up
  clear-all
  make-antibodies red base-red-antibodies
  make-antibodies orange base-orange-antibodies
  make-antibodies yellow base-yellow-antibodies
  make-antibodies green base-green-antibodies
  make-antibodies violet base-violet-antibodies
  make-cells 50
  reset-ticks
end

to make-cells [n]
  create-cells n [
    set shape "cell"
    set color red
    setxy random-xcor random-ycor
    set dying? false
    set death-ticks 0
    set size 4
  ]
end

to make-antibodies [c n]
  create-antibodies n [
      set shape "antibody"
      set color c
      setxy random-xcor random-ycor
      set size 3
    ]
end

to make-viruses [c n]
  create-viruses n [
    set shape "retrovirus triangle"
    set color c
    setxy random-xcor random-ycor
    set size 2
    set attached? false
    set target nobody
    set age random 50
  ]
end

to go

  set count-red count antibodies with [color = red]
  set count-orange count antibodies with [color = orange]
  set count-yellow count antibodies with [color = yellow]
  set count-green count antibodies with [color = green]
  set count-violet count antibodies with [color = violet]


  process-viruses
  process-antibodies
  process-cells
  tick
end

to process-viruses
  ask viruses [
    ifelse not attached? [
      rt random 10
      lt random 10
      fd 0.2
      set target min-one-of (cells with [not dying?] in-radius 1) [distance myself]
      if not (target = nobody) [
        move-to target
        set attached? true
        ask target [
          set dying? true
          set death-ticks 20
        ]
      ]
    ]

    [
      if [death-ticks] of target < 11 [
        hatch 3 [
          set attached? false
          set target nobody
          set heading random 360
          fd ((random-float 2) + 0.5)
          set age random 50
        ]
        set target nobody
        set attached? false
      ]
    ]
    set age age + 1
    if age > 250 [die]
  ]
end

to process-antibodies
  ask antibodies [
    rt random 10
    lt random 10
    fd 0.2
    let targ min-one-of ((viruses in-radius 1) with [color = [color] of myself]) [distance myself]
    if not (targ = nobody) [
      ask targ [die]
      hatch 1 [
        set heading random 360
        fd ((random-float 1) + 0.5)
      ]
    ]
    if (color = red and count-red > base-red-antibodies)
    or (color = orange and count-orange > base-orange-antibodies)
    or (color = yellow and count-yellow > base-yellow-antibodies)
    or (color = green and count-green > base-green-antibodies)
    or (color = violet and count-violet > base-violet-antibodies)
    [
      if random 500 = 0 [die]
    ]
  ]
end

to process-cells
    ask cells [
    ifelse not dying? [
      rt random 10
      lt random 10
      if any? (other cells in-radius 3.5)
      [
        face min-one-of other cells [distance myself]
        rt 180 + random 10
      ]
      fd 0.05
      if random-float 1 < 0.01 and count cells < 60
      [
        hatch 1
        [fd 2]
      ]
    ]
    [
      ifelse death-ticks mod 2 = 0 and death-ticks > 10
      [
        set color 17
      ]
      [
        set color 14
      ]
      set death-ticks death-ticks - 1
      if death-ticks <= 10 [
        set shape "cell dead"
        set color 11 + (4 * (death-ticks / 10))
      ]
      if death-ticks = 0 [die]
    ]
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
210
10
713
514
-1
-1
15.0
1
10
1
1
1
0
1
1
1
-16
16
-16
16
1
1
1
ticks
30.0

BUTTON
81
15
148
48
NIL
set-up
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
81
52
148
85
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
719
10
923
137
Cells
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -2674135 true "" "plot count cells"

PLOT
719
143
1012
321
Antibodies
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Red Type" 1.0 0 -2674135 true "" "plot count-red"
"Orange Type" 1.0 0 -955883 true "" "plot count-orange"
"Yellow Type" 1.0 0 -1184463 true "" "plot count-yellow"
"Green Type" 1.0 0 -13840069 true "" "plot count-green"
"Violet Type" 1.0 0 -8630108 true "" "plot count-violet"

BUTTON
49
101
178
134
Make Red Virus
make-viruses red 1
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
49
138
178
171
Make Orange Virus
make-viruses orange 1
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
49
174
177
207
Make Yellow Virus
make-viruses yellow 1
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
50
211
177
244
Make Green Virus
make-viruses green 1
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
50
248
177
281
Make Violet Virus
make-viruses violet 1
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
10
288
179
321
base-red-antibodies
base-red-antibodies
0
10
4.0
1
1
NIL
HORIZONTAL

SLIDER
10
326
179
359
base-orange-antibodies
base-orange-antibodies
0
10
4.0
1
1
NIL
HORIZONTAL

SLIDER
9
363
178
396
base-yellow-antibodies
base-yellow-antibodies
0
10
4.0
1
1
NIL
HORIZONTAL

SLIDER
8
400
177
433
base-green-antibodies
base-green-antibodies
0
10
4.0
1
1
NIL
HORIZONTAL

SLIDER
8
436
177
469
base-violet-antibodies
base-violet-antibodies
0
10
4.0
1
1
NIL
HORIZONTAL

PLOT
720
325
1011
517
Viruses
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Red Type" 1.0 0 -2674135 true "" "plot count viruses with [color = red]"
"Orange Type" 1.0 0 -955883 true "" "plot count viruses with [color = orange]"
"Yellow Type" 1.0 0 -1184463 true "" "plot count viruses with [color = yellow]"
"Green Type" 1.0 0 -13840069 true "" "plot count viruses with [color = green]"
"Violet Type" 1.0 0 -8630108 true "" "plot count viruses with [color = violet]"

@#$#@#$#@
## WHAT IS IT?

This model simulates the adaptive immune system, exploring how antibodes are created and how organisms become immune to specific diseases. 

## HOW IT WORKS

There are three agent classes: cells, viruses, and antibodies. The model starts off with four antibodies each for the different virus types (differentiated by color -- red, orange, yellow, green, or violet). These antibodies respond to viruses which target cells and multiply. More antibodies of a certain type are created with the introduction of more viruses of the same type. 

## HOW TO USE IT

Press SET-UP and GO to begin running the model. SET-UP will also reset the model. MAKE-[color]-VIRUS buttons introduce one virus of that color into the model. The BASE-[color]-ANTIBODIES sliders dictate how many antibodies of each color are present before the model starts running.

## THINGS TO NOTICE

Keep an eye on the graphs on the right; these will help you keep track of the number of cells, antibodies, and viruses.

## THINGS TO TRY

Try changing the number of base antibodies at the start of the simulation.

## CURRICULAR USE

This model is from the CT-STEM [Epidemiology Unit](https://ct-stem.northwestern.edu/curriculum/preview/1660/pem_code/K7CSRKV3UF6TAEVP9D5T/). In this unit, students explore types of disease, differences in transmission and spread, immune responses, and ways to prevent and treat disease.

The model can be found on [page 4](https://ct-stem.northwestern.edu/curriculum/preview/1677/page/4/pem_code/K7CSRKV3UF6TAEVP9D5T/) of lesson 5, "Responses to Disease"

## EXTENDING THE MODEL

Possible model changes include decreasing or increasing the number of virus/antibody types.

## NETLOGO FEATURES

This model uses custom shapes for cells ("cell"), viruses ("retrovirus triangle", and antibodies ("antibody").

## RELATED MODELS

Checkout the Innate Immunity model on page 3 of the same [CT-STEM lesson](https://ct-stem.northwestern.edu/curriculum/preview/1677/pem_code/K7CSRKV3UF6TAEVP9D5T/).

## CREDITS AND REFERENCES

If you mention this model or the NetLogo software in a publication, we ask that you include the citations below.

For the model itself:

* Rasmussen, L., Levites, L., Wilensky, U. (2020). Adaptive Immunity. http://ccl.northwestern.edu/netlogo/models/AdaptiveImmunity (link not yet available) Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

Please cite the NetLogo software as:

* Wilensky, U. (1999). NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

This model was developed as part of the CT-STEM Project at Northwestern University and was made possible through generous support from the National Science Foundation (grants CNS-1138461, CNS-1441041, DRL-1020101, DRL-1640201 and DRL-1842374) and the Spencer Foundation (Award #201600069). Any opinions, findings, or recommendations expressed in this material are those of the author(s) and do not necessarily reflect the views of the funding organizations. For more information visit https://ct-stem.northwestern.edu/.

Special thanks to the CT-STEM models team for preparing these models for inclusion in the Models Library including: Kelvin Lao, Jamie Lee, Alanda Zong, Lexie (Xinyue) Zhao, and Jacob Kelter.
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

antibody
false
0
Polygon -7500403 true true 243 80 258 95 185 146 184 123
Polygon -13345367 true false 180 255 154 255 154 111 225 60 240 75 180 120 180 255
Polygon -13345367 true false 120 255 146 255 146 111 75 60 60 75 120 120 120 255
Polygon -7500403 true true 57 80 42 95 116 147 116 123

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

cell
false
0
Circle -7500403 false true 30 30 240
Circle -7500403 false true 15 15 270
Circle -7500403 true true 45 75 120
Polygon -7500403 true true 240 195 240 180 225 165 225 165 195 165 195 180 225 195 240 195
Polygon -7500403 true true 210 210 210 225 195 240 195 240 165 240 165 225 195 210 210 210
Polygon -7500403 true true 90 210 90 225 105 240 105 240 135 240 135 225 105 210 90 210
Polygon -7500403 true true 135 75 165 45 180 90 210 75 195 135 240 120 180 150 195 120 195 90 180 105 165 75 165 60

cell dead
false
0
Circle -7500403 true true 45 45 120
Polygon -7500403 true true 255 210 255 195 240 180 240 180 210 180 210 195 240 210 255 210
Polygon -7500403 true true 180 195 180 210 165 225 165 225 135 225 135 210 165 195 180 195
Polygon -7500403 true true 45 225 45 240 60 255 60 255 90 255 90 240 60 225 45 225
Polygon -7500403 true true 165 60 195 30 210 75 240 60 225 120 270 105 210 135 225 105 225 75 210 90 195 60 195 45
Line -7500403 true 15 60 60 30
Line -7500403 true 30 60 60 45
Line -7500403 true 90 15 120 15
Line -7500403 true 90 30 120 30
Line -7500403 true 15 105 0 150
Line -7500403 true 30 105 15 150
Line -7500403 true 120 15 135 0
Line -7500403 true 120 30 135 15
Line -7500403 true 0 150 15 195
Line -7500403 true 15 150 30 195
Line -7500403 true 0 255 30 285
Line -7500403 true 15 255 60 285
Line -7500403 true 240 30 270 60
Line -7500403 true 240 15 285 45
Line -7500403 true 165 15 210 0
Line -7500403 true 180 30 210 15
Line -7500403 true 285 105 255 165
Line -7500403 true 300 120 285 180
Line -7500403 true 90 285 150 255
Line -7500403 true 105 300 180 270
Line -7500403 true 195 240 255 225
Line -7500403 true 210 255 270 255

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

retrovirus triangle
false
10
Rectangle -13345367 true true 135 240 165 255
Rectangle -13345367 true true 135 180 165 195
Rectangle -13345367 true true 135 210 165 225
Polygon -13345367 true true 105 30 105 135 135 165 165 165 195 135 195 30 150 0
Line -1 false 150 0 105 30
Line -1 false 195 30 150 0
Line -1 false 105 30 105 135
Line -1 false 195 30 195 135
Line -1 false 105 135 135 165
Line -1 false 195 135 165 165
Line -1 false 150 150 135 45
Line -1 false 165 45 150 150
Line -1 false 135 45 165 45
Line -1 false 135 45 105 30
Line -1 false 165 45 195 30
Line -1 false 135 45 150 0
Line -1 false 165 45 150 0
Line -1 false 135 45 105 135
Line -1 false 165 45 195 135
Line -1 false 150 150 105 135
Line -1 false 150 150 195 135
Line -1 false 150 150 135 165
Line -1 false 150 150 165 165
Rectangle -1 true false 135 165 165 180
Rectangle -1 true false 135 195 165 210
Rectangle -1 true false 135 225 165 240
Rectangle -13345367 true true 120 255 180 270
Line -1 false 135 165 135 255
Line -1 false 165 165 165 255
Line -1 false 135 255 120 195
Line -1 false 120 195 90 270
Line -1 false 120 255 75 180
Line -1 false 75 180 30 300
Line -1 false 120 255 90 150
Line -1 false 90 150 60 285
Line -1 false 225 180 270 300
Line -1 false 180 255 225 180
Line -1 false 210 150 240 285
Line -1 false 180 255 210 150
Line -1 false 165 255 180 195
Line -1 false 180 195 210 270
Line -1 false 165 255 135 255
Rectangle -1 false false 120 255 180 270

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.2.2
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
