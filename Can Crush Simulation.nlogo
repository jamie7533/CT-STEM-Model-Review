globals
[
  tick-advance-amount                   ;; how much we advance the tick counter this time through
  max-tick-advance-amount               ;; the largest tick-advance-amount is allowed to be
  box-edge                              ;; distance of box edge from axes
  instant-pressure                      ;; the pressure at this tick or instant in time
  pressure-history                      ;; a history of the four instant-pressure values
  pressure                              ;; the pressure average of the pressure-history (for curve smoothing in the pressure plots)
  zero-pressure-count                   ;; how many zero entries are in pressure-history
  wall-hits-per-particle                ;; average number of wall hits per particle
  length-horizontal-surface             ;; the size of the wall surfaces that run horizontally - the top and bottom of the box
  length-vertical-surface               ;; the size of the wall surfaces that run vertically - the left and right of the box
  walls                                 ;; agentset containing patches that are the walls of the box
  total-particle-number
  init-avg-speed init-avg-energy        ;; initial averages
  avg-speed avg-energy particle0-speed  ;; current averages
  outside-energy
  min-outside-energy
  max-outside-energy
  volume
  temperature
  collide?
  initial-number

  wait-time ;; making sure students can see when particles slow down or speed up

  can-state
  original-vol
  can-vol ;; in terms of percentage, to see how much the can has crushed
  can-size
  can-crushed?
  percent-crush
  crush-failed?

  outside-coefficient
  water-temp

  time-on-fire

  was-seeing-inside?

  can-flashing?
  can-flash-start
  last-can-flash
]

breed [ particles particle ]
breed [ cans can ]
breed [ fires fire ]
breed [ ices ice ]
breed [ flashers flasher ]
breed [ steams steam ]



cans-own [ my-temp ]

particles-own
[
  speed mass energy          ;; particle info
  wall-hits                  ;; # of wall hits during this clock cycle ("big tick")
  momentum-difference        ;; used to calculate pressure from wall hits
  last-collision
  momentum-instant
]

flashers-own [
  my-target
  birth-tick
  last-tick
]


to setup
  clear-all
  reset-ticks
  set can-state "n/a"
  set initial-number initial-water-amount * 2
  set collide? true
  set min-outside-energy 0
  set max-outside-energy 300
  set-default-shape particles "circle"
  set max-tick-advance-amount 0.1073
  ;; box has constant size...
  set box-edge 16
  ;;; the length of the horizontal or vertical surface of
  ;;; the inside of the box must exclude the two patches
  ;; that are the where the perpendicular walls join it,
  ;;; but must also add in the axes as an additional patch
  ;;; example:  a box with an box-edge of 10, is drawn with
  ;;; 19 patches of wall space on the inside of the box
  set length-horizontal-surface  ( 2 * (box-edge - 1) + 1)
  set length-vertical-surface  ( 2 * (box-edge - 1) + 1)
  set outside-energy 50
  make-box
  if can-volume = "240 mL" [
    set can-vol 240
    set original-vol can-vol
    set can-size 10
  ]
  if can-volume = "355 mL" [
    set can-vol 355
    set original-vol can-vol
    set can-size 13
  ]
  if can-volume = "473 mL" [
    set can-vol 473
    set original-vol can-vol
    set can-size 15
  ]
  make-can-crush-experiment
  let cn one-of cans
  let stm one-of steams
  ask stm [create-link-with cn [tie hide-link]]
  make-particles initial-number
  set pressure-history [0 0 0]  ;; plotted pressure will be averaged over the past 3 entries
  set zero-pressure-count 0
  update-variables
  set temperature (avg-energy * 6)
  set init-avg-speed avg-speed

  ask particles with [ who = 0 ] [set particle0-speed speed]
  set init-avg-energy avg-energy

  set outside-coefficient (50 / 23)

  set was-seeing-inside? false

  set percent-crush 0
  set can-crushed? false
  set can-flashing? false
  set crush-failed? false

  do-recolor
  set total-particle-number initial-number
  calculate-tick-advance-amount
end


to go
  reset-timer
  ifelse see-inside? [
    ask walls [ set pcolor box-color ]
    if not any? flashers [
      ifelse can-crushed? [
        ask cans [ set shape "soda can crushed box" ]
      ][
        ask cans [ set shape "soda can with box" ]
      ]
    ]
  ][
    ask walls [ set pcolor black ]
    ifelse can-crushed? [
      ask cans [ set shape "soda can crushed 2" ]
    ][
      ask cans [ set shape "soda can" ]
    ]
  ]


  ask particles [ bounce ]
  ask particles [ move ]
  if collide?
  [
    ask particles
      [ check-for-collision ]
  ]
  tick-advance tick-advance-amount

  if floor ticks > floor (ticks - tick-advance-amount)
  [
    ifelse any? particles
      [ set wall-hits-per-particle mean [wall-hits] of particles  ]
    [ set wall-hits-per-particle 0 ]
    ask particles
      [ set wall-hits 0 ]

    calculate-pressure
    update-variables
    update-plots

  ]
  calculate-tick-advance-amount
  ask particles with [ who = 0 ]
    [ set particle0-speed speed ]

  set temperature (avg-energy * 6)
  do-recolor

  soda-can-go
  show-steam

  display

  wait ((30 - max [speed] of particles) / 500 )


end

to soda-can-go

  ;; evaporate the water if super hot & at the top of the box
  ;; added + 25 seconds 2/11/21 (wants the delay between displayed steam and particles leaving)
  if time-on-fire >= 7.092 * initial-water-amount + 89.74 + 25 [ particles-evaporate-if-hot ] ; jamie 9/23 - match line 444 (steam timing)

  fire-burns

  show-or-hide-the-box

  ask cans [
    follow-the-mouse

    ifelse on-fire? [

      if can-state != "on fire" [ set time-on-fire 0 ]

      set my-temp my-temp + ((100 - my-temp) * 0.001) ;; doing the difference drives rate here
      set can-state "on fire"
      set time-on-fire time-on-fire + 0.1
    ]
    [
      ifelse in-water? [
        set can-state "in water"
        set my-temp my-temp + ((water-temp - my-temp) * 0.1) ;; doing the difference drives rate again
        crush-if-you-can
      ]
      [
        set can-state "in air"
        set my-temp my-temp + ((23 - my-temp) * 0.005)
      ]
    ]


    if my-temp > 100 [ set my-temp 100 ]
    if my-temp < 0 [ set my-temp 0 ]

    set outside-energy my-temp * outside-coefficient

    recolor-soda-can

  ]

  flash-if-just-seeing
end

to particles-evaporate-if-hot

  ;; if the can is in the water & looking down, particles cannot leave it
  if any? cans with [in-water? and heading = 195] [ stop ]

  ;; if not, evaporate from the top of the can
  if count particles > 1 [
    let evaporate-candidate max-one-of (particles with [speed > 19])[ycor]
    if evaporate-candidate != nobody [
      if [ycor] of evaporate-candidate > box-edge - 1 [
        ask evaporate-candidate [ die ]
      ]
    ]
  ]
end

to fire-burns
  ask fires [
    set size 3 + random-float 1.5
    if size > 4.5 [ set size 4.5 ]
  ]
end

to show-or-hide-the-box
  ifelse see-inside? [
    ask particles [ set hidden? false ]
  ][
    ask particles [ set hidden? true ]
    ask walls [ set pcolor black ]
  ]
end


;; can procedure
to follow-the-mouse
  if  mouse-inside? [
    if abs mouse-xcor < max-pxcor - 5 [ set xcor mouse-xcor ]
    if abs mouse-ycor < max-pycor - 10 [ set ycor mouse-ycor ]


    ifelse mouse-down? [
      set heading 195
    ][
      set heading 15
    ]
  ]
end


;; can procedure
;;; crushes the can & calculates the percentage if the user
;;; followed the procedure correctly
;;; the mechanism is pretty much hand coded
to crush-if-you-can

  ;;;
  ;;; first, let's elimintate all the conditions when the can can't crush
  ;;;
  if heading != 195 [ stop ]
  if time-on-fire < 60 [ stop ]

  if not (initial-water-amount > 4 and initial-water-amount < 25) [ stop ] ;;; there needs to be just enough water; not too much, not too little.

  if not in-water? [ stop ]
  if not any? particles [ stop ]

  if my-temp >= water-temp + 3 [ stop ]


  ;;; if the can can crush, let's calculate how much it crushed

  ;; the amount of water (roughly)
  let iwa initial-water-amount


  ;; the amount of time on fire (roughly)
  let tof time-on-fire
  if tof > 180 [ set tof 180 ]

  if not can-crushed? [
  ;; added a random-normal distribution with standard deviation 2 for variation in results
  set percent-crush random-normal (70.0101 - (1.1478 * water-temp) - (4.6072 * iwa) + (0.2180 * tof)) 2 ;; jamie - multiple linear regression
  ]

  ifelse percent-crush > 0 [
    ifelse see-inside? [
      set shape "soda can crushed box"
    ][
      set shape "soda can crushed 2"
    ]
    set can-crushed? true
    if percent-crush > 100 [ set percent-crush 100 ]
  ][
    set percent-crush 0
    set crush-failed? true
  ]


end

to see-inside
  ifelse see-inside? [
    set see-inside? false
  ][
    set see-inside? true
  ]
end

to flash-if-just-seeing

  if not was-seeing-inside? and see-inside? [
    create-flashers 2 [
      set birth-tick ticks
      set last-tick birth-tick
      set size 48
      set shape "flasher"
      set color white
      set hidden? true
    ]
  ]

  let cn one-of cans

  if any? flashers [
    ask flashers [
      if birth-tick < ticks - 1 [
        set can-flashing? true
        set can-flash-start ticks
        set last-can-flash can-flash-start
        die
      ]

      if ticks - 0.2 > last-tick [
        ifelse hidden? [
          ;ask cn [ set shape "soda can with box flash"]
          set hidden? false
        ][
          ;ask cn [ set shape "soda can with box"]
          set hidden? true
        ]
        set last-tick ticks
      ]
    ]


  ]

  if can-flashing? [
    ask cn [
      if can-flash-start < ticks - 1 [
        ; ask flashers [ die ]
        set can-flashing? false
      ]

      ifelse can-crushed? [
        if ticks - 0.4 > last-can-flash [
          ifelse [shape] of cn = "soda can crushed box" [
            ask cn [ set shape "soda can crushed flash"]
          ][
            ask cn [ set shape "soda can crushed box"]
          ]
          set last-can-flash ticks
        ]
      ][
        if ticks - 0.4 > last-can-flash [
          ifelse [shape] of cn = "soda can with box" [
            ask cn [ set shape "soda can with box flash"]
          ][
            ask cn [ set shape "soda can with box"]
          ]
          set last-can-flash ticks
        ]
      ]
    ]
  ]


  set was-seeing-inside? see-inside?
end

to show-steam ;jamie - display steam above can
  let cn one-of cans
  let stm one-of steams
  ifelse time-on-fire >= 7.092 * initial-water-amount + 89.74 ; used to be 155; jamie 9/23
  and [heading] of cn = 15
  and [my-temp] of cn > 30;still warm
  and can-crushed? = false
  and not crush-failed? [
    ask stm [set hidden? false]
  ][
    ask stm [set hidden? true]
  ]
end

to-report on-fire?
  report any? (fires with [ycor  < [ycor] of myself - 5]) in-radius 10
end

to-report in-water?
  report any? (ices with [ycor < [ycor] of myself]) in-radius 9
end

;;;;;;;;;;;


to calculate-tick-advance-amount
  ifelse any? particles with [ speed > 0 ]
    [ set tick-advance-amount min list (1 / (ceiling max [speed] of particles )) max-tick-advance-amount ]
  [ set tick-advance-amount max-tick-advance-amount ]
end

;;; Pressure is defined as the force per unit area.  In this context,
;;; that means the total momentum per unit time transferred to the walls
;;; by particle hits, divided by the surface area of the walls.  (Here
;;; we're in a two dimensional world, so the "surface area" of the walls
;;; is just their length.)  Each wall contributes a different amount
;;; to the total pressure in the box, based on the number of collisions, the
;;; direction of each collision, and the length of the wall.  Conservation of momentum
;;; in hits ensures that the difference in momentum for the particles is equal to and
;;; opposite to that for the wall.  The force on each wall is the rate of change in
;;; momentum imparted to the wall, or the sum of change in momentum for each particle:
;;; F = SUM  [d(mv)/dt] = SUM [m(dv/dt)] = SUM [ ma ], in a direction perpendicular to
;;; the wall surface.  The pressure (P) on a given wall is the force (F) applied to that
;;; wall over its surface area.  The total pressure in the box is sum of each wall's
;;; pressure contribution.

to calculate-pressure
  ;; by summing the momentum change for each particle,
  ;; the wall's total momentum change is calculated
  set pressure 15 * sum [momentum-difference] of particles
  set pressure-history lput pressure but-first pressure-history
  ask particles
    [ set momentum-difference 0 ]  ;; once the contribution to momentum has been calculated
                                   ;; this value is reset to zero till the next wall hit
end

to bounce  ;; particle procedure
           ;; get the coordinates of the patch we'll be on if we go forward 1
  let new-patch patch-ahead 1
  let new-px [pxcor] of new-patch
  let new-py [pycor] of new-patch
  ;; if we're not about to hit a wall, we don't need to do any further checks
  if (abs new-px != box-edge and abs new-py != box-edge)
    [ stop ]
  ;; if hitting left or right wall, reflect heading around x axis
  if (abs new-px = box-edge)
    [ set heading (- heading)
      set wall-hits wall-hits + 1
      ;;  if the particle is hitting a vertical wall, only the horizontal component of the speed
      ;;  vector can change.  The change in velocity for this component is 2 * the speed of the particle,
      ;; due to the reversing of direction of travel from the collision with the wall
      set momentum-instant  (abs (dx * 2 * mass * speed) / length-vertical-surface)
      set momentum-difference momentum-difference + momentum-instant
  ]
  ;; if hitting top or bottom wall, reflect heading around y axis
  if (abs new-py = box-edge)
    [ set heading (180 - heading)
      set wall-hits wall-hits + 1
      ;;  if the particle is hitting a horizontal wall, only the vertical component of the speed
      ;;  vector can change.  The change in velocity for this component is 2 * the speed of the particle,
      ;; due to the reversing of direction of travel from the collision with the wall
      set momentum-instant  (abs (dy * 2 * mass * speed) / length-horizontal-surface)
      set momentum-difference momentum-difference + momentum-instant
  ]
  if [heated-wall?] of patch new-px new-py   ;; check if the patch ahead of us is heated
    [
      ;      set energy ((energy +  outside-energy ) / 2)
      set energy outside-energy
      ; set speed sqrt (2 * energy / mass )

      ;; umit change --> to make sure particles move really slow when they slow
      set speed sqrt (2 * energy / mass )
      set speed (ifelse-value
        speed >= 18 [ speed ]
        speed < 18 [ speed * 0.75 ]
        speed < 15 [ speed * 0.5 ]
        speed < 11 [ speed * 0.3 ]
        [speed * 0.1])
  ]

end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to bounce-dark  ;; particle procedure
                ;; get the coordinates of the patch we'll be on if we go forward 1
  let new-patch patch-ahead 1
  let new-px [pxcor] of new-patch
  let new-py [pycor] of new-patch
  ;; if we're not about to hit a wall, we don't need to do any further checks
  if (abs new-px != box-edge and abs new-py != box-edge)
    [stop]
  ;; if hitting left or right wall, reflect heading around x axis
  if (abs new-px = box-edge)
    [ set heading (- heading)
      set wall-hits wall-hits + 1
      ;;  if the particle is hitting a vertical wall, only the horizontal component of the speed
      ;;  vector can change.  The change in velocity for this component is 2 * the speed of the particle,
      ;; due to the reversing of direction of travel from the collision with the wall
      set momentum-instant  (abs (dx * 2 * mass * speed) / length-vertical-surface)
      set momentum-difference momentum-difference + momentum-instant
  ]
  ;; if hitting top or bottom wall, reflect heading around y axis
  if (abs new-py = box-edge)
    [ set heading (180 - heading)
      set wall-hits wall-hits + 1
      ;;  if the particle is hitting a horizontal wall, only the vertical component of the speed
      ;;  vector can change.  The change in velocity for this component is 2 * the speed of the particle,
      ;; due to the reversing of direction of travel from the collision with the wall
      set momentum-instant  (abs (dy * 2 * mass * speed) / length-horizontal-surface)
      set momentum-difference momentum-difference + momentum-instant
  ]
  if [heated-wall?] of patch new-px new-py   ;; check if the patch ahead of us is heated
    [ set energy ((energy +  outside-energy ) / 2)
      set speed sqrt (2 * energy / mass )
  ]


end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to move  ;; particle procedure
  if patch-ahead (speed * tick-advance-amount) != patch-here
    [ set last-collision nobody ]
  jump (speed * tick-advance-amount)
end


to check-for-collision  ;; particle procedure
                        ;; Here we impose a rule that collisions only take place when there
                        ;; are exactly two particles per patch.  We do this because when the
                        ;; student introduces new particles from the side, we want them to
                        ;; form a uniform wavefront.
                        ;;
                        ;; Why do we want a uniform wavefront?  Because it is actually more
                        ;; realistic.  (And also because the curriculum uses the uniform
                        ;; wavefront to help teach the relationship between particle collisions,
                        ;; wall hits, and pressure.)
                        ;;
                        ;; Why is it realistic to assume a uniform wavefront?  Because in reality,
                        ;; whether a collision takes place would depend on the actual headings
                        ;; of the particles, not merely on their proximity.  Since the particles
                        ;; in the wavefront have identical speeds and near-identical headings,
                        ;; in reality they would not collide.  So even though the two-particles
                        ;; rule is not itself realistic, it produces a realistic result.  Also,
                        ;; unless the number of particles is extremely large, it is very rare
                        ;; for three or more particles to land on the same patch (for example,
                        ;; with 400 particles it happens less than 1% of the time).  So imposing
                        ;; this additional rule should have only a negligible effect on the
                        ;; aggregate behavior of the system.
                        ;;
                        ;; Why does this rule produce a uniform wavefront?  The particles all
                        ;; start out on the same patch, which means that without the only-two
                        ;; rule, they would all start colliding with each other immediately,
                        ;; resulting in much random variation of speeds and headings.  With
                        ;; the only-two rule, they are prevented from colliding with each other
                        ;; until they have spread out a lot.  (And in fact, if you observe
                        ;; the wavefront closely, you will see that it is not completely smooth,
                        ;; because some collisions eventually do start occurring when it thins out while fanning.)
  let others-here nobody

  if breed = particles
    [ set others-here other particles-here ]

  if count others-here = 1
    [ ;; the following conditions are imposed on collision candidates:
      ;;   1. they must have a lower who number than my own, because collision
      ;;      code is asymmetrical: it must always happen from the point of view
      ;;      of just one particle.
      ;;   2. they must not be the same particle that we last collided with on
      ;;      this patch, so that we have a chance to leave the patch after we've
      ;;      collided with someone.
      let candidate one-of others-here with
        [ who < [who] of myself and myself != last-collision ]
      ;; we also only collide if one of us has non-zero speed. It's useless
      ;; (and incorrect, actually) for two particles with zero speed to collide.
      if (candidate != nobody) and (speed > 0 or [speed] of candidate > 0)
      [
        collide-with candidate
        set last-collision candidate
        ask candidate [ set last-collision myself ]
      ]
  ]
end

;; implements a collision with another particle.
;;
;; THIS IS THE HEART OF THE PARTICLE SIMULATION, AND YOU ARE STRONGLY ADVISED
;; NOT TO CHANGE IT UNLESS YOU REALLY UNDERSTAND WHAT YOU'RE DOING!
;;
;; The two particles colliding are self and other-particle, and while the
;; collision is performed from the point of view of self, both particles are
;; modified to reflect its effects. This is somewhat complicated, so I'll
;; give a general outline here:
;;   1. Do initial setup, and determine the heading between particle centers
;;      (call it theta).
;;   2. Convert the representation of the velocity of each particle from
;;      speed/heading to a theta-based vector whose first component is the
;;      particle's speed along theta, and whose second component is the speed
;;      perpendicular to theta.
;;   3. Modify the velocity vectors to reflect the effects of the collision.
;;      This involves:
;;        a. computing the velocity of the center of mass of the whole system
;;           along direction theta
;;        b. updating the along-theta components of the two velocity vectors.
;;   4. Convert from the theta-based vector representation of velocity back to
;;      the usual speed/heading representation for each particle.
;;   5. Perform final cleanup and update derived quantities.
to collide-with [ other-particle ] ;; particle procedure
  let mass2 0
  let speed2 0
  let heading2 0
  let theta 0
  let v1t 0
  let v1l 0
  let v2t 0
  let v2l 0
  let vcm 0


  ;;; PHASE 1: initial setup

  ;; for convenience, grab some quantities from other-particle
  set mass2 [mass] of other-particle
  set speed2 [speed] of other-particle
  set heading2 [heading] of other-particle

  ;; since particles are modeled as zero-size points, theta isn't meaningfully
  ;; defined. we can assign it randomly without affecting the model's outcome.
  set theta (random-float 360)



  ;;; PHASE 2: convert velocities to theta-based vector representation

  ;; now convert my velocity from speed/heading representation to components
  ;; along theta and perpendicular to theta
  set v1t (speed * cos (theta - heading))
  set v1l (speed * sin (theta - heading))

  ;; do the same for other-particle
  set v2t (speed2 * cos (theta - heading2))
  set v2l (speed2 * sin (theta - heading2))



  ;;; PHASE 3: manipulate vectors to implement collision

  ;; compute the velocity of the system's center of mass along theta
  set vcm (((mass * v1t) + (mass2 * v2t)) / (mass + mass2) )

  ;; now compute the new velocity for each particle along direction theta.
  ;; velocity perpendicular to theta is unaffected by a collision along theta,
  ;; so the next two lines actually implement the collision itself, in the
  ;; sense that the effects of the collision are exactly the following changes
  ;; in particle velocity.
  set v1t (2 * vcm - v1t)
  set v2t (2 * vcm - v2t)



  ;;; PHASE 4: convert back to normal speed/heading

  ;; now convert my velocity vector into my new speed and heading
  set speed sqrt ((v1t ^ 2) + (v1l ^ 2))
  set energy (0.5 * mass * (speed ^ 2))
  ;; if the magnitude of the velocity vector is 0, atan is undefined. but
  ;; speed will be 0, so heading is irrelevant anyway. therefore, in that
  ;; case we'll just leave it unmodified.
  if v1l != 0 or v1t != 0
    [ set heading (theta - (atan v1l v1t)) ]

  ;; and do the same for other-particle
  ask other-particle [
    set speed sqrt ((v2t ^ 2) + (v2l ^ 2))
    set energy (0.5 * mass * (speed ^ 2))
    if v2l != 0 or v2t != 0
      [ set heading (theta - (atan v2l v2t)) ]
  ]


end



to update-variables
  set avg-speed  mean [speed] of particles
  set avg-energy mean [energy] of particles
end

;;
;;; visualization procedures


to do-recolor
  ask particles [
    if speed-as-color? = "red-green-blue" [ recolor-rgb ]
    if speed-as-color? = "purple shades" [ recolor-shaded ]
    if speed-as-color?  = "one color" [ recolor-none ]
    if speed-as-color? = "custom color" [ ]
  ]
end

to recolor-rgb  ;; particle procedure
  ifelse speed < (0.5 * 10)
  [set color blue]
  [
    ifelse speed > (1.5 * 10)
      [ set color red ]
    [ set color green ]
  ]
end



to recolor-shaded
  ifelse speed < 27
    [ set color 111 + speed / 3 ]
  [ set color 119.999 ]
end


to recolor-none
  set color green - 1
end


to turn-labels-on
  ask turtles
    [ set label who
      set label-color orange + 3
  ]
end

;;;
;;; reporters
;;;



;; reports color of box according to temperature and position
;; if only one side is heated, the other walls will be yellow
to-report box-color
  ifelse heated-wall?
    [ report scale-color red outside-energy -60 340 ]
  [ report yellow ]
end

;; reports true if there is a heated wall at the given location
to-report heated-wall?
  if (( abs pxcor = box-edge) and (abs pycor <= box-edge)) or
  ((abs pycor = box-edge) and (abs pxcor <= box-edge))
  [ report true ]
  report false
end

;;;
;;; drawing procedures
;;;


;; draws the macro-level soda can crush experiment
to make-can-crush-experiment

  make-fire
  make-water
  make-can
  make-steam

end

to make-fire
  ask patches with [pycor > min-pycor + 9
    and pycor < min-pycor + 14
    and pxcor > max-pxcor - 20
    and pxcor < max-pxcor - 3]
  [
    set pcolor gray

    sprout-fires 1 [
      set heading 0
      fd 2
      set shape "fire"
      set size 3
      set color one-of [orange red]
    ]
  ]
end

to make-water
  ask patches with [distancexy (min-pxcor + 12) (min-pycor + 16) < 7
    and pycor < min-pycor + 16
    and pxcor < min-pxcor + 30
    and pxcor > min-pxcor + 4]
  [
    set pcolor blue

    sprout-ices 1 [ set heading 0 fd 2 set size 3 ]

    if water-temperature = "ice bath (approx. 5C)" [
      set water-temp 5
      ask ices [
        set shape "box"
        set color one-of [85 85 86 87 88 ] ;; 85 = cyan
      ]
    ]

    if water-temperature = "room temperature (approx. 23 C)" [
      set water-temp 23
      ask ices [
        set shape "square"
        set color blue
      ]
    ]

    if water-temperature = "hot water (approx. 55 C)" [
      set water-temp 55
      ask ices [
        set shape "square"
        set color blue + 2
      ]
    ]

  ]
end

to make-can
  create-cans 1 [
    ifelse see-inside? [
      set shape "soda can with box"
    ][
      set shape "soda can"
    ]

    set size can-size ;jamie
    set heading 15
    set my-temp 23
    recolor-soda-can
    set outside-energy my-temp * outside-coefficient
  ]
end

to make-steam ;jamie
  create-steams 1 [
    set shape "steam"
    set heading 90
    set size 10
    set color gray
    fd 2 ;move to right position relative to can
    lt 90
    fd 8
    set heading 15
    set hidden? true
  ]
end

to recolor-soda-can
  set color scale-color red my-temp -50 200
end


;;;;;;;;;;;;;;;;;;;;

;; draws the box
to make-box
  set walls patches with [ ((abs pxcor = box-edge) and (abs pycor <= box-edge)) or
    ((abs pycor = box-edge) and (abs pxcor <= box-edge)) ]


  ask walls
    [ if see-inside? [ set pcolor box-color ] ]
end

;; creates initial particles

to make-particles [number]
  create-particles number
  [
    setup-particle
    random-position
    if not see-inside? [ set hidden? true ]
  ]

end

to setup-particle  ;; particle procedure
                   ;set speed random-float 10
  set speed 6.3 ;; umit - let's start with all of them with the same speed (6.3 -- roughly the initial stabilizing value)
  set mass 1.0
  set energy (0.5 * mass * speed * speed)
  set last-collision nobody
  set wall-hits 0
  set momentum-difference 0
  random-position
end

;; place particle at random location inside the box.
to random-position ;; particle procedure
  setxy ((1 - box-edge) + random-float ((2 * box-edge) - 2))
  ((1 - box-edge) + random-float ((2 * box-edge) - 2))
end



; Copyright 2005 Uri Wilensky.
; See Info tab for full copyright and license.
@#$#@#$#@
GRAPHICS-WINDOW
215
15
871
512
-1
-1
8.0
1
10
1
1
1
0
0
0
1
-40
40
-30
30
1
1
1
ticks
30.0

BUTTON
10
85
195
145
go/stop
go
T
1
T
OBSERVER
NIL
G
NIL
NIL
0

BUTTON
10
15
195
71
setup/reset
setup
NIL
1
T
OBSERVER
NIL
R
NIL
NIL
1

CHOOSER
885
15
1095
60
speed-as-color?
speed-as-color?
"red-green-blue" "purple shades" "one color"
1

SWITCH
10
380
195
413
see-inside?
see-inside?
1
1
-1000

BUTTON
9
423
194
498
see inside
see-inside
NIL
1
T
OBSERVER
NIL
S
NIL
NIL
1

SLIDER
10
160
195
193
initial-water-amount
initial-water-amount
0
50
9.0
1
1
ml
HORIZONTAL

CHOOSER
10
210
195
255
water-temperature
water-temperature
"room temperature (approx. 23 C)" "hot water (approx. 55 C)" "ice bath (approx. 5C)"
2

MONITOR
725
25
855
90
can location
can-state
17
1
16

PLOT
885
75
1095
230
particles in the cross-section
ticks
#
0.0
10.0
0.0
20.0
true
false
"let max-y (initial-water-amount * 2)\nset-plot-y-range 0 max-y" ""
PENS
"particles" 1.0 0 -16050907 true "" "plot count particles"

MONITOR
885
350
1092
415
percent crush
word (round percent-crush) \" %\"
0
1
16

MONITOR
885
430
1095
495
time on fire
word (precision time-on-fire 1) \"s\"
1
1
16

CHOOSER
10
270
195
315
can-volume
can-volume
"240 mL" "355 mL" "473 mL"
1

@#$#@#$#@
## WHAT IS IT?

This model explores the relationships between volume, temperature, and particle speeds/behavior by simulating a can crush lab. The key concept behind the lab is a quick temperature change to a heated can containing water.

## HOW IT WORKS

### The Particles

The particles are modeled as hard balls with no internal energy except that which is due to their motion. Collisions between particles are elastic. The total kinetic energy of the two particles after the encounter is equal to their total kinetic energy before the encounter. Collisions with the wall are not. When a particle hits the wall, it bounces off the wall but does not loose any energy to the wall. It does not gain any energy from the wall, either. The plunger moves according to the total force applied on it by the particles at an instance.

The exact way two particles collide is as follows: 1. A particle moves in a straight line without changing its speed, unless it collides with another particle or bounces off the wall. 2. Two particles "collide" if they find themselves on the same patch. In this model, two turtles are aimed so that they will collide at the origin. 3. An angle of collision for the particles is chosen, as if they were two solid balls that hit, and this angle describes the direction of the line connecting their centers. 4. The particles exchange momentum and energy only along this line, conforming to the conservation of momentum and energy for elastic collisions. 5. Each particle is assigned its new speed, heading and energy.

### The Can
The starting temperature, can volume, and water volume/temperature are all recorded at the beginning. These numbers, along with the time the can spends on the fire, factor into calculating how crushed a can is at the end of the experiment. Calculations are based on prior lab experiments with some randomness for realistic variations in results.

## HOW TO USE IT

SETUP/RESET - reconfigures the model to its beginning state
GO - begins the simulation
INITIAL-WATER-AMOUNT - a slider to set the starting volume of water in mL
WATER-TEMPERATURE - can pick between starting temperatures of room temperature, near freezing, or hot water
CAN-VOLUME - the starting volume for the can; these volumes are based on the volumes used in the referenced lab experiment
SEE-INSIDE? - a checkbox or button to observe a particle view of a portion of the inside of the can (the view corresponds to the small square cutout that appears on the can)


The can follows your mouse. Hover over the fire to heat it up. Click to flip the can upside down over the bowl of water. Monitors in the lower right help you track how much the can reduced in size and how much time the can spent on the fire.

## THINGS TO NOTICE

Notice the number of particles being tracked in the graph on the right (corresponds to the number of particles in the particle view). What happens as more water heats up and evaporates.

## THINGS TO TRY

Try using the key shortcuts in the upper right corner of the buttons for SETUP, GO, and SEE-INSIDE. Definitely try exploring the see-inside functionality and noticing how the particles change as the temperature changes.

## EXTENDING THE MODEL

Water temperature and can volume could be implemented as sliders to allow for an even greater variety of results. The current model also relies on a hardcoded algorithm that takes into account the different temperatures and  volume; a neat extension would be changing the simulation into one that is purely agent-based.

## NETLOGO FEATURES

This model uses custom turtle shapes for the soda cans and steam animation created in the shapes editor.

## RELATED MODELS

See the Connected Chemistry models under Curricular Models in the models library, especially the ones for volume, temperature, and pressure.

## CREDITS AND REFERENCES

If you mention this model or the NetLogo software in a publication, we ask that you include the citations below.

For the model itself:

Aslan, U., Lee, J., and Wilensky, U. (2020). NetLogo Can Crusher Simulation model. https://ct-stem.northwestern.edu/curriculum/preview/2625/page/5/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

Please cite the NetLogo software as:

Wilensky, U. (1999). NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.
To cite the Particulate Nature of Matter curriculum as a whole, please use:

Novak, M., Brady, C., Holbert, N., Soylu, F. and Wilensky, U. (2010). Particulate Nature of Matter curriculum. http://ccl.northwestern.edu/curriculum/pnom/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL. Thanks to Umit Aslan and Mitchell Estberg for updating these models for inclusion the in Models Library.

### COPYRIGHT AND LICENSE
Copyright 2010 Uri Wilensky.

CC BY-NC-SA 3.0

This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 3.0 License. To view a copy of this license, visit https://creativecommons.org/licenses/by-nc-sa/3.0/ or send a letter to Creative Commons, 559 Nathan Abbott Way, Stanford, California 94305, USA.

Commercial licenses are also available. To inquire about commercial licenses, please contact Uri Wilensky at uri@northwestern.edu.
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

circle
false
0
Circle -7500403 true true 35 35 230

clock
true
0
Circle -7500403 true true 30 30 240
Polygon -16777216 true false 150 31 128 75 143 75 143 150 158 150 158 75 173 75
Circle -16777216 true false 135 135 30

cloud
false
0
Circle -7500403 true true 13 118 94
Circle -7500403 true true 86 101 127
Circle -7500403 true true 51 51 108
Circle -7500403 true true 118 43 95
Circle -7500403 true true 158 68 134

fire
false
0
Polygon -7500403 true true 151 286 134 282 103 282 59 248 40 210 32 157 37 108 68 146 71 109 83 72 111 27 127 55 148 11 167 41 180 112 195 57 217 91 226 126 227 203 256 156 256 201 238 263 213 278 183 281
Polygon -955883 true false 126 284 91 251 85 212 91 168 103 132 118 153 125 181 135 141 151 96 185 161 195 203 193 253 164 286
Polygon -2674135 true false 155 284 172 268 172 243 162 224 148 201 130 233 131 260 135 282

flasher
false
0
Rectangle -7500403 false true 45 45 255 255
Rectangle -7500403 false true 54 53 246 246

line
true
0
Line -7500403 true 150 0 150 300

nothing
true
0

soda can
true
0
Polygon -16777216 false false 60 240 66 257 90 285 134 299 164 299 209 284 234 259 240 240
Rectangle -7500403 true true 60 75 240 240
Polygon -7500403 true true 60 238 66 256 90 283 135 298 165 298 210 283 235 256 240 238
Polygon -7500403 true true 60 75 66 57 90 30 135 15 165 15 210 30 235 57 240 75
Polygon -7500403 true true 60 75 66 93 90 120 135 135 165 135 210 120 235 93 240 75
Polygon -16777216 false false 59 75 66 57 89 30 134 15 164 15 209 30 234 56 239 75 235 91 209 120 164 135 134 135 89 120 64 90
Line -16777216 false 210 120 210 285
Line -16777216 false 90 120 90 285
Line -16777216 false 125 131 125 296
Line -16777216 false 65 93 65 258
Line -16777216 false 175 131 175 296
Line -16777216 false 235 93 235 258
Circle -16777216 false false 135 90 30
Circle -16777216 false false 135 120 0
Polygon -16777216 false false 135 120 120 105 120 90 135 75 165 75 180 90 180 105 165 120

soda can crushed
true
0
Polygon -7500403 true true 75 90 120 180 60 210 240 210 195 150 225 90 75 90
Polygon -16777216 false false 60 240 66 257 90 285 134 299 164 299 209 284 234 259 240 240
Polygon -7500403 true true 60 208 66 226 90 253 135 268 165 268 210 253 235 226 240 208
Polygon -7500403 true true 60 75 66 57 90 30 135 15 165 15 210 30 235 57 240 75
Polygon -7500403 true true 60 75 66 93 90 120 135 135 165 135 210 120 235 93 240 75
Polygon -16777216 false false 59 75 66 57 89 30 134 15 164 15 209 30 234 56 239 75 235 91 209 120 164 135 134 135 89 120 64 90
Circle -16777216 false false 135 90 30
Circle -16777216 false false 135 120 0
Polygon -16777216 false false 135 120 120 105 120 90 135 75 165 75 180 90 180 105 165 120
Polygon -7500403 true true 60 75 60 210
Line -16777216 false 135 135 150 180
Line -16777216 false 150 180 90 225
Line -16777216 false 165 135 180 180
Line -16777216 false 180 180 120 240
Line -16777216 false 120 240 135 270
Line -16777216 false 90 225 90 255

soda can crushed 2
true
0
Polygon -7500403 true true 75 90 120 180 60 210 240 210 195 150 225 90 75 90
Polygon -7500403 true true 60 208 66 226 90 253 135 268 165 268 210 253 235 226 240 208
Polygon -7500403 true true 60 75 66 57 90 30 135 15 165 15 210 30 235 57 240 75
Polygon -7500403 true true 60 75 66 93 90 120 135 135 165 135 210 120 235 93 240 75
Polygon -16777216 false false 59 75 66 57 89 30 134 15 164 15 209 30 234 56 239 75 235 91 209 120 164 135 134 135 89 120 64 90
Circle -16777216 false false 135 90 30
Circle -16777216 false false 135 120 0
Polygon -16777216 false false 135 120 120 105 120 90 135 75 165 75 180 90 180 105 165 120
Polygon -7500403 true true 60 75 60 210
Line -16777216 false 77 203 90 231
Line -16777216 false 91 232 119 244
Line -16777216 false 119 244 145 253
Line -16777216 false 144 253 182 242
Line -16777216 false 183 243 209 228
Line -16777216 false 210 228 230 200
Line -16777216 false 130 135 140 181
Line -16777216 false 161 135 152 183
Line -16777216 false 187 129 173 174
Line -16777216 false 109 128 127 183
Line -16777216 false 127 184 91 232
Line -16777216 false 140 182 143 255
Line -16777216 false 153 183 182 242
Line -16777216 false 173 175 211 229
Line -16777216 false 198 124 186 160
Line -16777216 false 187 161 224 210

soda can crushed box
true
0
Polygon -7500403 true true 75 90 120 180 60 210 240 210 195 150 225 90 75 90
Polygon -7500403 true true 60 208 66 226 90 253 135 268 165 268 210 253 235 226 240 208
Polygon -7500403 true true 60 75 66 57 90 30 135 15 165 15 210 30 235 57 240 75
Polygon -7500403 true true 60 75 66 93 90 120 135 135 165 135 210 120 235 93 240 75
Polygon -16777216 false false 59 75 66 57 89 30 134 15 164 15 209 30 234 56 239 75 235 91 209 120 164 135 134 135 89 120 64 90
Circle -16777216 false false 135 90 30
Circle -16777216 false false 135 120 0
Polygon -16777216 false false 135 120 120 105 120 90 135 75 165 75 180 90 180 105 165 120
Polygon -7500403 true true 60 75 60 210
Line -16777216 false 77 203 90 231
Line -16777216 false 91 232 119 244
Line -16777216 false 119 244 145 253
Line -16777216 false 144 253 182 242
Line -16777216 false 183 243 209 228
Line -16777216 false 210 228 230 200
Line -16777216 false 130 135 140 181
Line -16777216 false 161 135 152 183
Line -16777216 false 187 129 173 174
Line -16777216 false 109 128 127 183
Line -16777216 false 127 184 91 232
Line -16777216 false 140 182 143 255
Line -16777216 false 153 183 182 242
Line -16777216 false 173 175 211 229
Line -16777216 false 198 124 186 160
Line -16777216 false 187 161 224 210
Rectangle -16777216 true false 140 174 153 187

soda can crushed flash
true
0
Polygon -7500403 true true 75 90 120 180 60 210 240 210 195 150 225 90 75 90
Polygon -7500403 true true 60 208 66 226 90 253 135 268 165 268 210 253 235 226 240 208
Polygon -7500403 true true 60 75 66 57 90 30 135 15 165 15 210 30 235 57 240 75
Polygon -7500403 true true 60 75 66 93 90 120 135 135 165 135 210 120 235 93 240 75
Polygon -16777216 false false 59 75 66 57 89 30 134 15 164 15 209 30 234 56 239 75 235 91 209 120 164 135 134 135 89 120 64 90
Circle -16777216 false false 135 90 30
Circle -16777216 false false 135 120 0
Polygon -16777216 false false 135 120 120 105 120 90 135 75 165 75 180 90 180 105 165 120
Polygon -7500403 true true 60 75 60 210
Line -16777216 false 77 203 90 231
Line -16777216 false 91 232 119 244
Line -16777216 false 119 244 145 253
Line -16777216 false 144 253 182 242
Line -16777216 false 183 243 209 228
Line -16777216 false 210 228 230 200
Line -16777216 false 130 135 140 181
Line -16777216 false 161 135 152 183
Line -16777216 false 187 129 173 174
Line -16777216 false 109 128 127 183
Line -16777216 false 127 184 91 232
Line -16777216 false 140 182 143 255
Line -16777216 false 153 183 182 242
Line -16777216 false 173 175 211 229
Line -16777216 false 198 124 186 160
Line -16777216 false 187 161 224 210
Rectangle -1 true false 140 174 153 187

soda can with box
true
0
Polygon -16777216 false false 60 240 66 257 90 285 134 299 164 299 209 284 234 259 240 240
Rectangle -7500403 true true 60 75 240 240
Polygon -7500403 true true 60 238 66 256 90 283 135 298 165 298 210 283 235 256 240 238
Polygon -7500403 true true 60 75 66 57 90 30 135 15 165 15 210 30 235 57 240 75
Polygon -7500403 true true 60 75 66 93 90 120 135 135 165 135 210 120 235 93 240 75
Polygon -16777216 false false 59 75 66 57 89 30 134 15 164 15 209 30 234 56 239 75 235 91 209 120 164 135 134 135 89 120 64 90
Line -16777216 false 210 120 210 285
Line -16777216 false 90 120 90 285
Line -16777216 false 125 131 125 296
Line -16777216 false 65 93 65 258
Line -16777216 false 175 131 175 296
Line -16777216 false 235 93 235 258
Circle -16777216 false false 135 90 30
Circle -16777216 false false 135 120 0
Polygon -16777216 false false 135 120 120 105 120 90 135 75 165 75 180 90 180 105 165 120
Rectangle -1 false false 135 180 150 195
Rectangle -16777216 true false 135 180 150 195

soda can with box flash
true
0
Polygon -16777216 false false 60 240 66 257 90 285 134 299 164 299 209 284 234 259 240 240
Rectangle -7500403 true true 60 75 240 240
Polygon -7500403 true true 60 238 66 256 90 283 135 298 165 298 210 283 235 256 240 238
Polygon -7500403 true true 60 75 66 57 90 30 135 15 165 15 210 30 235 57 240 75
Polygon -7500403 true true 60 75 66 93 90 120 135 135 165 135 210 120 235 93 240 75
Polygon -16777216 false false 59 75 66 57 89 30 134 15 164 15 209 30 234 56 239 75 235 91 209 120 164 135 134 135 89 120 64 90
Line -16777216 false 210 120 210 285
Line -16777216 false 90 120 90 285
Line -16777216 false 125 131 125 296
Line -16777216 false 65 93 65 258
Line -16777216 false 175 131 175 296
Line -16777216 false 235 93 235 258
Circle -16777216 false false 135 90 30
Circle -16777216 false false 135 120 0
Polygon -16777216 false false 135 120 120 105 120 90 135 75 165 75 180 90 180 105 165 120
Rectangle -1 false false 135 180 150 195
Rectangle -1 true false 135 180 150 195

square
false
0
Rectangle -7500403 true true 0 0 297 299

steam
true
0
Line -11221820 false 180 210 180 285
Line -11221820 false 150 225 150 285
Line -11221820 false 135 210 135 285
Circle -11221820 true false 13 118 94
Circle -7500403 true true 86 101 127
Circle -7500403 true true 51 51 108
Circle -11221820 true false 118 43 95
Circle -7500403 true true 158 68 134
Line -7500403 true 105 195 105 285
Line -7500403 true 120 210 120 285
Line -7500403 true 165 210 165 285
Line -7500403 true 195 195 195 285
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
