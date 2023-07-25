#lang racket
(require racket/draw)
(require colors)

(define (makeoutputname testnum prefix)
  (let ((suffix
        (cond
          [(< testnum 10) (format "00~v.png" testnum)]
          [(< testnum 100) (format "0~v.png" testnum)]
          [(format "~v.png" testnum)])))
  (string-append prefix suffix)))
; ==============================================================================================================================================================================================


(define imageWidth 2048)
(define imageHeight 1152)

(define notAchievedSmallestRequiredPolygon #t)

(define (checkPolygonSize polygonLength currentPicture)
  (cond [(<= polygonLength 0.000000000001) (printf "Polygon <= 0.000000000001 achieved on picture #") (print currentPicture) (printf "\n") (set! notAchievedSmallestRequiredPolygon #f)])
 )


(define myPolygon (new dc-path%))

(define colorVector (make-vector 3))
(vector-set! colorVector 0 0)
(vector-set! colorVector 1 0)
(vector-set! colorVector 2 0)


(define (changeColorVector hue saturation value)
  (define tempVector (make-vector 3))
  (vector-set! tempVector 0 hue)
  (vector-set! tempVector 1 saturation)
  (vector-set! tempVector 2 value)

  (for/vector ([i (vector-length tempVector)])
    (vector-set! colorVector i (+ (vector-ref colorVector i) (vector-ref tempVector i)))
    (cond [(> (vector-ref colorVector i) 1 ) (vector-set! colorVector i (- (vector-ref colorVector i) 1))]
          [(< (vector-ref colorVector i) 0) (vector-set! colorVector i (+ (vector-ref colorVector i) 1))])
   )
)

(define (makePictures polygon worldWidth worldHeight numPictures currentPicture)
  ;============= Picture Setup =======================
  (define myTarget(make-bitmap imageWidth imageHeight))
  (define dc (new bitmap-dc% [bitmap myTarget]))
  (send dc set-brush (make-color 255 255 131) 'solid)
  (send dc draw-rectangle 0 0 imageWidth imageHeight)
  (send dc set-pen "black" 2 'solid)
  
 ;---------------------------------------------------

  ;========== Zooming Calculations =================
  (define xWorldMin (-(/ worldWidth 2)))
  (define yWorldMin (-(/ worldHeight 2)))
  (define xWorldMax (/ worldWidth 2))
  (define yWorldMax (/ worldHeight 2))
  (define xScale (/ imageWidth worldWidth))
  (define yScale (/ imageHeight worldHeight))
  (define xTrans (* -1 (* xWorldMin xScale)))
  (define yTrans (* -1 (* yWorldMin yScale)))

  ;================== Draw Shape ===========================
  (define (drawShape shape x y xScale yScale xTrans yTrans)
  (send shape scale xScale yScale)
  (send shape translate (+ x xTrans) (+ y yTrans))
  (send dc draw-path shape)
  (send shape translate (* -1 xTrans) (* -1 yTrans))
  (send shape scale (/ 1 xScale) (/ 1 yScale))
  )
  ;--------------------------------------------------------- 

  ;========================== The Fractal =================================================
  (define (drawSpirals shape numShapes overallRadius xScale yScale xTrans yTrans)
    (define goldenRatio 1.61803398875)
    (define goldenAngle (* goldenRatio (* 2 pi)))
    (define overallArea (* (expt overallRadius 2) pi))
    (define avgArea (/ overallArea numShapes))
    (define minSmallShapeArea (* avgArea (- 1 (/ 5 9))))

    ; 3 + (5/9) instead of 1 + (3/9) for video
    (define maxSmallShapeArea (* avgArea (+ 3 (/ 3 9))))
  
    (define (recursion shape shapeNum cumulativeArea)
      (define ratio (/ shapeNum numShapes))
      (define angle (* shapeNum goldenAngle))
      (define smallShapeArea (+ minSmallShapeArea (* (- maxSmallShapeArea minSmallShapeArea) ratio)))
      (define smallShapeRadius (sqrt (/ smallShapeArea pi)))
      (define spiralRadius (sqrt (/ cumulativeArea pi)))
   
      (send shape reset)

      (send shape move-to 0 0)
      (send shape line-to 2 0)
      (send shape line-to 2 2)
      (send shape line-to 0 2)
      (send shape close)
      (send shape move-to 0 0)
      (send shape line-to 1 -1)
      (send shape line-to 3 -1)
      (send shape line-to 2 0)
      (send shape close)
      (send shape move-to 2 2)
      (send shape line-to 3 1)
      (send shape line-to 3 -1)
      (send shape line-to 2 0)
      (send shape close)
      (send shape scale (/ 1 xScale) (/ 1 yScale))

      
      (cond [notAchievedSmallestRequiredPolygon (checkPolygonSize (* 3 (/ 1 xScale)) currentPicture)])


      ; ----- Safely change brush values -----
      ;(changeColorVector (colorConversion shapeNum numShapes)(colorConversionAngle angle) (colorConversionAngle angle))
      
      (changeColorVector (colorConversion smallShapeArea maxSmallShapeArea) (colorConversionAngle angle) 0.2)
      
      ;; Good for looking at color values
      ;(print (vector-ref colorVector 0))
      ;(printf "  ")
      ;(print (vector-ref colorVector 1))
      ;(printf "  ")
      ;(print (vector-ref colorVector 2))
      ;(printf "\n")


      ; Used in video
      (send dc set-brush (hsv->color(hsv (vector-ref colorVector 0) (vector-ref colorVector 1) (vector-ref colorVector 2))) 'solid)

      ; For greenish color 
      ;(send dc set-brush (hsv->color(hsv 0.22 1 (colorConversion smallShapeArea maxSmallShapeArea))) 'solid)

      (send shape scale  (/ smallShapeRadius 2) (/ smallShapeRadius 2))
      (drawShape shape (* (cos angle) spiralRadius) (* (sin angle) spiralRadius) xScale yScale xTrans yTrans)
      
      (cond [(<= shapeNum numShapes) (recursion shape (+ 1 shapeNum) (+ smallShapeArea cumulativeArea))])
      )

    (define (colorConversion smaller larger)
      (cond [(>= 0 smaller) 0.01]
            [(> larger smaller) (/ smaller larger)]
            [else 0.01])

      )

    (define (colorConversionAngle rads)
      (cond [(> rads (* 2 pi)) (colorConversionAngle (- rads (* 2 pi)))]
            [else (/ rads (* 2 pi))])

    )
    
    (recursion shape 0 0)
  )
  ;-----------------------------------------------------------------------------------------------------------------------------

  ;======================================= Drawing each Iteration ===================================================

  
  (send dc set-brush (make-color 252 252 252) 'solid)

  ; (/ imageWidth 4) instead of (/ imageWidth 2) to see more of the image
  (drawSpirals myPolygon 1500 (/ imageWidth 4) xScale yScale xTrans yTrans)

  ;; Uncomment to see world sizes
  ;(print worldWidth)
  ;(printf "\n")
  ;(print worldHeight)
  ;(printf "\n")


  (send myTarget save-file (makeoutputname currentPicture "fractal.png") 'png)

  (cond [(= 0 (modulo currentPicture 50)) (printf "Picture #") (print currentPicture) (printf ": ") (print worldWidth) (printf "x") (print worldHeight) (printf "\n")])
  
  (cond [(< currentPicture numPictures) (makePictures polygon (* worldWidth 0.95) (* worldHeight 0.95) numPictures (+ 1 currentPicture))])

  myTarget
  
)


(makePictures myPolygon 2048 1152 0 0)