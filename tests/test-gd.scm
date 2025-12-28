;;; test-gd.scm - Comprehensive GD Graphics Library Test Suite
;;;
;;; Tests the modernized gd.c bindings for libgd 2.3.3+
;;; Run with: ./siod -v01,-m2 test-gd.scm

(require-so (so-ext "gd"))

(define (test-gd)
  (puts "=== GD Graphics Library Test Suite ===\n\n")
  
  ;; Test 1: Version Information
  (puts "[TEST 1] Version information\n")
  (puts "  Version: ")
  (print *gd-version*)
  (puts "\n")
  
  ;; Test 2: Image Creation
  (puts "\n[TEST 2] Image creation\n")
  (define img (gdImageCreate 400 300))
  (puts "  Created 400x300 image: ")
  (print img)
  (puts "\n")
  
  ;; Test 3: Color Allocation
  (puts "\n[TEST 3] Color allocation\n")
  (define white (gdImageColorAllocate img 255 255 255))
  (define black (gdImageColorAllocate img 0 0 0))
  (define red (gdImageColorAllocate img 255 0 0))
  (define green (gdImageColorAllocate img 0 255 0))
  (define blue (gdImageColorAllocate img 0 0 255))
  (define yellow (gdImageColorAllocate img 255 255 0))
  (puts "  Allocated colors: white=")
  (print white)
  (puts " black=")
  (print black)
  (puts " red=")
  (print red)
  (puts "\n")
  
  ;; Test 4: Lines
  (puts "\n[TEST 4] Drawing lines\n")
  (gdImageLine img 0 0 399 299 black)
  (gdImageLine img 399 0 0 299 black)
  (puts "  Drew diagonal lines\n")
  
  ;; Test 5: Rectangles
  (puts "\n[TEST 5] Drawing rectangles\n")
  (gdImageRectangle img 50 50 350 250 blue)
  (gdImageFilledRectangle img 100 100 150 150 red)
  (puts "  Drew outlined and filled rectangles\n")
  
  ;; Test 6: Arcs
  (puts "\n[TEST 6] Drawing arcs\n")
  (gdImageArc img 200 150 100 100 0 360 green)
  (gdImageArc img 200 150 80 80 45 135 yellow)
  (puts "  Drew circle and arc segment\n")
  
  ;; Test 7: Pixels
  (puts "\n[TEST 7] Setting pixels\n")
  (gdImageSetPixel img 200 150 red)
  (gdImageSetPixel img 201 150 red)
  (gdImageSetPixel img 200 151 red)
  (gdImageSetPixel img 201 151 red)
  (puts "  Drew 2x2 pixel cluster\n")
  
  ;; Test 8: Polygons
  (puts "\n[TEST 8] Drawing polygons\n")
  (define triangle (gdPoint 80 80 120 80 100 40))
  (gdImagePolygon img triangle yellow)
  (define filled-triangle (gdPoint 280 80 320 80 300 40))
  (gdImageFilledPolygon img filled-triangle green)
  (puts "  Drew outlined and filled triangles\n")
  
  ;; Test 9: Text rendering with all fonts
  (puts "\n[TEST 9] Text rendering\n")
  (gdImageString img gdFontTiny 10 10 "Tiny" black)
  (gdImageString img gdFontSmall 10 30 "Small" black)
  (gdImageString img gdFontMediumBold 10 50 "Medium" black)
  (gdImageString img gdFontLarge 10 70 "Large" black)
  (gdImageString img gdFontGiant 10 95 "Giant" black)
  (puts "  Rendered text with all 5 fonts\n")
  
  ;; Test 10: Vertical text
  (puts "\n[TEST 10] Vertical text\n")
  (gdImageStringUp img gdFontLarge 370 280 "Vertical" blue)
  (puts "  Rendered vertical text\n")
  
  ;; Test 11: Single character
  (puts "\n[TEST 11] Character rendering\n")
  (gdImageChar img gdFontGiant 180 200 65 red)  ; 'A'
  (gdImageCharUp img gdFontGiant 220 220 66 blue) ; 'B' vertical
  (puts "  Rendered individual characters\n")
  
  ;; Test 12: Font dimensions
  (puts "\n[TEST 12] Font dimensions\n")
  (puts "  gdFontTiny: ")
  (print (gdFont.w gdFontTiny))
  (puts "x")
  (print (gdFont.h gdFontTiny))
  (puts " pixels\n")
  (puts "  gdFontGiant: ")
  (print (gdFont.w gdFontGiant))
  (puts "x")
  (print (gdFont.h gdFontGiant))
  (puts " pixels\n")
  
  ;; Test 13: Transparency
  (puts "\n[TEST 13] Transparency\n")
  (define img2 (gdImageCreate 100 100))
  (define bg (gdImageColorAllocate img2 255 0 255)) ; Magenta background
  (define fg (gdImageColorAllocate img2 0 0 0))
  (gdImageColorTransparent img2 bg)
  (gdImageRectangle img2 10 10 90 90 fg)
  (puts "  Set transparent color\n")
  
  ;; Test 14: Interlacing
  (puts "\n[TEST 14] Interlacing\n")
  (gdImageInterlace img 1)
  (puts "  Enabled interlacing\n")
  
  ;; Test 15: Fill operations
  (puts "\n[TEST 15] Fill operations\n")
  (define img3 (gdImageCreate 100 100))
  (define white3 (gdImageColorAllocate img3 255 255 255))
  (define black3 (gdImageColorAllocate img3 0 0 0))
  (define red3 (gdImageColorAllocate img3 255 0 0))
  (gdImageRectangle img3 10 10 90 90 black3)
  (gdImageFill img3 50 50 red3)
  (puts "  Performed flood fill\n")
  
  ;; Test 16: Save to GIF
  (puts "\n[TEST 16] Save to GIF file\n")
  (define f1 (fopen "test-output.gif" "wb"))
  (gdImageGif img f1)
  (fclose f1)
  (puts "  Saved main test image to test-output.gif\n")
  
  (define f2 (fopen "test-transparent.gif" "wb"))
  (gdImageGif img2 f2)
  (fclose f2)
  (puts "  Saved transparent image to test-transparent.gif\n")
  
  (define f3 (fopen "test-fill.gif" "wb"))
  (gdImageGif img3 f3)
  (fclose f3)
  (puts "  Saved filled image to test-fill.gif\n")
  
  ;; Test 17: Load from GIF
  (puts "\n[TEST 17] Load from GIF file\n")
  (define f4 (fopen "test-output.gif" "rb"))
  (define loaded-img (gdImageCreateFromGif f4))
  (fclose f4)
  (puts "  Loaded image from test-output.gif: ")
  (print loaded-img)
  (puts "\n")
  
  ;; Test 18: Color queries
  (puts "\n[TEST 18] Color queries\n")
  (define exact-color (gdImageColorExact img 255 0 0))
  (puts "  Exact color for RGB(255,0,0): ")
  (print exact-color)
  (puts "\n")
  (define closest-color (gdImageColorClosest img 250 5 5))
  (puts "  Closest color for RGB(250,5,5): ")
  (print closest-color)
  (puts "\n")
  
  ;; Test 19: Complex polygon (hexagon)
  (puts "\n[TEST 19] Complex polygon (hexagon)\n")
  (define img4 (gdImageCreate 200 200))
  (define white4 (gdImageColorAllocate img4 255 255 255))
  (define black4 (gdImageColorAllocate img4 0 0 0))
  (define blue4 (gdImageColorAllocate img4 0 0 255))
  (define hexagon (gdPoint 100 30   ; top
                           130 50   ; top-right
                           130 90   ; bottom-right
                           100 110  ; bottom
                           70 90    ; bottom-left
                           70 50))  ; top-left
  (gdImageFilledPolygon img4 hexagon blue4)
  (gdImagePolygon img4 hexagon black4)
  (define f5 (fopen "test-hexagon.gif" "wb"))
  (gdImageGif img4 f5)
  (fclose f5)
  (puts "  Created hexagon in test-hexagon.gif\n")
  
  ;; Test 20: Styled constants
  (puts "\n[TEST 20] Style constants\n")
  (puts "  gdStyled: ")
  (print gdStyled)
  (puts "\n")
  (puts "  gdBrushed: ")
  (print gdBrushed)
  (puts "\n")
  (puts "  gdStyledBrushed: ")
  (print gdStyledBrushed)
  (puts "\n")
  (puts "  gdTiled: ")
  (print gdTiled)
  (puts "\n")
  (puts "  gdTransparent: ")
  (print gdTransparent)
  (puts "\n")
  
  (puts "\n=== All tests completed successfully! ===\n")
  (puts "\nGenerated files:\n")
  (puts "  - test-output.gif (main test image)\n")
  (puts "  - test-transparent.gif (transparency test)\n")
  (puts "  - test-fill.gif (flood fill test)\n")
  (puts "  - test-hexagon.gif (complex polygon)\n"))

;; Run the tests
(test-gd)
