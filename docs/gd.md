# GD Graphics Library Bindings for SIOD

## Overview

The `gd` module provides SIOD bindings to the GD Graphics Library (libgd 2.3.3+), enabling creation and manipulation of images in various formats. GD is designed for fast image generation, particularly useful for charts, graphs, diagrams, and static visualizations.

**Homepage:** https://libgd.github.io/  
**Original SIOD bindings:** George J. Carrette (1996)  
**Modernized for libgd 2.3.3:** 2025

## What GD Is (and Isn't)

**GD is for:**
- Creating static images (PNG, GIF, JPEG, etc.)
- Drawing primitives: lines, rectangles, circles, polygons
- Text rendering with built-in bitmap fonts
- Generating charts, graphs, diagrams programmatically
- Server-side image generation

**GD is NOT for:**
- Interactive graphics (use SDL2 or RayLib instead)
- Real-time rendering
- 3D graphics
- Complex image editing (use ImageMagick/GraphicsMagick)

## Loading the Module

```scheme
(require-so (so-ext "gd"))
```

## Basic Workflow

1. **Create** an image: `(gdImageCreate width height)`
2. **Allocate** colors: `(gdImageColorAllocate image r g b)`
3. **Draw** primitives: lines, rectangles, text, etc.
4. **Output** to file: `(gdImageGif image file)`
5. **Clean up**: Image is garbage-collected automatically

## Core Functions

### Image Creation

#### `(gdImageCreate width height)`
Creates a new palette-based image (up to 256 colors).

```scheme
(define img (gdImageCreate 400 300))
```

**Returns:** gdImage object

### Color Allocation

#### `(gdImageColorAllocate image red green blue)`
Allocates a color in the image palette. RGB values are 0-255.

```scheme
(define white (gdImageColorAllocate img 255 255 255))
(define black (gdImageColorAllocate img 0 0 0))
(define red (gdImageColorAllocate img 255 0 0))
```

**Returns:** Color index (integer)

**Note:** The first color allocated becomes the background color.

#### `(gdImageColorClosest image red green blue)`
Finds the closest color to the specified RGB values.

#### `(gdImageColorExact image red green blue)`
Finds the exact color, or -1 if not found.

### Drawing Primitives

#### `(gdImageLine image x1 y1 x2 y2 color)`
Draws a line from (x1,y1) to (x2,y2).

```scheme
(gdImageLine img 0 0 399 299 black)  ; Diagonal line
```

#### `(gdImageRectangle image x1 y1 x2 y2 color)`
Draws a rectangle outline.

```scheme
(gdImageRectangle img 50 50 350 250 black)
```

#### `(gdImageFilledRectangle image x1 y1 x2 y2 color)`
Draws a filled rectangle.

```scheme
(gdImageFilledRectangle img 100 100 300 200 red)
```

#### `(gdImageArc image cx cy width height start-deg end-deg color)`
Draws an arc. Angles are in degrees (0-360).

```scheme
(gdImageArc img 200 150 100 100 0 180 black)  ; Semi-circle
```

#### `(gdImageSetPixel image x y color)`
Sets a single pixel.

```scheme
(gdImageSetPixel img 200 150 red)
```

### Polygons

#### `(gdPoint x1 y1 x2 y2 ... xn yn)`
Creates a gdPoint array for polygon drawing.

```scheme
(define triangle (gdPoint 200 50   ; Point 1
                          100 250  ; Point 2
                          300 250)); Point 3
```

#### `(gdImagePolygon image points color)`
Draws a polygon outline.

```scheme
(gdImagePolygon img triangle black)
```

#### `(gdImageFilledPolygon image points color)`
Draws a filled polygon.

```scheme
(gdImageFilledPolygon img triangle red)
```

### Filling

#### `(gdImageFill image x y color)`
Flood-fills from point (x,y) with the specified color.

```scheme
(gdImageFill img 200 150 red)  ; Fill from center
```

#### `(gdImageFillToBorder image x y border-color fill-color)`
Fills until encountering the border color.

### Text Rendering

Built-in bitmap fonts are available:
- `gdFontTiny` - 5x8 pixels
- `gdFontSmall` - 6x13 pixels
- `gdFontMediumBold` - 7x13 pixels bold
- `gdFontLarge` - 8x16 pixels
- `gdFontGiant` - 9x15 pixels

#### `(gdImageString image font x y text color)`
Draws horizontal text.

```scheme
(gdImageString img gdFontLarge 10 10 "Hello, World!" black)
```

#### `(gdImageStringUp image font x y text color)`
Draws vertical text (rotated 90 degrees).

#### `(gdImageChar image font x y char-code color)`
Draws a single character.

```scheme
(gdImageChar img gdFontGiant 100 100 65 black)  ; 'A'
```

#### `(gdImageCharUp image font x y char-code color)`
Draws a single character rotated 90 degrees.

### Font Utilities

#### `(gdFont.w font)`
Returns font width in pixels.

#### `(gdFont.h font)`
Returns font height in pixels.

```scheme
(define w (gdFont.w gdFontLarge))  ; Returns 8
(define h (gdFont.h gdFontLarge))  ; Returns 16
```

### Transparency

#### `(gdImageColorTransparent image color)`
Sets the transparent color for GIF images.

```scheme
(gdImageColorTransparent img white)  ; White becomes transparent
```

### Output

#### `(gdImageGif image file)`
Writes image to a GIF file.

```scheme
(define f (fopen "output.gif" "wb"))
(gdImageGif img f)
(fclose f)
```

#### `(gdImageGifmem image buffer)`
Writes GIF data to a memory buffer. Returns number of bytes written.

### Input

#### `(gdImageCreateFromGif file)`
Loads an image from a GIF file.

```scheme
(define f (fopen "input.gif" "rb"))
(define img (gdImageCreateFromGif f))
(fclose f)
```

#### `(gdImageCreateFromXbm file)`
Loads an image from an X bitmap file.

### Image Properties

#### `(gdImageInterlace image flag)`
Sets interlacing on (non-zero) or off (zero) for GIF output.

```scheme
(gdImageInterlace img 1)  ; Enable interlacing
```

## Constants

- `gdStyled` - Use styled line
- `gdBrushed` - Use brushed line
- `gdStyledBrushed` - Combination
- `gdTiled` - Use tiled fill
- `gdTransparent` - Transparent color value

## Complete Example

```scheme
(require-so (so-ext "gd"))

;; Create a 400x300 image
(define img (gdImageCreate 400 300))

;; Allocate colors (first color = background)
(define white (gdImageColorAllocate img 255 255 255))
(define black (gdImageColorAllocate img 0 0 0))
(define red (gdImageColorAllocate img 255 0 0))
(define blue (gdImageColorAllocate img 0 0 255))

;; Draw a rectangle
(gdImageRectangle img 50 50 350 250 black)

;; Draw a filled circle (using arc)
(gdImageArc img 200 150 100 100 0 360 blue)

;; Draw some text
(gdImageString img gdFontGiant 120 10 "Hello, GD!" black)

;; Draw a triangle
(define triangle (gdPoint 200 80 120 220 280 220))
(gdImageFilledPolygon img triangle red)

;; Save to file
(define f (fopen "test.gif" "wb"))
(gdImageGif img f)
(fclose f)

(puts "Image saved to test.gif\n")
```

## Limitations

- Maximum 256 colors in palette mode (use libgd's truecolor functions for more)
- Built-in fonts are bitmap only (for TrueType fonts, use libgd's FreeType functions)
- Output is limited to GIF and XBM in these bindings (libgd supports PNG, JPEG, WebP, etc.)

## Future Enhancements

Potential additions to these bindings:
- PNG output (via `gdImagePng`)
- JPEG output (via `gdImageJpeg`)
- Truecolor image support (via `gdImageCreateTrueColor`)
- Image filters and transformations
- FreeType font rendering

## See Also

- libgd documentation: https://libgd.github.io/manuals/2.3.3/
- `gd-utilities.scm` - High-level helper functions
- `test-gd.scm` - Comprehensive test suite

## Version Information

Access the version string:

```scheme
*gd-version*  ; Returns version information
```
