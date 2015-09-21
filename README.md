# pixelprocessing
R programs from Advanced Signal and Image Processing Class

Scripts are grouped according to these categories: Shape, Image, Texture, Color

# Shape

#### process_freeman

Determine the Freeman vector codes (as an image or a vector), Fourier descriptors, shape vertices and boundary

#### freeman_gradient

Compute for the gradient and running sums from a Freeman vector

#### magnify_shape

Magnify a shape

#### rotate_shape

Rotate shape about an origin

#### invariant_moments_shape

Compute for  the invariant and normalized central moments of a shape

# Image

#### scaleimage

* Magnify an image
* translate image to a new origin
* increase image background, i.e. zero padding

#### edge_detect

Edge detection using Sobel, Sobel-Feldman, Prewitt operators

#### invariant_moments

Computing the invariant and normalized central moments of an image based on
the work of Hu (1962)

#### hu_moments

Alternative computation of Hu invariant moments

#### complex_moments

Compute for the complex rotation invariant moments based on the work of Flusser, Suk, Zitova

# Texture

#### lbp

Function to compute the LBP of an image at each pixel, excluding the ones at the boundaries

# Color

#### ncc

Function to compute for the normalized chromaticity colorspace and generation of 2D color histograms
