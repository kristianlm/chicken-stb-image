  [CHICKEN]: http://call-cc.org
  [stb_image.h]: https://github.com/nothings/stb

# chicken-stb-image

This is a [CHICKEN] egg that wraps [stb_image.h] version 2.19 from
Sean Barrett and friends. It works on [CHICKEN] 4 and 5.

# API

    [procedure] (read-image #!key channels)
    [procedure] (load-image u8vector #!key channels)

Decodes an image into raw pixel data. `read-image` reads the image
from from `(current-input-port)` while `load-image` gets this from the
provided u8vector. The image type is detected by content, and may be:
JPEG, PNG, TGA, BMP, PSD, GIF, HDR, PIC, PNM as explained in the
heading comments of `stb_image.h`.

Both procedures return 4 values:

1. raw pixel data (as u8vector)
2. width
3. height
4. number of channels

You can force the number of channels with the `channels` keyword. If
`channels` is `#f` or not given, the number of channels in the
original image will be used.

The size of the pixel data blob is always `(* width height channels)`
bytes. The first pixel is the top-left-most in the image. Each pixel
is `channel` number of bytes long. The number of channels define the
pixel color, interleaved as follows:

1. grey
2. grey, alpha
3. red, green, blue
4. red, green, blue, alpha

Note that [stb_image.h]'s `hdr` and `pnm` support only works with
`load-image`.

    [procedure] (read-image-info)
	[procedure] (load-image-info blob)

Like `read-image` and `load-image`, but does not load pixel data and
should be faster. Returns three values:

1. width
2. height
3. number of channels

Note that on 32-bit systems, blobs cannot be larger than 16M. This
limits image sizes as pixel data is stored as blobs.

## Examples

For quick testing, you can read PNGs from ImageMagick's `convert`:

    $ convert -size 4x4 xc:blue -draw 'line 0,0 4,4' png:- | csi -R stb-image -p '(read-image)'
    #u8(0 0 0     0 0 234   0 0 255   0 0 255
        0 0 234   0 0 0     0 0 234   0 0 255
        0 0 255   0 0 234   0 0 0     0 0 234
        0 0 255   0 0 255   0 0 234   0 0 0)
    4
    4
    3

Which reveal the black line along the diagonal and informs that the
image is 4x4 with 3-channels.
