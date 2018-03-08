import pytesseract

from PIL        import Image, ImageEnhance, ImageFilter

from os         import listdir
from os.path    import isfile, join, abspath, exists

#
# TODO: 
#       integrate gif support, tesseract may need special compilation params for this
#

FILE_EXT  = ".jpeg"   
SRC_DIR   = "./src/"
DEST_DIR  = "."

src_files = [f for f in listdir(SRC_DIR) if f.endswith(FILE_EXT)]

#
# TODO:
#       abspath(src_files[-1]) doesn't include /src/ in the path for some reason
#

test_file = abspath(join(SRC_DIR, src_files[-1]))
image     = Image.open(test_file)

#
# TODO: (image optimizations)
#       .filter(ImageFilter.MedianFilter())
#


text = pytesseract.image_to_string(Image.open(test_file))

print text

