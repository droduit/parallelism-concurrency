
import common._

package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24
  
  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))
    def apply(x: Int, y: Int): RGBA = data(y * width + x)
    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    // TODO implement using while loops
    /*
    val pixels = for {
      x <- clamp(x-radius, 0, src.width-1) to clamp(x+radius, 0, src.width-1)
      y <- clamp(y-radius, 0, src.height-1) to clamp(y+radius, 0, src.height-1)
    } yield src(x,y);
    
    val size = pixels.size
    
    pixels.foldLeft(0)(0,0,0,0) {
      case (p, (r,g,b,a)) {
        rgba(r/countPixels, g/countPixels, b/countPixels, a/countPixels)
      }
    }
   */
      var r : Int = 0;
      var g : Int = 0;
      var b : Int = 0;
      var a : Int = 0;
      
      var minXOffset:Int = clamp(x-radius, 0, src.width-1);
      var minYOffset:Int = clamp(y-radius, 0, src.height-1);
      
      var maxXOffset:Int = clamp(x+radius, 0, src.width-1);
      var maxYOffset:Int = clamp(y+radius, 0, src.height-1);
      
      var i : Int = minXOffset;
      var j : Int = minYOffset;
      
      var countPixels:Int = 0;
      
      while(i <= maxXOffset) {
        j = minYOffset;
        while(j <= maxYOffset) {
          
          def pixel : RGBA = src.apply(i,j);
           
          r += red(pixel);
          g += green(pixel);
          b += blue(pixel);
          a += alpha(pixel);
          
          j = j+1;
          countPixels = countPixels+1;
        }
        i = i+1;
      }
      
      rgba(r/countPixels, g/countPixels, b/countPixels, a/countPixels);
    
  }

}
