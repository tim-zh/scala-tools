import java.awt.color.ColorSpace
import java.awt.image.{ BufferedImage, ColorConvertOp }
import java.io.File
import javax.imageio.ImageIO

object Img {
  def read(img: String) = read(new File(img))

  def read(img: File) = Img(ImageIO.read(img))

  def int2Argb(i: Int) = (i >> 24 & 0xFF, i >> 16 & 0xFF, i >> 8 & 0xFF, i >> 0 & 0xFF)

  def int2Ahsb(i: Int) = {
    val (a, r, g, b) = int2Argb(i)
    val Array(h, s, br) = java.awt.Color.RGBtoHSB(r, g, b, null)
    (a, h, s, br)
  }

  def argb2Int(a: Int, r: Int, g: Int, b: Int) = a << 24 | r << 16 | g << 8 | b << 0

  def ahsb2Int(a: Int, h: Float, s: Float, b: Float) = a << 24 | java.awt.Color.HSBtoRGB(h, s, b)

  private val colorConvert = new ColorConvertOp(ColorSpace.getInstance(ColorSpace.CS_GRAY), null)
}

case class Img(img: BufferedImage) {
  override def clone(): Img =
    Img(new BufferedImage(img.getColorModel, img.copyData(null), img.getColorModel.isAlphaPremultiplied, null))

  def write(filename: String, formatName: String = "png"): Unit = ImageIO.write(img, formatName, new File(filename))

  /**
   * @param f (x, y, argb) => newArgb
   */
  def filter(f: (Int, Int, Int) => Int) = {
    val copy = clone()
    for {
      x <- 0 until img.getWidth
      y <- 0 until img.getHeight
    } copy.img.setRGB(x, y, f(x, y, copy.img.getRGB(x, y)))
    copy
  }

  def resize(width: Int, height: Int) = {
    val newImg = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    val g = newImg.createGraphics
    g.drawImage(img, 0, 0, width, height, null)
    g.dispose()
    Img(newImg)
  }

  def grayscale = Img(Img.colorConvert.filter(img, null))

  def histogram = {
    val map = scala.collection.mutable.Map[Int, Int]().withDefaultValue(0)
    for {
      x <- 0 until img.getWidth
      y <- 0 until img.getHeight
    } {
      val rgb = img.getRGB(x, y)
      map.update(rgb, map(rgb) + 1)
    }
    map.toSeq.sortBy(_._2).reverse
  }
}
