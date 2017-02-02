import java.io._
import scala.collection.mutable.ArrayBuilder

object Io {
  def readLines(file: String) = scala.io.Source.fromFile(file).getLines()

  def readString(file: String) = scala.io.Source.fromFile(file).mkString

  def write(file: String)(p: PrintWriter => Unit) = {
    val pw = new PrintWriter(file, "UTF-8")
    try
      p(pw)
    finally
      pw.close()
  }

  def write(file: String, data: Array[Byte]) = {
    val s = new FileOutputStream(file)
    try
      s.write(data)
    finally
      s.close()
  }

  def foreach(root: String)(f: File => Unit): Unit = foreach(new File(root))(f)

  def foreach(root: File)(f: File => Unit): Unit = {
    f(root)
    if (root.isDirectory)
      root.listFiles().foreach(foreach(_)(f))
  }

  private def _list(root: File, builder: ArrayBuilder[File]): ArrayBuilder[File] = {
    if (root.isDirectory)
      root.listFiles().foreach(_list(_, builder))
    else
      builder += root
    builder
  }

  def list(root: String) = _list(new File(root), ArrayBuilder.make()).result()
}
