import java.io._

object Io {
  def writeTo(file: String)(p: PrintWriter => Unit) = writeTo(new File(file))(p)

  def writeTo(file: File)(p: PrintWriter => Unit) = {
    val pw = new PrintWriter(file, "UTF-8")
    try
      p(pw)
    finally
      pw.close()
  }

  def writeTo(file: String, data: Array[Byte]) = writeTo(new File(file), data)

  def writeTo(file: File, data: Array[Byte]) = {
    val s = new FileOutputStream(file)
    try
      s.write(data)
    finally
      s.close()
  }

  def foreachIn(root: String)(f: File => Unit): Unit = foreachIn(new File(root))(f)

  def foreachIn(root: File)(f: File => Unit): Unit = {
    f(root)
    if (root.isDirectory)
      root.listFiles().foreach(foreachIn(_)(f))
  }

  @inline private def _list(root: File): Array[File] =
    if (root.isDirectory)
      root.listFiles().flatMap(_list)
    else
      Array(root)

  def list(root: String) = _list(new File(root))

  def list(root: File) = _list(root)
}
