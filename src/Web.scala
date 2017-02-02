import java.io._
import java.net.ServerSocket

object Web {
  def openBrowser(port: Int, path: String = "/") =
    java.awt.Desktop.getDesktop.browse(new java.net.URI(s"http://localhost:$port$path"))

  /**
   * Example:
   * {{{new HttpServer(8080, { case (request, server) =>
   (request.method, request.path) match {
     case ("GET", "/") => TextResponse("helloworld")
     case ("GET", "/img") => FileResponse("123.png", "image/png").and(_.withHeader("X-x", "O-o"))
     case ("GET", s) => Response.guessType(s)
   }
  }, println).start()}}}
   */
  class HttpServer(port: Int, handler: (Request, HttpServer) => Response, log: => String => Unit = _ => ()) {
    private var started = false
    private var stopped = false
    private val serverSocket = new ServerSocket(port)

    def start() = if (!started) {
      started = true
      log("started")
      try
        while (!stopped) {
          val socket = serverSocket.accept()
          log(s"open connection ${socket.hashCode()}")
          try {
            val request = readRequestFrom(new BufferedReader(new InputStreamReader(socket.getInputStream())))
            writeResponse(request, socket.getOutputStream())
          } finally
            socket.close()
          log(s"close connection ${socket.hashCode()}")
        }
      finally
        serverSocket.close()
      log("stopped")
    }

    def stop() = stopped = true

    private def readRequestFrom(input: BufferedReader) = {
      val Array(method, path, _*) = input.readLine().split(" ")
      log(method + " " + path)
      var headers = Map[String, String]()
      var line = input.readLine()
      while (line != null && line.nonEmpty) {
        log(line)
        val Array(header, value, _*) = line.split(": ")
        headers += (header -> value)
        line = input.readLine()
      }
      log("")
      Request(method, path, headers)
    }

    private def writeResponse(request: Request, output: OutputStream) =
      try
        handler(request, this).writeTo(output)
      catch {
        case _: MatchError => Response.notFound.writeTo(output)
        case e: Throwable =>
          val sw = new StringWriter
          val pw = new PrintWriter(sw)
          e.printStackTrace(pw)
          log(sw.toString)
          Response.serverError.writeTo(output)
      }
  }

  case class Request private (method: String, path: String, headers: Map[String, String])

  object Response {
    private val map = Map(
      "css" -> "text/css",
      "flv" -> "video/x-flv",
      "gif" -> "image/gif",
      "h264" -> "video/h264",
      "htm" -> "text/html",
      "html" -> "text/html",
      "js" -> "application/javascript",
      "json" -> "application/json",
      "jpeg" -> "image/jpeg",
      "jpg" -> "image/jpeg",
      "pdf" -> "application/pdf",
      "png" -> "image/png",
      "svg" -> "image/svg+xml",
      "webm" -> "video/webm"
    )

    def guessType(filename: String) = {
      val dot = filename.lastIndexOf('.')
      val ext = if (dot != -1 && dot != filename.length - 1)
        filename.substring(dot + 1)
      else
        ""
      FileResponse(filename, map.getOrElse(ext, "text/plain"))
    }

    val notFound = TextResponse("Not found").and(_.withStatus("HTTP/1.1 404 Not found"))

    val serverError = TextResponse("Internal Server Error").and(_.withStatus("HTTP/1.1 500 Internal Server Error"))
  }

  trait Response {
    def info: ResponseInfo
    def writeTo(output: OutputStream): Unit
    def and(f: ResponseInfo => ResponseInfo): Response
  }

  case class TextResponse(body: String, info: ResponseInfo = ResponseInfo()) extends Response {
    override def writeTo(output: OutputStream) = {
      val result = info.withHeader("Content-Length", body.length.toString).serialized + "\r\n\r\n" + body
      output.write(result.getBytes)
      output.flush()
    }

    override def and(f: (ResponseInfo) => ResponseInfo) = copy(info = f(info))
  }

  case class FileResponse(filename: String, contentType: String, info: ResponseInfo = ResponseInfo()) extends Response {
    override def writeTo(output: OutputStream) = {
      import java.nio.file.{ Files, Paths }
      val body = Files.readAllBytes(Paths.get(filename))
      val result = info.withContentType(contentType).withHeader("Content-Length", body.length.toString).serialized +
          "\r\n\r\n"
      output.write(result.getBytes)
      output.write(body)
      output.flush()
    }

    override def and(f: (ResponseInfo) => ResponseInfo) = copy(info = f(info))
  }

  case class ResponseInfo(status: String = "HTTP/1.1 200 OK",
                          headers: Map[String, String] = Map("Content-Type" -> "text/html", "Connection" -> "close")) {
    def withStatus(value: String) = copy(status = value)

    def withHeader(header: String, value: String) = copy(headers = headers.updated(header, value))

    def withContentType(value: String) = withHeader("Content-Type", value)

    def serialized = status + "\r\n" + headers.map { case (key, value) => s"$key: $value" }.mkString("\r\n")
  }
}
