package nettlesome

trait Protocolic:
  type Self
  type On
  type Request
  type Response
  type Server
  def server(port: On)(lambda: Request ?=> Response): Server
