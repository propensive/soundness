package honeycomb

/*object Formatter {

  def format(root: Node[_]): Array[Byte] = {

    def size(indent: Int, node: Node[_]): Int = node match {
      case e: Element[_] =>
        val tagLength = if(e.unclosed) e.tagName.length + 2 else e.tagName.length*2 + 5
        tagLength + e.attributes.map(_.toString.length).sum + e.attributes.size
      case Text(str) =>
        str.length
      case Elements(Nil) =>
        0
      case Elements(h :: t) =>
        size(indent, h) + size(indent, Elements(t))
    }

    val array = new Array[Byte](size(0, root))

    def write(indent: Int, pos: Int, node: Node[_]): Unit = node match {
      case e: Element[_] =>
        array(pos) = '<'.toByte
        var idx = 1
        while(idx < 1 + tagName)
        for(i <- 1 to e.tagName.length) array(i) = e.tagName(i - 1).toByte
        for(a <- e.attributes)
      case Text(str) =>
        
      case Elements(Nil) =>
        
      case Elements(h :: t) =>
        
    }

    write(0, 0, root)

    array
  }

}*/
