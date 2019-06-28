package wikipedia

import scala.io.Source
import java.io.File

object WikipediaData {
  private[wikipedia] val xmlPages1 = loadWikiFile("/wikipedia/wikipedia-shard1.dat")
  private[wikipedia] val xmlPages2 = loadWikiFile("/wikipedia/wikipedia-shard2.dat")

  private[wikipedia] def loadWikiFile(data: String) = {
    val stream = this.getClass.getResourceAsStream(data)
    try {
      Source.fromInputStream(stream, "utf-8").getLines().toList
    } finally {
      stream.close()
    }
  }

  private[wikipedia] def readXML(xml: List[String]) = xml.map { line =>
    val subs = "</title><text>"
    val i = line.indexOf(subs)
    val title = line.substring(14, i)
    val text  = line.substring(i + subs.length, line.length-16)
    WikipediaArticle(title, text)
  }

  val articles = readXML(xmlPages1) ++ readXML(xmlPages2)

}
