package pubsub.command

import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.InputStream

import pubsub.Client

class CommandReader(inStream: InputStream, client: Client) {
  val inputBuffer = new BufferedReader(new InputStreamReader(inStream))

  def fetchCommand(): Command = {
    val line = inputBuffer.readLine()

    if (line == null || line.startsWith("leave")) {
      EndOfClient(client)
    }
    else {
      val quoteIndex = line.indexOf('\'')
      val hasPayload = quoteIndex != -1
      val parts =
        if(!hasPayload) {
          line.split(" ").toList
        } else {
          val (command, payload) = line.splitAt(quoteIndex)
          command.split(" ").toList :+ payload
        }

      parts match {
        case "subscribe" :: topic :: Nil   => Subscribe(topic, client)
        case "unsubscribe" :: topic :: Nil => Unsubscribe(topic, client)
        case "rename" :: newName :: Nil    => Rename(newName, client)

        case "publish" :: topic :: msg :: Nil if hasPayload && msg != "\'" =>
          var message = msg
          while(!message.endsWith("\'")) {
            message += "\n" + inputBuffer.readLine()
          }
          Publish(topic, message, client)

        case _ => MalformedCommand(client)
      }
    }
  }
}
